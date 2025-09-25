use crate::{adapter::StreamKind, limiter::{self, RateLimiter}};

use super::{
    super::{
        Exchange, Kline, MarketKind, Ticker, TickerInfo, TickerStats, Timeframe, Trade,
        connect::{State, connect_ws},
        de_string_to_f32, de_string_to_u64,
        limiter::HTTP_CLIENT,
    },
    Connection, Event, StreamError,
};

use super::super::depth::{DepthPayload, DepthUpdate, LocalDepthCache, Order};

use fastwebsockets::{Frame, OpCode};
use iced_futures::{
    futures::{SinkExt, Stream, channel::mpsc},
    stream,
};
use serde::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, sync::LazyLock, time::Duration};
use tokio::sync::Mutex;

const WS_DOMAIN: &str = "ws.okx.com";

const LIMIT: usize = 20;

const REFILL_RATE: Duration = Duration::from_secs(2);
const LIMITER_BUFFER_PCT: f32 = 0.05;

static OKEX_LIMITER: LazyLock<Mutex<OkexLimiter>> =
    LazyLock::new(|| Mutex::new(OkexLimiter::new(LIMIT, REFILL_RATE)));

pub struct OkexLimiter {
    bucket: limiter::FixedWindowBucket,
}

impl OkexLimiter {
    pub fn new(limit: usize, refill_rate: Duration) -> Self {
        let effective_limit = (limit as f32 * (1.0 - LIMITER_BUFFER_PCT)) as usize;
        Self {
            bucket: limiter::FixedWindowBucket::new(effective_limit, refill_rate),
        }
    }
}

impl RateLimiter for OkexLimiter {
    fn prepare_request(&mut self, weight: usize) -> Option<Duration> {
        self.bucket.calculate_wait_time(weight)
    }

    fn update_from_response(&mut self, _response: &reqwest::Response, weight: usize) {
        self.bucket.consume_tokens(weight);
    }

    fn should_exit_on_response(&self, response: &reqwest::Response) -> bool {
        response.status() == 429
    }
}

#[derive(Deserialize, Debug)]
struct SonicTrade {
    #[serde(rename = "ts", deserialize_with = "de_string_to_u64")]
    pub time: u64,
    #[serde(rename = "px", deserialize_with = "de_string_to_f32")]
    pub price: f32,
    #[serde(rename = "sz", deserialize_with = "de_string_to_f32")]
    pub qty: f32,
    #[serde(rename = "side")]
    pub is_sell: String,
}

struct SonicDepth {
    pub update_id: u64,
    pub bids: Vec<Order>,
    pub asks: Vec<Order>,
}

enum StreamData {
    Trade(Vec<SonicTrade>),
    Depth(SonicDepth, String, u64),
}

fn feed_de(slice: &[u8], _ticker: Ticker) -> Result<StreamData, StreamError> {
    let v: Value =
        serde_json::from_slice(slice).map_err(|e| StreamError::ParseError(e.to_string()))?;

    let mut channel = String::new();
    if let Some(arg) = v.get("arg")
        && let Some(ch) = arg.get("channel").and_then(|c| c.as_str())
    {
        channel = ch.to_string();
    }

    if let Some(action) = v.get("action").and_then(|a| a.as_str())
        && let Some(data_arr) = v.get("data")
        && let Some(first) = data_arr.get(0)
    {
        let bids: Vec<Order> = if let Some(b) = first.get("bids") {
            serde_json::from_value(b.clone())
                .map_err(|e| StreamError::ParseError(e.to_string()))?
        } else {
            Vec::new()
        };
        let asks: Vec<Order> = if let Some(a) = first.get("asks") {
            serde_json::from_value(a.clone())
                .map_err(|e| StreamError::ParseError(e.to_string()))?
        } else {
            Vec::new()
        };

        let seq_id = first.get("seqId").and_then(|s| s.as_u64()).unwrap_or(0);

        let time = first
            .get("ts")
            .and_then(|t| t.as_str())
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(0);

        let depth = SonicDepth {
            update_id: seq_id,
            bids,
            asks,
        };

        match channel.as_str() {
            "books" => {
                let dtype = if action == "update" {
                    "delta"
                } else {
                    "snapshot"
                };
                return Ok(StreamData::Depth(depth, dtype.to_string(), time));
            }
            _ => {
                return Err(StreamError::ParseError(
                    "Depth message for non-depth subscription".to_string(),
                ));
            }
        }
    }

    if let Some(data_arr) = v.get("data") {
        let trades: Vec<SonicTrade> = serde_json::from_value(data_arr.clone())
            .map_err(|e| StreamError::ParseError(e.to_string()))?;

        if matches!(channel.as_str(), "trades" | "trade") {
            return Ok(StreamData::Trade(trades));
        }
    }

    Err(StreamError::ParseError("Unknown data".to_string()))
}

async fn try_connect(
    streams: &Value,
    exchange: Exchange,
    output: &mut mpsc::Sender<Event>,
    topic: &str,
) -> State {
    let url = format!("wss://{WS_DOMAIN}/ws/v5/{topic}");

    match connect_ws(WS_DOMAIN, &url).await {
        Ok(mut websocket) => {
            if let Err(e) = websocket
                .write_frame(Frame::text(fastwebsockets::Payload::Borrowed(
                    streams.to_string().as_bytes(),
                )))
                .await
            {
                let _ = output
                    .send(Event::Disconnected(
                        exchange,
                        format!("Failed subscribing: {e}"),
                    ))
                    .await;
                return State::Disconnected;
            }

            let _ = output.send(Event::Connected(exchange, Connection)).await;
            State::Connected(websocket)
        }
        Err(err) => {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            let _ = output
                .send(Event::Disconnected(
                    exchange,
                    format!("Failed to connect: {err}"),
                ))
                .await;
            State::Disconnected
        }
    }
}

pub fn connect_market_stream(ticker: Ticker) -> impl Stream<Item = Event> {
    stream::channel(100, async move |mut output| {
        let mut state: State = State::Disconnected;

        let (symbol_str, _market_type) = ticker.to_full_symbol_and_type();
        let exchange = ticker.exchange;

        let inst_id = symbol_str.replace('_', "-");

        let subscribe_message = serde_json::json!({
            "op": "subscribe",
            "args": [
                { "channel": "trades", "instId": inst_id },
                { "channel": "books",  "instId": inst_id },
            ],
        });

        let mut trades_buffer: Vec<Trade> = vec![];
        let mut orderbook = LocalDepthCache::default();

        loop {
            match &mut state {
                State::Disconnected => {
                    state = try_connect(&subscribe_message, exchange, &mut output, "public").await;
                }
                State::Connected(ws) => match ws.read_frame().await {
                    Ok(msg) => match msg.opcode {
                        OpCode::Text => {
                            if let Ok(data) = feed_de(&msg.payload[..], ticker) {
                                match data {
                                    StreamData::Trade(de_trade_vec) => {
                                        for de_trade in &de_trade_vec {
                                            let trade = Trade::new(
                                                de_trade.time,
                                                de_trade.is_sell == "sell"
                                                    || de_trade.is_sell == "SELL",
                                                de_trade.price,
                                                de_trade.qty,
                                            );
                                            trades_buffer.push(trade);
                                        }
                                    }
                                    StreamData::Depth(de_depth, data_type, time) => {
                                        let depth = DepthPayload {
                                            last_update_id: de_depth.update_id,
                                            time,
                                            bids: de_depth
                                                .bids
                                                .iter()
                                                .map(|x| Order {
                                                    price: x.price,
                                                    qty: x.qty,
                                                })
                                                .collect(),
                                            asks: de_depth
                                                .asks
                                                .iter()
                                                .map(|x| Order {
                                                    price: x.price,
                                                    qty: x.qty,
                                                })
                                                .collect(),
                                        };

                                        if (data_type == "snapshot") || (depth.last_update_id == 1)
                                        {
                                            orderbook.update(DepthUpdate::Snapshot(depth));
                                        } else if data_type == "delta" {
                                            orderbook.update(DepthUpdate::Diff(depth));

                                            let _ = output
                                                .send(Event::DepthReceived(
                                                    StreamKind::DepthAndTrades { ticker },
                                                    time,
                                                    orderbook.depth.clone(),
                                                    std::mem::take(&mut trades_buffer)
                                                        .into_boxed_slice(),
                                                ))
                                                .await;
                                        }
                                    }
                                }
                            }
                        }
                        OpCode::Close => {
                            state = State::Disconnected;
                            let _ = output
                                .send(Event::Disconnected(
                                    exchange,
                                    "Connection closed".to_string(),
                                ))
                                .await;
                        }
                        _ => {}
                    },
                    Err(e) => {
                        state = State::Disconnected;
                        let _ = output
                            .send(Event::Disconnected(
                                exchange,
                                "Error reading frame: ".to_string() + &e.to_string(),
                            ))
                            .await;
                    }
                },
            }
        }
    })
}

pub fn connect_kline_stream(
    streams: Vec<(Ticker, Timeframe)>,
    market_type: MarketKind,
) -> impl Stream<Item = Event> {
    stream::channel(100, async move |mut output| {
        let mut state = State::Disconnected;

        let mut args: Vec<Value> = Vec::with_capacity(streams.len());
        let mut lookup: HashMap<String, (Ticker, Timeframe)> = HashMap::new();
        for (ticker, timeframe) in &streams {
            if let Some(bar) = timeframe_to_okx_bar(*timeframe) {
                let (symbol, _mt) = ticker.to_full_symbol_and_type();
                let inst_id = symbol.replace('_', "-");
                args.push(serde_json::json!({
                    "channel": format!("candle{bar}"),
                    "instId": inst_id,
                }));
                lookup.insert(inst_id, (*ticker, *timeframe));
            }
        }

        let exchange = match market_type {
            MarketKind::Spot => Exchange::OkxSpot,
            MarketKind::LinearPerps => Exchange::OkxLinear,
            MarketKind::InversePerps => Exchange::OkxInverse,
        };

        let subscribe_message = serde_json::json!({
            "op": "subscribe",
            "args": args,
        });

        loop {
            match &mut state {
                State::Disconnected => {
                    state =
                        try_connect(&subscribe_message, exchange, &mut output, "business").await;
                }
                State::Connected(ws) => match ws.read_frame().await {
                    Ok(msg) => match msg.opcode {
                        OpCode::Text => {
                            if let Ok(v) = serde_json::from_slice::<Value>(&msg.payload[..]) {
                                let channel = v["arg"]["channel"].as_str().unwrap_or("");
                                if !channel.starts_with("candle") {
                                    continue;
                                }

                                let inst = match v["arg"]["instId"].as_str() {
                                    Some(s) => s,
                                    None => continue,
                                };
                                let (ticker, timeframe) = match lookup.get(inst) {
                                    Some(t) => *t,
                                    None => continue,
                                };

                                if let Some(data) = v.get("data").and_then(|d| d.as_array()) {
                                    for row in data {
                                        let time = row
                                            .get(0)
                                            .and_then(|x| x.as_str())
                                            .and_then(|s| s.parse::<u64>().ok());
                                        let open = row
                                            .get(1)
                                            .and_then(|x| x.as_str())
                                            .and_then(|s| s.parse::<f32>().ok());
                                        let high = row
                                            .get(2)
                                            .and_then(|x| x.as_str())
                                            .and_then(|s| s.parse::<f32>().ok());
                                        let low = row
                                            .get(3)
                                            .and_then(|x| x.as_str())
                                            .and_then(|s| s.parse::<f32>().ok());
                                        let close = row
                                            .get(4)
                                            .and_then(|x| x.as_str())
                                            .and_then(|s| s.parse::<f32>().ok());
                                        let volume = row
                                            .get(5)
                                            .and_then(|x| x.as_str())
                                            .and_then(|s| s.parse::<f32>().ok());

                                        let (ts, open, high, low, close) =
                                            match (time, open, high, low, close) {
                                                (
                                                    Some(ts),
                                                    Some(open),
                                                    Some(high),
                                                    Some(low),
                                                    Some(close),
                                                ) => (ts, open, high, low, close),
                                                _ => continue,
                                            };

                                        let total_volume = volume.unwrap_or(0.0);
                                        let (buy_volume, sell_volume) =
                                            calculate_safe_buy_sell_volumes(
                                                open,
                                                close,
                                                total_volume,
                                            );

                                        let kline = Kline {
                                            time: ts,
                                            open,
                                            high,
                                            low,
                                            close,
                                            volume: (buy_volume, sell_volume),
                                        };
                                        let _ = output
                                            .send(Event::KlineReceived(
                                                StreamKind::Kline { ticker, timeframe },
                                                kline,
                                            ))
                                            .await;
                                    }
                                }
                            }
                        }
                        OpCode::Close => {
                            state = State::Disconnected;
                            let _ = output
                                .send(Event::Disconnected(
                                    exchange,
                                    "Connection closed".to_string(),
                                ))
                                .await;
                        }
                        _ => {}
                    },
                    Err(e) => {
                        state = State::Disconnected;
                        let _ = output
                            .send(Event::Disconnected(
                                exchange,
                                "Error reading frame: ".to_string() + &e.to_string(),
                            ))
                            .await;
                    }
                },
            }
        }
    })
}

// Note: OKX volumes are used as provided by the API without conversions

fn okx_inst_type(m: MarketKind) -> &'static str {
    match m {
        MarketKind::Spot => "SPOT",
        MarketKind::LinearPerps | MarketKind::InversePerps => "SWAP",
    }
}

fn timeframe_to_okx_bar(tf: Timeframe) -> Option<&'static str> {
    Some(match tf {
        Timeframe::M1 => "1m",
        Timeframe::M3 => "3m",
        Timeframe::M5 => "5m",
        Timeframe::M15 => "15m",
        Timeframe::M30 => "30m",
        Timeframe::H1 => "1H",
        Timeframe::H2 => "2H",
        Timeframe::H4 => "4H",
        _ => return None,
    })
}

pub async fn fetch_ticksize(
    market_type: MarketKind,
) -> Result<std::collections::HashMap<Ticker, Option<TickerInfo>>, StreamError> {
    let inst_type = okx_inst_type(market_type);
    let url = format!(
        "https://www.okx.com/api/v5/public/instruments?instType={}",
        inst_type
    );

    let response_text = HTTP_CLIENT
        .get(&url)
        .send()
        .await
        .map_err(StreamError::FetchError)?
        .text()
        .await
        .map_err(StreamError::FetchError)?;

    let doc: Value = serde_json::from_str(&response_text)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    let list = doc["data"]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Result list is not an array".to_string()))?;

    let exchange = match market_type {
        MarketKind::Spot => Exchange::OkxSpot,
        MarketKind::LinearPerps => Exchange::OkxLinear,
        MarketKind::InversePerps => Exchange::OkxInverse,
    };

    let mut map = std::collections::HashMap::new();

    for item in list {
        // Convert OKX dashed instId to internal underscore symbol
        let symbol = match item["instId"].as_str() {
            Some(s) => s,
            None => continue,
        };
        let symbol = symbol.replace('-', "_");

        if item["state"].as_str().unwrap_or("") != "live" {
            continue;
        }

        let accept = match market_type {
            MarketKind::Spot => item["quoteCcy"].as_str() == Some("USDT"),
            MarketKind::LinearPerps => {
                item["ctType"].as_str() == Some("linear")
                    && (item["settleCcy"].as_str() == Some("USDT"))
            }
            MarketKind::InversePerps => item["ctType"].as_str() == Some("inverse"),
        };
        if !accept {
            continue;
        }

        // Symbol allowlist handled by ticker creation and consumer side

        let min_ticksize = item["tickSz"]
            .as_str()
            .and_then(|s| s.parse::<f32>().ok())
            .ok_or_else(|| StreamError::ParseError("Tick size not found".to_string()))?;
        let min_qty = item["lotSz"]
            .as_str()
            .and_then(|s| s.parse::<f32>().ok())
            .ok_or_else(|| StreamError::ParseError("Lot size not found".to_string()))?;
        let ticker = Ticker::new(symbol, exchange);
        let info = TickerInfo { ticker, min_ticksize, min_qty };

        map.insert(ticker, Some(info));
    }

    Ok(map)
}

pub async fn fetch_ticker_prices(
    market_type: MarketKind,
) -> Result<std::collections::HashMap<Ticker, TickerStats>, StreamError> {
    let inst_type = okx_inst_type(market_type);
    let url = format!(
        "https://www.okx.com/api/v5/market/tickers?instType={}",
        inst_type
    );

    let response_text =
        limiter::http_request_with_limiter(&url, &OKEX_LIMITER, 1, None, None).await?;

    let doc: Value = serde_json::from_str(&response_text)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    let list = doc["data"]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Result list is not an array".to_string()))?;

    let exchange = match market_type {
        MarketKind::Spot => Exchange::OkxSpot,
        MarketKind::LinearPerps => Exchange::OkxLinear,
        MarketKind::InversePerps => Exchange::OkxInverse,
    };

    let mut map = std::collections::HashMap::new();

    for item in list {
        let symbol = match item["instId"].as_str() {
            Some(s) => s,
            None => continue,
        };
        let symbol = symbol.replace('-', "_");

        // Symbol allowlist handled by ticker creation and consumer side

        let last_trade_price = item["last"].as_str().and_then(|s| s.parse::<f32>().ok());
        let open24h = item["open24h"].as_str().and_then(|s| s.parse::<f32>().ok());

        let Some(vol24h) = item["volCcy24h"]
            .as_str()
            .and_then(|s| s.parse::<f32>().ok())
        else {
            continue;
        };

        let (last_price, previous_daily_open) =
            if let (Some(last), Some(previous_daily_open)) = (last_trade_price, open24h) {
                (last, previous_daily_open)
            } else {
                continue;
            };
        let daily_price_chg = if previous_daily_open > 0.0 {
            (last_price - previous_daily_open) / previous_daily_open * 100.0
        } else {
            0.0
        };

        let volume_usd =
            if market_type == MarketKind::LinearPerps || market_type == MarketKind::InversePerps {
                vol24h * last_price
            } else {
                vol24h
            };

        map.insert(
            Ticker::new(&symbol, exchange),
            TickerStats {
                mark_price: last_price,
                daily_price_chg,
                daily_volume: volume_usd,
            },
        );
    }

    Ok(map)
}

pub async fn fetch_klines(
    ticker: Ticker,
    timeframe: Timeframe,
    range: Option<(u64, u64)>,
) -> Result<Vec<Kline>, StreamError> {
    let (symbol_str, _market) = ticker.to_full_symbol_and_type();

    let bar = timeframe_to_okx_bar(timeframe).ok_or_else(|| {
        StreamError::InvalidRequest(format!("Unsupported timeframe: {timeframe}"))
    })?;

    let inst_id = symbol_str.replace('_', "-");
    let mut url = format!(
        "https://www.okx.com/api/v5/market/history-candles?instId={}&bar={}&limit={}",
        inst_id,
        bar,
        match range {
            Some((start, end)) => {
                ((end - start) / timeframe.to_milliseconds()).clamp(1, 1000)
            }
            None => 200,
        }
    );

    if let Some((start, end)) = range {
        url.push_str(&format!("&before={start}&after={end}"));
    }

    let response_text =
        limiter::http_request_with_limiter(&url, &OKEX_LIMITER, 1, None, None).await?;

    let doc: Value = serde_json::from_str(&response_text)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    let list = doc["data"]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Kline result is not an array".to_string()))?;

    let mut klines: Vec<Kline> = Vec::with_capacity(list.len());

    for row in list {
        let time = row
            .get(0)
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<u64>().ok());
        let open = row
            .get(1)
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f32>().ok());
        let high = row
            .get(2)
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f32>().ok());
        let low = row
            .get(3)
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f32>().ok());
        let close = row
            .get(4)
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f32>().ok());
        let volume = row
            .get(5)
            .and_then(|v| v.as_str())
            .and_then(|s| s.parse::<f32>().ok());

        let (ts, open, high, low, close) = match (time, open, high, low, close) {
            (Some(ts), Some(o), Some(h), Some(l), Some(c)) => (ts, o, h, l, c),
            _ => continue,
        };
        let total_volume = volume.unwrap_or(0.0);
        let (buy_volume, sell_volume) =
            calculate_safe_buy_sell_volumes(open, close, total_volume);

        klines.push(Kline {
            time: ts,
            open,
            high,
            low,
            close,
            volume: (buy_volume, sell_volume),
        });
    }

    klines.sort_by_key(|k| k.time);
    Ok(klines)
}
 
fn calculate_safe_buy_sell_volumes(open: f32, close: f32, total_volume: f32) -> (f32, f32) {
    if total_volume <= 0.0 || !total_volume.is_finite() {
        return (0.0, 0.0);
    }

    if open <= 0.0 || !open.is_finite() || !close.is_finite() {
        let half_volume = total_volume * 0.5;
        return (half_volume, half_volume);
    }

    let price_change = close - open;
    let price_change_ratio = price_change / open;

    let capped_ratio = price_change_ratio.clamp(-0.2, 0.2);

    let buy_ratio = 0.5 + (capped_ratio * 2.0).tanh() * 0.15;
    let buy_ratio = buy_ratio.clamp(0.2, 0.8);

    let buy_volume = total_volume * buy_ratio;
    let sell_volume = total_volume * (1.0 - buy_ratio);

    if buy_volume.is_finite() && sell_volume.is_finite() && buy_volume >= 0.0 && sell_volume >= 0.0 {
        (buy_volume, sell_volume)
    } else {
        let half_volume = total_volume * 0.5;
        (half_volume, half_volume)
    }
}
