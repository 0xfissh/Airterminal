use serde::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, sync::LazyLock, time::Duration};

use fastwebsockets::{Frame, OpCode};
use hyper::upgrade::Upgraded;
use hyper_util::rt::TokioIo;

use iced_futures::{
    futures::{SinkExt, Stream, channel::mpsc},
    stream,
};

use super::{
    super::{
        Exchange, Kline, MarketKind, OpenInterest, StreamKind, Ticker, TickerInfo, TickerStats,
        Timeframe, Trade,
        connect::{State, connect_ws},
        de_string_to_f32, de_string_to_u64,
        depth::{DepthPayload, DepthUpdate, LocalDepthCache, Order},
    },
    Connection, Event, StreamError,
};

use crate::limiter::{self, http_request_with_limiter, RateLimiter};
use tokio::sync::Mutex;

const WS_DOMAIN: &str = "ws.okx.com";

// OKX public REST is not heavily rate-limited per key; use a conservative fixed window
const LIMIT: usize = 20;
const REFILL_RATE: Duration = Duration::from_secs(2);
const LIMITER_BUFFER_PCT: f32 = 0.05;

static OKX_LIMITER: LazyLock<Mutex<OkxLimiter>> =
    LazyLock::new(|| Mutex::new(OkxLimiter::new(LIMIT, REFILL_RATE)));

pub struct OkxLimiter {
    bucket: limiter::FixedWindowBucket,
}

impl OkxLimiter {
    pub fn new(limit: usize, refill_rate: Duration) -> Self {
        let effective_limit = (limit as f32 * (1.0 - LIMITER_BUFFER_PCT)) as usize;
        Self {
            bucket: limiter::FixedWindowBucket::new(effective_limit, refill_rate),
        }
    }
}

impl RateLimiter for OkxLimiter {
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

fn exchange_from_market_type(market: MarketKind) -> Exchange {
    match market {
        MarketKind::Spot => Exchange::OkxSpot,
        MarketKind::LinearPerps => Exchange::OkxLinear,
        MarketKind::InversePerps => Exchange::OkxInverse,
    }
}

#[derive(Deserialize, Debug)]
struct DeTrade {
    #[serde(rename = "ts", deserialize_with = "de_string_to_u64")]
    time: u64,
    #[serde(rename = "px", deserialize_with = "de_string_to_f32")]
    price: f32,
    #[serde(rename = "sz", deserialize_with = "de_string_to_f32")]
    qty: f32,
    #[serde(rename = "side")]
    side: String,
}

struct DeDepth {
    update_id: u64,
    time: u64,
    bids: Vec<Order>,
    asks: Vec<Order>,
    data_type: String, // snapshot | update
}

enum StreamData {
    Trade(Vec<DeTrade>),
    Depth(DeDepth),
}

fn parse_ws_message(slice: &[u8]) -> Result<StreamData, StreamError> {
    let v: Value = serde_json::from_slice(slice)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    // Depth messages include action + data[0]
    if let (Some(action), Some(data_arr)) = (v.get("action"), v.get("data")) {
        let action = action.as_str().unwrap_or("").to_string();
        if let Some(first) = data_arr.get(0) {
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

            return Ok(StreamData::Depth(DeDepth {
                update_id: seq_id,
                time,
                bids,
                asks,
                data_type: if action == "update" { "delta".into() } else { "snapshot".into() },
            }));
        }
    }

    // Trade messages have data as array of trades
    if let Some(data_arr) = v.get("data") {
        let trades: Vec<DeTrade> = serde_json::from_value(data_arr.clone())
            .map_err(|e| StreamError::ParseError(e.to_string()))?;
        return Ok(StreamData::Trade(trades));
    }

    Err(StreamError::ParseError("Unknown ws message".to_string()))
}

async fn connect(topic: &str) -> Result<fastwebsockets::FragmentCollector<TokioIo<Upgraded>>, StreamError> {
    let url = format!("wss://{}/ws/v5/{}", WS_DOMAIN, topic);
    connect_ws(WS_DOMAIN, &url).await
}

async fn try_connect(
    subscribe_message: &Value,
    exchange: Exchange,
    topic: &str,
    output: &mut mpsc::Sender<Event>,
) -> State {
    match connect(topic).await {
        Ok(mut websocket) => {
            if let Err(e) = websocket
                .write_frame(Frame::text(fastwebsockets::Payload::Borrowed(
                    subscribe_message.to_string().as_bytes(),
                )))
                .await
            {
                let _ = output
                    .send(Event::Disconnected(exchange, format!("Failed subscribing: {e}")))
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

fn inst_id_from_internal(symbol: &str) -> String {
    if let Some(stripped) = symbol.strip_suffix("_PERP") {
        format!("{}-SWAP", stripped.replace("_", "-"))
    } else {
        symbol.replace("_", "-")
    }
}

fn internal_from_inst_id(inst_id: &str) -> String {
    if let Some(stripped) = inst_id.strip_suffix("-SWAP") {
        format!("{}_PERP", stripped.replace("-", "_"))
    } else {
        inst_id.replace("-", "_")
    }
}

pub fn connect_market_stream(ticker: Ticker) -> impl Stream<Item = Event> {
    stream::channel(100, async move |mut output| {
        let mut state: State = State::Disconnected;

        let (symbol_str, _mt) = ticker.to_full_symbol_and_type();
        let inst_id = inst_id_from_internal(&symbol_str);
        let exchange = ticker.exchange;

        let subscribe_message = serde_json::json!({
            "op": "subscribe",
            "args": [
                { "channel": "trades", "instId": inst_id },
                { "channel": "books",  "instId": inst_id },
            ]
        });

        let mut trades_buffer: Vec<Trade> = Vec::new();
        let mut orderbook = LocalDepthCache::default();

        loop {
            match &mut state {
                State::Disconnected => {
                    state = try_connect(&subscribe_message, exchange, "public", &mut output).await;
                }
                State::Connected(ws) => match ws.read_frame().await {
                    Ok(msg) => match msg.opcode {
                        OpCode::Text => {
                            if let Ok(data) = parse_ws_message(&msg.payload[..]) {
                                match data {
                                    StreamData::Trade(de_trades) => {
                                        for t in &de_trades {
                                            trades_buffer.push(Trade::new(
                                                t.time,
                                                t.side.eq_ignore_ascii_case("sell"),
                                                t.price,
                                                t.qty,
                                            ));
                                        }
                                    }
                                    StreamData::Depth(d) => {
                                        let depth = DepthPayload {
                                            last_update_id: d.update_id,
                                            time: d.time,
                                            bids: d.bids,
                                            asks: d.asks,
                                        };

                                        if d.data_type == "snapshot" || depth.last_update_id == 1 {
                                            orderbook.update(DepthUpdate::Snapshot(depth));
                                        } else if d.data_type == "delta" {
                                            orderbook.update(DepthUpdate::Diff(depth));

                                            let _ = output
                                                .send(Event::DepthReceived(
                                                    StreamKind::DepthAndTrades { ticker },
                                                    orderbook.time,
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
        let mut state: State = State::Disconnected;

        let exchange = exchange_from_market_type(market_type);

        let mut args: Vec<Value> = Vec::with_capacity(streams.len());
        let mut lookup: HashMap<String, (Ticker, Timeframe)> = HashMap::new();
        for (ticker, timeframe) in &streams {
            if let Some(bar) = timeframe_to_okx_bar(*timeframe) {
                let inst_id = inst_id_from_internal(&ticker.to_full_symbol_and_type().0);
                args.push(serde_json::json!({
                    "channel": format!("candle{}", bar),
                    "instId": inst_id,
                }));
                lookup.insert(inst_id, (*ticker, *timeframe));
            }
        }

        let subscribe_message = serde_json::json!({
            "op": "subscribe",
            "args": args,
        });

        loop {
            match &mut state {
                State::Disconnected => {
                    state = try_connect(&subscribe_message, exchange, "business", &mut output).await;
                }
                State::Connected(ws) => match ws.read_frame().await {
                    Ok(msg) => match msg.opcode {
                        OpCode::Text => {
                            if let Ok(v) = serde_json::from_slice::<Value>(&msg.payload[..]) {
                                let channel = v["arg"]["channel"].as_str().unwrap_or("");
                                if !channel.starts_with("candle") { continue; }

                                let inst_id = match v["arg"]["instId"].as_str() { Some(s) => s.to_string(), None => continue };
                                let Some((ticker, timeframe)) = lookup.get(&inst_id).copied() else { continue };

                                if let Some(data) = v.get("data").and_then(|d| d.as_array()) {
                                    for row in data {
                                        let time = row.get(0).and_then(|x| x.as_str()).and_then(|s| s.parse::<u64>().ok());
                                        let open = row.get(1).and_then(|x| x.as_str()).and_then(|s| s.parse::<f32>().ok());
                                        let high = row.get(2).and_then(|x| x.as_str()).and_then(|s| s.parse::<f32>().ok());
                                        let low = row.get(3).and_then(|x| x.as_str()).and_then(|s| s.parse::<f32>().ok());
                                        let close = row.get(4).and_then(|x| x.as_str()).and_then(|s| s.parse::<f32>().ok());
                                        let volume = row.get(5).and_then(|x| x.as_str()).and_then(|s| s.parse::<f32>().ok());

                                        let (ts, open, high, low, close) = match (time, open, high, low, close) {
                                            (Some(ts), Some(o), Some(h), Some(l), Some(c)) => (ts, o, h, l, c),
                                            _ => continue,
                                        };

                                        let total_volume = volume.unwrap_or(0.0);
                                        let (buy_volume, sell_volume) = calculate_safe_buy_sell_volumes(open, close, total_volume);

                                        let kline = Kline { time: ts, open, high, low, close, volume: (buy_volume, sell_volume) };
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

fn calculate_safe_buy_sell_volumes(open: f32, close: f32, total_volume: f32) -> (f32, f32) {
    if total_volume <= 0.0 || !total_volume.is_finite() { return (0.0, 0.0); }
    if open <= 0.0 || !open.is_finite() || !close.is_finite() {
        let half = total_volume * 0.5; return (half, half);
    }
    let price_change_ratio = (close - open) / open;
    let capped_ratio = price_change_ratio.clamp(-0.2, 0.2);
    let mut buy_ratio = 0.5 + (capped_ratio * 2.0).tanh() * 0.15;
    buy_ratio = buy_ratio.clamp(0.2, 0.8);
    let buy = total_volume * buy_ratio;
    let sell = total_volume - buy;
    if buy.is_finite() && sell.is_finite() && buy >= 0.0 && sell >= 0.0 { (buy, sell) } else { let half = total_volume * 0.5; (half, half) }
}

fn okx_inst_type(market: MarketKind) -> &'static str {
    match market {
        MarketKind::Spot => "SPOT",
        MarketKind::LinearPerps | MarketKind::InversePerps => "SWAP",
    }
}

pub async fn fetch_ticksize(
    market_type: MarketKind,
) -> Result<HashMap<Ticker, Option<TickerInfo>>, StreamError> {
    let inst_type = okx_inst_type(market_type);
    let url = format!(
        "https://www.okx.com/api/v5/public/instruments?instType={}",
        inst_type
    );

    let response_text = http_request_with_limiter(&url, &OKX_LIMITER, 1, None, None).await?;

    let doc: Value = serde_json::from_str(&response_text)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    let list = doc["data"]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Result list is not an array".to_string()))?;

    let exchange = exchange_from_market_type(market_type);

    let mut map = HashMap::new();
    for item in list {
        let Some(inst_id) = item["instId"].as_str() else { continue };
        if item["state"].as_str().unwrap_or("") != "live" { continue; }

        // Filter by quote/settle currency depending on market
        let accept = match market_type {
            MarketKind::Spot => item["quoteCcy"].as_str() == Some("USDT"),
            MarketKind::LinearPerps => item["ctType"].as_str() == Some("linear") && item["settleCcy"].as_str() == Some("USDT"),
            MarketKind::InversePerps => item["ctType"].as_str() == Some("inverse"),
        };
        if !accept { continue; }

        let min_ticksize = item["tickSz"].as_str().and_then(|s| s.parse::<f32>().ok())
            .ok_or_else(|| StreamError::ParseError("Tick size not found".to_string()))?;
        let min_qty = item["lotSz"].as_str().and_then(|s| s.parse::<f32>().ok())
            .ok_or_else(|| StreamError::ParseError("Lot size not found".to_string()))?;

        let internal_symbol = internal_from_inst_id(inst_id);
        let ticker = Ticker::new(internal_symbol, exchange);
        map.insert(ticker, Some(TickerInfo { ticker, min_ticksize, min_qty }));
    }

    Ok(map)
}

pub async fn fetch_ticker_prices(
    market_type: MarketKind,
) -> Result<HashMap<Ticker, TickerStats>, StreamError> {
    let inst_type = okx_inst_type(market_type);
    let url = format!(
        "https://www.okx.com/api/v5/market/tickers?instType={}",
        inst_type
    );

    let response_text = http_request_with_limiter(&url, &OKX_LIMITER, 1, None, None).await?;

    let doc: Value = serde_json::from_str(&response_text)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    let list = doc["data"]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Result list is not an array".to_string()))?;

    let exchange = exchange_from_market_type(market_type);
    let mut map = HashMap::new();

    for item in list {
        let Some(inst_id) = item["instId"].as_str() else { continue };

        let last_price = item["last"].as_str().and_then(|s| s.parse::<f32>().ok());
        let open24h = item["open24h"].as_str().and_then(|s| s.parse::<f32>().ok());
        let Some(vol24h) = item["volCcy24h"].as_str().and_then(|s| s.parse::<f32>().ok()) else { continue };

        let (last_price, previous_daily_open) = match (last_price, open24h) {
            (Some(l), Some(o)) => (l, o),
            _ => continue,
        };

        let daily_price_chg = if previous_daily_open > 0.0 {
            (last_price - previous_daily_open) / previous_daily_open * 100.0
        } else { 0.0 };

        let volume_usd = if matches!(market_type, MarketKind::LinearPerps | MarketKind::InversePerps) {
            vol24h * last_price
        } else {
            vol24h
        };

        let internal_symbol = internal_from_inst_id(inst_id);
        map.insert(
            Ticker::new(internal_symbol, exchange),
            TickerStats { mark_price: last_price, daily_price_chg, daily_volume: volume_usd },
        );
    }

    Ok(map)
}

pub async fn fetch_klines(
    ticker: Ticker,
    timeframe: Timeframe,
    range: Option<(u64, u64)>,
) -> Result<Vec<Kline>, StreamError> {
    let (symbol_str, _mt) = ticker.to_full_symbol_and_type();
    let inst_id = inst_id_from_internal(&symbol_str);
    let bar = timeframe_to_okx_bar(timeframe)
        .ok_or_else(|| StreamError::InvalidRequest(format!("Unsupported timeframe: {timeframe}")))?;

    let mut url = format!(
        "https://www.okx.com/api/v5/market/history-candles?instId={}&bar={}&limit={}",
        inst_id,
        bar,
        match range { Some((start, end)) => ((end - start) / timeframe.to_milliseconds()).clamp(1, 1000), None => 200 }
    );

    if let Some((start, end)) = range { url.push_str(&format!("&before={start}&after={end}")); }

    let response_text = http_request_with_limiter(&url, &OKX_LIMITER, 1, None, None).await?;

    let doc: Value = serde_json::from_str(&response_text)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;
    let list = doc["data"]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Kline result is not an array".to_string()))?;

    let mut klines: Vec<Kline> = Vec::with_capacity(list.len());
    for row in list {
        let time = row.get(0).and_then(|v| v.as_str()).and_then(|s| s.parse::<u64>().ok());
        let open = row.get(1).and_then(|v| v.as_str()).and_then(|s| s.parse::<f32>().ok());
        let high = row.get(2).and_then(|v| v.as_str()).and_then(|s| s.parse::<f32>().ok());
        let low = row.get(3).and_then(|v| v.as_str()).and_then(|s| s.parse::<f32>().ok());
        let close = row.get(4).and_then(|v| v.as_str()).and_then(|s| s.parse::<f32>().ok());
        let volume = row.get(5).and_then(|v| v.as_str()).and_then(|s| s.parse::<f32>().ok()).unwrap_or(0.0);

        let (ts, open, high, low, close) = match (time, open, high, low, close) { (Some(ts), Some(o), Some(h), Some(l), Some(c)) => (ts, o, h, l, c), _ => continue };
        let (buy_volume, sell_volume) = calculate_safe_buy_sell_volumes(open, close, volume);

        klines.push(Kline { time: ts, open, high, low, close, volume: (buy_volume, sell_volume) });
    }

    klines.sort_by_key(|k| k.time);
    Ok(klines)
}

pub async fn fetch_historical_oi(
    _ticker: Ticker,
    _range: Option<(u64, u64)>,
    _period: Timeframe,
) -> Result<Vec<OpenInterest>, StreamError> {
    // Return empty data to keep the caller flow stable.
    Ok(vec![])
}


