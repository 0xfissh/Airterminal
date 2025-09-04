use serde::Deserialize;
use std::collections::HashMap;

use fastwebsockets::{FragmentCollector, OpCode};
use hyper::upgrade::Upgraded;
use hyper_util::rt::TokioIo;

use iced_futures::{
    futures::{SinkExt, Stream},
    stream,
};

use super::{
    super::{
        Exchange, Kline, MarketKind, OpenInterest, StreamKind, Ticker, TickerInfo, TickerStats,
        Timeframe, Trade,
        connect::{State, setup_tcp_connection, setup_tls_connection, setup_websocket_connection},
        de_string_to_f32,
        depth::{DepthPayload, DepthUpdate, LocalDepthCache, Order},
        str_f32_parse,
    },
    Connection, Event, StreamError,
};

// Hyperliquid API constants
const HYPERLIQUID_DOMAIN: &str = "api.hyperliquid.xyz";
const HYPERLIQUID_WS_DOMAIN: &str = "api.hyperliquid.xyz";

// Hyperliquid only supports perpetuals
fn exchange_from_market_type(_market: MarketKind) -> Exchange {
    Exchange::HyperliquidPerps
}

// Hyperliquid-specific heatmap intervals (500ms and 1s only)
pub const HYPERLIQUID_HEATMAP_INTERVALS: [u64; 2] = [500, 1000];

// Hyperliquid precision constraints
const MAX_DECIMALS_PERP: u8 = 6;
const SIG_FIG_LIMIT: i32 = 5;

// Hyperliquid data structures
#[derive(Deserialize, Clone)]
pub struct FetchedDepth {
    #[serde(rename = "levels")]
    levels: Vec<Vec<HyperliquidLevel>>,
}

#[derive(Deserialize, Clone)]
pub struct HyperliquidLevel {
    #[serde(rename = "px")]
    price: String,
    #[serde(rename = "sz")]
    size: String,
}

#[derive(Deserialize, Debug, Clone)]
struct HyperliquidKline {
    #[serde(rename = "t")]
    time: u64,
    #[serde(rename = "o", deserialize_with = "de_string_to_f32")]
    open: f32,
    #[serde(rename = "h", deserialize_with = "de_string_to_f32")]
    high: f32,
    #[serde(rename = "l", deserialize_with = "de_string_to_f32")]
    low: f32,
    #[serde(rename = "c", deserialize_with = "de_string_to_f32")]
    close: f32,
    #[serde(rename = "v", deserialize_with = "de_string_to_f32")]
    volume: f32,
    #[serde(rename = "i")]
    interval: String,
}

#[derive(Deserialize, Debug)]
struct HyperliquidTrade {
    #[serde(rename = "time")]
    time: u64,
    #[serde(rename = "px", deserialize_with = "de_string_to_f32")]
    price: f32,
    #[serde(rename = "sz", deserialize_with = "de_string_to_f32")]
    qty: f32,
    #[serde(rename = "side")]
    side: String,
    // Additional Hyperliquid-specific fields
    hash: String,
    users: [String; 2], // [buyer, seller]
}

#[derive(Deserialize)]
struct HyperliquidDepth {
    #[serde(rename = "levels")]
    levels: Vec<Vec<HyperliquidLevel>>,
    #[serde(rename = "time")]
    time: u64,
}

#[derive(Deserialize, Debug)]
struct WsTrade {
    side: String,
    px: String,
    sz: String,
    time: u64,
    hash: String,
    users: [String; 2], // [buyer, seller]
}

#[derive(Deserialize, Debug)]
struct WsBook {
    levels: Vec<Vec<WsLevel>>,
    time: u64,
}

#[derive(Deserialize, Debug)]
struct WsLevel {
    px: String,
    sz: String,
}

#[derive(Deserialize, Debug)]
struct Candle {
    #[serde(rename = "t")]
    open_time: u64,
    #[serde(rename = "s")]
    coin: String,
    #[serde(rename = "i")]
    interval: String,
    #[serde(rename = "o")]
    open: String,
    #[serde(rename = "c")]
    close: String,
    #[serde(rename = "h")]
    high: String,
    #[serde(rename = "l")]
    low: String,
    #[serde(rename = "v")]
    volume: String,
}

enum StreamData {
    Trades(Vec<HyperliquidTrade>),
    Depth(HyperliquidDepth),
    Kline(Ticker, HyperliquidKline),
}

fn feed_de(slice: &[u8], _market: MarketKind) -> Result<StreamData, StreamError> {
    let exchange = Exchange::HyperliquidPerps;

    // Parse the JSON to check the structure
    let json_value: serde_json::Value = serde_json::from_slice(slice)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    // Check if this is a subscription response or error (ignore these)
    if let Some(channel) = json_value.get("channel").and_then(|c| c.as_str()) {
        if channel == "subscriptionResponse" {
            // Subscription responses are normal, not errors - just ignore them silently
            return Err(StreamError::ParseError("Ignored subscription response".to_string()));
        }
        
        if channel == "error" {
            log::warn!("Received WebSocket error: {}", String::from_utf8_lossy(slice));
            return Err(StreamError::ParseError("WebSocket error".to_string()));
        }

        // Handle actual data messages
        if let Some(data) = json_value.get("data") {
            match channel {
                "trades" => {
                    let trades: Vec<WsTrade> = serde_json::from_value(data.clone())
                        .map_err(|e| StreamError::ParseError(e.to_string()))?;
                    
                    let converted_trades: Vec<HyperliquidTrade> = trades.into_iter().map(|trade| {
                        HyperliquidTrade {
                            time: trade.time,
                            price: trade.px.parse().unwrap_or(0.0),
                            qty: trade.sz.parse().unwrap_or(0.0),
                            side: trade.side,
                            hash: trade.hash,
                            users: trade.users,
                        }
                    }).collect();
                    
                    return Ok(StreamData::Trades(converted_trades));
                }
                "l2Book" => {
                    let ws_book: WsBook = serde_json::from_value(data.clone())
                        .map_err(|e| StreamError::ParseError(e.to_string()))?;
                    
                    let depth = HyperliquidDepth {
                        time: ws_book.time,
                        levels: ws_book.levels.into_iter().map(|level_group| {
                            level_group.into_iter().map(|level| {
                                HyperliquidLevel {
                                    price: level.px,
                                    size: level.sz,
                                }
                            }).collect()
                        }).collect(),
                    };
                    
                    return Ok(StreamData::Depth(depth));
                }
                "candle" => {
                    // Handle both array and single object formats for candle data
                    let candles: Vec<Candle> = if data.is_array() {
                        serde_json::from_value(data.clone())
                            .map_err(|e| StreamError::ParseError(e.to_string()))?
                    } else {
                        // If it's a single candle object, wrap it in an array
                        let single_candle: Candle = serde_json::from_value(data.clone())
                            .map_err(|e| StreamError::ParseError(e.to_string()))?;
                        vec![single_candle]
                    };
                    
                    if let Some(candle) = candles.first() {
                        let kline = HyperliquidKline {
                            time: candle.open_time,
                            open: candle.open.parse().unwrap_or(0.0),
                            high: candle.high.parse().unwrap_or(0.0),
                            low: candle.low.parse().unwrap_or(0.0),
                            close: candle.close.parse().unwrap_or(0.0),
                            volume: candle.volume.parse().unwrap_or(0.0),
                            interval: candle.interval.clone(),
                        };
                        
                        let ticker = Ticker::new(candle.coin.to_uppercase(), exchange);
                        return Ok(StreamData::Kline(ticker, kline));
                    }
                }
                _ => {}
            }
        }
    }

    Err(StreamError::ParseError(
        "Failed to parse ws data".to_string(),
    ))
}

async fn connect() -> Result<FragmentCollector<TokioIo<Upgraded>>, StreamError> {
    let domain = HYPERLIQUID_WS_DOMAIN;
    let tcp_stream = setup_tcp_connection(domain).await?;
    let tls_stream = setup_tls_connection(domain, tcp_stream).await?;
    let url = format!("wss://{domain}/ws");
    let ws = setup_websocket_connection(domain, tls_stream, &url).await?;
    Ok(ws)
}

/// Safe volume calculation function to prevent CVD spikes (mirrors Bybit adapter logic)
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

pub fn connect_market_stream(ticker: Ticker) -> impl Stream<Item = Event> {
    stream::channel(100, async move |mut output| {
        let mut state = State::Disconnected;

        let (symbol_str, market) = ticker.to_full_symbol_and_type();
        let exchange = exchange_from_market_type(market);

        let mut orderbook: LocalDepthCache = LocalDepthCache::default();
        let mut trades_buffer: Vec<Trade> = Vec::new();

        loop {
            match &mut state {
                State::Disconnected => {
                    if let Ok(mut websocket) = connect().await {
                        // Subscribe to trades and depth
                        let trade_sub = serde_json::json!({
                            "method": "subscribe",
                            "subscription": {
                                "type": "trades",
                                "coin": symbol_str
                            }
                        });

                        let depth_sub = serde_json::json!({
                            "method": "subscribe", 
                            "subscription": {
                                "type": "l2Book",
                                "coin": symbol_str,
                                "nSigFigs": null  // Request full precision to get maximum levels
                            }
                        });

                        // Send subscriptions
                        let trade_sub_str = trade_sub.to_string();
                        let depth_sub_str = depth_sub.to_string();
                        let trade_msg = fastwebsockets::Frame::text(
                            fastwebsockets::Payload::Borrowed(trade_sub_str.as_bytes())
                        );
                        let depth_msg = fastwebsockets::Frame::text(
                            fastwebsockets::Payload::Borrowed(depth_sub_str.as_bytes())
                        );

                        if let Err(e) = websocket.write_frame(trade_msg).await {
                            log::error!("Failed to send trade subscription: {}", e);
                            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
                            continue;
                        }
                        
                        if let Err(e) = websocket.write_frame(depth_msg).await {
                            log::error!("Failed to send depth subscription: {}", e);
                            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
                            continue;
                        }

                        // Fetch initial depth snapshot
                        let (tx, rx) = tokio::sync::oneshot::channel();
                        tokio::spawn(async move {
                            let result = fetch_depth(&ticker).await;
                            let _ = tx.send(result);
                        });

                        match rx.await {
                            Ok(Ok(depth)) => {
                                orderbook.update(DepthUpdate::Snapshot(depth));
                                state = State::Connected(websocket);
                                let _ = output.send(Event::Connected(exchange, Connection)).await;
                            }
                            Ok(Err(e)) => {
                                let _ = output
                                    .send(Event::Disconnected(
                                        exchange,
                                        format!("Depth fetch failed: {e}"),
                                    ))
                                    .await;
                            }
                            Err(e) => {
                                let _ = output
                                    .send(Event::Disconnected(
                                        exchange,
                                        format!("Channel error: {e}"),
                                    ))
                                    .await;
                            }
                        }
                    } else {
                        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;

                        let _ = output
                            .send(Event::Disconnected(
                                exchange,
                                "Failed to connect to websocket".to_string(),
                            ))
                            .await;
                    }
                }
                State::Connected(ws) => {
                    match ws.read_frame().await {
                        Ok(msg) => match msg.opcode {
                            OpCode::Text => {
                                match feed_de(&msg.payload[..], market) {
                                    Ok(data) => match data {
                                        StreamData::Trades(trades) => {
                                                                    for hl_trade in trades {
                            let trade = Trade {
                                time: hl_trade.time,
                                is_sell: hl_trade.side == "A", // A = Ask (sell), B = Bid (buy)
                                price: hl_trade.price,
                                qty: hl_trade.qty,
                                transaction_hash: Some(hl_trade.hash),
                                users: Some(hl_trade.users),
                            };
                            trades_buffer.push(trade);
                        }
                    }
                    StreamData::Depth(depth_data) => {
                        let depth_payload = new_depth_cache(&depth_data);
                        orderbook.update(DepthUpdate::Snapshot(depth_payload));

                        let _ = output
                            .send(Event::DepthReceived(
                                StreamKind::DepthAndTrades { ticker },
                                depth_data.time,
                                orderbook.depth.clone(),
                                std::mem::take(&mut trades_buffer)
                                    .into_boxed_slice(),
                            ))
                            .await;
                    }
                    _ => {}
                                    },
                                    Err(e) => {
                                        // Ignore subscription response errors as they are normal
                                        if !e.to_string().contains("Ignored subscription response") {
                                            log::error!("Failed to parse WebSocket message: {}", e);
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
                    };
                }
            }
        }
    })
}

pub fn connect_kline_stream(
    streams: Vec<(Ticker, Timeframe)>,
    _market: MarketKind,
) -> impl Stream<Item = Event> {
    stream::channel(100, async move |mut output| {
        let mut state = State::Disconnected;
        let exchange = Exchange::HyperliquidPerps;

        loop {
            match &mut state {
                State::Disconnected => {
                    if let Ok(mut websocket) = connect().await {
                        // Subscribe to all candle streams in a single connection
                        let mut subscription_success = true;
                        
                        for (ticker, timeframe) in &streams {
                            let (symbol_str, _) = ticker.to_full_symbol_and_type();
                            // Convert timeframe to Hyperliquid format
                            let interval = match timeframe {
                                Timeframe::M1 => "1m",
                                Timeframe::M3 => "3m",
                                Timeframe::M5 => "5m", 
                                Timeframe::M15 => "15m",
                                Timeframe::M30 => "30m",
                                Timeframe::H1 => "1h",
                                Timeframe::H2 => "2h",
                                Timeframe::H4 => "4h",
                                _ => "5m", // Default fallback
                            };
                            
                            let candle_sub = serde_json::json!({
                                "method": "subscribe",
                                "subscription": {
                                    "type": "candle",
                                    "coin": symbol_str,
                                    "interval": interval
                                }
                            });

                            let candle_sub_str = candle_sub.to_string();
                            
                            let candle_msg = fastwebsockets::Frame::text(
                                fastwebsockets::Payload::Owned(candle_sub_str.into_bytes()),
                            );

                            if let Err(e) = websocket.write_frame(candle_msg).await {
                                log::error!("Failed to subscribe to candle stream for {}/{}: {}", ticker, timeframe, e);
                                subscription_success = false;
                                break;
                            }
                        }

                        if subscription_success {
                            state = State::Connected(websocket);
                            let _ = output.send(Event::Connected(exchange, Connection)).await;
                        } else {
                            let _ = output
                                .send(Event::Disconnected(
                                    exchange,
                                    "Failed to subscribe to candle streams".to_string(),
                                ))
                                .await;
                        }
                    } else {
                        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;

                        let _ = output
                            .send(Event::Disconnected(
                                exchange,
                                "Failed to connect to websocket".to_string(),
                            ))
                            .await;
                    }
                }
                State::Connected(ws) => {
                    match ws.read_frame().await {
                        Ok(msg) => match msg.opcode {
                            OpCode::Text => {
                                match feed_de(&msg.payload[..], MarketKind::LinearPerps) {
                                    Ok(StreamData::Kline(ticker, hl_kline)) => {
                                        let (buy_volume, sell_volume) =
                                            calculate_safe_buy_sell_volumes(
                                                hl_kline.open,
                                                hl_kline.close,
                                                hl_kline.volume,
                                            );

                                        let kline = Kline {
                                            time: hl_kline.time,
                                            open: hl_kline.open,
                                            high: hl_kline.high,
                                            low: hl_kline.low,
                                            close: hl_kline.close,
                                            volume: (buy_volume, sell_volume),
                                        };

                                        // Find matching timeframe by converting interval format
                                        let matching_timeframe = match hl_kline.interval.as_str() {
                                            "1m" => Some(Timeframe::M1),
                                            "3m" => Some(Timeframe::M3),
                                            "5m" => Some(Timeframe::M5),
                                            "15m" => Some(Timeframe::M15),
                                            "30m" => Some(Timeframe::M30),
                                            "1h" => Some(Timeframe::H1),
                                            "2h" => Some(Timeframe::H2),
                                            "4h" => Some(Timeframe::H4),
                                            _ => None,
                                        };

                                        if let Some(timeframe) = matching_timeframe {
                                                                                    // Check if this ticker/timeframe combination is in our subscribed streams
                                        if streams.iter().any(|(sub_ticker, sub_tf)| {
                                            sub_ticker == &ticker && sub_tf == &timeframe
                                        }) {
                                            let _ = output
                                                .send(Event::KlineReceived(
                                                    StreamKind::Kline { ticker, timeframe },
                                                    kline,
                                                ))
                                                .await;
                                        }
                                    } else {
                                        log::warn!("Unknown interval format: {}", hl_kline.interval);
                                    }
                                }
                                Ok(StreamData::Trades(_)) => {}
                                Ok(StreamData::Depth(_)) => {}
                                Err(e) => {
                                    // Ignore subscription response errors as they are normal
                                    if !e.to_string().contains("Ignored subscription response") {
                                        log::error!("Failed to parse WebSocket message: {}", e);
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
                    }
                }
            }
        }
    })
}

fn new_depth_cache(depth: &HyperliquidDepth) -> DepthPayload {
    let bids = if !depth.levels.is_empty() {
        &depth.levels[0]
    } else {
        &Vec::new()
    };
    
    let asks = if depth.levels.len() > 1 {
        &depth.levels[1]
    } else {
        &Vec::new()
    };

    // For Hyperliquid, we want to ensure we have enough price levels
    // to support the 50-level requirement (25 bid + 25 ask)
    // We'll take up to 50 levels from each side to ensure sufficient granularity
    // This gives us more raw data to work with when the tick size filtering is applied
    let max_levels = 50;
    
    let processed_bids: Vec<Order> = bids
        .iter()
        .take(max_levels)
        .map(|x| Order {
            price: str_f32_parse(&x.price),
            qty: str_f32_parse(&x.size),
        })
        .collect();
    
    let processed_asks: Vec<Order> = asks
        .iter()
        .take(max_levels)
        .map(|x| Order {
            price: str_f32_parse(&x.price),
            qty: str_f32_parse(&x.size),
        })
        .collect();

    DepthPayload {
        last_update_id: depth.time,
        time: depth.time,
        bids: processed_bids,
        asks: processed_asks,
    }
}

async fn fetch_depth(ticker: &Ticker) -> Result<DepthPayload, StreamError> {
    let (symbol_str, _market_type) = ticker.to_full_symbol_and_type();

    let url = format!("https://{HYPERLIQUID_DOMAIN}/info");
    
    let request_body = serde_json::json!({
        "type": "l2Book",
        "coin": symbol_str,
        "nSigFigs": null  // Request full precision to get maximum levels
    });

    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .json(&request_body)
        .send()
        .await
        .map_err(StreamError::FetchError)?;
    
    let text = response.text().await.map_err(StreamError::FetchError)?;

    let fetched_depth: FetchedDepth =
        serde_json::from_str(&text).map_err(|e| StreamError::ParseError(e.to_string()))?;

    let bids = if !fetched_depth.levels.is_empty() {
        &fetched_depth.levels[0]
    } else {
        &Vec::new()
    };
    
    let asks = if fetched_depth.levels.len() > 1 {
        &fetched_depth.levels[1]
    } else {
        &Vec::new()
    };

    // For Hyperliquid, we want to ensure we have enough price levels
    // to support the 20-level requirement (10 bid + 10 ask)
    // We'll take up to 50 levels from each side to ensure sufficient granularity
    // This gives us more raw data to work with when the tick size filtering is applied
    let max_levels = 50;
    
    let processed_bids: Vec<Order> = bids
        .iter()
        .take(max_levels)
        .map(|x| Order {
            price: str_f32_parse(&x.price),
            qty: str_f32_parse(&x.size),
        })
        .collect();
    
    let processed_asks: Vec<Order> = asks
        .iter()
        .take(max_levels)
        .map(|x| Order {
            price: str_f32_parse(&x.price),
            qty: str_f32_parse(&x.size),
        })
        .collect();
    
    let depth = DepthPayload {
        last_update_id: chrono::Utc::now().timestamp_millis() as u64,
        time: chrono::Utc::now().timestamp_millis() as u64,
        bids: processed_bids,
        asks: processed_asks,
    };

    Ok(depth)
}

pub async fn fetch_klines(
    ticker: Ticker,
    timeframe: Timeframe,
    range: Option<(u64, u64)>,
) -> Result<Vec<Kline>, StreamError> {
    let (symbol_str, _market_type) = ticker.to_full_symbol_and_type();
    
    // Convert timeframe to Hyperliquid format (same as WebSocket subscription)
    let interval = match timeframe {
        Timeframe::M1 => "1m",
        Timeframe::M3 => "3m",
        Timeframe::M5 => "5m", 
        Timeframe::M15 => "15m",
        Timeframe::M30 => "30m",
        Timeframe::H1 => "1h",
        Timeframe::H2 => "2h",
        Timeframe::H4 => "4h",
        _ => "5m", // Default fallback
    };

    let url = format!("https://{HYPERLIQUID_DOMAIN}/info");
    
    // Hyperliquid might expect a different symbol format
    // Let's try without any suffix and see if that works
    let clean_symbol = symbol_str.replace("-PERP", "").replace("-USDT", "").replace("-USDC", "");

    // If no range is provided, use a default range (last 24 hours)
    let (start_time, end_time) = if let Some((start, end)) = range {
        (start, end)
    } else {
        let now = chrono::Utc::now().timestamp_millis() as u64;
        let one_day_ms = 24 * 60 * 60 * 1000;
        (now - one_day_ms, now)
    };

    let request_body = serde_json::json!({
        "type": "candleSnapshot",
        "req": {
            "coin": clean_symbol,
            "interval": interval,
            "startTime": start_time,
            "endTime": end_time
        }
    });

    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .json(&request_body)
        .send()
        .await
        .map_err(StreamError::FetchError)?;
    
    // Check if the response was successful
    if !response.status().is_success() {
        let status = response.status();
        let error_text = response.text().await.unwrap_or_else(|_| "Unknown error".to_string());
        return Err(StreamError::ParseError(format!("HTTP error {}: {}", status, error_text)));
    }
    
    let text = response.text().await.map_err(|e| {
        StreamError::FetchError(e)
    })?;

    // Check if response is empty or contains an error
    if text.trim().is_empty() {
        return Err(StreamError::ParseError("Empty response from Hyperliquid API".to_string()));
    }

    // Parse the response as a generic JSON value first to understand the structure
    let response_value: serde_json::Value = serde_json::from_str(&text)
        .map_err(|e| StreamError::ParseError(format!("Failed to parse response as JSON: {e}")))?;

    // The Hyperliquid API returns an array of candle data
    let klines = if let Some(candles_array) = response_value.as_array() {
        let mut klines = Vec::new();
        
        for candle_value in candles_array {
            // Each candle is an object with fields: t, T, s, i, o, c, h, l, v, n
            if let Some(candle_obj) = candle_value.as_object() {
                let time = candle_obj.get("t").and_then(|v| v.as_u64()).unwrap_or(0);
                let open = candle_obj.get("o").and_then(|v| v.as_str()).unwrap_or("0").parse::<f32>().unwrap_or(0.0);
                let high = candle_obj.get("h").and_then(|v| v.as_str()).unwrap_or("0").parse::<f32>().unwrap_or(0.0);
                let low = candle_obj.get("l").and_then(|v| v.as_str()).unwrap_or("0").parse::<f32>().unwrap_or(0.0);
                let close = candle_obj.get("c").and_then(|v| v.as_str()).unwrap_or("0").parse::<f32>().unwrap_or(0.0);
                let volume = candle_obj.get("v").and_then(|v| v.as_str()).unwrap_or("0").parse::<f32>().unwrap_or(0.0);
                
                let (buy_volume, sell_volume) =
                    calculate_safe_buy_sell_volumes(open, close, volume);

                let kline = Kline {
                    time,
                    open,
                    high,
                    low,
                    close,
                    volume: (buy_volume, sell_volume),
                };
                klines.push(kline);
            }
        }
        
        klines
    } else {
        // If we can't parse the response, return an error
        return Err(StreamError::ParseError("Failed to parse klines response".to_string()));
    };

    Ok(klines)
}

pub async fn fetch_ticksize(
    _market: MarketKind,
) -> Result<HashMap<Ticker, Option<TickerInfo>>, StreamError> {
    let exchange = Exchange::HyperliquidPerps;

    // Use metaAndAssetCtxs to get both universe and price data in one call
    let url = format!("https://{HYPERLIQUID_DOMAIN}/info");
    let request_body = serde_json::json!({
        "type": "metaAndAssetCtxs"
    });

    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .json(&request_body)
        .send()
        .await
        .map_err(StreamError::FetchError)?;
    let text = response.text().await.map_err(StreamError::FetchError)?;

    let response_json: serde_json::Value = serde_json::from_str(&text)
        .map_err(|e| StreamError::ParseError(e.to_string()))?;

    // Response format: [meta, [asset_contexts...]]
    let meta = response_json
        .get(0)
        .ok_or_else(|| StreamError::ParseError("Missing meta data in response".to_string()))?;
    let asset_contexts = response_json
        .get(1)
        .and_then(|arr| arr.as_array())
        .ok_or_else(|| StreamError::ParseError("Missing asset contexts array".to_string()))?;

    let universe = meta
        .get("universe")
        .and_then(|u| u.as_array())
        .ok_or_else(|| StreamError::ParseError("Missing universe in meta data".to_string()))?;

    let mut ticker_info_map = HashMap::new();

    for (index, asset) in universe.iter().enumerate() {
        let symbol_str = asset
            .get("name")
            .and_then(|v| v.as_str())
            .ok_or_else(|| StreamError::ParseError("Missing symbol name".to_string()))?
            .to_uppercase();

        let sz_decimals = asset
            .get("szDecimals")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| StreamError::ParseError("Missing szDecimals".to_string()))?
            as u32;

        if let Some(asset_ctx) = asset_contexts.get(index) {
            // Prefer midPx, then markPx, then oraclePx
            let price = ["midPx", "markPx", "oraclePx"].iter().find_map(|k| {
                asset_ctx
                    .get(k)
                    .and_then(|v| v.as_str())
                    .and_then(|s| s.parse::<f32>().ok())
            });

            if let Some(p) = price {
                let tick_size = compute_tick_size(p, sz_decimals, MarketKind::LinearPerps);
                let min_qty = 10.0_f32.powi(-(sz_decimals as i32));

                let ticker = Ticker::new(&symbol_str, exchange);
                let info = TickerInfo {
                    ticker,
                    min_ticksize: tick_size,
                    min_qty,
                };
                ticker_info_map.insert(ticker, Some(info));
            }
        }
    }

    Ok(ticker_info_map)
}

/// Compute effective tick size based on current price, szDecimals and market rules.
/// Mirrors the working logic used upstream to avoid gaps at price levels.
fn compute_tick_size(price: f32, sz_decimals: u32, market: MarketKind) -> f32 {
    if price <= 0.0 {
        return 0.001;
    }

    // Integer-only if price is very large
    if price >= 100_000.0 {
        return 1.0;
    }

    let max_system_decimals = match market {
        MarketKind::LinearPerps => MAX_DECIMALS_PERP as i32,
        _ => MAX_DECIMALS_PERP as i32,
    };
    let decimal_cap = (max_system_decimals - sz_decimals as i32).max(0);
    if decimal_cap == 0 {
        return 1.0;
    }

    // Integer digits in price
    let int_digits = if price >= 1.0 {
        (price.abs().log10().floor() as i32 + 1).max(1)
    } else {
        0
    };

    // If integer digits already exceed sig fig limit -> integer only
    if int_digits > SIG_FIG_LIMIT {
        return 1.0;
    }

    let effective_decimals = if int_digits > 0 {
        // Remaining significant figures go to fractional digits
        let remaining_sig = (SIG_FIG_LIMIT - int_digits).max(0);
        remaining_sig.min(decimal_cap)
    } else {
        // price < 1: leading zeros after decimal don't count toward sig figs
        // leading_zeros = -floor(log10(price)) - 1
        let leading_zeros = {
            let lg = price.abs().log10().floor() as i32; // negative
            (-lg - 1).max(0)
        };
        let allowed = leading_zeros + SIG_FIG_LIMIT;
        allowed.min(decimal_cap)
    };

    if effective_decimals <= 0 {
        1.0
    } else {
        10_f32.powi(-effective_decimals)
    }
}

// Removed redundant get_hyperliquid_tick_size and local rounding helpers.

const HYPERLIQUID_FILTER_VOLUME: f32 = 100_000.0; // Lowered from 1M to 100K

pub async fn fetch_ticker_prices(
    _market: MarketKind,
) -> Result<HashMap<Ticker, TickerStats>, StreamError> {
    let exchange = Exchange::HyperliquidPerps;

    let url = format!("https://{HYPERLIQUID_DOMAIN}/info");
    
    // Get all asset contexts in one request
    let request_body = serde_json::json!({
        "type": "metaAndAssetCtxs"
    });



    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .json(&request_body)
        .send()
        .await
        .map_err(StreamError::FetchError)?;
    
    let text = response.text().await.map_err(StreamError::FetchError)?;
    let response_data: serde_json::Value = serde_json::from_str(&text)
        .map_err(|e| StreamError::ParseError(format!("Failed to parse response: {e}")))?;

    // The response is an array: [metadata, assetCtxs]
    let response_array = response_data
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Response is not an array".to_string()))?;

    if response_array.len() < 2 {
        return Err(StreamError::ParseError("Invalid response format".to_string()));
    }

    // Get metadata (universe)
    let metadata = &response_array[0];
    let universe = metadata["universe"]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Missing universe array".to_string()))?;

    // Get asset contexts
    let asset_ctxs = response_array[1]
        .as_array()
        .ok_or_else(|| StreamError::ParseError("Missing asset contexts array".to_string()))?;

    let mut ticker_price_map = HashMap::new();



    // Iterate through universe and asset contexts together
    for (i, asset_info) in universe.iter().enumerate() {
        let symbol = asset_info["name"]
            .as_str()
            .ok_or_else(|| StreamError::ParseError("Missing symbol name".to_string()))?
            .to_uppercase(); // Convert to uppercase to match Ticker encoding

        // Get corresponding asset context
        if let Some(asset_ctx) = asset_ctxs.get(i) {
            // Extract mark price
            let mark_price = asset_ctx["markPx"]
                .as_str()
                .ok_or_else(|| StreamError::ParseError("Missing markPx".to_string()))?
                .parse::<f32>()
                .map_err(|e| StreamError::ParseError(format!("Failed to parse markPx: {e}")))?;

            // Extract previous day price for change calculation
            let prev_day_px = asset_ctx["prevDayPx"]
                .as_str()
                .ok_or_else(|| StreamError::ParseError("Missing prevDayPx".to_string()))?
                .parse::<f32>()
                .map_err(|e| StreamError::ParseError(format!("Failed to parse prevDayPx: {e}")))?;

            // Calculate daily price change percentage
            let daily_price_chg = if prev_day_px > 0.0 {
                ((mark_price - prev_day_px) / prev_day_px) * 100.0
            } else {
                0.0
            };

            // Extract 24hr volume
            let daily_volume = asset_ctx["dayNtlVlm"]
                .as_str()
                .ok_or_else(|| StreamError::ParseError("Missing dayNtlVlm".to_string()))?
                .parse::<f32>()
                .map_err(|e| StreamError::ParseError(format!("Failed to parse dayNtlVlm: {e}")))?;

            // Only include assets with sufficient volume
            if daily_volume >= HYPERLIQUID_FILTER_VOLUME {
                let ticker_stats = TickerStats {
                    mark_price,
                    daily_price_chg,
                    daily_volume,
                };

                ticker_price_map.insert(Ticker::new(&symbol, exchange), ticker_stats);
            }
        }
    }

    Ok(ticker_price_map)
}

pub async fn fetch_historical_oi(
    _ticker: Ticker,
    _range: Option<(u64, u64)>,
    _period: Timeframe,
) -> Result<Vec<OpenInterest>, StreamError> {
    // Return empty vector for now
    Ok(Vec::new())
} 
