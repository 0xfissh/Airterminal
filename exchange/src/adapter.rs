use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use crate::{Kline, OpenInterest, TickerInfo, TickerStats, Trade, depth::Depth};

use super::{Ticker, Timeframe};
use serde::{Deserialize, Serialize};

pub mod binance;
pub mod bybit;
pub mod hyperliquid;
pub mod okx;

#[derive(thiserror::Error, Debug)]
pub enum StreamError {
    #[error("{0}")]
    FetchError(#[from] reqwest::Error),
    #[error("Parsing: {0}")]
    ParseError(String),
    #[error("Stream: {0}")]
    WebsocketError(String),
    #[error("Invalid request: {0}")]
    InvalidRequest(String),
    #[error("{0}")]
    UnknownError(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum MarketKind {
    Spot,
    LinearPerps,
    InversePerps,
}

impl std::fmt::Display for MarketKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MarketKind::Spot => "Spot",
                MarketKind::LinearPerps => "Linear",
                MarketKind::InversePerps => "Inverse",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum StreamKind {
    Kline { ticker: Ticker, timeframe: Timeframe },
    DepthAndTrades { ticker: Ticker },
}

impl StreamKind {
    pub fn exchange_and_ticker(&self) -> (Exchange, Ticker) {
        match self {
                StreamKind::Kline { ticker, .. }
                | StreamKind::DepthAndTrades { ticker } => (ticker.exchange, *ticker),
        }
    }

    pub fn as_depth_stream(&self) -> Option<(Exchange, Ticker)> {
        match self {
                StreamKind::DepthAndTrades { ticker } => Some((ticker.exchange, *ticker)),
            _ => None,
        }
    }

    pub fn as_kline_stream(&self) -> Option<(Exchange, Ticker, Timeframe)> {
        match self {
                StreamKind::Kline { ticker, timeframe } => {
                    Some((ticker.exchange, *ticker, *timeframe))
                }
            _ => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct UniqueStreams {
    streams: HashMap<Exchange, HashMap<Ticker, HashSet<StreamKind>>>,
    specs: HashMap<Exchange, StreamSpecs>,
}

impl UniqueStreams {
    pub fn new() -> Self {
        Self {
            streams: HashMap::new(),
            specs: HashMap::new(),
        }
    }

    pub fn from<'a>(streams: impl Iterator<Item = &'a StreamKind>) -> Self {
        let mut unique_streams = UniqueStreams::new();
        for stream in streams {
            unique_streams.add(*stream);
        }
        unique_streams
    }

    pub fn add(&mut self, stream: StreamKind) {
        let (exchange, ticker) = match stream {
            StreamKind::Kline { ticker, .. } | StreamKind::DepthAndTrades { ticker } => {
                (ticker.exchange, ticker)
            }
        };

        self.streams
            .entry(exchange)
            .or_default()
            .entry(ticker)
            .or_default()
            .insert(stream);

        self.update_specs_for_exchange(exchange);
    }

    fn update_specs_for_exchange(&mut self, exchange: Exchange) {
        let depth_streams = self.depth_streams(Some(exchange));
        let kline_streams = self.kline_streams(Some(exchange));

        self.specs.insert(
            exchange,
            StreamSpecs {
                depth: depth_streams,
                kline: kline_streams,
            },
        );
    }

    fn streams<T, F>(&self, exchange_filter: Option<Exchange>, stream_extractor: F) -> Vec<T>
    where
        F: Fn(Exchange, &StreamKind) -> Option<T>,
    {
        match exchange_filter {
            Some(exchange) => self.streams.get(&exchange).map_or(vec![], |ticker_map| {
                ticker_map
                    .values()
                    .flatten()
                    .filter_map(|stream| stream_extractor(exchange, stream))
                    .collect()
            }),
            None => self
                .streams
                .iter()
                .flat_map(|(exchange, ticker_map)| {
                    ticker_map
                        .values()
                        .flatten()
                        .filter_map(|stream| stream_extractor(*exchange, stream))
                        .collect::<Vec<_>>()
                })
                .collect(),
        }
    }

    pub fn depth_streams(&self, exchange_filter: Option<Exchange>) -> Vec<(Exchange, Ticker)> {
        self.streams(exchange_filter, |exchange, stream| {
            stream
                .as_depth_stream()
                .map(|(_, ticker)| (exchange, ticker))
        })
    }

    pub fn kline_streams(
        &self,
        exchange_filter: Option<Exchange>,
    ) -> Vec<(Exchange, Ticker, Timeframe)> {
        self.streams(exchange_filter, |exchange, stream| {
            stream
                .as_kline_stream()
                .map(|(_, ticker, timeframe)| (exchange, ticker, timeframe))
        })
    }

    pub fn combined(&self) -> &HashMap<Exchange, StreamSpecs> {
        &self.specs
    }
}

#[derive(Debug, Clone, Default)]
pub struct StreamSpecs {
    pub depth: Vec<(Exchange, Ticker)>,
    pub kline: Vec<(Exchange, Ticker, Timeframe)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Exchange {
    BinanceLinear,
    BinanceInverse,
    BinanceSpot,
    BybitLinear,
    BybitInverse,
    BybitSpot,
    HyperliquidPerps,
    OkxLinear,
    OkxInverse,
    OkxSpot,
}

impl std::fmt::Display for Exchange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Exchange::BinanceLinear => "Binance Linear",
                Exchange::BinanceInverse => "Binance Inverse",
                Exchange::BinanceSpot => "Binance Spot",
                Exchange::BybitLinear => "Bybit Linear",
                Exchange::BybitInverse => "Bybit Inverse",
                Exchange::BybitSpot => "Bybit Spot",
                Exchange::HyperliquidPerps => "Hyperliquid Perps",
                Exchange::OkxLinear => "Okx Linear",
                Exchange::OkxInverse => "Okx Inverse",
                Exchange::OkxSpot => "Okx Spot",
            }
        )
    }
}

impl FromStr for Exchange {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Binance Linear" => Ok(Exchange::BinanceLinear),
            "Binance Inverse" => Ok(Exchange::BinanceInverse),
            "Binance Spot" => Ok(Exchange::BinanceSpot),
            "Bybit Linear" => Ok(Exchange::BybitLinear),
            "Bybit Inverse" => Ok(Exchange::BybitInverse),
            "Bybit Spot" => Ok(Exchange::BybitSpot),
            "Hyperliquid Perps" => Ok(Exchange::HyperliquidPerps),
            "Okx Linear" => Ok(Exchange::OkxLinear),
            "Okx Inverse" => Ok(Exchange::OkxInverse),
            "Okx Spot" => Ok(Exchange::OkxSpot),
            _ => Err(format!("Invalid exchange: {}", s)),
        }
    }
}

impl Exchange {
    pub const ALL: [Exchange; 10] = [
        Exchange::BinanceLinear,
        Exchange::BinanceInverse,
        Exchange::BinanceSpot,
        Exchange::BybitLinear,
        Exchange::BybitInverse,
        Exchange::BybitSpot,
        Exchange::HyperliquidPerps,
        Exchange::OkxLinear,
        Exchange::OkxInverse,
        Exchange::OkxSpot,
    ];

    pub fn get_market_type(&self) -> MarketKind {
        match self {
            Exchange::BinanceLinear | Exchange::BybitLinear => MarketKind::LinearPerps,
            Exchange::BinanceInverse | Exchange::BybitInverse => MarketKind::InversePerps,
            Exchange::BinanceSpot | Exchange::BybitSpot => MarketKind::Spot,
            Exchange::HyperliquidPerps => MarketKind::LinearPerps,
            Exchange::OkxLinear => MarketKind::LinearPerps,
            Exchange::OkxInverse => MarketKind::InversePerps,
            Exchange::OkxSpot => MarketKind::Spot,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Connection;

#[derive(Debug, Clone)]
pub enum Event {
    Connected(Exchange, Connection),
    Disconnected(Exchange, String),
    DepthReceived(StreamKind, u64, Depth, Box<[Trade]>),
    KlineReceived(StreamKind, Kline),
}

#[derive(Debug, Clone, Hash)]
pub struct StreamConfig<I> {
    pub id: I,
    pub market_type: MarketKind,
}

impl<I> StreamConfig<I> {
    pub fn new(id: I, exchange: Exchange) -> Self {
        let market_type = exchange.get_market_type();

        Self { id, market_type }
    }
}

pub async fn fetch_ticker_info(
    exchange: Exchange,
) -> Result<HashMap<Ticker, Option<TickerInfo>>, StreamError> {
    let market_type = exchange.get_market_type();

    match exchange {
        Exchange::BinanceLinear | Exchange::BinanceInverse | Exchange::BinanceSpot => {
            binance::fetch_ticksize(market_type).await
        }
        Exchange::BybitLinear | Exchange::BybitInverse | Exchange::BybitSpot => {
            bybit::fetch_ticksize(market_type).await
        }
        Exchange::HyperliquidPerps => {
            hyperliquid::fetch_ticksize(market_type).await
        }
        Exchange::OkxLinear | Exchange::OkxInverse | Exchange::OkxSpot => {
            okx::fetch_ticksize(market_type).await
        }
    }
}

pub async fn fetch_ticker_prices(
    exchange: Exchange,
) -> Result<HashMap<Ticker, TickerStats>, StreamError> {
    let market_type = exchange.get_market_type();

    match exchange {
        Exchange::BinanceLinear | Exchange::BinanceInverse | Exchange::BinanceSpot => {
            binance::fetch_ticker_prices(market_type).await
        }
        Exchange::BybitLinear | Exchange::BybitInverse | Exchange::BybitSpot => {
            bybit::fetch_ticker_prices(market_type).await
        }
        Exchange::HyperliquidPerps => {
            hyperliquid::fetch_ticker_prices(market_type).await
        }
        Exchange::OkxLinear | Exchange::OkxInverse | Exchange::OkxSpot => {
            okx::fetch_ticker_prices(market_type).await
        }
    }
}

pub async fn fetch_klines(
    exchange: Exchange,
    ticker: Ticker,
    timeframe: Timeframe,
    range: Option<(u64, u64)>,
) -> Result<Vec<Kline>, StreamError> {
    match exchange {
        Exchange::BinanceLinear | Exchange::BinanceInverse | Exchange::BinanceSpot => {
            binance::fetch_klines(ticker, timeframe, range).await
        }
        Exchange::BybitLinear | Exchange::BybitInverse | Exchange::BybitSpot => {
            // Use the enhanced function that attempts to get accurate buy/sell volumes
            bybit::fetch_klines_with_trade_data(ticker, timeframe, range).await
        }
        Exchange::HyperliquidPerps => {
            hyperliquid::fetch_klines(ticker, timeframe, range).await
        }
        Exchange::OkxLinear | Exchange::OkxInverse | Exchange::OkxSpot => {
            okx::fetch_klines(ticker, timeframe, range).await
        }
    }
}

pub async fn fetch_open_interest(
    exchange: Exchange,
    ticker: Ticker,
    timeframe: Timeframe,
    range: Option<(u64, u64)>,
) -> Result<Vec<OpenInterest>, StreamError> {
    match exchange {
        Exchange::BinanceLinear | Exchange::BinanceInverse => {
            binance::fetch_historical_oi(ticker, range, timeframe).await
        }
        Exchange::BybitLinear | Exchange::BybitInverse => {
            bybit::fetch_historical_oi(ticker, range, timeframe).await
        }
        Exchange::HyperliquidPerps => {
            hyperliquid::fetch_historical_oi(ticker, range, timeframe).await
        }
        Exchange::OkxLinear | Exchange::OkxInverse => {
            Err(StreamError::InvalidRequest("Open interest not supported for OKX".to_string()))
        }
        _ => Err(StreamError::InvalidRequest("Invalid exchange".to_string())),
    }
}
