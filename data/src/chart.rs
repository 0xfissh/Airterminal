use exchange::adapter::Exchange;
use serde::{Deserialize, Serialize};

pub mod heatmap;
pub mod indicator;
pub mod kline;
pub mod timeandsales;

use super::aggr::{ticks::TickAggr, time::TimeSeries};

pub use kline::KlineChartKind;

pub enum PlotData {
    TimeBased(TimeSeries),
    TickBased(TickAggr),
}

impl PlotData {
    pub fn latest_y_midpoint(&self, calculate_target_y: impl Fn(exchange::Kline) -> f32) -> f32 {
        match self {
            PlotData::TimeBased(timeseries) => timeseries
                .latest_kline()
                .map_or(0.0, |kline| calculate_target_y(*kline)),
            PlotData::TickBased(tick_aggr) => tick_aggr
                .latest_dp()
                .map_or(0.0, |(dp, _)| calculate_target_y(dp.kline)),
        }
    }

    pub fn visible_price_range(
        &self,
        start_interval: u64,
        end_interval: u64,
    ) -> Option<(f32, f32)> {
        match self {
            PlotData::TimeBased(timeseries) => {
                timeseries.min_max_price_in_range(start_interval, end_interval)
            }
            PlotData::TickBased(tick_aggr) => {
                tick_aggr.min_max_price_in_range(start_interval as usize, end_interval as usize)
            }
        }
    }
}

pub trait PlotConstants {
    fn min_scaling(&self) -> f32;
    fn max_scaling(&self) -> f32;
    fn max_cell_width(&self) -> f32;
    fn min_cell_width(&self) -> f32;
    fn max_cell_height(&self) -> f32;
    fn min_cell_height(&self) -> f32;
    fn default_cell_width(&self) -> f32;
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ViewConfig {
    pub crosshair: bool,
    pub splits: Vec<f32>,
    pub autoscale: Option<Autoscale>,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, Default, PartialEq)]
pub enum Autoscale {
    #[default]
    CenterLatest,
    FitToVisible,
}

// ChartLayout is now replaced by ViewConfig
// This alias is kept for compatibility during transition
pub type ChartLayout = ViewConfig;

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub enum VisualConfig {
    Heatmap(heatmap::Config),
    TimeAndSales(timeandsales::Config),
    Kline(kline::Config),
}

impl VisualConfig {
    pub fn heatmap(&self) -> Option<heatmap::Config> {
        match self {
            Self::Heatmap(cfg) => Some(*cfg),
            _ => None,
        }
    }

    pub fn time_and_sales(&self) -> Option<timeandsales::Config> {
        match self {
            Self::TimeAndSales(cfg) => Some(*cfg),
            _ => None,
        }
    }

    pub fn kline(&self) -> Option<kline::Config> {
        match self {
            Self::Kline(cfg) => Some(*cfg),
            _ => None,
        }
    }
}

/// Defines how chart data is aggregated and displayed along the x-axis.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Basis {
    /// Time-based aggregation where each datapoint represents a fixed time interval.
    ///
    /// The u64 value represents milliseconds. Common values include:
    /// - `60_000` (1 minute)
    /// - `300_000` (5 minutes)
    /// - `3_600_000` (1 hour)
    Time(u64),

    /// Trade-based aggregation where each datapoint represents a fixed number of trades.
    ///
    /// The u64 value represents the number of trades per aggregation unit.
    /// Common values include 100, 500, or 1000 trades per bar/candle.
    Tick(u64),
}

impl Basis {
    pub fn is_time(&self) -> bool {
        matches!(self, Basis::Time(_))
    }

    pub fn default_time(ticker_info: Option<exchange::TickerInfo>) -> Self {
        let interval = ticker_info.map_or(100, |info| {
            match info.exchange() {
                Exchange::BybitSpot => 200,
                Exchange::HyperliquidPerps => 1000, // Use 1s as default for Hyperliquid
                _ => 100,
            }
        });
        Basis::Time(interval)
    }
}

impl From<exchange::Timeframe> for Basis {
    fn from(timeframe: exchange::Timeframe) -> Self {
        Basis::Time(timeframe.to_milliseconds())
    }
}

impl std::fmt::Display for Basis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Basis::Time(millis) => match *millis {
                500 => write!(f, "500ms"),
                1_000 => write!(f, "1s"),
                60_000 => write!(f, "1m"),
                180_000 => write!(f, "3m"),
                300_000 => write!(f, "5m"),
                900_000 => write!(f, "15m"),
                1_800_000 => write!(f, "30m"),
                3_600_000 => write!(f, "1h"),
                7_200_000 => write!(f, "2h"),
                14_400_000 => write!(f, "4h"),
                _ => write!(f, "{millis}ms"),
            },
            Basis::Tick(count) => write!(f, "{count}T"),
        }
    }
}
