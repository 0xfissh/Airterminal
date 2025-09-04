use serde::{Deserialize, Serialize};

const DEFAULT_BUFFER_SIZE: usize = 900;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Serialize, Default)]
pub enum DisplayStyle {
    #[default]
    Shade,
    Histogram,
}

impl std::fmt::Display for DisplayStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DisplayStyle::Shade => write!(f, "Shade"),
            DisplayStyle::Histogram => write!(f, "Histogram"),
        }
    }
}

impl DisplayStyle {
    pub const ALL: [DisplayStyle; 2] = [
        DisplayStyle::Shade,
        DisplayStyle::Histogram,
    ];
}

#[derive(Debug, Copy, Clone, PartialEq, Deserialize, Serialize)]
pub struct Config {
    pub trade_size_filter: f32,
    #[serde(default = "default_buffer_filter")]
    pub buffer_filter: usize,
    pub stacked_bar_ratio: StackedBarRatio,
    #[serde(default)]
    pub show_usd_size: bool,
    #[serde(default)]
    pub display_style: DisplayStyle,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            trade_size_filter: 0.0,
            buffer_filter: DEFAULT_BUFFER_SIZE,
            stacked_bar_ratio: StackedBarRatio::default(),
            show_usd_size: false,
            display_style: DisplayStyle::default(),
        }
    }
}

fn default_buffer_filter() -> usize {
    DEFAULT_BUFFER_SIZE
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Default, Copy)]
pub enum StackedBarRatio {
    TotalVolume,
    #[default]
    Count,
    AverageSize,
    VolumeImbalance,
}

impl std::fmt::Display for StackedBarRatio {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackedBarRatio::TotalVolume => write!(f, "Total Volume"),
            StackedBarRatio::Count => write!(f, "Count"),
            StackedBarRatio::AverageSize => write!(f, "Average Size"),
            StackedBarRatio::VolumeImbalance => write!(f, "Volume Imbalance"),
        }
    }
}

impl StackedBarRatio {
    pub const ALL: [StackedBarRatio; 4] = [
        StackedBarRatio::TotalVolume,
        StackedBarRatio::Count,
        StackedBarRatio::AverageSize,
        StackedBarRatio::VolumeImbalance,
    ];
}
