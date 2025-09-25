use crate::TooltipPosition;
use crate::style::{self, icon_text};
use crate::widget::{labeled_slider, tooltip};
use data::audio::{SoundCache, StreamCfg};
use exchange::adapter::{Exchange, MarketKind, StreamKind};

use exchange::Trade;
use iced::widget::{button, column, container, row, text};
use iced::widget::{checkbox, pick_list, slider, Space};
use iced::{Element, padding, Length};
use std::collections::HashMap;

const HARD_THRESHOLD: usize = 4;

#[derive(Debug, Clone, Copy)]
pub enum Message {
    SoundLevelChanged(f32),
    ToggleStream(bool, (Exchange, exchange::Ticker)),
    ToggleCard(Exchange, exchange::Ticker),
    SetThreshold(Exchange, exchange::Ticker, data::audio::Threshold),
    SetThresholdType(Exchange, exchange::Ticker, ThresholdType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ThresholdType {
    Count,
    Qty,
}

impl std::fmt::Display for ThresholdType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ThresholdType::Count => write!(f, "Trade Count"),
            ThresholdType::Qty => write!(f, "USD Value"),
        }
    }
}

impl ThresholdType {
    const ALL: [ThresholdType; 2] = [ThresholdType::Count, ThresholdType::Qty];
}

pub struct AudioStream {
    cache: SoundCache,
    streams: HashMap<Exchange, HashMap<exchange::Ticker, StreamCfg>>,
    expanded_card: Option<(Exchange, exchange::Ticker)>,
}

impl AudioStream {
    pub fn new(cfg: data::AudioStream) -> Self {
        let mut streams: HashMap<Exchange, HashMap<exchange::Ticker, StreamCfg>> = HashMap::new();

        for (exchange_ticker, stream_cfg) in cfg.streams {
            let exchange = exchange_ticker.exchange;
            let ticker = exchange_ticker.ticker;

            streams
                .entry(exchange)
                .or_default()
                .insert(ticker, stream_cfg);
        }

        AudioStream {
            cache: SoundCache::with_default_sounds(cfg.volume)
                .expect("Failed to create sound cache"),
            streams,
            expanded_card: None,
        }
    }

    pub fn update(&mut self, message: Message) {
        match message {
            Message::SoundLevelChanged(value) => {
                self.cache.set_volume(value);
            }
            Message::ToggleStream(is_checked, (exchange, ticker)) => {
                if is_checked {
                    if let Some(streams) = self.streams.get_mut(&exchange) {
                        if let Some(cfg) = streams.get_mut(&ticker) {
                            cfg.enabled = true;
                        } else {
                            streams.insert(ticker, StreamCfg::default());
                        }
                    } else {
                        self.streams
                            .entry(exchange)
                            .or_default()
                            .insert(ticker, StreamCfg::default());
                    }
                } else if let Some(streams) = self.streams.get_mut(&exchange) {
                    if let Some(cfg) = streams.get_mut(&ticker) {
                        cfg.enabled = false;
                    }
                } else {
                    self.streams
                        .entry(exchange)
                        .or_default()
                        .insert(ticker, StreamCfg::default());
                }
            }
            Message::ToggleCard(exchange, ticker) => {
                self.expanded_card = match self.expanded_card {
                    Some((ex, tk)) if ex == exchange && tk == ticker => None,
                    _ => Some((exchange, ticker)),
                };
            }
            Message::SetThreshold(exchange, ticker, threshold) => {
                if let Some(streams) = self.streams.get_mut(&exchange) {
                    if let Some(cfg) = streams.get_mut(&ticker) {
                        cfg.threshold = threshold;
                    }
                }
            }
            Message::SetThresholdType(exchange, ticker, threshold_type) => {
                if let Some(streams) = self.streams.get_mut(&exchange) {
                    if let Some(cfg) = streams.get_mut(&ticker) {
                        cfg.threshold = match threshold_type {
                            ThresholdType::Count => data::audio::Threshold::Count(10),
                            ThresholdType::Qty => data::audio::Threshold::Qty(5000.0),
                        };
                    }
                }
            }
        }
    }

    pub fn view(&self, active_streams: Vec<(Exchange, exchange::Ticker)>) -> Element<'_, Message> {
        let volume_container = {
            let volume_slider = {
                let volume_pct = self.cache.get_volume().unwrap_or(0.0);

                labeled_slider(
                    "Volume",
                    0.0..=100.0,
                    volume_pct,
                    Message::SoundLevelChanged,
                    |value| format!("{value}%"),
                    Some(1.0),
                )
            };

            column![text("Sound").size(14), volume_slider,].spacing(8)
        };

        let audio_contents = {
            let mut available_streams = column![].spacing(4);

            if active_streams.is_empty() {
                available_streams = available_streams.push(text("No trade streams found"));
            } else {
                for (exchange, ticker) in active_streams {
                    let mut column = column![].padding(padding::left(4));

                    let is_audio_enabled = self
                        .is_stream_audio_enabled(&StreamKind::DepthAndTrades { ticker });

                    let stream_checkbox =
                        checkbox(format!("{exchange} - {ticker}"), is_audio_enabled).on_toggle(
                            move |is_checked| Message::ToggleStream(is_checked, (exchange, ticker)),
                        );

                    let mut stream_row = row![stream_checkbox, Space::new().width(Length::Fill),]
                        .height(36)
                        .align_y(iced::Alignment::Center)
                        .padding(4)
                        .spacing(4);

                    let is_expanded = self
                        .expanded_card
                        .is_some_and(|(ex, tk)| ex == exchange && tk == ticker);

                    if is_audio_enabled {
                        stream_row = stream_row.push(tooltip(
                            button(icon_text(style::Icon::Cog, 12))
                                .on_press(Message::ToggleCard(exchange, ticker))
                                .style(move |theme, status| {
                                    style::button::transparent(theme, status, is_expanded)
                                }),
                            Some("Toggle filters for triggering a sound"),
                            TooltipPosition::Top,
                        ));
                    }

                    column = column.push(stream_row);

                    if is_expanded && is_audio_enabled {
                        if let Some(cfg) = self.streams.get(&exchange).and_then(|s| s.get(&ticker))
                        {
                            // Threshold type selection
                            let current_threshold_type = match cfg.threshold {
                                data::audio::Threshold::Count(_) => ThresholdType::Count,
                                data::audio::Threshold::Qty(_) => ThresholdType::Qty,
                            };

                            let threshold_type_picker = pick_list(
                                ThresholdType::ALL,
                                Some(current_threshold_type),
                                move |threshold_type| {
                                    Message::SetThresholdType(exchange, ticker, threshold_type)
                                },
                            );

                            column = column.push(
                                column![
                                    text("Threshold type").size(12),
                                    threshold_type_picker
                                ]
                                .padding(8)
                                .spacing(4),
                            );

                            // Threshold value configuration
                            match cfg.threshold {
                                data::audio::Threshold::Count(v) => {
                                    let threshold_slider =
                                        slider(1.0..=100.0, v as f32, move |value| {
                                            Message::SetThreshold(
                                                exchange,
                                                ticker,
                                                data::audio::Threshold::Count(value as usize),
                                            )
                                        });

                                    column = column.push(
                                        column![
                                            text(format!("Buy/sell trade count in buffer ≥ {}", v)),
                                            threshold_slider
                                        ]
                                        .padding(8)
                                        .spacing(4),
                                    );
                                }
                                data::audio::Threshold::Qty(v) => {
                                    // Ensure the value is at least the minimum
                                    let clamped_value = v.max(1000.0);
                                    
                                    let threshold_slider =
                                        slider(1000.0..=100000.0, clamped_value, move |value| {
                                            Message::SetThreshold(
                                                exchange,
                                                ticker,
                                                data::audio::Threshold::Qty(value),
                                            )
                                        })
                                        .step(1000.0);

                                    column = column.push(
                                        column![
                                            text(format!("Any trade's USD size ≥ ${:.0}", clamped_value)),
                                            threshold_slider
                                        ]
                                        .padding(8)
                                        .spacing(4),
                                    );
                                }
                            }
                        }
                    }

                    available_streams =
                        available_streams.push(container(column).style(style::modal_container));
                }
            }

            column![text("Audio streams").size(14), available_streams,].spacing(8)
        };

        container(column![volume_container, audio_contents,].spacing(20))
            .max_width(320)
            .padding(24)
            .style(style::dashboard_modal)
            .into()
    }

    pub fn volume(&self) -> Option<f32> {
        self.cache.get_volume()
    }

    pub fn play(&self, sound: &str) -> Result<(), String> {
        self.cache.play(sound)
    }

    pub fn is_stream_audio_enabled(&self, stream: &StreamKind) -> bool {
        match stream {
            StreamKind::DepthAndTrades { ticker } => self
                .streams
                .get(&ticker.exchange)
                .and_then(|streams| streams.get(ticker))
                .is_some_and(|cfg| cfg.enabled),
            _ => false,
        }
    }

    pub fn should_play_sound(&self, stream: &StreamKind) -> Option<StreamCfg> {
        if self.cache.is_muted() {
            return None;
        }

        let StreamKind::DepthAndTrades { ticker } = stream else {
            return None;
        };

        match self
            .streams
            .get(&ticker.exchange)
            .and_then(|streams| streams.get(ticker))
        {
            Some(cfg) if cfg.enabled => Some(*cfg),
            _ => None,
        }
    }

    pub fn try_play_sound(
        &self,
        stream: &StreamKind,
        trades_buffer: &[Trade],
    ) -> Result<(), String> {
        let Some(cfg) = self.should_play_sound(stream) else {
            return Ok(());
        };

        match cfg.threshold {
            data::audio::Threshold::Count(v) => {
                let (buy_count, sell_count) =
                    trades_buffer.iter().fold((0, 0), |(buy_c, sell_c), trade| {
                        if trade.is_sell {
                            (buy_c, sell_c + 1)
                        } else {
                            (buy_c + 1, sell_c)
                        }
                    });

                if buy_count < v && sell_count < v {
                    return Ok(());
                }

                let sound = |count: usize, is_sell: bool| {
                    if count > (v * HARD_THRESHOLD) {
                        if is_sell {
                            data::audio::HARD_SELL_SOUND
                        } else {
                            data::audio::HARD_BUY_SOUND
                        }
                    } else if is_sell {
                        data::audio::SELL_SOUND
                    } else {
                        data::audio::BUY_SOUND
                    }
                };

                match buy_count.cmp(&sell_count) {
                    std::cmp::Ordering::Greater => {
                        self.play(sound(buy_count, false))?;
                    }
                    std::cmp::Ordering::Less => {
                        self.play(sound(sell_count, true))?;
                    }
                    std::cmp::Ordering::Equal => {
                        self.play(sound(buy_count, false))?;
                        self.play(sound(sell_count, true))?;
                    }
                }
            }
            data::audio::Threshold::Qty(threshold_usd) => {
                // Get market type from stream to handle inverse perps correctly
                let market_type = match stream {
                    StreamKind::DepthAndTrades { ticker } => ticker.market_type(),
                    _ => return Ok(()),
                };

                // Check if any trade in the buffer exceeds the USD threshold
                let significant_trades: Vec<(&Trade, f32)> = trades_buffer
                    .iter()
                    .map(|trade| {
                        let usd_value = match market_type {
                            MarketKind::InversePerps => trade.qty, // Already in USD for inverse perps
                            _ => trade.price * trade.qty,
                        };
                        (trade, usd_value)
                    })
                    .filter(|(_, usd_value)| *usd_value >= threshold_usd)
                    .collect();

                if significant_trades.is_empty() {
                    return Ok(());
                }

                // Count buy and sell trades that meet the threshold
                let (buy_count, sell_count) = significant_trades.iter().fold(
                    (0, 0),
                    |(buy_c, sell_c), (trade, _)| {
                        if trade.is_sell {
                            (buy_c, sell_c + 1)
                        } else {
                            (buy_c + 1, sell_c)
                        }
                    },
                );

                // Find the largest trade value for intensity determination
                let max_trade_value = significant_trades
                    .iter()
                    .map(|(_, usd_value)| *usd_value)
                    .fold(0.0f32, f32::max);

                let hard_threshold_usd = threshold_usd * HARD_THRESHOLD as f32;

                let sound = |_count: usize, is_sell: bool| {
                    if max_trade_value > hard_threshold_usd {
                        if is_sell {
                            data::audio::HARD_SELL_SOUND
                        } else {
                            data::audio::HARD_BUY_SOUND
                        }
                    } else if is_sell {
                        data::audio::SELL_SOUND
                    } else {
                        data::audio::BUY_SOUND
                    }
                };

                match buy_count.cmp(&sell_count) {
                    std::cmp::Ordering::Greater => {
                        self.play(sound(buy_count, false))?;
                    }
                    std::cmp::Ordering::Less => {
                        self.play(sound(sell_count, true))?;
                    }
                    std::cmp::Ordering::Equal => {
                        if buy_count > 0 {
                            self.play(sound(buy_count, false))?;
                            self.play(sound(sell_count, true))?;
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

impl From<&AudioStream> for data::AudioStream {
    fn from(audio_stream: &AudioStream) -> Self {
        let mut streams = HashMap::new();

        for (&exchange, ticker_map) in &audio_stream.streams {
            for (&ticker, cfg) in ticker_map {
                let exchange_ticker = exchange::SerTicker::from_parts(exchange, ticker);
                streams.insert(exchange_ticker, *cfg);
            }
        }

        data::AudioStream {
            volume: audio_stream.cache.get_volume(),
            streams,
        }
    }
}
