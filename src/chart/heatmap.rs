use crate::style;
use data::chart::{
    Basis, ViewConfig, PlotConstants,
    heatmap::{Config, GroupedTrade, HistoricalDepth, QtyScale},
    indicator::HeatmapIndicator,
};
use data::util::{abbr_large_numbers, count_decimals, currency_abbr, format_with_commas};
use exchange::{TickerInfo, Trade, adapter::MarketKind, depth::Depth};

use super::{Chart, ViewState, Interaction, Message, scale::PriceInfoLabel};

use iced::widget::canvas::{self, Event, Geometry, Path};
use iced::{
    Alignment, Color, Element, Point, Rectangle, Renderer, Size, Theme, Vector, mouse,
    theme::palette::Extended,
};

use ordered_float::OrderedFloat;
use std::{
    collections::{BTreeMap, HashMap},
    time::Instant,
};

const CLEANUP_THRESHOLD: usize = 4800;

impl Chart for HeatmapChart {
    type IndicatorType = HeatmapIndicator;

    fn state(&self) -> &ViewState {
        &self.chart
    }

    fn mut_state(&mut self) -> &mut ViewState {
        &mut self.chart
    }

    fn invalidate(&mut self) {
        self.invalidate(None);
    }

    fn view_indicators(&self, _indicators: &[Self::IndicatorType]) -> Vec<Element<'_, Message>> {
        vec![]
    }

    fn visible_timerange(&self) -> (u64, u64) {
        let chart = self.state();
        let visible_region = chart.visible_region(chart.bounds.size());

        (
            chart.x_to_interval(visible_region.x),
            chart.x_to_interval(visible_region.x + visible_region.width),
        )
    }

    fn interval_keys(&self) -> Option<Vec<u64>> {
        None
    }

    fn autoscaled_coords(&self) -> Vector {
        let chart = self.state();
        Vector::new(
            0.5 * (chart.bounds.width / chart.scaling) - (90.0 / chart.scaling),
            0.0,
        )
    }

    fn supports_fit_autoscaling(&self) -> bool {
        false
    }

    fn is_empty(&self) -> bool {
        self.timeseries.is_empty()
    }
}

impl PlotConstants for HeatmapChart {
    fn min_scaling(&self) -> f32 {
        data::chart::heatmap::MIN_SCALING
    }

    fn max_scaling(&self) -> f32 {
        data::chart::heatmap::MAX_SCALING
    }

    fn max_cell_width(&self) -> f32 {
        data::chart::heatmap::MAX_CELL_WIDTH
    }

    fn min_cell_width(&self) -> f32 {
        data::chart::heatmap::MIN_CELL_WIDTH
    }

    fn max_cell_height(&self) -> f32 {
        data::chart::heatmap::MAX_CELL_HEIGHT
    }

    fn min_cell_height(&self) -> f32 {
        data::chart::heatmap::MIN_CELL_HEIGHT
    }

    fn default_cell_width(&self) -> f32 {
        data::chart::heatmap::DEFAULT_CELL_WIDTH
    }
}

enum IndicatorData {
    Volume,
    SessionVolumeProfile(HashMap<OrderedFloat<f32>, (f32, f32)>),
}

// Tooltip drawing configuration for aggregated trade tooltips
struct TooltipStyle {
    primary_font_size: f32,
    primary_line_height: f32,
    secondary_font_size: f32,
    secondary_line_height: f32,
    padding: f32,
    separator_height: f32,
    min_width: f32,
    left_offset: f32,
    right_offset: f32,
    above_offset: f32,
}

impl Default for TooltipStyle {
    fn default() -> Self {
        TooltipStyle {
            primary_font_size: 12.0,
            primary_line_height: 14.0,
            secondary_font_size: 10.0,
            secondary_line_height: 12.0,
            padding: 8.0,
            separator_height: 4.0,
            min_width: 120.0,
            left_offset: 20.0,
            right_offset: 10.0,
            above_offset: 20.0,
        }
    }
}

pub struct HeatmapChart {
    chart: ViewState,
    timeseries: BTreeMap<u64, (Box<[GroupedTrade]>, (f32, f32))>,
    indicators: HashMap<HeatmapIndicator, IndicatorData>,
    pause_buffer: Vec<(u64, Box<[Trade]>, Depth)>,
    heatmap: HistoricalDepth,
    visual_config: Config,
    show_hover_tooltips: bool,
    last_tick: Instant,
}

impl HeatmapChart {
    pub fn new(
        layout: ViewConfig,
        basis: Basis,
        tick_size: f32,
        enabled_indicators: &[HeatmapIndicator],
        ticker_info: Option<TickerInfo>,
        config: Option<Config>,
    ) -> Self {
        // Set initial translation to provide space for orderbook bars when autoscale is off
        let initial_translation = if layout.autoscale.is_none() {
            // Use a consistent offset that accounts for orderbook bars (50px) + text + margin
            Vector::new(-70.0, 0.0)
        } else {
            Vector::default()
        };

        HeatmapChart {
            chart: ViewState {
                cell_width: data::chart::heatmap::DEFAULT_CELL_WIDTH,
                cell_height: 4.0,
                tick_size,
                decimals: count_decimals(tick_size),
                ticker_info,
                basis,
                layout,
                translation: initial_translation,
                ..Default::default()
            },
            indicators: {
                enabled_indicators
                    .iter()
                    .map(|&indicator| {
                        let data = match indicator {
                            HeatmapIndicator::Volume => IndicatorData::Volume,
                            HeatmapIndicator::SessionVolumeProfile => {
                                IndicatorData::SessionVolumeProfile(HashMap::new())
                            }
                        };
                        (indicator, data)
                    })
                    .collect()
            },
            pause_buffer: vec![],
            heatmap: HistoricalDepth::new(
                ticker_info.expect("basis set without ticker info").min_qty,
                tick_size,
                basis,
            ),
            timeseries: BTreeMap::new(),
            visual_config: config.unwrap_or_default(),
            show_hover_tooltips: config.as_ref().map_or(true, |c| c.show_hover_tooltips),
            last_tick: Instant::now(),
        }
    }

    pub fn insert_datapoint(
        &mut self,
        trades_buffer: &[Trade],
        depth_update_t: u64,
        depth: &Depth,
    ) {
        let chart = &mut self.chart;

        let mid_price = depth.mid_price().unwrap_or(chart.base_price_y);
        chart.last_price = Some(PriceInfoLabel::Neutral(mid_price));

        // if current orderbook not visible, pause the data insertion and buffer them instead
        let is_paused = { chart.translation.x * chart.scaling > chart.bounds.width / 2.0 };

        if is_paused {
            self.pause_buffer.push((
                depth_update_t,
                trades_buffer.to_vec().into_boxed_slice(),
                depth.clone(),
            ));

            return;
        } else if !self.pause_buffer.is_empty() {
            self.pause_buffer.sort_by_key(|(time, _, _)| *time);

            for (time, trades, depth) in std::mem::take(&mut self.pause_buffer) {
                self.process_datapoint(&trades, time, &depth);
            }
        } else {
            self.cleanup_old_data();
        }

        self.process_datapoint(trades_buffer, depth_update_t, depth);
    }

    fn cleanup_old_data(&mut self) {
        if self.timeseries.len() > CLEANUP_THRESHOLD {
            let keys_to_remove = self
                .timeseries
                .keys()
                .take(CLEANUP_THRESHOLD / 10)
                .copied()
                .collect::<Vec<u64>>();

            for key in keys_to_remove {
                self.timeseries.remove(&key);
            }

            if let Some(oldest_time) = self.timeseries.keys().next().copied() {
                self.heatmap.cleanup_old_price_levels(oldest_time);
            }
        }
    }

    fn process_datapoint(&mut self, trades_buffer: &[Trade], depth_update: u64, depth: &Depth) {
        let chart = &mut self.chart;

        let aggregate_time: u64 = match chart.basis {
            Basis::Time(interval) => interval,
            Basis::Tick(_) => todo!(),
        };

        let rounded_depth_update = (depth_update / aggregate_time) * aggregate_time;

        {
            let (mut buy_volume, mut sell_volume) = (0.0, 0.0);
            let mut grouped_trades: Vec<GroupedTrade> = Vec::with_capacity(trades_buffer.len());

            for trade in trades_buffer {
                if trade.is_sell {
                    sell_volume += trade.qty;
                } else {
                    buy_volume += trade.qty;
                }

                let grouped_price = if trade.is_sell {
                    (trade.price * (1.0 / chart.tick_size)).floor() * chart.tick_size
                } else {
                    (trade.price * (1.0 / chart.tick_size)).ceil() * chart.tick_size
                };

                match grouped_trades
                    .binary_search_by(|probe| probe.compare_with(trade.price, trade.is_sell))
                {
                    Ok(index) => grouped_trades[index].qty += trade.qty,
                    Err(index) => grouped_trades.insert(
                        index,
                        GroupedTrade {
                            is_sell: trade.is_sell,
                            price: grouped_price,
                            qty: trade.qty,
                            transaction_hash: trade.transaction_hash.clone(),
                            users: trade.users.clone(),
                        },
                    ),
                }
            }

            if let Some(IndicatorData::SessionVolumeProfile(data)) = self
                .indicators
                .get_mut(&HeatmapIndicator::SessionVolumeProfile)
            {
                for trade in &grouped_trades {
                    if trade.is_sell {
                        data.entry(OrderedFloat(trade.price))
                            .or_insert_with(|| (0.0, 0.0))
                            .1 += trade.qty;
                    } else {
                        data.entry(OrderedFloat(trade.price))
                            .or_insert_with(|| (0.0, 0.0))
                            .0 += trade.qty;
                    }
                }
            }

            match self.timeseries.entry(rounded_depth_update) {
                std::collections::btree_map::Entry::Vacant(entry) => {
                    entry.insert((grouped_trades.into_boxed_slice(), (buy_volume, sell_volume)));
                }
                std::collections::btree_map::Entry::Occupied(mut entry) => {
                    let (existing_trades, (existing_buy, existing_sell)) = entry.get_mut();

                    *existing_buy += buy_volume;
                    *existing_sell += sell_volume;

                    let mut merged_trades = existing_trades.to_vec();

                    for trade in grouped_trades {
                        match merged_trades.binary_search_by(|probe| {
                            probe.compare_with(trade.price, trade.is_sell)
                        }) {
                            Ok(index) => merged_trades[index].qty += trade.qty,
                            Err(index) => merged_trades.insert(index, trade),
                        }
                    }

                    *existing_trades = merged_trades.into_boxed_slice();
                }
            }
        };

        self.heatmap
            .insert_latest_depth(depth, rounded_depth_update);

        {
            let mid_price = depth.mid_price().unwrap_or(chart.base_price_y);
            chart.base_price_y = (mid_price / (chart.tick_size)).round() * (chart.tick_size);
        }

        chart.latest_x = rounded_depth_update;
    }

    pub fn visual_config(&self) -> Config {
        self.visual_config
    }

    pub fn set_visual_config(&mut self, visual_config: Config) {
        self.visual_config = visual_config;
        self.show_hover_tooltips = visual_config.show_hover_tooltips;
        self.invalidate(Some(Instant::now()));
    }

    pub fn set_basis(&mut self, basis: Basis) {
        self.chart.basis = basis;

        self.timeseries.clear();
        self.heatmap = HistoricalDepth::new(
            self.chart
                .ticker_info
                .expect("basis set without ticker info")
                .min_qty,
            self.chart.tick_size,
            basis,
        );

        let autoscaled_coords = self.autoscaled_coords();
        self.chart.translation = autoscaled_coords;

        self.invalidate(None);
    }

    pub fn basis_interval(&self) -> Option<u64> {
        match self.chart.basis {
            Basis::Time(interval) => Some(interval),
            Basis::Tick(_) => None,
        }
    }

    pub fn chart_layout(&self) -> ViewConfig {
        self.chart.get_chart_layout()
    }

    pub fn change_tick_size(&mut self, new_tick_size: f32) {
        let chart_state = self.mut_state();

        let basis = chart_state.basis;

        chart_state.cell_height = 4.0;
        chart_state.tick_size = new_tick_size;
        chart_state.decimals = count_decimals(new_tick_size);

        if let Some(IndicatorData::SessionVolumeProfile(data)) = self
            .indicators
            .get_mut(&HeatmapIndicator::SessionVolumeProfile)
        {
            data.clear();
        }

        self.timeseries.clear();
        self.heatmap = HistoricalDepth::new(
            self.chart
                .ticker_info
                .expect("basis set without ticker info")
                .min_qty,
            new_tick_size,
            basis,
        );
    }

    pub fn toggle_indicator(&mut self, indicator: HeatmapIndicator) {
        match self.indicators.entry(indicator) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                entry.remove();
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                let data = match indicator {
                    HeatmapIndicator::Volume => IndicatorData::Volume,
                    HeatmapIndicator::SessionVolumeProfile => {
                        IndicatorData::SessionVolumeProfile(HashMap::new())
                    }
                };
                entry.insert(data);
            }
        }
    }

    pub fn toggle_hover_tooltips(&mut self) {
        self.show_hover_tooltips = !self.show_hover_tooltips;
        
        // Update the visual config too
        self.visual_config.show_hover_tooltips = self.show_hover_tooltips;
    }

    pub fn is_hover_tooltips_enabled(&self) -> bool {
        self.show_hover_tooltips
    }

    pub fn base_price(&self) -> f32 {
        self.chart.base_price_y
    }

    fn format_price(&self, price: f32) -> String {
        format!("{:.1$}", price, self.chart.decimals)
    }

    

    // Estimate text width for monospace font to size tooltip backgrounds
    fn estimate_text_width(text: &str, font_size: f32) -> f32 {
        let char_width_ratio: f32 = 0.64; // tuned for AZERET_MONO
        (text.chars().count() as f32) * font_size * char_width_ratio
    }

    // Some Hyperliquid trades can return an all-zero hash indicating a pending transaction
    fn is_pending_hash(hash: &str) -> bool {
        let trimmed = hash.trim();
        if !trimmed.starts_with("0x") {
            return false;
        }
        trimmed.chars().skip(2).all(|c| c == '0')
    }

    // Split a long value into exactly two lines, keeping the label on the first line
    // We bias the first line by the label length to keep both lines visually balanced
    fn two_lines_with_label(label: &str, value: &str) -> (String, String) {
        if value.is_empty() {
            return (format!("{}", label), String::new());
        }

        let label_len = label.chars().count() + 1; // include a space after label
        let value_chars: Vec<char> = value.chars().collect();
        let value_len = value_chars.len();
        let first_len = ((value_len + label_len) as f32 / 2.0).ceil() as usize;

        let first_part: String = value_chars[..first_len.min(value_len)].iter().collect();
        let second_part: String = if first_len < value_len {
            value_chars[first_len..].iter().collect()
        } else {
            String::new()
        };

        (format!("{} {}", label, first_part), second_part)
    }

    
    // Split a long value into exactly three lines, keeping the label on the first line.
    // Tries to balance the characters across the three lines.
    fn three_lines_with_label(label: &str, value: &str) -> (String, String, String) {
        if value.is_empty() {
            return (format!("{}", label), String::new(), String::new());
        }

        let value_chars: Vec<char> = value.chars().collect();
        let value_len = value_chars.len();

        // Split value into three roughly equal chunks
        let chunk = (value_len as f32 / 3.0).ceil() as usize;
        let c1_end = chunk.min(value_len);
        let c2_end = (chunk * 2).min(value_len);

        let p1: String = value_chars[..c1_end].iter().collect();
        let p2: String = value_chars[c1_end..c2_end].iter().collect();
        let p3: String = value_chars[c2_end..].iter().collect();

        (format!("{} {}", label, p1), p2, p3)
    }

    fn build_trade_lines_for_tooltip(
        &self,
        trade: &GroupedTrade,
        market_type: MarketKind,
        is_sell_group: bool,
    ) -> Vec<String> {
        // Size text
        let size_text = if self.visual_config.show_usd_values {
            let usd_value = match market_type {
                MarketKind::InversePerps => trade.qty,
                _ => trade.qty * trade.price,
            };
            currency_abbr(usd_value)
        } else {
            format_with_commas(trade.qty)
        };

        let mut lines = vec![
            format!("Price: {}", self.format_price(trade.price)),
            format!("Size: {}", size_text),
            format!("Side: {}", if is_sell_group { "Sell" } else { "Buy" }),
        ];

        if let Some(ticker_info) = &self.chart.ticker_info {
            if ticker_info.ticker.exchange == exchange::adapter::Exchange::HyperliquidPerps {
                if let (Some(hash), Some(users)) = (&trade.transaction_hash, &trade.users) {
                    let display_hash = if Self::is_pending_hash(hash) {
                        "pending".to_string()
                    } else {
                        hash.clone()
                    };

                    let (hash_l1, hash_l2, hash_l3) = if display_hash == "pending" {
                        (String::from("Hash: null"), String::new(), String::new())
                    } else {
                        Self::three_lines_with_label("Hash:", &display_hash)
                    };

                    // Order buyer/seller differently for buy vs sell groups
                    let (first_l1, first_l2, second_l1, second_l2) = if is_sell_group {
                        // Sell group: show Buyer first then Seller
                        let (buyer_l1, buyer_l2) = Self::two_lines_with_label("Buyer:", &users[0]);
                        let (seller_l1, seller_l2) = Self::two_lines_with_label("Seller:", &users[1]);
                        (buyer_l1, buyer_l2, seller_l1, seller_l2)
                    } else {
                        // Buy group: show Seller first then Buyer
                        let (seller_l1, seller_l2) = Self::two_lines_with_label("Seller:", &users[1]);
                        let (buyer_l1, buyer_l2) = Self::two_lines_with_label("Buyer:", &users[0]);
                        (seller_l1, seller_l2, buyer_l1, buyer_l2)
                    };

                    lines.push("-------------".to_string());
                    lines.push(hash_l1);
                    if !hash_l2.is_empty() { lines.push(hash_l2); }
                    if !hash_l3.is_empty() { lines.push(hash_l3); }
                    lines.push(first_l1);
                    if !first_l2.is_empty() { lines.push(first_l2); }
                    lines.push(second_l1);
                    if !second_l2.is_empty() { lines.push(second_l2); }
                }
            }
        }

        lines
    }

    fn compute_tooltip_dimensions(&self, all_lines: &[Vec<String>], style: &TooltipStyle) -> (f32, f32) {
        // Compute max width with per-line font sizes
        let mut max_line_width: f32 = 0.0;
        for lines in all_lines {
            let mut after_sep = false;
            for line in lines {
                let width = if after_sep {
                    Self::estimate_text_width(line, style.secondary_font_size)
                } else {
                    Self::estimate_text_width(line, style.primary_font_size)
                };
                if width > max_line_width { max_line_width = width; }
                if line == "-------------" { after_sep = true; }
            }
        }

        let tooltip_width = style.min_width.max(max_line_width + style.padding * 2.0);

        // Height with per-line heights
        let mut total_height: f32 = style.padding * 2.0;
        for lines in all_lines {
            let mut after_sep = false;
            for _line in lines {
                if _line == "-------------" { after_sep = true; continue; }
                total_height += if after_sep { style.secondary_line_height } else { style.primary_line_height };
            }
        }
        if !all_lines.is_empty() {
            total_height += style.separator_height * (all_lines.len().saturating_sub(1) as f32);
        }

        (tooltip_width, total_height)
    }

    fn draw_aggregated_group_tooltip(
        &self,
        frame: &mut canvas::Frame,
        all_lines: &[Vec<String>],
        is_sell_group: bool,
        cursor_in_chart: Point,
        palette: &Extended,
        style: &TooltipStyle,
    ) {
        if all_lines.is_empty() { return; }

        let (tooltip_width, total_height) = self.compute_tooltip_dimensions(all_lines, style);

        // Position: buys on the left, sells on the right; both above cursor
        let tooltip_x = if is_sell_group {
            cursor_in_chart.x + style.right_offset
        } else {
            cursor_in_chart.x - tooltip_width - style.left_offset
        };
        let tooltip_y = cursor_in_chart.y - total_height - style.above_offset;

        // Draw background
        frame.fill_rectangle(
            Point::new(tooltip_x, tooltip_y),
            Size::new(tooltip_width, total_height),
            Color { a: 0.95, ..palette.background.base.color },
        );

        // Draw lines
        let mut y_offset = tooltip_y + style.padding;
        for (i, lines) in all_lines.iter().enumerate() {
            let mut after_sep = false;
            let mut current_group: Option<u8> = None; // 1 buyer, 2 seller, 3 hash
            for line in lines {
                // Do not advance y-offset for separator; avoids extra spacing at group boundaries
                if line == "-------------" { after_sep = true; current_group = None; continue; }

                let line_color = if !after_sep {
                    if is_sell_group { palette.danger.base.color } else { palette.success.base.color }
                } else if line.starts_with("Buyer:") {
                    current_group = Some(1);
                    palette.success.base.color
                } else if line.starts_with("Seller:") {
                    current_group = Some(2);
                    palette.danger.base.color
                } else if line.starts_with("Hash:") {
                    current_group = Some(3);
                    palette.background.base.text
                } else {
                    match current_group {
                        Some(1) => palette.success.base.color,
                        Some(2) => palette.danger.base.color,
                        Some(3) => palette.background.base.text,
                        _ => palette.background.base.text,
                    }
                };

                frame.fill_text(canvas::Text {
                    content: line.clone(),
                    position: Point::new(tooltip_x + style.padding, y_offset),
                    size: iced::Pixels(if after_sep { style.secondary_font_size } else { style.primary_font_size }),
                    color: line_color,
                    font: style::AZERET_MONO,
                    ..canvas::Text::default()
                });
                y_offset += if after_sep { style.secondary_line_height } else { style.primary_line_height };
            }

            if i < all_lines.len() - 1 {
                y_offset += style.separator_height / 2.0;
                frame.fill_rectangle(
                    Point::new(
                        tooltip_x + style.padding,
                        y_offset - (style.separator_height / 2.0),
                    ),
                    Size::new(tooltip_width - (style.padding * 2.0), 1.0),
                    Color { a: 0.3, ..Color::WHITE },
                );
                y_offset += style.separator_height / 2.0;
            }
        }
    }

    pub fn invalidate(&mut self, now: Option<Instant>) -> Option<super::Action> {
        // Apply autoscale logic - heatmap only supports CenterLatest
        match self.chart.layout.autoscale {
            Some(_) => {
                let autoscaled_coords = self.autoscaled_coords();
                self.chart.translation = autoscaled_coords;
            }
            None => {}
        }

        self.chart.cache.clear_all();

        if let Some(t) = now {
            self.last_tick = t;
        }

        None
    }

    pub fn last_update(&self) -> Instant {
        self.last_tick
    }

    fn calc_qty_scales(&self, earliest: u64, latest: u64, highest: f32, lowest: f32) -> QtyScale {
        let market_type = match self.chart.ticker_info {
            Some(ref ticker_info) => ticker_info.market_type(),
            None => return QtyScale::default(),
        };

        let (mut max_aggr_volume, mut max_trade_qty) = (0.0f32, 0.0f32);
        let mut max_depth_qty = 0.0f32;

        self.timeseries
            .range(earliest..=latest)
            .for_each(|(_, (trades, _))| {
                let (mut buy_volume, mut sell_volume) = (0.0, 0.0);

                trades.iter().for_each(|trade| {
                    max_trade_qty = max_trade_qty.max(trade.qty);

                    if trade.is_sell {
                        sell_volume += trade.qty;
                    } else {
                        buy_volume += trade.qty;
                    }
                });

                max_aggr_volume = max_aggr_volume.max(buy_volume).max(sell_volume);
            });

        self.heatmap
            .iter_time_filtered(earliest, latest, highest, lowest)
            .for_each(|(price, runs)| {
                runs.iter()
                    .filter_map(|run| {
                        let visible_run = run.with_range(earliest, latest)?;

                        let order_size = match market_type {
                            MarketKind::InversePerps => visible_run.qty(), // Already in USD for inverse perps
                            _ => **price * visible_run.qty(),
                        };

                        if order_size > self.visual_config.order_size_filter {
                            Some(visible_run)
                        } else {
                            None
                        }
                    })
                    .for_each(|run| {
                        max_depth_qty = max_depth_qty.max(run.qty());
                    });
            });

        QtyScale {
            max_trade_qty,
            max_aggr_volume,
            max_depth_qty,
        }
    }
}

impl canvas::Program<Message> for HeatmapChart {
    type State = Interaction;

    fn update(
        &self,
        interaction: &mut Interaction,
        event: &Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<canvas::Action<Message>> {
        super::canvas_interaction(self, interaction, event, bounds, cursor)
    }

    fn draw(
        &self,
        interaction: &Self::State,
        renderer: &Renderer,
        theme: &Theme,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        let chart = self.state();

        if chart.bounds.width == 0.0 {
            return vec![];
        }

        let market_type = match self.chart.ticker_info {
            Some(ref ticker_info) => ticker_info.market_type(),
            None => return vec![],
        };

        let center = Vector::new(bounds.width / 2.0, bounds.height / 2.0);
        let bounds_size = bounds.size();

        let palette = theme.extended_palette();

        let volume_indicator = self.indicators.contains_key(&HeatmapIndicator::Volume);

        let heatmap = chart.cache.main.draw(renderer, bounds_size, |frame| {
            frame.translate(center);
            frame.scale(chart.scaling);
            frame.translate(chart.translation);

            let region = chart.visible_region(frame.size());

            let (earliest, latest) = chart.interval_range(&region);
            let (highest, lowest) = chart.price_range(&region);

            if latest < earliest {
                return;
            }

            let cell_height = chart.cell_height;
            let cell_height_scaled = cell_height * chart.scaling;

            let qty_scales = self.calc_qty_scales(earliest, latest, highest, lowest);

            let max_depth_qty = qty_scales.max_depth_qty;
            let (max_aggr_volume, max_trade_qty) =
                (qty_scales.max_aggr_volume, qty_scales.max_trade_qty);

            if let Some(merge_strat) = self.visual_config().coalescing {
                let coalesced_visual_runs = self.heatmap.coalesced_runs(
                    earliest,
                    latest,
                    highest,
                    lowest,
                    market_type,
                    self.visual_config.order_size_filter,
                    merge_strat,
                );

                for (price_of_run, visual_run) in coalesced_visual_runs {
                    let y_position = chart.price_to_y(price_of_run.into_inner());

                    let run_start_time_clipped = visual_run.start_time.max(earliest);
                    let run_until_time_clipped = visual_run.until_time.min(latest);

                    if run_start_time_clipped >= run_until_time_clipped {
                        continue;
                    }

                    let start_x = chart.interval_to_x(run_start_time_clipped);
                    let end_x = chart.interval_to_x(run_until_time_clipped).min(0.0);

                    let width = end_x - start_x;

                    if width > 0.001 {
                        let color_alpha = (visual_run.qty() / max_depth_qty).min(1.0);

                        frame.fill_rectangle(
                            Point::new(start_x, y_position - (cell_height / 2.0)),
                            Size::new(width, cell_height),
                            depth_color(palette, visual_run.is_bid, color_alpha),
                        );

                        // Store tooltip data for later drawing (removed inline tooltip drawing)
                    }
                }
            } else {
                self.heatmap
                    .iter_time_filtered(earliest, latest, highest, lowest)
                    .for_each(|(price, runs)| {
                        let y_position = chart.price_to_y(price.0);

                        runs.iter()
                            .filter(|run| {
                                let order_size = match market_type {
                                    MarketKind::InversePerps => run.qty(), // Already in USD for inverse perps
                                    _ => **price * run.qty(),
                                };
                                order_size > self.visual_config.order_size_filter
                            })
                            .for_each(|run| {
                                let start_x = chart.interval_to_x(run.start_time.max(earliest));
                                let end_x =
                                    chart.interval_to_x(run.until_time.min(latest)).min(0.0);

                                let width = end_x - start_x;

                                if width > 0.0 {
                                    let color_alpha = (run.qty() / max_depth_qty).min(1.0);
                                    let width_unscaled = width / chart.scaling;

                                    if width_unscaled > 40.0
                                        && cell_height_scaled >= 10.0
                                        && color_alpha > 0.4
                                    {
                                        frame.fill_text(canvas::Text {
                                            content: abbr_large_numbers(run.qty()),
                                            position: Point::new(
                                                start_x + (cell_height / 2.0),
                                                y_position,
                                            ),
                                            size: iced::Pixels(cell_height),
                                            color: Color::WHITE,
                                            align_y: Alignment::Center.into(),
                                            font: style::AZERET_MONO,
                                            ..canvas::Text::default()
                                        });

                                        frame.fill_rectangle(
                                            Point::new(start_x, y_position - (cell_height / 2.0)),
                                            Size::new(width, cell_height),
                                            depth_color(palette, run.is_bid, color_alpha),
                                        );

                                        frame.fill_rectangle(
                                            Point::new(start_x, y_position - (cell_height / 2.0)),
                                            Size::new(1.0, cell_height),
                                            Color::WHITE,
                                        );
                                    } else {
                                        frame.fill_rectangle(
                                            Point::new(start_x, y_position - (cell_height / 2.0)),
                                            Size::new(width, cell_height),
                                            depth_color(palette, run.is_bid, color_alpha),
                                        );
                                    }

                                    // Store tooltip data for later drawing (removed inline tooltip drawing)
                                }
                            });
                    });
            }

            if let Some((latest_timestamp, _)) = self.timeseries.last_key_value() {
                let max_qty = self
                    .heatmap
                    .latest_order_runs(highest, lowest, *latest_timestamp)
                    .map(|(_, run)| run.qty())
                    .fold(f32::MIN, f32::max)
                    .ceil()
                    * 5.0
                    / 5.0;

                if !max_qty.is_infinite() {
                    self.heatmap
                        .latest_order_runs(highest, lowest, *latest_timestamp)
                        .for_each(|(price, run)| {
                            let y_position = chart.price_to_y(price.0);
                            let bar_width = (run.qty() / max_qty) * 50.0;

                            frame.fill_rectangle(
                                Point::new(0.0, y_position - (cell_height / 2.0)),
                                Size::new(bar_width, cell_height),
                                depth_color(palette, run.is_bid, 0.5),
                            );
                        });

                    // max bid/ask quantity text
                    let text_size = 9.0 / chart.scaling;
                    let text_content = abbr_large_numbers(max_qty);
                    let text_position = Point::new(50.0, region.y);

                    frame.fill_text(canvas::Text {
                        content: text_content,
                        position: text_position,
                        size: iced::Pixels(text_size),
                        color: palette.background.base.text,
                        font: style::AZERET_MONO,
                        ..canvas::Text::default()
                    });
                }
            };

            // Collect all hovered trades FIRST before drawing any tooltips
            let mut all_hovered_trades: Vec<(GroupedTrade, Point)> = Vec::new();
            // Hide hover tooltips while using measurement ruler
            let is_using_ruler = matches!(interaction, Interaction::Ruler { start } if start.is_some());
            if self.show_hover_tooltips && !is_using_ruler {
                if let Some(cursor_pos) = cursor.position_in(bounds) {
                     let cursor_in_chart = Point::new(
                         (cursor_pos.x - center.x) / chart.scaling - chart.translation.x,
                         (cursor_pos.y - center.y) / chart.scaling - chart.translation.y
                     );

                     self.timeseries.range(earliest..=latest).for_each(
                         |(time, (trades, _))| {
                             let x_position = chart.interval_to_x(*time);

                             trades.iter().for_each(|trade| {
                                 let y_position = chart.price_to_y(trade.price);

                                 let trade_size = match market_type {
                                     MarketKind::InversePerps => trade.qty, // Already in USD for inverse perps
                                     _ => trade.qty * trade.price,
                                 };

                                 if trade_size > self.visual_config.trade_size_filter {
                                    let radius = {
                                        if let Some(trade_size_scale) = self.visual_config.trade_size_scale {
                                            let scale_factor = (trade_size_scale as f32) / 100.0;
                                            1.0 + (trade.qty / max_trade_qty) * (28.0 - 1.0) * scale_factor
                                        } else {
                                            cell_height / 2.0
                                        }
                                    };

                                    let trade_point = Point::new(x_position, y_position);
                                    if cursor_in_chart.distance(trade_point) <= radius {
                                         all_hovered_trades.push((trade.clone(), trade_point)); // Clone trade data
                                     }
                                 }
                             });
                         }
                     );
                }
            }

            self.timeseries.range(earliest..=latest).for_each(
                |(time, (trades, (buy_volume, sell_volume)))| {
                    let x_position = chart.interval_to_x(*time);

                    trades.iter().for_each(|trade| {
                        let y_position = chart.price_to_y(trade.price);

                        let trade_size = match market_type {
                            MarketKind::InversePerps => trade.qty, // Already in USD for inverse perps
                            _ => trade.qty * trade.price,
                        };

                        if trade_size > self.visual_config.trade_size_filter {
                            let base_color = if trade.is_sell {
                                palette.danger.base.color
                            } else {
                                palette.success.base.color
                            };

                            // Apply opacity from config
                            let color = Color {
                                a: base_color.a * self.visual_config.trade_opacity,
                                ..base_color
                            };

                            let radius = {
                                if let Some(trade_size_scale) = self.visual_config.trade_size_scale
                                {
                                    let scale_factor = (trade_size_scale as f32) / 100.0;
                                    1.0 + (trade.qty / max_trade_qty) * (28.0 - 1.0) * scale_factor
                                } else {
                                    cell_height / 2.0
                                }
                            };

                            frame.fill(
                                &Path::circle(Point::new(x_position, y_position), radius),
                                color,
                            );
                        }
                    });

                    if volume_indicator {
                        let bar_width = (chart.cell_width / 2.0) * 0.9;

                        let buy_bar_height =
                            (buy_volume / max_aggr_volume) * (bounds.height / chart.scaling) * 0.1;
                        let sell_bar_height =
                            (sell_volume / max_aggr_volume) * (bounds.height / chart.scaling) * 0.1;

                        if buy_bar_height > sell_bar_height {
                            frame.fill_rectangle(
                                Point::new(x_position, (region.y + region.height) - buy_bar_height),
                                Size::new(bar_width, buy_bar_height),
                                palette.success.base.color,
                            );

                            frame.fill_rectangle(
                                Point::new(
                                    x_position,
                                    (region.y + region.height) - sell_bar_height,
                                ),
                                Size::new(bar_width, sell_bar_height),
                                palette.danger.base.color,
                            );
                        } else {
                            frame.fill_rectangle(
                                Point::new(
                                    x_position,
                                    (region.y + region.height) - sell_bar_height,
                                ),
                                Size::new(bar_width, sell_bar_height),
                                palette.danger.base.color,
                            );

                            frame.fill_rectangle(
                                Point::new(x_position, (region.y + region.height) - buy_bar_height),
                                Size::new(bar_width, buy_bar_height),
                                palette.success.base.color,
                            );
                        }
                    }
                },
            );

            if let Some(IndicatorData::SessionVolumeProfile(data)) =
                self.indicators.get(&HeatmapIndicator::SessionVolumeProfile)
            {
                let max_vpsr = data
                    .iter()
                    .filter(|(price, _)| {
                        **price <= OrderedFloat(highest) && **price >= OrderedFloat(lowest)
                    })
                    .map(|(_, (buy_v, sell_v))| buy_v + sell_v)
                    .fold(0.0, f32::max);

                let max_bar_width = (bounds.width / chart.scaling) * 0.1;

                let min_segment_width = 2.0;
                let segments = ((max_bar_width / min_segment_width).floor() as usize).clamp(10, 40);

                for i in 0..segments {
                    let segment_width = max_bar_width / segments as f32;
                    let segment_x = region.x + (i as f32 * segment_width);

                    let alpha = 0.95 - (0.85 * (i as f32 / (segments - 1) as f32).powf(2.0));

                    frame.fill_rectangle(
                        Point::new(segment_x, region.y),
                        Size::new(segment_width, region.height),
                        palette.background.weakest.color.scale_alpha(alpha),
                    );
                }

                let vpsr_height = cell_height_scaled * 0.8;

                data.iter()
                    .filter(|(price, _)| {
                        **price <= OrderedFloat(highest) && **price >= OrderedFloat(lowest)
                    })
                    .for_each(|(price, (buy_v, sell_v))| {
                        let y_position = chart.price_to_y(**price);

                        super::draw_horizontal_volume_bars(
                            frame,
                            region.x,
                            y_position,
                            *buy_v,
                            *sell_v,
                            max_vpsr,
                            vpsr_height,
                            max_bar_width,
                            palette.success.weak.color,
                            palette.danger.weak.color,
                            1.0,
                        );
                    });

                if max_vpsr > 0.0 {
                    let text_size = 9.0 / chart.scaling;
                    let text_content = abbr_large_numbers(max_vpsr);

                    let text_position = Point::new(region.x + max_bar_width, region.y);

                    frame.fill_text(canvas::Text {
                        content: text_content,
                        position: text_position,
                        size: iced::Pixels(text_size),
                        color: palette.background.base.text,
                        font: style::AZERET_MONO,
                        ..canvas::Text::default()
                    });
                }
            }

            if volume_indicator && max_aggr_volume > 0.0 {
                let text_size = 9.0 / chart.scaling;
                let text_content = abbr_large_numbers(max_aggr_volume);
                let text_width = (text_content.len() as f32 * text_size) / 1.5;

                let text_position = Point::new(
                    (region.x + region.width) - text_width,
                    (region.y + region.height) - (bounds.height / chart.scaling) * 0.1 - text_size,
                );

                frame.fill_text(canvas::Text {
                    content: text_content,
                    position: text_position,
                    size: iced::Pixels(text_size),
                    color: palette.background.base.text,
                    font: style::AZERET_MONO,
                    ..canvas::Text::default()
                });
            }

            // Draw aggregated trade tooltips OUTSIDE the main loop
            if self.show_hover_tooltips && !is_using_ruler && !all_hovered_trades.is_empty() {
                if let Some(cursor_pos) = cursor.position_in(bounds) {
                    let cursor_in_chart = Point::new(
                        (cursor_pos.x - center.x) / chart.scaling - chart.translation.x,
                        (cursor_pos.y - center.y) / chart.scaling - chart.translation.y
                    );

                    // Group trades by side using the aggregated list
                    let mut buy_trades: Vec<_> = all_hovered_trades.iter()
                        .filter(|(trade, _)| !trade.is_sell)
                        .collect();
                    let mut sell_trades: Vec<_> = all_hovered_trades.iter()
                        .filter(|(trade, _)| trade.is_sell)
                        .collect();

                    // Sort both groups by price
                    buy_trades.sort_by(|a, b| b.0.price.partial_cmp(&a.0.price).unwrap_or(std::cmp::Ordering::Equal));
                    sell_trades.sort_by(|a, b| b.0.price.partial_cmp(&a.0.price).unwrap_or(std::cmp::Ordering::Equal));

                    // Build lines for each group using helper
                    let mut buy_lines: Vec<Vec<String>> = Vec::with_capacity(buy_trades.len());
                    for (trade, _) in &buy_trades {
                        buy_lines.push(self.build_trade_lines_for_tooltip(trade, market_type, false));
                    }
                    let mut sell_lines: Vec<Vec<String>> = Vec::with_capacity(sell_trades.len());
                    for (trade, _) in &sell_trades {
                        sell_lines.push(self.build_trade_lines_for_tooltip(trade, market_type, true));
                    }

                    let style_cfg = TooltipStyle::default();
                    if !buy_lines.is_empty() {
                        self.draw_aggregated_group_tooltip(frame, &buy_lines, false, cursor_in_chart, palette, &style_cfg);
                    }
                    if !sell_lines.is_empty() {
                        self.draw_aggregated_group_tooltip(frame, &sell_lines, true, cursor_in_chart, palette, &style_cfg);
                    }
                }
            }

            // Draw order tooltips in foreground (above all other elements)
            if self.show_hover_tooltips && !is_using_ruler {
                if let Some(cursor_pos) = cursor.position_in(bounds) {
                    let cursor_in_chart = Point::new(
                        (cursor_pos.x - center.x) / chart.scaling - chart.translation.x,
                        (cursor_pos.y - center.y) / chart.scaling - chart.translation.y
                    );

                    // Show order tooltip regardless of trade tooltip; avoid overlap visually by preferring offset near cursor
                    if let Some(merge_strat) = self.visual_config().coalescing {
                        let coalesced_visual_runs = self.heatmap.coalesced_runs(
                            earliest,
                            latest,
                            highest,
                            lowest,
                            market_type,
                            self.visual_config.order_size_filter,
                            merge_strat,
                        );

                        for (price_of_run, visual_run) in coalesced_visual_runs {
                            let y_position = chart.price_to_y(price_of_run.into_inner());
                            let run_start_time_clipped = visual_run.start_time.max(earliest);
                            let run_until_time_clipped = visual_run.until_time.min(latest);

                            if run_start_time_clipped >= run_until_time_clipped {
                                continue;
                            }

                            let start_x = chart.interval_to_x(run_start_time_clipped);
                            let end_x = chart.interval_to_x(run_until_time_clipped).min(0.0);
                            let width = end_x - start_x;

                            if width > 0.001 {
                                let order_rect = Rectangle {
                                    x: start_x,
                                    y: y_position - (cell_height / 2.0),
                                    width,
                                    height: cell_height,
                                };

                                if order_rect.contains(cursor_in_chart) {
                                    let size_text = if self.visual_config.show_usd_values {
                                        let usd_value = match market_type {
                                            MarketKind::InversePerps => visual_run.qty(), // Already in USD for inverse perps
                                            _ => visual_run.qty() * price_of_run.into_inner(),
                                        };
                                        currency_abbr(usd_value)
                                    } else {
                                        format_with_commas(visual_run.qty())
                                    };
                                    
                                    let tooltip_text = format!(
                                        "Price: {}\nSize: {}\nSide: {}", 
                                        self.format_price(price_of_run.into_inner()),
                                        size_text,
                                        if visual_run.is_bid { "Bid" } else { "Ask" }
                                    );

                                    let tooltip_lines: Vec<&str> = tooltip_text.split('\n').collect();
                                    let line_height = 12.0;
                                    let padding = 8.0;
                                    let font_size = 10.0;
                                    
                                    let char_width = font_size * 0.65;
                                    let tooltip_width = tooltip_lines.iter()
                                        .map(|line| line.len() as f32 * char_width)
                                        .fold(0.0, f32::max) + padding * 2.0;
                                    let tooltip_height = (tooltip_lines.len() as f32 * line_height) + padding * 2.0;

                                    let tooltip_x = cursor_in_chart.x + 10.0;
                                    let tooltip_y = cursor_in_chart.y + 10.0;

                                    // Draw background
                                    frame.fill_rectangle(
                                        Point::new(tooltip_x, tooltip_y),
                                        Size::new(tooltip_width, tooltip_height),
                                        Color {
                                            a: 0.95,
                                            ..palette.background.base.color
                                        },
                                    );

                                    // Draw text
                                    let mut y_offset = tooltip_y + padding;
                                    for line in tooltip_lines {
                                        frame.fill_text(canvas::Text {
                                            content: line.to_string(),
                                            position: Point::new(tooltip_x + padding, y_offset),
                                            size: iced::Pixels(font_size),
                                            color: palette.background.base.text,
                                            font: style::AZERET_MONO,
                                            ..canvas::Text::default()
                                        });
                                        y_offset += line_height;
                                    }
                                    break; // Only show one tooltip at a time
                                }
                            }
                        }
                    } else {
                        // Check regular depth runs for tooltips
                        'outer: for (price, runs) in self.heatmap.iter_time_filtered(earliest, latest, highest, lowest) {
                            let y_position = chart.price_to_y(price.0);

                            for run in runs.iter().filter(|run| {
                                let order_size = match market_type {
                                    MarketKind::InversePerps => run.qty(),
                                    _ => **price * run.qty(),
                                };
                                order_size > self.visual_config.order_size_filter
                            }) {
                                let start_x = chart.interval_to_x(run.start_time.max(earliest));
                                let end_x = chart.interval_to_x(run.until_time.min(latest)).min(0.0);
                                let width = end_x - start_x;

                                if width > 0.0 {
                                    let order_rect = Rectangle {
                                        x: start_x,
                                        y: y_position - (cell_height / 2.0),
                                        width,
                                        height: cell_height,
                                    };

                                    if order_rect.contains(cursor_in_chart) {
                                        let size_text = if self.visual_config.show_usd_values {
                                            let usd_value = match market_type {
                                                MarketKind::InversePerps => run.qty(), // Already in USD for inverse perps
                                                _ => run.qty() * price.0,
                                            };
                                            currency_abbr(usd_value)
                                        } else {
                                            format_with_commas(run.qty())
                                        };
                                        
                                        let tooltip_text = format!(
                                            "Price: {}\nSize: {}\nSide: {}", 
                                            self.format_price(price.0),
                                            size_text,
                                            if run.is_bid { "Bid" } else { "Ask" }
                                        );

                                        let tooltip_lines: Vec<&str> = tooltip_text.split('\n').collect();
                                        let line_height = 12.0;
                                        let padding = 8.0;
                                        let font_size = 10.0;
                                        
                                        let char_width = font_size * 0.65;
                                        let tooltip_width = tooltip_lines.iter()
                                            .map(|line| line.len() as f32 * char_width)
                                            .fold(0.0, f32::max) + padding * 2.0;
                                        let tooltip_height = (tooltip_lines.len() as f32 * line_height) + padding * 2.0;

                                        let tooltip_x = cursor_in_chart.x + 10.0;
                                        let tooltip_y = cursor_in_chart.y + 10.0;

                                        // Draw background
                                        frame.fill_rectangle(
                                            Point::new(tooltip_x, tooltip_y),
                                            Size::new(tooltip_width, tooltip_height),
                                            Color {
                                                a: 0.95,
                                                ..palette.background.base.color
                                            },
                                        );

                                        // Draw text
                                        let mut y_offset = tooltip_y + padding;
                                        for line in tooltip_lines {
                                            frame.fill_text(canvas::Text {
                                                content: line.to_string(),
                                                position: Point::new(tooltip_x + padding, y_offset),
                                                size: iced::Pixels(font_size),
                                                color: palette.background.base.text,
                                                font: style::AZERET_MONO,
                                                ..canvas::Text::default()
                                            });
                                            y_offset += line_height;
                                        }
                                        break 'outer; // Only show one tooltip at a time
                                    }
                                }
                            }
                        }
                    }
                }
            }

            let is_paused = chart.translation.x * chart.scaling > chart.bounds.width / 2.0;
            if is_paused {
                let bar_width = 8.0 / chart.scaling;
                let bar_height = 32.0 / chart.scaling;
                let padding = 24.0 / chart.scaling;

                let total_icon_width = bar_width * 3.0;

                let pause_bar = Rectangle {
                    x: (region.x + region.width) - total_icon_width - padding,
                    y: region.y + padding,
                    width: bar_width,
                    height: bar_height,
                };

                frame.fill_rectangle(
                    pause_bar.position(),
                    pause_bar.size(),
                    palette.background.base.text.scale_alpha(0.4),
                );

                frame.fill_rectangle(
                    pause_bar.position() + Vector::new(pause_bar.width * 2.0, 0.0),
                    pause_bar.size(),
                    palette.background.base.text.scale_alpha(0.4),
                );
            }
        });

        if chart.layout.crosshair && !self.timeseries.is_empty() {
            let crosshair = chart.cache.crosshair.draw(renderer, bounds_size, |frame| {
                if let Some(cursor_position) = cursor.position_in(bounds) {
                    // Heatmap: supply a measure builder that returns total volume instead of bars
                    let measure = |from: u64, to: u64| -> Option<String> {
                        // Sum volume from timeseries within [from, to]
                        let mut vol = 0.0f32;
                        for (_, (_, (buy, sell))) in self.timeseries.range(from..=to) {
                            vol += buy + sell;
                        }
                        if vol > 0.0 {
                            Some(format!("vol {}", abbr_large_numbers(vol)))
                        } else {
                            None
                        }
                    };

                    chart.draw_crosshair(
                        frame,
                        theme,
                        bounds_size,
                        cursor_position,
                        interaction,
                        Some(&measure),
                    );
                }
            });

            vec![heatmap, crosshair]
        } else {
            vec![heatmap]
        }
    }

    fn mouse_interaction(
        &self,
        interaction: &Interaction,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> mouse::Interaction {
        match interaction {
            Interaction::Panning { .. } => mouse::Interaction::Grabbing,
            Interaction::Zoomin { .. } => mouse::Interaction::ZoomIn,
            Interaction::None | Interaction::Ruler { .. } => {
                if cursor.is_over(bounds) && self.chart.layout.crosshair {
                    return mouse::Interaction::Crosshair;
                }
                mouse::Interaction::default()
            }
        }
    }
}

fn depth_color(palette: &Extended, is_bid: bool, alpha: f32) -> Color {
    if is_bid {
        palette.success.strong.color.scale_alpha(alpha)
    } else {
        palette.danger.strong.color.scale_alpha(alpha)
    }
}

