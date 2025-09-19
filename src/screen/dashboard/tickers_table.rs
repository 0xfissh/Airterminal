use std::collections::HashMap;

use crate::style::{self, Icon, icon_text};
use data::InternalError;
use exchange::{
    Ticker, TickerInfo, TickerStats,
    adapter::{Exchange, MarketKind, fetch_ticker_info, fetch_ticker_prices},
};
use iced::{
    Alignment, Element, Length, Renderer, Size, Subscription, Task, Theme,
    alignment::{Horizontal, Vertical},
    padding,
    widget::{
        Button, Space, Text, button, column, container, horizontal_rule, row,
        scrollable::{self, AbsoluteOffset},
        text, text_input,
    },
};

const ACTIVE_UPDATE_INTERVAL: u64 = 25;
const INACTIVE_UPDATE_INTERVAL: u64 = 300;

// Compact browser layout
const BROWSER_WIDTH: f32 = 412.0;
const TICKER_CARD_HEIGHT: f32 = 33.6; // ~20% taller than 28.0
const SEARCH_BAR_HEIGHT: f32 = 72.0;
// Spacing and header constants for accurate visibility math
const CARD_SPACING: f32 = 4.0;
const EXCHANGE_FILTERS_ROW_HEIGHT: f32 = 32.0;
const SORT_BUTTON_ROW_HEIGHT: f32 = 32.0;
const MARKET_BUTTON_ROW_HEIGHT: f32 = 28.0;
const SORT_COLUMN_SPACING: f32 = 4.0; // internal spacing inside sort column
const COLUMN_SPACING_WITH_SORT: f32 = 8.0; // spacing between sections when sort is shown
const COLUMN_SPACING_NO_SORT: f32 = 4.0; // spacing between sections when sort is hidden
const HRULE_THICKNESS: f32 = 1.0;

// Enhanced virtualization constants
// Note: Using fixed buffer calculations instead of these constants for better stability

pub fn fetch_tickers_info() -> Task<Message> {
    let fetch_tasks = Exchange::ALL
        .iter()
        .map(|exchange| {
            Task::perform(fetch_ticker_info(*exchange), move |result| match result {
                Ok(ticker_info) => Message::UpdateTickersInfo(*exchange, ticker_info),
                Err(err) => Message::ErrorOccurred(InternalError::Fetch(err.to_string())),
            })
        })
        .collect::<Vec<Task<Message>>>();

    Task::batch(fetch_tasks)
}

pub enum Action {
    TickerSelected(TickerInfo, Exchange, String),
    ErrorOccurred(data::InternalError),
    Fetch(Task<Message>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TickerTab {
    All,
    Bybit,
    Binance,
    Hyperliquid,
    Okx,
    Favorites,
}

#[derive(Clone)]
struct TickerDisplayData {
    display_ticker: String,
    price_change_display: String,
    volume_display: String,
    mark_price_display: String,
    card_color_alpha: f32,
}

#[derive(Clone)]
struct TickerEntry {
    ticker: Ticker,
    stats: TickerStats,
    is_favorite: bool,
    display_data: Option<TickerDisplayData>, // Lazy-computed
    search_string: String, // Cached full symbol string for fast searching
}

#[derive(Debug, Clone, PartialEq)]
pub enum SortOptions {
    VolumeAsc,
    VolumeDesc,
    ChangeAsc,
    ChangeDesc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChartType {
    Heatmap,
    Footprint,
    Candlestick,
    TimeAndSales,
}

impl ChartType {
    fn all() -> &'static [ChartType] {
        &[
            ChartType::Heatmap,
            ChartType::Footprint,
            ChartType::Candlestick,
            ChartType::TimeAndSales,
        ]
    }
}

#[derive(Debug, Clone)]
pub enum Message {
    ChangeTickersTableTab(TickerTab),
    UpdateSearchQuery(String),
    ChangeSortOption(SortOptions),
    ShowSortingOptions,
    TickerSelected(Ticker, Exchange, String),
    ExpandTickerCard(Option<(Ticker, Exchange)>),
    FavoriteTicker(Exchange, Ticker),
    Scrolled(scrollable::Viewport),
    SetMarketFilter(Option<MarketKind>),
    ToggleTable,
    FetchForTickerStats(Option<Exchange>),
    UpdateTickersInfo(Exchange, HashMap<Ticker, Option<TickerInfo>>),
    UpdateTickerStats(Exchange, HashMap<Ticker, TickerStats>),
    ErrorOccurred(data::InternalError),
    // Keyboard navigation
    KeyboardNavigate(iced::keyboard::Key, iced::keyboard::Modifiers),
}

pub struct TickersTable {
    // Single source of truth: Exchange -> Vec<TickerEntry>
    ticker_data: HashMap<Exchange, Vec<TickerEntry>>,
    // Filtered data is computed on-demand in view() method
    // Cached search query (normalized) with performance optimizations
    normalized_search: String,
    search_terms: Vec<String>, // Pre-split search terms for faster matching
    selected_tab: TickerTab,
    show_sort_options: bool,
    selected_sort_option: SortOptions,
    selected_market: Option<MarketKind>,
    expand_ticker_card: Option<(Ticker, Exchange)>,
    scroll_offset: AbsoluteOffset,
    is_show: bool,
    tickers_info: HashMap<Exchange, HashMap<Ticker, Option<TickerInfo>>>,
    // Performance tracking
    needs_filter_update: bool,
    // Cached sorted indices for performance
    cached_sort_option: Option<SortOptions>,
    cached_sorted_indices: Vec<(Exchange, usize)>,
    // Scroll performance tracking
    last_scroll_offset: AbsoluteOffset,
    scroll_velocity: f32,
    // Async operation optimization
    last_update_times: HashMap<Exchange, std::time::Instant>,
    pending_requests: std::collections::HashSet<Exchange>,
    // Keyboard navigation
    focused_index: Option<usize>,
    selected_chart_type: Option<ChartType>,
    // Shared favorites provided by dashboard; kept in a fast lookup map
    favorites_map: HashMap<Exchange, std::collections::HashSet<Ticker>>,
}

impl TickersTable {
    pub fn new(favorited_tickers: Vec<(Exchange, Ticker)>) -> (Self, Task<Message>) {
        // Build favorites map from provided list
        let mut favorites_map: HashMap<Exchange, std::collections::HashSet<Ticker>> = HashMap::new();
        for (ex, tk) in favorited_tickers {
            favorites_map.entry(ex).or_default().insert(tk);
        }

        (
            Self {
                ticker_data: HashMap::new(),
                normalized_search: String::new(),
                search_terms: Vec::new(),
                selected_tab: TickerTab::All,
                show_sort_options: false,
                selected_sort_option: SortOptions::VolumeDesc,
                expand_ticker_card: None,
                scroll_offset: AbsoluteOffset::default(),
                selected_market: None,
                is_show: false,
                tickers_info: HashMap::new(),
                needs_filter_update: true,
                cached_sort_option: None,
                cached_sorted_indices: Vec::new(),
                last_scroll_offset: AbsoluteOffset::default(),
                scroll_velocity: 0.0,
                last_update_times: HashMap::new(),
                pending_requests: std::collections::HashSet::new(),
                focused_index: None,
                selected_chart_type: Some(ChartType::Candlestick), // Default to candlestick
                favorites_map,
            },
            fetch_tickers_info(),
        )
    }

    pub fn set_favorites(&mut self, favorited_tickers: &[(Exchange, Ticker)]) {
        // Rebuild shared favorites map
        let mut new_map: HashMap<Exchange, std::collections::HashSet<Ticker>> = HashMap::new();
        for (ex, tk) in favorited_tickers {
            new_map.entry(*ex).or_default().insert(*tk);
        }
        self.favorites_map = new_map;

        // Reflect into current entries so Favorites tab and icons update immediately
        for (exchange, entries) in self.ticker_data.iter_mut() {
            let set = self
                .favorites_map
                .get(exchange)
                .cloned()
                .unwrap_or_default();
            for entry in entries.iter_mut() {
                entry.is_favorite = set.contains(&entry.ticker);
                entry.display_data = None; // invalidate display cache
            }
        }

        // Invalidate caches since filtering may have changed
        self.cached_sort_option = None;
        self.cached_sorted_indices.clear();
        self.needs_filter_update = true;
    }

    pub fn update_table(&mut self, exchange: Exchange, ticker_stats: HashMap<Ticker, TickerStats>) {
        // Get existing favorites for this exchange
        let existing_entries = self.ticker_data.get(&exchange);
        let mut favorites_map = std::collections::HashMap::new();

        if let Some(entries) = existing_entries {
            for entry in entries {
                if entry.is_favorite {
                    favorites_map.insert(entry.ticker, true);
                }
            }
        }

        // Create new entries with preserved favorites and apply shared favorites
        let entries: Vec<TickerEntry> = ticker_stats
            .into_iter()
            .map(|(ticker, stats)| {
                let (symbol, _) = ticker.to_full_symbol_and_type();
                let shared_fav = self
                    .favorites_map
                    .get(&exchange)
                    .map(|set| set.contains(&ticker))
                    .unwrap_or(false);
                TickerEntry {
                    ticker,
                    stats,
                    is_favorite: *favorites_map.get(&ticker).unwrap_or(&false) || shared_fav,
                    display_data: Some(Self::compute_display_data(&ticker, &stats)), // Pre-compute for performance
                    search_string: symbol, // Pre-compute for fast searching
                }
            })
            .collect();

        self.ticker_data.insert(exchange, entries);
        self.needs_filter_update = true;
    }

    fn get_filtered_tickers(&self) -> Vec<(Exchange, usize)> {
        // Use cached results if available and valid
        if !self.needs_filter_update &&
           self.cached_sort_option == Some(self.selected_sort_option.clone()) &&
           !self.cached_sorted_indices.is_empty() {
            return self.cached_sorted_indices.clone();
        }

        let mut filtered = Vec::new();

        // Collect all matching entries with their indices
        for (exchange, entries) in &self.ticker_data {
            // Filter by tab
            let tab_matches = match self.selected_tab {
                TickerTab::All => true,
                TickerTab::Favorites => entries.iter().any(|e| e.is_favorite),
                _ => Self::matches_exchange(*exchange, &self.selected_tab),
            };

            if !tab_matches {
                continue;
            }

            for (index, entry) in entries.iter().enumerate() {
                let (_, market) = entry.ticker.to_full_symbol_and_type();

                // Apply filters (use cached search string)
                let search_matches = self.matches_search(&entry.search_string);
                let market_matches = match self.selected_market {
                    Some(market_type) => market == market_type,
                    None => true,
                };
                let favorites_matches = match self.selected_tab {
                    TickerTab::Favorites => entry.is_favorite,
                    _ => true,
                };

                if search_matches && market_matches && favorites_matches {
                    filtered.push((*exchange, index));
                }
            }
        }

        // Sort the filtered results using unstable sort for better performance
        filtered.sort_unstable_by(|a, b| {
            let get_stats = |&(ex, idx): &(Exchange, usize)| {
                self.ticker_data[&ex][idx].stats
            };

            let stats_a = get_stats(a);
            let stats_b = get_stats(b);

            match self.selected_sort_option {
                SortOptions::VolumeDesc => stats_b.daily_volume.partial_cmp(&stats_a.daily_volume),
                SortOptions::VolumeAsc => stats_a.daily_volume.partial_cmp(&stats_b.daily_volume),
                SortOptions::ChangeDesc => stats_b.daily_price_chg.partial_cmp(&stats_a.daily_price_chg),
                SortOptions::ChangeAsc => stats_a.daily_price_chg.partial_cmp(&stats_b.daily_price_chg),
            }.unwrap_or(std::cmp::Ordering::Equal)
        });

        filtered
    }

    fn matches_search(&self, ticker_str: &str) -> bool {
        if self.search_terms.is_empty() {
            return true;
        }

        // Fast path: if we have a single search term, use contains
        if self.search_terms.len() == 1 {
            return ticker_str.contains(&self.search_terms[0]);
        }

        // Multi-term search: all terms must match (AND logic)
        self.search_terms.iter().all(|term| ticker_str.contains(term))
    }

    fn update_sort_cache(&mut self) {
        if !self.needs_filter_update {
            self.cached_sorted_indices = self.get_filtered_tickers();
            self.cached_sort_option = Some(self.selected_sort_option.clone());
        }
    }

    fn should_update_exchange(&self, exchange: Exchange) -> bool {
        if !self.is_show {
            return false; // Don't update if table is not visible
        }

        let now = std::time::Instant::now();
        let last_update = self.last_update_times.get(&exchange);

        match last_update {
            Some(time) => {
                let elapsed = now.duration_since(*time);
                let min_interval = if self.pending_requests.contains(&exchange) {
                    // Longer interval if request is pending
                    std::time::Duration::from_secs(10)
                } else {
                    // Normal update interval
                    std::time::Duration::from_secs(ACTIVE_UPDATE_INTERVAL)
                };
                elapsed >= min_interval
            }
            None => true, // Never updated before
        }
    }

    fn mark_exchange_updated(&mut self, exchange: Exchange) {
        self.last_update_times.insert(exchange, std::time::Instant::now());
        self.pending_requests.remove(&exchange);
    }

    fn navigate_up(&mut self) {
        let filtered_tickers = self.get_filtered_tickers();
        if let Some(current) = self.focused_index {
            if current > 0 {
                self.focused_index = Some(current - 1);
            }
        } else if !filtered_tickers.is_empty() {
            self.focused_index = Some(0);
        }
    }

    fn navigate_down(&mut self) {
        let filtered_tickers = self.get_filtered_tickers();
        let max_index = filtered_tickers.len().saturating_sub(1);
        if let Some(current) = self.focused_index {
            if current < max_index {
                self.focused_index = Some(current + 1);
            }
        } else if !filtered_tickers.is_empty() {
            self.focused_index = Some(0);
        }
    }

    fn select_focused_ticker(&mut self) -> Option<Action> {
        let filtered_tickers = self.get_filtered_tickers();
        if let Some(index) = self.focused_index
            && index < filtered_tickers.len() {
            let (exchange, entry_index) = filtered_tickers[index];
            let entry = &self.ticker_data[&exchange][entry_index];

            // Instead of directly selecting a chart, expand the ticker card
            // This will show the chart type selection options
            self.expand_ticker_card = Some((entry.ticker, exchange));
            // Reset to default chart type selection
            self.selected_chart_type = Some(ChartType::Candlestick);
            // No action needed since expansion happens locally
            return None;
        }
        None
    }

    fn clear_focus(&mut self) {
        self.focused_index = None;
        self.expand_ticker_card = None;
    }

    fn navigate_chart_type_up(&mut self) {
        if let Some(current) = self.selected_chart_type.clone() {
            let all_types = ChartType::all();
            if let Some(pos) = all_types.iter().position(|ct| *ct == current)
                && pos > 0 {
                self.selected_chart_type = Some(all_types[pos - 1].clone());
            }
        }
    }

    fn navigate_chart_type_down(&mut self) {
        if let Some(current) = self.selected_chart_type.clone() {
            let all_types = ChartType::all();
            if let Some(pos) = all_types.iter().position(|ct| *ct == current)
                && pos < all_types.len() - 1 {
                self.selected_chart_type = Some(all_types[pos + 1].clone());
            }
        }
    }

    fn select_chart_type(&mut self) -> Option<Action> {
        if let (Some((ticker, exchange)), Some(chart_type)) = (self.expand_ticker_card, self.selected_chart_type.clone()) {
            // Keep the expanded view open so user can select other chart types
            // Don't close with: self.expand_ticker_card = None;

            // Get ticker info and return action to open the selected chart
            let ticker_info = self
                .tickers_info
                .get(&exchange)
                .and_then(|info| info.get(&ticker))
                .copied()
                .flatten();

            if let Some(ticker_info) = ticker_info {
                let chart_type_str = match chart_type {
                    ChartType::Heatmap => "heatmap",
                    ChartType::Footprint => "footprint",
                    ChartType::Candlestick => "candlestick",
                    ChartType::TimeAndSales => "time&sales",
                };

                return Some(Action::TickerSelected(ticker_info, exchange, chart_type_str.to_string()));
            }
        }
        None
    }

    fn change_sort_option(&mut self, option: SortOptions) {
        let old_option = self.selected_sort_option.clone();
        if self.selected_sort_option == option {
            self.selected_sort_option = match self.selected_sort_option {
                SortOptions::VolumeDesc => SortOptions::VolumeAsc,
                SortOptions::VolumeAsc => SortOptions::VolumeDesc,
                SortOptions::ChangeDesc => SortOptions::ChangeAsc,
                SortOptions::ChangeAsc => SortOptions::ChangeDesc,
            };
        } else {
            self.selected_sort_option = option;
        }

        // Invalidate cache if sort option changed
        if old_option != self.selected_sort_option {
            self.cached_sort_option = None;
            self.cached_sorted_indices.clear();
        } else {
            // If sort option didn't change, update cache
            self.update_sort_cache();
        }
        self.needs_filter_update = true;
    }

    fn favorite_ticker(&mut self, exchange: Exchange, ticker: Ticker) {
        if let Some(entries) = self.ticker_data.get_mut(&exchange) {
            for entry in entries {
                if entry.ticker == ticker {
                    entry.is_favorite = !entry.is_favorite;
                    // Clear display cache since favorite status changed
                    entry.display_data = None;
                    break;
                }
            }
        }
        // Update shared favorites map
        let set = self.favorites_map.entry(exchange).or_default();
        if set.contains(&ticker) {
            set.remove(&ticker);
        } else {
            set.insert(ticker);
        }
        // Invalidate caches since filtering may have changed
        self.cached_sort_option = None;
        self.cached_sorted_indices.clear();
        self.needs_filter_update = true;
    }

    pub fn favorited_tickers(&self) -> Vec<(Exchange, Ticker)> {
        // Prefer the shared favorites map as the source of truth
        let mut out: Vec<(Exchange, Ticker)> = Vec::new();
        for (ex, set) in &self.favorites_map {
            for tk in set {
                out.push((*ex, *tk));
            }
        }
        out
    }

    fn compute_display_data(ticker: &Ticker, stats: &TickerStats) -> TickerDisplayData {
        let (ticker_str, market) = ticker.display_symbol_and_type();
        
        // Optimize string operations
        let display_ticker = if ticker_str.len() >= 11 {
            format!("{}...", &ticker_str[..9])
        } else {
            let suffix = match market {
                MarketKind::Spot => "",
                MarketKind::LinearPerps | MarketKind::InversePerps => "P",
            };
            format!("{}{}", ticker_str, suffix)
        };

        TickerDisplayData {
            display_ticker,
            price_change_display: data::util::pct_change(stats.daily_price_chg),
            volume_display: data::util::currency_abbr(stats.daily_volume),
            mark_price_display: stats.mark_price.to_string(),
            card_color_alpha: (stats.daily_price_chg / 8.0).clamp(-1.0, 1.0),
        }
    }

    fn matches_exchange(ex: Exchange, tab: &TickerTab) -> bool {
        match tab {
            TickerTab::Bybit => matches!(
                ex,
                Exchange::BybitLinear | Exchange::BybitSpot | Exchange::BybitInverse
            ),
            TickerTab::Binance => matches!(
                ex,
                Exchange::BinanceLinear | Exchange::BinanceInverse | Exchange::BinanceSpot
            ),
            TickerTab::Hyperliquid => matches!(
                ex,
                Exchange::HyperliquidPerps
            ),
            TickerTab::Okx => matches!(
                ex,
                Exchange::OkxLinear | Exchange::OkxInverse | Exchange::OkxSpot
            ),
            _ => false,
        }
    }

    fn create_ticker_container_with_focus<'a>(
        &'a self,
        is_visible: bool,
        exchange: Exchange,
        ticker: &'a Ticker,
        is_fav: bool,
        is_focused: bool,
    ) -> Element<'a, Message> {
        if !is_visible {
            // Return a placeholder with consistent height
            return container(column![])
                .width(Length::Fill)
                .height(Length::Fixed(TICKER_CARD_HEIGHT))
                .style(style::ticker_card)
                .into();
        }

        // Get entry and compute display data if needed
        let entries = &self.ticker_data[&exchange];
        let entry_index = entries.iter().position(|e| e.ticker == *ticker).unwrap();
        let entry = &entries[entry_index];

        // Compute display data (cached or fresh)
        let display_data = if let Some(ref data) = entry.display_data {
            data.clone()
        } else {
            // Compute and cache the display data
            let computed_data = Self::compute_display_data(ticker, &entry.stats);
            // Note: We can't mutate here, but this ensures we compute it once per render cycle
            computed_data
        };

        if let Some((selected_ticker, selected_exchange)) = &self.expand_ticker_card {
            if ticker == selected_ticker && exchange == *selected_exchange {
                container(create_expanded_ticker_card(
                    exchange,
                    ticker,
                    display_data,
                    is_fav,
                    &self.selected_chart_type,
                ))
                .style(style::ticker_card)
                .into()
            } else {
                create_ticker_card_with_focus(exchange, ticker, display_data.clone(), is_focused, is_fav)
            }
        } else {
            create_ticker_card_with_focus(exchange, ticker, display_data, is_focused, is_fav)
        }
    }



    fn is_container_visible(&self, index: usize, bounds: Size) -> bool {
        // Compute a conservative header height (everything above the ticker list)
        let mut header_height = SEARCH_BAR_HEIGHT;
        let section_spacing = if self.show_sort_options {
            COLUMN_SPACING_WITH_SORT
        } else {
            COLUMN_SPACING_NO_SORT
        };
        // spacing between search bar and next section
        header_height += section_spacing;
        if self.show_sort_options {
            // First row: sort buttons
            header_height += SORT_BUTTON_ROW_HEIGHT;
            // Optional market filter row for All/Bybit/Binance/Okx
            let show_market_filters = matches!(
                self.selected_tab,
                TickerTab::All | TickerTab::Bybit | TickerTab::Binance
            );
            if show_market_filters {
                header_height += MARKET_BUTTON_ROW_HEIGHT + SORT_COLUMN_SPACING;
            }
            // Horizontal rule and internal spacing
            header_height += HRULE_THICKNESS + SORT_COLUMN_SPACING;
            // spacing between sort section and exchange filter row
            header_height += section_spacing;
        }
        // Exchange filters row (ALL/Bybit/Binance/HL/Favorites)
        header_height += EXCHANGE_FILTERS_ROW_HEIGHT;

        // Position of this item (include per-item spacing)
        let item_top = header_height + (index as f32) * (TICKER_CARD_HEIGHT + CARD_SPACING);
        let item_bottom = item_top + TICKER_CARD_HEIGHT;

        // Generous visibility calculation with large buffer to prevent disappearing cards
        let buffer_height = bounds.height * 3.0; // 3x viewport height buffer
        let viewport_top = self.scroll_offset.y - buffer_height;
        let viewport_bottom = self.scroll_offset.y + bounds.height + buffer_height;

        item_bottom >= viewport_top && item_top <= viewport_bottom
    }


    pub fn is_open(&self) -> bool {
        self.is_show
    }

    pub fn update_ticker_info(
        &mut self,
        exchange: Exchange,
        info: HashMap<Ticker, Option<TickerInfo>>,
    ) -> Action {
        if let Some(tickers) = self.tickers_info.get_mut(&exchange) {
            for (ticker, ticker_info) in info {
                if let Some(existing_ticker_info) = tickers.get_mut(&ticker) {
                    *existing_ticker_info = ticker_info;
                } else {
                    tickers.insert(ticker, ticker_info);
                }
            }
        } else {
            self.tickers_info.insert(exchange, info);
        }

        let task = Task::perform(fetch_ticker_prices(exchange), move |result| match result {
            Ok(ticker_stats) => Message::UpdateTickerStats(exchange, ticker_stats),

            Err(err) => Message::ErrorOccurred(InternalError::Fetch(err.to_string())),
        });

        Action::Fetch(task)
    }

    pub fn update_ticker_stats(&mut self, exchange: Exchange, stats: HashMap<Ticker, TickerStats>) {
        let tickers = self
            .tickers_info
            .get(&exchange)
            .map(|info| info.keys().copied().collect::<Vec<_>>())
            .unwrap_or_default();

        let filtered_tickers_stats = stats
            .into_iter()
            .filter(|(ticker, _)| tickers.iter().any(|t| t == ticker))
            .collect::<HashMap<Ticker, TickerStats>>();

        self.update_table(exchange, filtered_tickers_stats);
    }

    pub fn update(&mut self, message: Message) -> Option<Action> {
        match message {
            Message::ChangeTickersTableTab(tab) => {
                self.selected_tab = tab.clone();
            // Clear market filter when switching to Hyperliquid (no spot/inverse distinction)
            if matches!(tab, TickerTab::Hyperliquid) {
                    self.selected_market = None;
                }
                self.needs_filter_update = true;
            }
            Message::UpdateSearchQuery(query) => {
                let normalized = query.to_uppercase();
                if self.normalized_search != normalized {
                    self.normalized_search = normalized;
                    // Pre-process search terms for faster matching
                    self.search_terms = if self.normalized_search.is_empty() {
                        Vec::new()
                    } else {
                        self.normalized_search
                            .split_whitespace()
                            .map(|s| s.to_string())
                            .collect()
                    };
                    self.needs_filter_update = true;
                }
            }
            Message::ChangeSortOption(option) => {
                self.change_sort_option(option);
            }
            Message::ShowSortingOptions => {
                self.show_sort_options = !self.show_sort_options;
            }
            Message::ExpandTickerCard(is_ticker) => {
                self.expand_ticker_card = is_ticker;
                // Clear keyboard selection when expanding via mouse
                // This ensures the blue highlight only shows during keyboard navigation
                self.selected_chart_type = None;
            }
            Message::FavoriteTicker(exchange, ticker) => {
                self.favorite_ticker(exchange, ticker);
            }
            Message::Scrolled(viewport) => {
                let new_offset = viewport.absolute_offset();
                // Calculate scroll velocity for predictive rendering
                let delta_y = new_offset.y - self.scroll_offset.y;
                self.scroll_velocity = delta_y.abs() / 16.67; // Assume 60fps

                self.last_scroll_offset = self.scroll_offset;
                self.scroll_offset = new_offset;
            }
            Message::SetMarketFilter(market) => {
                if self.selected_market == market {
                    self.selected_market = None;
                } else {
                    self.selected_market = market;
                }
                self.needs_filter_update = true;
            }
            Message::TickerSelected(ticker, exchange, chart_type) => {
                let ticker_info = self
                    .tickers_info
                    .get(&exchange)
                    .and_then(|info| info.get(&ticker))
                    .copied()
                    .flatten();

                if let Some(ticker_info) = ticker_info {
                    return Some(Action::TickerSelected(ticker_info, exchange, chart_type));
                } else {
                    log::warn!("Ticker info not found for {ticker:?} on {exchange:?}");
                }
            }
            Message::ToggleTable => {
                let was_open = self.is_show;
                self.is_show = !self.is_show;

                // Clear keyboard focus when table is toggled
                if !was_open && self.is_show {
                    self.focused_index = None; // Clear any keyboard focus
                }
            }
            Message::FetchForTickerStats(exchange) => {
                let task = if let Some(exchange) = exchange {
                    // Check if update is needed for this specific exchange
                    if !self.should_update_exchange(exchange) {
                        return None;
                    }
                    self.pending_requests.insert(exchange);

                    Task::perform(fetch_ticker_prices(exchange), move |result| match result {
                        Ok(ticker_stats) => Message::UpdateTickerStats(exchange, ticker_stats),
                        Err(err) => Message::ErrorOccurred(InternalError::Fetch(err.to_string())),
                    })
                } else {
                    // Batch fetch for all exchanges that need updates
                    let fetch_tasks = Exchange::ALL
                        .iter()
                        .filter(|&&ex| self.should_update_exchange(ex))
                        .map(|exchange| {
                            // Mark as pending
                            // Note: This requires mutable access, so we'll handle it differently
                            Task::perform(fetch_ticker_prices(*exchange), move |result| {
                                match result {
                                    Ok(ticker_stats) => {
                                        Message::UpdateTickerStats(*exchange, ticker_stats)
                                    }
                                    Err(err) => Message::ErrorOccurred(InternalError::Fetch(
                                        err.to_string(),
                                    )),
                                }
                            })
                        })
                        .collect::<Vec<Task<Message>>>();

                    // Mark all as pending
                    for &ex in Exchange::ALL.iter() {
                        if self.should_update_exchange(ex) {
                            self.pending_requests.insert(ex);
                        }
                    }

                    if fetch_tasks.is_empty() {
                        return None; // No updates needed
                    }

                    Task::batch(fetch_tasks)
                };

                return Some(Action::Fetch(task));
            }
            Message::UpdateTickerStats(exchange, stats) => {
                self.update_ticker_stats(exchange, stats);
                self.mark_exchange_updated(exchange);
            }
            Message::UpdateTickersInfo(exchange, info) => {
                self.update_ticker_info(exchange, info);

                let task =
                    Task::perform(fetch_ticker_prices(exchange), move |result| match result {
                        Ok(ticker_stats) => Message::UpdateTickerStats(exchange, ticker_stats),

                        Err(err) => Message::ErrorOccurred(InternalError::Fetch(err.to_string())),
                    });

                return Some(Action::Fetch(task));
            }
            Message::ErrorOccurred(err) => {
                log::error!("Error occurred: {err}");
                return Some(Action::ErrorOccurred(err));
            }
            Message::KeyboardNavigate(key, _modifiers) => {
                // If ticker card is expanded, handle chart type navigation
                if self.expand_ticker_card.is_some() {
                    match key {
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::ArrowUp) => {
                            self.navigate_chart_type_up();
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::ArrowDown) => {
                            self.navigate_chart_type_down();
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Enter) => {
                            if let Some(action) = self.select_chart_type() {
                                return Some(action);
                            }
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Backspace) => {
                            self.expand_ticker_card = None;
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Escape) => {
                            self.expand_ticker_card = None;
                        }
                        _ => {}
                    }
                } else {
                    // Normal ticker navigation
                    match key {
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::ArrowUp) => {
                            self.navigate_up();
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::ArrowDown) => {
                            self.navigate_down();
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Enter) => {
                            if let Some(action) = self.select_focused_ticker() {
                                return Some(action);
                            }
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Escape) => {
                            self.clear_focus();
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Backspace) => {
                            self.focused_index = None;
                        }
                        _ => {}
                    }
                }
            }
        }

        None
    }

    pub fn view(&self, bounds: Size) -> Element<'_, Message> {
        // Get filtered data (immutable operation)
        let filtered_tickers = self.get_filtered_tickers();

        let sorting_button = if self.show_sort_options {
            button(icon_text(Icon::Sort, 16).align_x(Horizontal::Center))
                .on_press(Message::ShowSortingOptions)
                .width(Length::Fixed(36.0))
                .height(Length::Fixed(36.0))
                .style(|theme, status| style::button::transparent(theme, status, true))
        } else {
            button(icon_text(Icon::Sort, 16).align_x(Horizontal::Center))
                .on_press(Message::ShowSortingOptions)
                .width(Length::Fixed(36.0))
                .height(Length::Fixed(36.0))
                .style(|theme, status| style::button::transparent(theme, status, false))
        };

        let search_bar_row = row![
            text_input("Search all markets", &self.normalized_search)
                .style(style::search_input)
                .padding(padding::left(10).right(10).top(8).bottom(8))
                .on_input(|value| {
                    let filtered_value = value.replace("/", "");
                    Message::UpdateSearchQuery(filtered_value)
                })
                .id("ticker_search"),
            sorting_button
        ]
        .align_y(Vertical::Center)
        .spacing(8)
        .height(Length::Fixed(SEARCH_BAR_HEIGHT));

        let sort_options_column = {
            // Only show market filters for exchanges that support them
            let show_spot = matches!(self.selected_tab, TickerTab::All | TickerTab::Bybit | TickerTab::Binance | TickerTab::Okx);
            let show_linear = matches!(self.selected_tab, TickerTab::All | TickerTab::Bybit | TickerTab::Binance | TickerTab::Okx);
            let show_inverse = matches!(self.selected_tab, TickerTab::All | TickerTab::Bybit | TickerTab::Binance | TickerTab::Okx);
            
            let spot_market_button = button(text("Spot").align_x(Horizontal::Center))
                .on_press(Message::SetMarketFilter(Some(MarketKind::Spot)))
                .style(|theme, status| style::button::transparent(theme, status, false));

            let linear_markets_btn = button(text("Linear").align_x(Horizontal::Center))
                .on_press(Message::SetMarketFilter(Some(MarketKind::LinearPerps)))
                .style(|theme, status| style::button::transparent(theme, status, false));

            let inverse_markets_btn = button(text("Inverse").align_x(Horizontal::Center))
                .on_press(Message::SetMarketFilter(Some(MarketKind::InversePerps)))
                .style(|theme, status| style::button::transparent(theme, status, false));

            let volume_content = row![
                Space::new(Length::Fill, Length::Shrink),
                row![
                    text("Volume").align_x(Horizontal::Center),
                    icon_text(
                        if self.selected_sort_option == SortOptions::VolumeDesc { Icon::SortDesc } else { Icon::SortAsc },
                        14
                    )
                ]
                .spacing(6)
                .align_y(Vertical::Center),
                Space::new(Length::Fill, Length::Shrink),
            ];

            let volume_sort_button = button(volume_content)
            .on_press(Message::ChangeSortOption(SortOptions::VolumeAsc))
            .width(Length::Fill)
            .height(Length::Fixed(32.0));

            let change_content = row![
                Space::new(Length::Fill, Length::Shrink),
                row![
                    text("Change").align_x(Horizontal::Center),
                    icon_text(
                        if self.selected_sort_option == SortOptions::ChangeDesc { Icon::SortDesc } else { Icon::SortAsc },
                        14
                    )
                ]
                .spacing(6)
                .align_y(Vertical::Center),
                Space::new(Length::Fill, Length::Shrink),
            ];

            let change_sort_button = button(change_content)
            .on_press(Message::ChangeSortOption(SortOptions::ChangeAsc))
            .width(Length::Fill)
            .height(Length::Fixed(32.0));

            column![
                row![
                    volume_sort_button.style({
                        let selected_option = self.selected_sort_option.clone();
                        move |theme, status| {
                            style::button::transparent(
                                theme,
                                status,
                                matches!(
                                    selected_option,
                                    SortOptions::VolumeAsc | SortOptions::VolumeDesc
                                ),
                            )
                        }
                    }),
                    change_sort_button.style({
                        let selected_option = self.selected_sort_option.clone();
                        move |theme, status| {
                            style::button::transparent(
                                theme,
                                status,
                                matches!(
                                    selected_option,
                                    SortOptions::ChangeAsc | SortOptions::ChangeDesc
                                ),
                            )
                        }
                    }),
                ]
                .spacing(8),
                if show_spot || show_linear || show_inverse {
                    row![
                        spot_market_button
                            .width(Length::FillPortion(1))
                            .height(Length::Fixed(28.0))
                            .style({
                                let selected_market = self.selected_market;
                                move |theme, status| {
                                    style::button::transparent(
                                        theme,
                                        status,
                                        matches!(selected_market, Some(MarketKind::Spot)),
                                    )
                                }
                            }),
                        linear_markets_btn
                            .width(Length::FillPortion(1))
                            .height(Length::Fixed(28.0))
                            .style({
                                let selected_market = self.selected_market;
                                move |theme, status| {
                                    style::button::transparent(
                                        theme,
                                        status,
                                        matches!(selected_market, Some(MarketKind::LinearPerps)),
                                    )
                                }
                            }),
                        inverse_markets_btn
                            .width(Length::FillPortion(1))
                            .height(Length::Fixed(28.0))
                            .style({
                                let selected_market = self.selected_market;
                                move |theme, status| {
                                    style::button::transparent(
                                        theme,
                                        status,
                                        matches!(selected_market, Some(MarketKind::InversePerps)),
                                    )
                                }
                            }),
                    ]
                    .spacing(8)
                } else {
                    row![]
                },
                horizontal_rule(1.0).style(style::split_ruler),
            ]
            .spacing(4)
        };

        let exchange_filters_row = {
            let all_button = create_tab_button(text("ALL").size(16).align_x(Horizontal::Center), &self.selected_tab, TickerTab::All)
                .width(Length::FillPortion(1))
                .height(Length::Fixed(32.0));
            let bybit_button =
                create_tab_button(icon_text(Icon::BybitLogo, 18).align_x(Horizontal::Center), &self.selected_tab, TickerTab::Bybit)
                    .width(Length::FillPortion(1))
                    .height(Length::Fixed(32.0));
            let binance_button =
                create_tab_button(icon_text(Icon::BinanceLogo, 18).align_x(Horizontal::Center), &self.selected_tab, TickerTab::Binance)
                    .width(Length::FillPortion(1))
                    .height(Length::Fixed(32.0));
            let hyperliquid_button =
                create_tab_button(icon_text(Icon::HyperliquidLogo, 16).align_x(Horizontal::Center), &self.selected_tab, TickerTab::Hyperliquid)
                    .width(Length::FillPortion(1))
                    .height(Length::Fixed(32.0));
            let okx_button =
                create_tab_button(icon_text(Icon::OkxLogo, 16).align_x(Horizontal::Center), &self.selected_tab, TickerTab::Okx)
                    .width(Length::FillPortion(1))
                    .height(Length::Fixed(32.0));
            let favorites_button = create_tab_button(
                icon_text(Icon::StarFilled, 18).align_x(Horizontal::Center),
                &self.selected_tab,
                TickerTab::Favorites,
            )
            .width(Length::FillPortion(1))
            .height(Length::Fixed(32.0));

            row![
                favorites_button,
                all_button,
                bybit_button,
                binance_button,
                hyperliquid_button,
                okx_button,
            ]
            .spacing(8)
            .width(Length::Fill)
        };

        let mut content = column![search_bar_row]
            .spacing(if self.show_sort_options { 8 } else { 4 })
            .padding(padding::right(10).left(10))
            .width(Length::Fixed(BROWSER_WIDTH));

        if self.show_sort_options {
            content = content.push(sort_options_column);
        };

        content = content.push(exchange_filters_row);

        let mut ticker_cards = column![].spacing(4);

        // Use pre-filtered and sorted data with simplified virtualization
        for (index, (exchange, entry_index)) in filtered_tickers.iter().enumerate() {
            let is_focused = Some(index) == self.focused_index;
            let entry = &self.ticker_data[exchange][*entry_index];
            
            // Always render the card, but use visibility to determine content detail
            let is_visible = self.is_container_visible(index, bounds);
            ticker_cards = ticker_cards.push(self.create_ticker_container_with_focus(
                is_visible, *exchange, &entry.ticker, entry.is_favorite, is_focused,
            ));
        }

        content = content.push(ticker_cards);

        container(
            scrollable::Scrollable::with_direction(
                content,
                scrollable::Direction::Vertical(
                    scrollable::Scrollbar::new().width(8).scroller_width(6),
                ),
            )
            .on_scroll(Message::Scrolled)
            .style(style::scroll_bar)
        )
        .width(Length::Fixed(BROWSER_WIDTH))
        .height(Length::Fill)
        .style(|theme: &Theme| {
            let palette = theme.extended_palette();
            iced::widget::container::Style {
                background: Some(iced::Background::Color(palette.background.base.color.scale_alpha(0.98))),
                text_color: Some(palette.background.base.text),
                border: iced::Border {
                    width: 1.0,
                    color: palette.background.weak.color,
                    radius: 6.0.into(),
                },
                ..Default::default()
            }
        })
        .into()
    }

    pub fn subscription(&self) -> Subscription<Message> {
        let timer = iced::time::every(std::time::Duration::from_secs(if self.is_open() {
            ACTIVE_UPDATE_INTERVAL
        } else {
            INACTIVE_UPDATE_INTERVAL
        }))
        .map(|_| Message::FetchForTickerStats(None));

        timer
    }

    // A 'static subscription usable when a table is known to be open; avoids borrowing self
    pub fn open_subscription() -> Subscription<Message> {
        let timer = iced::time::every(std::time::Duration::from_secs(ACTIVE_UPDATE_INTERVAL))
            .map(|_| Message::FetchForTickerStats(None));

        timer
    }
}



fn create_ticker_card_with_focus(
    exchange: Exchange,
    ticker: &Ticker,
    display_data: TickerDisplayData,
    is_focused: bool,
    is_fav: bool,
) -> Element<'static, Message> {
    // Left clickable area: icon + ticker name + volume
            let left_click_area = button(
        row![
            {
                let (icon, size) = style::exchange_icon_with_size(exchange);
                icon_text(icon, size)
            },
            Space::new(Length::Fixed(6.0), Length::Shrink),
            text(display_data.display_ticker.clone()).style(move |theme: &Theme| {
                // Contrast-safe color: blend from the theme's base text color toward success/danger
                // based on daily change magnitude. This ensures readability on light/dark themes.
                let palette = theme.extended_palette();
                let magnitude = display_data.card_color_alpha.abs().clamp(0.0, 1.0);
                let threshold = 0.15; // ignore tiny moves

                let base = palette.background.base.text; // guaranteed high-contrast text color
                let target = if display_data.card_color_alpha >= 0.0 {
                    palette.success.strong.color
                } else {
                    palette.danger.strong.color
                };

                let t = if magnitude <= threshold {
                    0.0
                } else {
                    ((magnitude - threshold) / (1.0 - threshold)).clamp(0.0, 1.0)
                };

                // Lerp from base text color -> target accent
                let color = iced::Color {
                    r: base.r + (target.r - base.r) * t,
                    g: base.g + (target.g - base.g) * t,
                    b: base.b + (target.b - base.b) * t,
                    a: base.a + (target.a - base.a) * t,
                };

                let mut st = iced::widget::text::Style::default();
                st.color = Some(color);
                st
            }),
            Space::new(Length::Fill, Length::Shrink),
            text(display_data.volume_display.clone()),
        ]
        .align_y(Alignment::Center)
        .spacing(4),
    )
    .style(|theme, status| style::button::transparent(theme, status, false))
    .on_press(Message::ExpandTickerCard(Some((*ticker, exchange))));

    let fav_icon = if is_fav {
        icon_text(Icon::StarFilled, 12).style(|_theme: &Theme| {
            let mut st = iced::widget::text::Style::default();
            st.color = Some(iced::Color::from_rgb(120.0/255.0, 86.0/255.0, 255.0/255.0));
            st
        })
    } else {
        icon_text(Icon::Star, 12)
    };

    let fav_button = button(fav_icon)
        .style(|theme, status| style::button::transparent(theme, status, false))
        .on_press(Message::FavoriteTicker(exchange, *ticker));

    let focused = is_focused;
    container(
        row![
            left_click_area.width(Length::Fill),
            Space::new(Length::Fixed(0.0), Length::Shrink),
            fav_button,
        ]
        .align_y(Alignment::Center)
        .spacing(2)
        .padding(padding::left(8).right(6).top(4).bottom(4)),
    )
    .style(move |theme: &Theme| {
        let mut base = style::ticker_card(theme);
        if focused {
            base.border = iced::Border {
                color: iced::Color::from_rgb(120.0/255.0, 86.0/255.0, 255.0/255.0),
                width: 2.0,
                radius: base.border.radius,
            };
        }
        base
    })
    .height(Length::Fixed(TICKER_CARD_HEIGHT))
    .into()
}

fn create_chart_type_button(
    label: &str,
    chart_type: ChartType,
    ticker: &Ticker,
    exchange: Exchange,
    selected_chart_type: &Option<ChartType>,
) -> iced::widget::Button<'static, Message> {
    let is_selected = selected_chart_type.as_ref() == Some(&chart_type);

    let chart_type_str = match chart_type {
        ChartType::Heatmap => "heatmap",
        ChartType::Footprint => "footprint",
        ChartType::Candlestick => "candlestick",
        ChartType::TimeAndSales => "time&sales",
    };

    // For chart type buttons, we want to keep the expanded view open
    // so users can select multiple chart types
    let button = button(text(label.to_string()).align_x(Horizontal::Center))
        .on_press(Message::TickerSelected(
            *ticker,
            exchange,
            chart_type_str.to_string()
        ))
        .width(Length::Fill)
        .height(Length::Fixed(28.0));

    if is_selected {
        button.style(|theme: &iced::Theme, status: iced::widget::button::Status| {
            let mut base = style::button::ticker_card(theme, status);
            // Add selection highlight
            base.border = iced::Border {
                color: iced::Color::from_rgb(0.2, 0.6, 1.0), // Blue highlight
                width: 2.0,
                radius: base.border.radius,
            };
            base
        })
    } else {
        button.style(style::button::ticker_card)
    }
}

fn create_expanded_ticker_card(
    exchange: Exchange,
    ticker: &Ticker,
    display_data: TickerDisplayData,
    is_fav: bool,
    selected_chart_type: &Option<ChartType>,
) -> Element<'static, Message> {
    let (ticker_str, market) = ticker.display_symbol_and_type();

    // Pre-compute color for daily change text (using same scheme as display_ticker)
    let change_style = move |theme: &Theme| {
        // Contrast-safe color: blend from the theme's base text color toward success/danger
        // based on daily change magnitude. This ensures readability on light/dark themes.
        let palette = theme.extended_palette();
        let magnitude = display_data.card_color_alpha.abs().clamp(0.0, 1.0);
        let threshold = 0.15; // ignore tiny moves

        let base = palette.background.base.text; // guaranteed high-contrast text color
        let target = if display_data.card_color_alpha >= 0.0 {
            palette.success.strong.color
        } else {
            palette.danger.strong.color
        };

        let t = if magnitude <= threshold {
            0.0
        } else {
            ((magnitude - threshold) / (1.0 - threshold)).clamp(0.0, 1.0)
        };

        // Lerp from base text color -> target accent
        let color = iced::Color {
            r: base.r + (target.r - base.r) * t,
            g: base.g + (target.g - base.g) * t,
            b: base.b + (target.b - base.b) * t,
            a: base.a + (target.a - base.a) * t,
        };
        
        let mut s = iced::widget::text::Style::default();
        s.color = Some(color);
        s
    };

    // Top bar with back + fav and title line
    let header = row![
        button(icon_text(Icon::Return, 12))
            .on_press(Message::ExpandTickerCard(None))
            .style(|theme, status| style::button::transparent(theme, status, false)),
        button(if is_fav {
            icon_text(Icon::StarFilled, 12).style(|_theme: &Theme| {
                let mut st = iced::widget::text::Style::default();
                st.color = Some(iced::Color::from_rgb(120.0/255.0, 86.0/255.0, 255.0/255.0));
                st
            })
        } else { icon_text(Icon::Star, 12) })
            .on_press(Message::FavoriteTicker(exchange, *ticker))
            .style(|theme, status| style::button::transparent(theme, status, false)),
        {
            let (icon, size) = style::exchange_icon_with_size(exchange);
            icon_text(icon, size)
        },
        text(format!("{} {}{}", ticker_str, market.to_string(), match market { MarketKind::Spot => "", MarketKind::LinearPerps | MarketKind::InversePerps => " Perp" })).size(14),
        Space::new(Length::Fill, Length::Shrink),
        button(icon_text(Icon::Link, 12))
            .on_press(Message::TickerSelected(*ticker, exchange, "link_group".to_string()))
            .style(|theme, status| style::button::transparent(theme, status, false)),
    ]
    .align_y(Alignment::Center)
    .spacing(6);

    // Stats row: three columns aligned
    let stats = column![
        row![
            text("Last Updated Price:").size(11),
            Space::new(Length::Fill, Length::Shrink),
            text(display_data.mark_price_display.clone()).size(12),
        ],
        row![
            text("Daily Change:").size(11),
            Space::new(Length::Fill, Length::Shrink),
            text(display_data.price_change_display.clone()).style(change_style).size(12),
        ],
        row![
            text("Daily Volume:").size(11),
            Space::new(Length::Fill, Length::Shrink),
            text(display_data.volume_display.clone()).size(12),
        ],
    ]
    .spacing(4)
    .padding(padding::left(4).right(4));

    // Chart buttons stacked, full width
    let chart_buttons = column![
        create_chart_type_button("Heatmap Chart", ChartType::Heatmap, ticker, exchange, selected_chart_type),
        create_chart_type_button("Footprint Chart", ChartType::Footprint, ticker, exchange, selected_chart_type),
        create_chart_type_button("Candlestick Chart", ChartType::Candlestick, ticker, exchange, selected_chart_type),
        create_chart_type_button("Time&Sales", ChartType::TimeAndSales, ticker, exchange, selected_chart_type),
    ]
    .spacing(6)
    .width(Length::Fill);

    column![
        header,
        Space::new(Length::Shrink, Length::Fixed(6.0)),
        container(stats).style(|theme: &Theme| {
            let palette = theme.extended_palette();
            iced::widget::container::Style {
                text_color: Some(palette.background.base.text.scale_alpha(0.95)),
                ..Default::default()
            }
        }),
        Space::new(Length::Shrink, Length::Fixed(6.0)),
        chart_buttons,
    ]
    .padding(padding::top(8).right(12).left(12).bottom(12))
    .spacing(8)
    .into()
}

fn create_tab_button<'a>(
    text: Text<'a, Theme, Renderer>,
    current_tab: &TickerTab,
    target_tab: TickerTab,
) -> Button<'a, Message, Theme, Renderer> {
    let mut btn =
        button(text)
            .style(|theme, status| style::button::transparent(theme, status, false))
            .padding(padding::left(8).right(8).top(6).bottom(6));
    if *current_tab != target_tab {
        btn = btn.on_press(Message::ChangeTickersTableTab(target_tab));
    }
    btn
}
