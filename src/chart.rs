pub mod heatmap;
pub mod indicator;
pub mod kline;
mod scale;

use iced::theme::palette::Extended;
use iced::widget::canvas::{self, Cache, Canvas, Event, Frame, LineDash, Stroke};
use iced::widget::{center, mouse_area, Space};
use iced::{
    Element, Length, Point, Rectangle, Size, Theme, Vector, alignment,
    keyboard, mouse::{self},
    padding,
    widget::{
        button, canvas::Path, column, container, row, text,
        tooltip::Position as TooltipPosition,
    },
};

// Helper function to create safe points that won't cause lyon_path panics
fn safe_point(x: f32, y: f32) -> Point {
    if x.is_finite() && y.is_finite() {
        Point::new(x, y)
    } else {
        Point::new(0.0, 0.0)
    }
}
use scale::{AxisLabelsX, AxisLabelsY, PriceInfoLabel};

use crate::widget::multi_split::{DRAG_SIZE, MultiSplit};
use crate::{style, widget::tooltip};
use data::chart::{Autoscale, Basis, PlotConstants, ViewConfig, indicator::Indicator};
use exchange::fetcher::{FetchRange, RequestHandler};
use exchange::{TickerInfo, Timeframe};

// Separate zoom sensitivities for different input devices
const ZOOM_SENSITIVITY_TRACKPAD: f32 = 150.0;  // Good for trackpad (pixel-based scrolling)
const ZOOM_SENSITIVITY_MOUSE: f32 = 30.0;     // Lower value = higher sensitivity for mouse wheel (line-based scrolling)

#[derive(Default, Debug, Clone, Copy)]
pub enum Interaction {
    #[default]
    None,
    Zoomin {
        last_position: Point,
    },
    Panning {
        translation: Vector,
        start: Point,
    },
    Ruler {
        start: Option<Point>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum AxisScaleClicked {
    X,
    Y,
}

// ChartConstants trait moved to data::chart::PlotConstants

#[derive(Debug, Clone)]
pub enum Message {
    Translated(Vector),
    Scaled(f32, Vector),
    AutoscaleToggle,
    SetAutoscale(Autoscale),
    CrosshairToggle,
    CrosshairMoved,
    YScaling(f32, f32, bool, f32),  // delta, cursor_to_center, is_wheel_scroll, zoom_sensitivity
    XScaling(f32, f32, bool, f32),  // delta, cursor_to_center, is_wheel_scroll, zoom_sensitivity
    BoundsChanged(Rectangle),
    SplitDragged(usize, f32),
    DoubleClick(AxisScaleClicked),
}

pub trait Chart: PlotConstants + canvas::Program<Message> {
    type IndicatorType: Indicator;

    fn state(&self) -> &ViewState;

    fn mut_state(&mut self) -> &mut ViewState;

    fn invalidate(&mut self);

    fn view_indicators(&self, enabled: &[Self::IndicatorType]) -> Vec<Element<'_, Message>>;

    fn visible_timerange(&self) -> (u64, u64);

    fn interval_keys(&self) -> Option<Vec<u64>>;

    fn autoscaled_coords(&self) -> Vector;

    fn supports_fit_autoscaling(&self) -> bool;

    fn is_empty(&self) -> bool;
}

fn canvas_interaction<T: Chart>(
    chart: &T,
    interaction: &mut Interaction,
    event: &Event,
    bounds: Rectangle,
    cursor: mouse::Cursor,
) -> Option<canvas::Action<Message>> {
    if let Event::Mouse(mouse::Event::ButtonReleased(_)) = event {
        match interaction {
            Interaction::Panning { .. } | Interaction::Zoomin { .. } => {
                *interaction = Interaction::None;
            }
            _ => {}
        }
    }

    if chart.state().bounds != bounds {
        return Some(canvas::Action::publish(Message::BoundsChanged(bounds)));
    }

    let shrunken_bounds = bounds.shrink(DRAG_SIZE * 4.0);
    let cursor_position = cursor.position_in(shrunken_bounds);

    if let Interaction::Ruler { .. } = interaction {
        if cursor_position.is_none() {
            *interaction = Interaction::None;
        }
    }

    match event {
        // Ignore mouse events for non-hovered charts to prevent cross-pane interactions
        Event::Mouse(_) if !cursor.is_over(shrunken_bounds) => None,
        Event::Mouse(mouse_event) => {
            let chart_state = chart.state();

            match mouse_event {
                mouse::Event::ButtonPressed(button) => {
                    let cursor_position = cursor_position?;
                    let message = match button {
                        mouse::Button::Left => {
                            match interaction {
                                Interaction::None | Interaction::Panning { .. } | Interaction::Zoomin { .. } => {
                                    *interaction = Interaction::Panning { translation: chart_state.translation, start: cursor_position };
                                    None
                                }
                                Interaction::Ruler { start } if start.is_none() => {
                                    *interaction = Interaction::Ruler { start: Some(cursor_position) };
                                    None
                                }
                                Interaction::Ruler { .. } => {
                                    *interaction = Interaction::None;
                                    None
                                }
                            }
                        }
                        _ => None,
                    };

                    Some(
                        message
                            .map_or(canvas::Action::request_redraw(), canvas::Action::publish)
                            .and_capture(),
                    )
                }
                mouse::Event::CursorMoved { .. } => {
                    let cursor_position = cursor_position?;
                    let message = match *interaction {
                        Interaction::Panning { translation, start } => Some(Message::Translated(
                            translation + (cursor_position - start) * (1.0 / chart_state.scaling),
                        )),
                        Interaction::None | Interaction::Ruler { .. } => {
                            if chart_state.layout.crosshair { Some(Message::CrosshairMoved) } else { None }
                        }
                        _ => None,
                    };

                    let action =
                        message.map_or(canvas::Action::request_redraw(), canvas::Action::publish);

                    Some(match interaction {
                        Interaction::None | Interaction::Ruler { .. } => action,
                        _ => action.and_capture(),
                    })
                }
                mouse::Event::WheelScrolled { delta } => {
                    let default_cell_width = T::default_cell_width(chart);
                    let min_cell_width = T::min_cell_width(chart);
                    let max_cell_width = T::max_cell_width(chart);
                    let max_scaling = T::max_scaling(chart);
                    let min_scaling = T::min_scaling(chart);

                    if matches!(interaction, Interaction::Panning { .. }) {
                        return Some(canvas::Action::capture());
                    }

                    let cursor_to_center = cursor.position_from(bounds.center())?;
                    let (y, zoom_sensitivity) = match delta {
                        mouse::ScrollDelta::Lines { y, .. } => (y, ZOOM_SENSITIVITY_MOUSE),
                        mouse::ScrollDelta::Pixels { y, .. } => (y, ZOOM_SENSITIVITY_TRACKPAD),
                    };

                    if let Some(Autoscale::FitToVisible) = chart_state.layout.autoscale {
                        return Some(
                            canvas::Action::publish(Message::XScaling(
                                y / 2.0,
                                cursor_to_center.x,
                                false,
                                zoom_sensitivity,
                            ))
                            .and_capture(),
                        );
                    }

                    let should_adjust_cell_width = match (y.signum(), chart_state.scaling) {
                        // zooming out at max scaling with increased cell width
                        (-1.0, scaling)
                            if scaling == max_scaling
                                && chart_state.cell_width > default_cell_width =>
                        {
                            true
                        }

                        // zooming in at min scaling with decreased cell width
                        (1.0, scaling)
                            if scaling == min_scaling
                                && chart_state.cell_width < default_cell_width =>
                        {
                            true
                        }

                        // zooming in at max scaling with room to increase cell width
                        (1.0, scaling)
                            if scaling == max_scaling
                                && chart_state.cell_width < max_cell_width =>
                        {
                            true
                        }

                        // zooming out at min scaling with room to decrease cell width
                        (-1.0, scaling)
                            if scaling == min_scaling
                                && chart_state.cell_width > min_cell_width =>
                        {
                            true
                        }

                        _ => false,
                    };

                    if should_adjust_cell_width {
                        return Some(
                            canvas::Action::publish(Message::XScaling(
                                y / 2.0,
                                cursor_to_center.x,
                                true,
                                zoom_sensitivity,
                            ))
                            .and_capture(),
                        );
                    }

                    // normal scaling cases
                    if (*y < 0.0 && chart_state.scaling > min_scaling)
                        || (*y > 0.0 && chart_state.scaling < max_scaling)
                    {
                        let old_scaling = chart_state.scaling;
                        let scaling = (chart_state.scaling * (1.0 + y / zoom_sensitivity)).clamp(min_scaling, max_scaling);

                        let denominator = old_scaling * scaling;
                        let vector_diff = if denominator.abs() > 0.0001 {
                            let factor = scaling - old_scaling;
                            Vector::new(
                                cursor_to_center.x * factor / denominator,
                                cursor_to_center.y * factor / denominator,
                            )
                        } else {
                            Vector::default()
                        };

                        let translation = chart_state.translation - vector_diff;

                        return Some(
                            canvas::Action::publish(Message::Scaled(scaling, translation))
                                .and_capture(),
                        );
                    }

                    Some(canvas::Action::capture())
                }
                _ => None,
            }
        }
        Event::Keyboard(keyboard_event) => {
            if cursor_position.is_none() {
                return None;
            }
            match keyboard_event {
                iced::keyboard::Event::KeyPressed { key, .. } => match key.as_ref() {
                    keyboard::Key::Named(keyboard::key::Named::Shift) => {
                        *interaction = Interaction::Ruler { start: None };
                        Some(canvas::Action::request_redraw().and_capture())
                    }
                    keyboard::Key::Named(keyboard::key::Named::Escape) => {
                        *interaction = Interaction::None;
                        Some(canvas::Action::request_redraw().and_capture())
                    }
                    _ => None,
                },
                _ => None,
            }
        }
        _ => None,
    }
}

pub enum Action {
    ErrorOccurred(data::InternalError),
    FetchRequested(uuid::Uuid, FetchRange),
}

pub fn update<T: Chart>(chart: &mut T, message: Message) {
    match message {
        Message::DoubleClick(scale) => {
            let default_chart_width = T::default_cell_width(chart);
            let autoscaled_coords = chart.autoscaled_coords();
            let supports_fit_autoscaling = chart.supports_fit_autoscaling();

            let state = chart.mut_state();

            match scale {
                AxisScaleClicked::X => {
                    state.cell_width = default_chart_width;
                    state.translation = autoscaled_coords;
                }
                AxisScaleClicked::Y => {
                    if supports_fit_autoscaling {
                        state.layout.autoscale = Some(Autoscale::FitToVisible);
                        state.scaling = 1.0;
                    } else {
                        state.layout.autoscale = Some(Autoscale::CenterLatest);
                    }
                }
            }
        }
        Message::Translated(translation) => {
            let state = chart.mut_state();

            if let Some(Autoscale::FitToVisible) = state.layout.autoscale {
                state.translation.x = translation.x;
            } else {
                state.translation = translation;
                state.layout.autoscale = None;
            }
        }
        Message::Scaled(scaling, translation) => {
            let state = chart.mut_state();
            state.scaling = scaling;
            state.translation = translation;

            state.layout.autoscale = None;
        }
        Message::AutoscaleToggle => {
            let supports_fit_autoscaling = chart.supports_fit_autoscaling();
            let state = chart.mut_state();

            let current_autoscale = state.layout.autoscale;
            state.layout.autoscale = {
                match current_autoscale {
                    None => Some(Autoscale::CenterLatest),
                    Some(Autoscale::CenterLatest) => {
                        if supports_fit_autoscaling {
                            Some(Autoscale::FitToVisible)
                        } else {
                            None
                        }
                    }
                    Some(Autoscale::FitToVisible) => None,
                }
            };

            if state.layout.autoscale.is_some() {
                state.scaling = 1.0;
            }
        }
        Message::SetAutoscale(autoscale) => {
            let supports_fit_autoscaling = chart.supports_fit_autoscaling();
            let state = chart.mut_state();

            // Only allow FitToVisible if the chart supports it
            state.layout.autoscale = Some(match autoscale {
                Autoscale::FitToVisible if supports_fit_autoscaling => Autoscale::FitToVisible,
                _ => Autoscale::CenterLatest,
            });

            state.scaling = 1.0;
        }
        Message::CrosshairToggle => {
            let state = chart.mut_state();
            state.layout.crosshair = !state.layout.crosshair;
        }
        Message::XScaling(delta, cursor_to_center_x, is_wheel_scroll, zoom_sensitivity) => {
            let min_cell_width = T::min_cell_width(chart);
            let max_cell_width = T::max_cell_width(chart);

            let state = chart.mut_state();

            if !(delta < 0.0 && state.cell_width > min_cell_width
                || delta > 0.0 && state.cell_width < max_cell_width)
            {
                return;
            }

            let is_fit_to_visible_zoom =
                !is_wheel_scroll && matches!(state.layout.autoscale, Some(Autoscale::FitToVisible));

            let zoom_factor = if is_fit_to_visible_zoom {
                zoom_sensitivity / 1.5
            } else if is_wheel_scroll {
                zoom_sensitivity
            } else {
                zoom_sensitivity * 3.0
            };

            let new_width = (state.cell_width * (1.0 + delta / zoom_factor))
                .clamp(min_cell_width, max_cell_width);

            if is_fit_to_visible_zoom {
                let anchor_interval = {
                    let latest_x_coord = state.interval_to_x(state.latest_x);
                    if state.is_interval_x_visible(latest_x_coord) {
                        state.latest_x
                    } else {
                        let visible_region = state.visible_region(state.bounds.size());
                        state.x_to_interval(visible_region.x + visible_region.width)
                    }
                };

                let old_anchor_chart_x = state.interval_to_x(anchor_interval);

                state.cell_width = new_width;

                let new_anchor_chart_x = state.interval_to_x(anchor_interval);

                let shift = new_anchor_chart_x - old_anchor_chart_x;
                state.translation.x -= shift;
            } else {
                let (old_scaling, old_translation_x) = { (state.scaling, state.translation.x) };

                let latest_x = state.interval_to_x(state.latest_x);
                let is_interval_x_visible = state.is_interval_x_visible(latest_x);

                let cursor_chart_x = {
                    if is_wheel_scroll || !is_interval_x_visible {
                        cursor_to_center_x / old_scaling - old_translation_x
                    } else {
                        latest_x / old_scaling - old_translation_x
                    }
                };

                let new_cursor_x = match state.basis {
                    Basis::Time(_) => {
                        let cursor_time = state.x_to_interval(cursor_chart_x);
                        state.cell_width = new_width;

                        state.interval_to_x(cursor_time)
                    }
                    Basis::Tick(_) => {
                        let tick_index = cursor_chart_x / state.cell_width;
                        state.cell_width = new_width;

                        tick_index * state.cell_width
                    }
                };

                if is_wheel_scroll || !is_interval_x_visible {
                    if !new_cursor_x.is_nan() && !cursor_chart_x.is_nan() {
                        state.translation.x -= new_cursor_x - cursor_chart_x;
                    }

                    state.layout.autoscale = None;
                }
            }
        }
        Message::YScaling(delta, cursor_to_center_y, is_wheel_scroll, zoom_sensitivity) => {
            let min_cell_height = T::min_cell_height(chart);
            let max_cell_height = T::max_cell_height(chart);

            let state = chart.mut_state();

            if state.layout.autoscale == Some(Autoscale::FitToVisible) {
                state.layout.autoscale = None;
            }

            if delta < 0.0 && state.cell_height > min_cell_height
                || delta > 0.0 && state.cell_height < max_cell_height
            {
                let (old_scaling, old_translation_y) = { (state.scaling, state.translation.y) };

                let zoom_factor = if is_wheel_scroll {
                    zoom_sensitivity
                } else {
                    zoom_sensitivity * 3.0
                };

                let new_height = (state.cell_height * (1.0 + delta / zoom_factor))
                    .clamp(min_cell_height, max_cell_height);

                let cursor_chart_y = cursor_to_center_y / old_scaling - old_translation_y;

                let cursor_price = state.y_to_price(cursor_chart_y);

                state.cell_height = new_height;

                let new_cursor_y = state.price_to_y(cursor_price);

                state.translation.y -= new_cursor_y - cursor_chart_y;

                if is_wheel_scroll {
                    state.layout.autoscale = None;
                }
            }
        }
        Message::BoundsChanged(bounds) => {
            let state = chart.mut_state();

            // calculate how center shifted
            let old_center_x = state.bounds.width / 2.0;
            let new_center_x = bounds.width / 2.0;
            let center_delta_x = (new_center_x - old_center_x) / state.scaling;

            state.bounds = bounds;

            if state.layout.autoscale != Some(Autoscale::CenterLatest) {
                state.translation.x += center_delta_x;
            }
        }
        Message::SplitDragged(split, size) => {
            let chart_state = chart.mut_state();

            if let Some(split) = chart_state.layout.splits.get_mut(split) {
                *split = (size * 100.0).round() / 100.0;
            }
        }
        Message::CrosshairMoved => {}
    }

    chart.invalidate();
}

pub fn view<'a, T: Chart>(
    chart: &'a T,
    indicators: &'a [T::IndicatorType],
    timezone: data::UserTimezone,
) -> Element<'a, Message> {
    let chart_state = chart.state();

    if chart.is_empty() {
        return center(text("Waiting for data...").size(16)).into();
    }

    let axis_labels_x = Canvas::new(AxisLabelsX {
        labels_cache: &chart_state.cache.x_labels,
        scaling: chart_state.scaling,
        translation_x: chart_state.translation.x,
        max: chart_state.latest_x,
        crosshair: chart_state.layout.crosshair,
        basis: chart_state.basis,
        cell_width: chart_state.cell_width,
        timezone,
        chart_bounds: chart_state.bounds,
        interval_keys: chart.interval_keys(),
    })
    .width(Length::Fill)
    .height(Length::Fill);

    let chart_controls = {
        let (autoscale_label, autoscale_tooltip, autoscale_active) = match chart_state.layout.autoscale {
            Some(Autoscale::CenterLatest) => ("C", "Center Latest", true),
            Some(Autoscale::FitToVisible) => ("A", "Fit to Visible", true),
            None => ("C", "Autoscale Off", false),
        };

        let center_button = button(text(autoscale_label).size(10).align_x(alignment::Horizontal::Center))
            .width(Length::Shrink)
            .height(Length::Fill)
            .on_press(Message::AutoscaleToggle)
            .style(move |theme, status| {
                style::button::transparent(theme, status, autoscale_active)
            });

        let crosshair_button = button(text("+").size(10).align_x(alignment::Horizontal::Center))
            .width(Length::Shrink)
            .height(Length::Fill)
            .on_press(Message::CrosshairToggle)
            .style(move |theme, status| {
                style::button::transparent(theme, status, chart_state.layout.crosshair)
            });

        container(
            row![
                Space::new().width(Length::Fill).height(Length::Fill),
                tooltip(center_button, Some(autoscale_tooltip), TooltipPosition::Top),
                tooltip(crosshair_button, Some("Crosshair"), TooltipPosition::Top),
            ]
            .spacing(2),
        )
        .padding(2)
    };

    let chart_content = {
        let axis_labels_y = Canvas::new(AxisLabelsY {
            labels_cache: &chart_state.cache.y_labels,
            translation_y: chart_state.translation.y,
            scaling: chart_state.scaling,
            decimals: chart_state.decimals,
            min: chart_state.base_price_y,
            last_price: chart_state.last_price,
            crosshair: chart_state.layout.crosshair,
            tick_size: chart_state.tick_size,
            cell_height: chart_state.cell_height,
            basis: chart_state.basis,
            chart_bounds: chart_state.bounds,
        })
        .width(Length::Fill)
        .height(Length::Fill);

        let main_chart: Element<_> = row![
            container(Canvas::new(chart).width(Length::Fill).height(Length::Fill))
                .width(Length::FillPortion(10))
                .height(Length::FillPortion(120)),
            container(Space::new().width(1).height(Length::Fill)).style(style::split_ruler_container),
            container(
                mouse_area(axis_labels_y)
                    .on_double_click(Message::DoubleClick(AxisScaleClicked::Y))
            )
            .width(Length::Fixed(64.0 + (chart_state.decimals as f32 * 4.0)))
            .height(Length::FillPortion(120))
        ]
        .into();

        let indicators = chart.view_indicators(indicators);

        if indicators.is_empty() {
            main_chart
        } else {
            let panels = std::iter::once(main_chart)
                .chain(indicators)
                .collect::<Vec<_>>();

            // Ensure the number of splits matches the number of indicator panels
            let expected_splits = panels.len() - 1; // panels = 1 main + N indicators
            let splits_vec: Vec<f32> = if chart_state.layout.splits.len() == expected_splits {
                chart_state.layout.splits.clone()
            } else {
                let main_split = chart_state
                    .layout
                    .splits
                    .first()
                    .copied()
                    .unwrap_or(0.8);
                data::util::calc_panel_splits(main_split, expected_splits, None)
            };

            MultiSplit::new(panels, splits_vec, |index, position| {
                Message::SplitDragged(index, position)
            })
            .into()
        }
    };

    column![
        chart_content,
        container(Space::new().width(Length::Fill).height(1)).style(style::split_ruler_container),
        row![
            container(
                mouse_area(axis_labels_x)
                    .on_double_click(Message::DoubleClick(AxisScaleClicked::X))
            )
            .padding(padding::right(1))
            .width(Length::FillPortion(10))
            .height(Length::Fixed(26.0)),
            chart_controls
                .width(Length::Fixed(64.0 + (chart_state.decimals as f32 * 4.0)))
                .height(Length::Fixed(26.0))
        ]
    ]
    .padding(1)
    .into()
}

#[derive(Default)]
pub struct Caches {
    main: Cache,
    x_labels: Cache,
    y_labels: Cache,
    crosshair: Cache,
}

impl Caches {
    fn clear_all(&self) {
        self.main.clear();
        self.x_labels.clear();
        self.y_labels.clear();
        self.crosshair.clear();
    }
}

// ChartData moved to data::chart::PlotData

pub struct ViewState {
    cache: Caches,
    bounds: Rectangle,
    translation: Vector,
    scaling: f32,
    cell_width: f32,
    cell_height: f32,
    basis: Basis,
    last_price: Option<PriceInfoLabel>,
    base_price_y: f32,
    latest_x: u64,
    tick_size: f32,
    decimals: usize,
    ticker_info: Option<TickerInfo>,
    layout: ViewConfig,
}

impl Default for ViewState {
    fn default() -> Self {
        ViewState {
            cache: Caches::default(),
            translation: Vector::default(),
            bounds: Rectangle::default(),
            basis: Timeframe::M5.into(),
            last_price: None,
            scaling: 1.0,
            cell_width: 4.0,
            cell_height: 3.0,
            base_price_y: 0.0,
            latest_x: 0,
            tick_size: 0.0,
            decimals: 0,
            ticker_info: None,
            layout: ViewConfig::default(),
        }
    }
}

impl ViewState {
    fn visible_region(&self, size: Size) -> Rectangle {
        if !self.scaling.is_finite() || self.scaling <= 0.0 {
            return Rectangle::new(Point::new(0.0, 0.0), Size::new(100.0, 100.0));
        }
        
        let width = size.width / self.scaling;
        let height = size.height / self.scaling;
        
        if !width.is_finite() || !height.is_finite() || !self.translation.x.is_finite() || !self.translation.y.is_finite() {
            return Rectangle::new(Point::new(0.0, 0.0), Size::new(100.0, 100.0));
        }

        Rectangle {
            x: -self.translation.x - width / 2.0,
            y: -self.translation.y - height / 2.0,
            width,
            height,
        }
    }

    fn is_interval_x_visible(&self, interval_x: f32) -> bool {
        let region = self.visible_region(self.bounds.size());

        interval_x >= region.x && interval_x <= region.x + region.width
    }

    fn interval_range(&self, region: &Rectangle) -> (u64, u64) {
        match self.basis {
            Basis::Tick(_) => (
                self.x_to_interval(region.x + region.width),
                self.x_to_interval(region.x),
            ),
            Basis::Time(interval) => (
                self.x_to_interval(region.x).saturating_sub(interval / 2),
                self.x_to_interval(region.x + region.width)
                    .saturating_add(interval / 2),
            ),
        }
    }

    fn price_range(&self, region: &Rectangle) -> (f32, f32) {
        let highest = self.y_to_price(region.y);
        let lowest = self.y_to_price(region.y + region.height);

        (highest, lowest)
    }

    fn interval_to_x(&self, value: u64) -> f32 {
        if !self.cell_width.is_finite() {
            return 0.0;
        }
        
        match self.basis {
            Basis::Time(timeframe) => {
                if timeframe == 0 {
                    return 0.0;
                }
                if value <= self.latest_x {
                    let diff = self.latest_x - value;
                    let result = -(diff as f32 / timeframe as f32) * self.cell_width;
                    if result.is_finite() { result } else { 0.0 }
                } else {
                    let diff = value - self.latest_x;
                    let result = (diff as f32 / timeframe as f32) * self.cell_width;
                    if result.is_finite() { result } else { 0.0 }
                }
            }
            Basis::Tick(_) => {
                let result = -((value as f32) * self.cell_width);
                if result.is_finite() { result } else { 0.0 }
            },
        }
    }

    fn x_to_interval(&self, x: f32) -> u64 {
        match self.basis {
            Basis::Time(interval) => {
                if x <= 0.0 {
                    let diff = (-x / self.cell_width * interval as f32) as u64;
                    self.latest_x.saturating_sub(diff)
                } else {
                    let diff = (x / self.cell_width * interval as f32) as u64;
                    self.latest_x.saturating_add(diff)
                }
            }
            Basis::Tick(_) => {
                let tick = -(x / self.cell_width);
                tick.round() as u64
            }
        }
    }

    fn price_to_y(&self, price: f32) -> f32 {
        if !price.is_finite() || !self.base_price_y.is_finite() || !self.tick_size.is_finite() || !self.cell_height.is_finite() {
            return 0.0;
        }
        let result = ((self.base_price_y - price) / self.tick_size) * self.cell_height;
        if result.is_finite() { result } else { 0.0 }
    }

    fn y_to_price(&self, y: f32) -> f32 {
        if !y.is_finite() || !self.base_price_y.is_finite() || !self.cell_height.is_finite() || !self.tick_size.is_finite() {
            return 0.0;
        }
        let result = self.base_price_y - (y / self.cell_height) * self.tick_size;
        if result.is_finite() { result } else { 0.0 }
    }

    fn draw_crosshair(
        &self,
        frame: &mut Frame,
        theme: &Theme,
        bounds: Size,
        cursor_position: Point,
        interaction: &Interaction,
        measure_text: Option<&dyn Fn(u64, u64) -> Option<String>>,
    ) -> (f32, u64) {
        let region = self.visible_region(bounds);

        let dashed_line = style::dashed_line(theme);

        let highest = self.y_to_price(region.y);
        let lowest = self.y_to_price(region.y + region.height);

        // Measurement ruler overlay when active (Shift + LMB)
        if let Interaction::Ruler { start: Some(start) } = interaction {
            let p1 = *start;
            let p2 = cursor_position;

            let snap_y = |y: f32| {
                let ratio = y / bounds.height;
                let price = highest + ratio * (lowest - highest);
                let rounded_price = data::util::round_to_tick(price, self.tick_size);
                let snap_ratio = (rounded_price - highest) / (lowest - highest);
                snap_ratio * bounds.height
            };

            let snap_x = |x: f32| {
                let (_, snap_ratio) = self.snap_x_to_timestamp(x, bounds, region);
                snap_ratio * bounds.width
            };

            let snapped_p1_x = snap_x(p1.x);
            let snapped_p1_y = snap_y(p1.y);
            let snapped_p2_x = snap_x(p2.x);
            let snapped_p2_y = snap_y(p2.y);

            let price1 = self.y_to_price(snapped_p1_y);
            let price2 = self.y_to_price(snapped_p2_y);

            let pct = if price1 != 0.0 { ((price2 - price1) / price1) * 100.0 } else { 0.0 };
            let pct_text = format!("{:.2}%", pct);

            let interval_diff: String = match self.basis {
                Basis::Time(_) => {
                    let (t1, _) = self.snap_x_to_timestamp(p1.x, bounds, region);
                    let (t2, _) = self.snap_x_to_timestamp(p2.x, bounds, region);
                    let diff_ms = if t1 > t2 { t1 - t2 } else { t2 - t1 };
                    data::util::format_duration_ms(diff_ms)
                }
                Basis::Tick(_) => {
                    let (tick1, _) = self.snap_x_to_timestamp(p1.x, bounds, region);
                    let (tick2, _) = self.snap_x_to_timestamp(p2.x, bounds, region);
                    let tick_diff = if tick1 > tick2 { tick1 - tick2 } else { tick2 - tick1 };
                    format!("{} ticks", tick_diff)
                }
            };

            let rect_x = snapped_p1_x.min(snapped_p2_x);
            let rect_y = snapped_p1_y.min(snapped_p2_y);
            let rect_w = (snapped_p1_x - snapped_p2_x).abs();
            let rect_h = (snapped_p1_y - snapped_p2_y).abs();

            let palette = theme.extended_palette();

            frame.fill_rectangle(
                Point::new(rect_x, rect_y),
                Size::new(rect_w, rect_h),
                palette.primary.base.color.scale_alpha(0.08),
            );

            let corners = [
                Point::new(rect_x, rect_y),
                Point::new(rect_x + rect_w, rect_y),
                Point::new(rect_x, rect_y + rect_h),
                Point::new(rect_x + rect_w, rect_y + rect_h),
            ];

            let (text_corner, idx) = corners
                .iter()
                .enumerate()
                .min_by(|(_, a), (_, b)| {
                    let da = (a.x - p2.x).hypot(a.y - p2.y);
                    let db = (b.x - p2.x).hypot(b.y - p2.y);
                    da.partial_cmp(&db).unwrap()
                })
                .map(|(i, &c)| (c, i))
                .unwrap();

            let text_padding = 8.0;
            let text_pos = match idx {
                0 => Point::new(text_corner.x + text_padding, text_corner.y + text_padding),
                1 => Point::new(text_corner.x - text_padding, text_corner.y + text_padding),
                2 => Point::new(text_corner.x + text_padding, text_corner.y - text_padding),
                3 => Point::new(text_corner.x - text_padding, text_corner.y - text_padding),
                _ => text_corner,
            };

            // Build label with optional measurement text (pane can provide total volume, etc.)
            let label_text = if let Some(builder) = measure_text {
                let (t1, _) = self.snap_x_to_timestamp(p1.x, bounds, region);
                let (t2, _) = self.snap_x_to_timestamp(p2.x, bounds, region);
                if let Some(measure) = builder(t1.min(t2), t1.max(t2)) {
                    format!("{} | {} | {}", measure, interval_diff, pct_text)
                } else {
                    format!("{} | {}", interval_diff, pct_text)
                }
            } else {
                format!("{} | {}", interval_diff, pct_text)
            };
            let text_size = 11.0;
            // Use a slightly larger monospaced char width factor to ensure text fits inside background
            let text_width = (label_text.len() as f32) * text_size * 0.65;
            let text_height = text_size * 1.2;
            let rect_padding = 4.0;

            let (bg_x, bg_y) = match idx {
                0 => (text_pos.x - rect_padding, text_pos.y - rect_padding),
                1 => (text_pos.x - text_width - rect_padding, text_pos.y - rect_padding),
                2 => (text_pos.x - rect_padding, text_pos.y - text_height - rect_padding),
                3 => (text_pos.x - text_width - rect_padding, text_pos.y - text_height - rect_padding),
                _ => (
                    text_pos.x - text_width / 2.0 - rect_padding,
                    text_pos.y - text_height / 2.0 - rect_padding,
                ),
            };

            let bg_width = text_width + rect_padding * 2.0;
            let bg_height = text_height + rect_padding * 2.0;

            frame.fill_rectangle(
                Point::new(bg_x, bg_y),
                Size::new(bg_width, bg_height),
                palette.background.weakest.color.scale_alpha(0.9),
            );

            // Center the text inside the background rectangle
            let center_pos = Point::new(bg_x + bg_width / 2.0, bg_y + bg_height / 2.0);
            frame.fill_text(canvas::Text {
                content: label_text,
                position: center_pos,
                color: palette.background.base.text,
                size: iced::Pixels(text_size),
                align_x: alignment::Horizontal::Center.into(),
                align_y: alignment::Vertical::Center.into(),
                font: style::AZERET_MONO,
                ..canvas::Text::default()
            });
        }

        let crosshair_ratio = cursor_position.y / bounds.height;
        let crosshair_price = highest + crosshair_ratio * (lowest - highest);

        let rounded_price = data::util::round_to_tick(crosshair_price, self.tick_size);
        let snap_ratio = (rounded_price - highest) / (lowest - highest);

        frame.stroke(
            &Path::line(
                safe_point(0.0, snap_ratio * bounds.height),
                safe_point(bounds.width, snap_ratio * bounds.height),
            ),
            dashed_line,
        );

        // Vertical time/tick line
        match self.basis {
            Basis::Time(timeframe) => {
                let earliest = self.x_to_interval(region.x) as f64;
                let latest = self.x_to_interval(region.x + region.width) as f64;

                let crosshair_ratio = f64::from(cursor_position.x / bounds.width);
                let crosshair_millis = earliest + crosshair_ratio * (latest - earliest);

                let rounded_timestamp =
                    (crosshair_millis / (timeframe as f64)).round() as u64 * timeframe;
                let snap_ratio =
                    ((rounded_timestamp as f64 - earliest) / (latest - earliest)) as f32;

                frame.stroke(
                    &Path::line(
                        safe_point(snap_ratio * bounds.width, 0.0),
                        safe_point(snap_ratio * bounds.width, bounds.height),
                    ),
                    dashed_line,
                );

                (rounded_price, rounded_timestamp)
            }
            Basis::Tick(aggregation) => {
                let crosshair_ratio = cursor_position.x / bounds.width;

                let (chart_x_min, chart_x_max) = (region.x, region.x + region.width);
                let crosshair_pos = chart_x_min + crosshair_ratio * region.width;

                let cell_index = (crosshair_pos / self.cell_width).round();

                let snapped_crosshair = cell_index * self.cell_width;

                let snap_ratio = (snapped_crosshair - chart_x_min) / (chart_x_max - chart_x_min);

                let rounded_tick = (-cell_index as u64) * aggregation;

                frame.stroke(
                    &Path::line(
                        safe_point(snap_ratio * bounds.width, 0.0),
                        safe_point(snap_ratio * bounds.width, bounds.height),
                    ),
                    dashed_line,
                );

                (rounded_price, rounded_tick)
            }
        }
    }

    fn draw_last_price_line(
        &self,
        frame: &mut canvas::Frame,
        palette: &Extended,
        region: Rectangle,
    ) {
        if let Some(price) = &self.last_price {
            let (mut y_pos, line_color) = price.get_with_color(palette);
            y_pos = self.price_to_y(y_pos);

            let marker_line = Stroke::with_color(
                Stroke {
                    width: 1.0,
                    line_dash: LineDash {
                        segments: &[2.0, 2.0],
                        offset: 4,
                    },
                    ..Default::default()
                },
                line_color.scale_alpha(0.5),
            );

            frame.stroke(
                &Path::line(
                    safe_point(0.0, y_pos),
                    safe_point(region.x + region.width, y_pos),
                ),
                marker_line,
            );
        }
    }



    pub fn get_chart_layout(&self) -> ViewConfig {
        self.layout.clone()
    }

    fn snap_x_to_timestamp(&self, x: f32, bounds: Size, region: Rectangle) -> (u64, f32) {
        let x_ratio = x / bounds.width;

        match self.basis {
            Basis::Time(timeframe) => {
                let interval = timeframe;
                let earliest = self.x_to_interval(region.x) as f64;
                let latest = self.x_to_interval(region.x + region.width) as f64;

                let millis_at_x = earliest + f64::from(x_ratio) * (latest - earliest);
                let rounded_timestamp = (millis_at_x / (interval as f64)).round() as u64 * interval;

                let snap_ratio = if latest - earliest > 0.0 {
                    ((rounded_timestamp as f64 - earliest) / (latest - earliest)) as f32
                } else {
                    0.5
                };

                (rounded_timestamp, snap_ratio)
            }
            Basis::Tick(aggregation) => {
                let (chart_x_min, chart_x_max) = (region.x, region.x + region.width);
                let chart_x = chart_x_min + x_ratio * (chart_x_max - chart_x_min);

                let cell_index = (chart_x / self.cell_width).round();
                let snapped_x = cell_index * self.cell_width;

                let snap_ratio = if chart_x_max - chart_x_min > 0.0 {
                    (snapped_x - chart_x_min) / (chart_x_max - chart_x_min)
                } else {
                    0.5
                };

                let rounded_tick = (-cell_index as u64) * aggregation;

                (rounded_tick, snap_ratio)
            }
        }
    }
}

fn request_fetch(handler: &mut RequestHandler, range: FetchRange) -> Option<Action> {
    match handler.add_request(range) {
        Ok(Some(req_id)) => Some(Action::FetchRequested(req_id, range)),
        Ok(None) => None,
        Err(reason) => {
            log::error!("Failed to request {:?}: {}", range, reason);
            // TODO: handle this more explicitly, maybe by returning Action::ErrorOccurred
            None
        }
    }
}

fn draw_horizontal_volume_bars(
    frame: &mut canvas::Frame,
    start_x: f32,
    y_position: f32,
    buy_qty: f32,
    sell_qty: f32,
    max_qty: f32,
    bar_height: f32,
    width_factor: f32,
    buy_color: iced::Color,
    sell_color: iced::Color,
    bar_color_alpha: f32,
) {
    let total_qty = buy_qty + sell_qty;
    if total_qty <= 0.0 {
        return;
    }

    let total_bar_width = (total_qty / max_qty) * width_factor;

    let buy_proportion = buy_qty / total_qty;
    let sell_proportion = sell_qty / total_qty;

    let buy_bar_width = buy_proportion * total_bar_width;
    let sell_bar_width = sell_proportion * total_bar_width;

    let start_y = y_position - (bar_height / 2.0);

    if sell_qty > 0.0 {
        frame.fill_rectangle(
            Point::new(start_x, start_y),
            Size::new(sell_bar_width, bar_height),
            sell_color.scale_alpha(bar_color_alpha),
        );
    }

    if buy_qty > 0.0 {
        frame.fill_rectangle(
            Point::new(start_x + sell_bar_width, start_y),
            Size::new(buy_bar_width, bar_height),
            buy_color.scale_alpha(bar_color_alpha),
        );
    }
}
