use std::collections::BTreeMap;

use iced::widget::canvas::{self, Cache, Event, Geometry, Path, Stroke};
use iced::widget::{Canvas, container, row, vertical_rule};
use iced::{Element, Length};
use iced::{Point, Rectangle, Renderer, Size, Theme, Vector, mouse};

use crate::chart::{Basis, Caches, ViewState, Interaction, Message};
use crate::style::{self, dashed_line};

use data::util::{format_with_commas, round_to_tick};

pub fn indicator_elem<'a>(
    chart_state: &'a ViewState,
    cache: &'a Caches,
    data_points: &'a BTreeMap<u64, f32>,
    earliest: u64,
    latest: u64,
) -> Element<'a, Message> {
    let (max_cvd, min_cvd) = {
        match chart_state.basis {
            Basis::Time(_) => {
                let mut iter = data_points.range(earliest..=latest);
                if let Some((_, first_cvd)) = iter.next() {
                    iter.fold((*first_cvd, *first_cvd), |(max, min), (_, cvd)| {
                        (max.max(*cvd), min.min(*cvd))
                    })
                } else {
                    (0.0, 0.0) // Default range when no data
                }
            },
            Basis::Tick(_) => {
                let earliest = earliest as usize;
                let latest = latest as usize;

                let filtered_data: Vec<f32> = data_points
                    .iter()
                    .rev()
                    .enumerate()
                    .filter(|(index, _)| *index <= latest && *index >= earliest)
                    .map(|(_, (_, cvd))| *cvd)
                    .collect();

                if filtered_data.is_empty() {
                    (0.0, 0.0) // Default range when no data
                } else {
                    let max_cvd = filtered_data.iter().fold(f32::NEG_INFINITY, |a, &b| a.max(b));
                    let min_cvd = filtered_data.iter().fold(f32::INFINITY, |a, &b| a.min(b));
                    (max_cvd, min_cvd)
                }
            }
        }
    };

    // Add some padding to the range, but ensure valid range
    let range = max_cvd - min_cvd;
    let (max_cvd, min_cvd) = if range > 0.0 && range.is_finite() {
        let padding = range * 0.05;
        (max_cvd + padding, min_cvd - padding)
    } else {
        // Fallback to a small default range
        (max_cvd + 1.0, min_cvd - 1.0)
    };

    let indi_chart = Canvas::new(CVDIndicator {
        indicator_cache: &cache.main,
        crosshair_cache: &cache.crosshair,
        chart_state,
        data_points,
        max_cvd,
        min_cvd,
    })
    .height(Length::Fill)
    .width(Length::Fill);

    let indi_labels = Canvas::new(super::IndicatorLabel {
        label_cache: &cache.y_labels,
        max: max_cvd,
        min: min_cvd,
        crosshair: chart_state.layout.crosshair,
        chart_bounds: chart_state.bounds,
    })
    .height(Length::Fill)
    .width(Length::Fixed(64.0 + (chart_state.decimals as f32 * 4.0)));

    row![
        indi_chart,
        vertical_rule(1).style(style::split_ruler),
        container(indi_labels),
    ]
    .into()
}

pub struct CVDIndicator<'a> {
    pub indicator_cache: &'a Cache,
    pub crosshair_cache: &'a Cache,
    pub max_cvd: f32,
    pub min_cvd: f32,
    pub data_points: &'a BTreeMap<u64, f32>,
    pub chart_state: &'a ViewState,
}

impl CVDIndicator<'_> {
    fn visible_region(&self, size: Size) -> Rectangle {
        let width = size.width / self.chart_state.scaling;
        let height = size.height / self.chart_state.scaling;

        Rectangle {
            x: -self.chart_state.translation.x - width / 2.0,
            y: 0.0,
            width,
            height,
        }
    }
}

impl canvas::Program<Message> for CVDIndicator<'_> {
    type State = Interaction;

    fn update(
        &self,
        interaction: &mut Interaction,
        event: &Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<canvas::Action<Message>> {
        match event {
            Event::Mouse(mouse::Event::CursorMoved { .. }) => {
                let message = match *interaction {
                    Interaction::None => {
                        if self.chart_state.layout.crosshair && cursor.is_over(bounds) {
                            Some(Message::CrosshairMoved)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                let action =
                    message.map_or(canvas::Action::request_redraw(), canvas::Action::publish);

                Some(match interaction {
                    Interaction::None => action,
                    _ => action.and_capture(),
                })
            }
            _ => None,
        }
    }

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        theme: &Theme,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        let chart_state = self.chart_state;

        if chart_state.bounds.width == 0.0 {
            return vec![];
        }

        let center = Vector::new(bounds.width / 2.0, bounds.height / 2.0);
        let palette = theme.extended_palette();

        let indicator = self.indicator_cache.draw(renderer, bounds.size(), |frame| {
            frame.translate(center);
            frame.scale(chart_state.scaling);
            frame.translate(Vector::new(
                chart_state.translation.x,
                (-bounds.height / chart_state.scaling) / 2.0,
            ));

            let region = self.visible_region(frame.size());
            let (earliest, latest) = chart_state.interval_range(&region);

            let cvd_range = self.max_cvd - self.min_cvd;
            if cvd_range <= 0.0 {
                return;
            }

            // Draw zero line
            let zero_y = if self.min_cvd <= 0.0 && self.max_cvd >= 0.0 {
                let zero_ratio = -self.min_cvd / cvd_range;
                (bounds.height / chart_state.scaling) * (1.0 - zero_ratio)
            } else {
                -1.0 // Off-screen if zero is not in range
            };

            if zero_y >= 0.0 && zero_y <= (bounds.height / chart_state.scaling) {
                let zero_line = Stroke {
                    width: 1.0,
                    ..Stroke::default()
                };
                frame.stroke(
                    &Path::line(
                        Point::new(region.x, zero_y),
                        Point::new(region.x + region.width, zero_y),
                    ),
                    Stroke::with_color(zero_line, palette.background.strong.text.scale_alpha(0.3)),
                );
            }

            match chart_state.basis {
                Basis::Time(_) => {
                    if latest < earliest {
                        return;
                    }

                    let points: Vec<Point> = self
                        .data_points
                        .range(earliest..=latest)
                        .map(|(timestamp, cvd)| {
                            let x_position = chart_state.interval_to_x(*timestamp);
                            let normalized_height = (cvd - self.min_cvd) / cvd_range;
                            let y_position = (bounds.height / chart_state.scaling)
                                * (1.0 - normalized_height);

                            Point::new(x_position, y_position)
                        })
                        .collect();

                    // Draw line connecting CVD points
                    if points.len() >= 2 {
                        for window in points.windows(2) {
                            let stroke = Stroke {
                                width: 2.0,
                                ..Stroke::default()
                            };
                            let color = if window[1].y < window[0].y {
                                palette.success.base.color // Rising CVD (green)
                            } else {
                                palette.danger.base.color // Falling CVD (red)
                            };
                            frame.stroke(
                                &Path::line(window[0], window[1]),
                                Stroke::with_color(stroke, color),
                            );
                        }
                    }

                    // Draw points
                    for point in points {
                        let radius = (chart_state.cell_width * 0.15).min(3.0);
                        frame.fill(
                            &Path::circle(point, radius),
                            palette.background.base.text,
                        );
                    }
                }
                Basis::Tick(_) => {
                    let earliest = earliest as usize;
                    let latest = latest as usize;

                    let points: Vec<Point> = self
                        .data_points
                        .iter()
                        .rev()
                        .enumerate()
                        .filter(|(index, _)| *index <= latest && *index >= earliest)
                        .map(|(index, (_, cvd))| {
                            let x_position = chart_state.interval_to_x(index as u64);
                            let normalized_height = (cvd - self.min_cvd) / cvd_range;
                            let y_position = (bounds.height / chart_state.scaling)
                                * (1.0 - normalized_height);

                            Point::new(x_position, y_position)
                        })
                        .collect();

                    // Draw line connecting CVD points
                    if points.len() >= 2 {
                        for window in points.windows(2) {
                            let stroke = Stroke {
                                width: 2.0,
                                ..Stroke::default()
                            };
                            let color = if window[1].y > window[0].y {
                                palette.success.base.color // Rising CVD (green)
                            } else {
                                palette.danger.base.color // Falling CVD (red)
                            };
                            frame.stroke(
                                &Path::line(window[0], window[1]),
                                Stroke::with_color(stroke, color),
                            );
                        }
                    }

                    // Draw points
                    for point in points {
                        let radius = (chart_state.cell_width * 0.15).min(3.0);
                        frame.fill(
                            &Path::circle(point, radius),
                            palette.background.base.text,
                        );
                    }
                }
            }
        });

        if chart_state.layout.crosshair {
            let crosshair = self.crosshair_cache.draw(renderer, bounds.size(), |frame| {
                let dashed_line = dashed_line(theme);

                if let Some(cursor_position) = cursor.position_in(chart_state.bounds) {
                    let region = self.visible_region(frame.size());

                    // Vertical time line
                    let earliest = chart_state.x_to_interval(region.x) as f64;
                    let latest = chart_state.x_to_interval(region.x + region.width) as f64;

                    let crosshair_ratio = f64::from(cursor_position.x / bounds.width);

                    let (rounded_interval, snap_ratio) = match chart_state.basis {
                        Basis::Time(timeframe) => {
                            let crosshair_millis = earliest + crosshair_ratio * (latest - earliest);

                            let rounded_timestamp =
                                (crosshair_millis / (timeframe as f64)).round() as u64 * timeframe;
                            let snap_ratio = ((rounded_timestamp as f64 - earliest)
                                / (latest - earliest))
                                as f32;

                            (rounded_timestamp, snap_ratio)
                        }
                        Basis::Tick(_) => {
                            let chart_x_min = region.x;
                            let chart_x_max = region.x + region.width;

                            let crosshair_pos = chart_x_min + crosshair_ratio as f32 * region.width;

                            let cell_index =
                                (crosshair_pos / chart_state.cell_width).round() as i32;
                            let snapped_position = cell_index as f32 * chart_state.cell_width;

                            let snap_ratio =
                                (snapped_position - chart_x_min) / (chart_x_max - chart_x_min);

                            let tick_value = chart_state.x_to_interval(snapped_position);

                            (tick_value, snap_ratio)
                        }
                    };

                    frame.stroke(
                        &Path::line(
                            Point::new(snap_ratio * bounds.width, 0.0),
                            Point::new(snap_ratio * bounds.width, bounds.height),
                        ),
                        dashed_line,
                    );

                    let cvd_data = match chart_state.basis {
                        Basis::Time(_) => {
                            let exact_match = self
                                .data_points
                                .iter()
                                .find(|(interval, _)| **interval == rounded_interval);

                            if exact_match.is_none()
                                && rounded_interval
                                    > self.data_points.keys().last().copied().unwrap_or(0)
                            {
                                self.data_points.iter().last()
                            } else {
                                exact_match
                            }
                        }
                        Basis::Tick(_) => {
                            let index_from_end = rounded_interval as usize;

                            if index_from_end < self.data_points.len() {
                                self.data_points.iter().rev().nth(index_from_end)
                            } else if !self.data_points.is_empty() {
                                let right_edge = chart_state.x_to_interval(region.x + region.width);

                                if rounded_interval <= right_edge {
                                    self.data_points.iter().next_back()
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                    };

                    if let Some((_, cvd_value)) = cvd_data {
                        let tooltip_text = format!("CVD: {}", format_with_commas(*cvd_value));
                        let tooltip_bg_width = tooltip_text.len() as f32 * 8.0;

                        frame.fill_rectangle(
                            Point::new(4.0, 0.0),
                            Size::new(tooltip_bg_width, 14.0),
                            palette.background.weakest.color.scale_alpha(0.9),
                        );

                        let text = canvas::Text {
                            content: tooltip_text,
                            position: Point::new(8.0, 2.0),
                            size: iced::Pixels(9.0),
                            color: palette.background.base.text,
                            font: style::AZERET_MONO,
                            ..canvas::Text::default()
                        };
                        frame.fill_text(text);
                    }
                } else if let Some(cursor_position) = cursor.position_in(bounds) {
                    // Horizontal CVD line
                    let highest = self.max_cvd;
                    let lowest = self.min_cvd;

                    let tick_size = data::util::guesstimate_ticks(highest - lowest);

                    let crosshair_ratio = cursor_position.y / bounds.height;
                    let crosshair_cvd = highest + crosshair_ratio * (lowest - highest);

                    let rounded_cvd = round_to_tick(crosshair_cvd, tick_size);
                    let snap_ratio = (rounded_cvd - highest) / (lowest - highest);

                    frame.stroke(
                        &Path::line(
                            Point::new(0.0, snap_ratio * bounds.height),
                            Point::new(bounds.width, snap_ratio * bounds.height),
                        ),
                        dashed_line,
                    );
                }
            });

            vec![indicator, crosshair]
        } else {
            vec![indicator]
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
            Interaction::None if cursor.is_over(bounds) => {
                if self.chart_state.layout.crosshair {
                    mouse::Interaction::Crosshair
                } else {
                    mouse::Interaction::default()
                }
            }
            _ => mouse::Interaction::default(),
        }
    }
} 