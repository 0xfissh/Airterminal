use crate::screen::dashboard::pane::Message;
use crate::screen::dashboard::panel::timeandsales;
use crate::widget::{classic_slider_row, labeled_slider};
use crate::{style, tooltip, widget::scrollable_content};
use data::chart::{
    KlineChartKind, VisualConfig,
    heatmap::{self, CoalesceKind},
    kline::ClusterKind,
};
use data::util::format_with_commas;
use iced::{
    Alignment, Element, Length,
    widget::{
        button, column, container, pane_grid, pick_list, radio, Space,
        row, slider, text, tooltip::Position as TooltipPosition,
    },
};

pub fn heatmap_cfg_view<'a>(cfg: heatmap::Config, pane: pane_grid::Pane) -> Element<'a, Message> {
    let trade_size_slider = {
        let filter = cfg.trade_size_filter;

        labeled_slider(
            "Trade",
            0.0..=50000.0,
            filter,
            move |value| {
                Message::VisualConfigChanged(
                    Some(pane),
                    VisualConfig::Heatmap(heatmap::Config {
                        trade_size_filter: value,
                        ..cfg
                    }),
                )
            },
            |value| format!("${}", format_with_commas(*value)),
            Some(500.0),
        )
    };

    let order_size_slider = {
        let filter = cfg.order_size_filter;

        labeled_slider(
            "Order",
            0.0..=500_000.0,
            filter,
            move |value| {
                Message::VisualConfigChanged(
                    Some(pane),
                    VisualConfig::Heatmap(heatmap::Config {
                        order_size_filter: value,
                        ..cfg
                    }),
                )
            },
            |value| format!("${}", format_with_commas(*value)),
            Some(5000.0),
        )
    };

    let circle_scaling_slider = {
        if let Some(radius_scale) = cfg.trade_size_scale {
            classic_slider_row(
                text("Circle radius scaling"),
                slider(10..=200, radius_scale, move |value| {
                    Message::VisualConfigChanged(
                        Some(pane),
                        VisualConfig::Heatmap(heatmap::Config {
                            trade_size_scale: Some(value),
                            ..cfg
                        }),
                    )
                })
                .step(10)
                .into(),
                Some(text(format!("{}%", radius_scale)).size(13)),
            )
        } else {
            container(row![]).into()
        }
    };

    let coalescer_cfg: Element<_> = {
        if let Some(coalescing) = cfg.coalescing {
            let threshold_pct = coalescing.threshold();

            let coalescer_kinds = {
                let average = radio(
                    "Average",
                    CoalesceKind::Average(threshold_pct),
                    Some(coalescing),
                    move |value| {
                        Message::VisualConfigChanged(
                            Some(pane),
                            VisualConfig::Heatmap(heatmap::Config {
                                coalescing: Some(value),
                                ..cfg
                            }),
                        )
                    },
                )
                .spacing(4);

                let first = radio(
                    "First",
                    CoalesceKind::First(threshold_pct),
                    Some(coalescing),
                    move |value| {
                        Message::VisualConfigChanged(
                            Some(pane),
                            VisualConfig::Heatmap(heatmap::Config {
                                coalescing: Some(value),
                                ..cfg
                            }),
                        )
                    },
                )
                .spacing(4);

                let max = radio(
                    "Max",
                    CoalesceKind::Max(threshold_pct),
                    Some(coalescing),
                    move |value| {
                        Message::VisualConfigChanged(
                            Some(pane),
                            VisualConfig::Heatmap(heatmap::Config {
                                coalescing: Some(value),
                                ..cfg
                            }),
                        )
                    },
                )
                .spacing(4);

                row![
                    text("Merge method: "),
                    row![average, first, max].spacing(12)
                ]
                .spacing(12)
            };

            let threshold_slider = classic_slider_row(
                text("Size similarity"),
                slider(0.05..=0.8, threshold_pct, move |value| {
                    Message::VisualConfigChanged(
                        Some(pane),
                        VisualConfig::Heatmap(heatmap::Config {
                            coalescing: Some(coalescing.with_threshold(value)),
                            ..cfg
                        }),
                    )
                })
                .step(0.05)
                .into(),
                Some(text(format!("{:.0}%", threshold_pct * 100.0)).size(13)),
            );

            container(column![coalescer_kinds, threshold_slider,].spacing(8))
                .style(style::modal_container)
                .padding(8)
                .into()
        } else {
            row![].into()
        }
    };

    let tooltip_section = {
        let mut elements = vec![
            iced::widget::checkbox("Dynamic circle radius", cfg.trade_size_scale.is_some())
                .on_toggle(move |value| {
                    Message::VisualConfigChanged(
                        Some(pane),
                        VisualConfig::Heatmap(heatmap::Config {
                            trade_size_scale: if value { Some(100) } else { None },
                            ..cfg
                        }),
                    )
                })
                .into(),
            circle_scaling_slider,
        ];

        let opacity_slider = classic_slider_row(
            text("Circle opacity"),
            slider(0.0..=1.0, cfg.trade_opacity, move |value| {
                Message::VisualConfigChanged(
                    Some(pane),
                    VisualConfig::Heatmap(heatmap::Config {
                        trade_opacity: value,
                        ..cfg
                    }),
                )
            })
            .step(0.05)
            .into(),
            Some(text(format!("{:.0}%", cfg.trade_opacity * 100.0)).size(13)),
        );
        
        elements.push(opacity_slider);

        // Header row with toggle and settings button
        let is_open = cfg.hover_tooltip_settings_open;
        let hover_header = row![
            iced::widget::checkbox("Show hover tooltips", cfg.show_hover_tooltips)
                .on_toggle(move |value| {
                    Message::VisualConfigChanged(
                        Some(pane),
                        VisualConfig::Heatmap(heatmap::Config {
                            show_hover_tooltips: value,
                            ..cfg
                        }),
                    )
                }),
            Space::new().width(Length::Fill),
            button(style::icon_text(style::Icon::Cog, 12))
                .on_press(Message::VisualConfigChanged(
                    Some(pane),
                    VisualConfig::Heatmap(heatmap::Config {
                        hover_tooltip_settings_open: !cfg.hover_tooltip_settings_open,
                        ..cfg
                    }),
                ))
                .style(move |theme, status| style::button::transparent(theme, status, is_open)),
        ]
        .align_y(Alignment::Center)
        .spacing(8);

        let hover_inner: Element<_> = if cfg.show_hover_tooltips && cfg.hover_tooltip_settings_open {
            column![
                iced::widget::checkbox("Show USD values", cfg.show_usd_values)
                    .on_toggle(move |value| {
                        Message::VisualConfigChanged(
                            Some(pane),
                            VisualConfig::Heatmap(heatmap::Config {
                                show_usd_values: value,
                                ..cfg
                            }),
                        )
                    })
            ]
            .padding(8)
            .spacing(6)
            .into()
        } else {
            row![].into()
        };

        elements.push(
            container(column![hover_header, hover_inner].spacing(6).padding(8))
                .style(style::modal_container)
                .into(),
        );

        column(elements).spacing(12)
    };

    container(scrollable_content(
        column![
            column![
                text("Size filters").size(14),
                column![trade_size_slider, order_size_slider].spacing(6),
            ]
            .spacing(14)
            .padding(10)
            .align_x(Alignment::Start),
            container(Space::new().width(Length::Fill).height(1)).style(style::split_ruler_container),
            column![
                text("Noise filters").size(14),
                iced::widget::checkbox(
                    "Merge orders if sizes are similar",
                    cfg.coalescing.is_some(),
                )
                .on_toggle(move |value| {
                    Message::VisualConfigChanged(
                        Some(pane),
                        VisualConfig::Heatmap(heatmap::Config {
                            coalescing: if value {
                                Some(CoalesceKind::Average(0.15))
                            } else {
                                None
                            },
                            ..cfg
                        }),
                    )
                }),
                coalescer_cfg,
            ]
            .spacing(14)
            .padding(10)
            .align_x(Alignment::Start),
            container(Space::new().width(Length::Fill).height(1)).style(style::split_ruler_container),
            column![
                text("Trade visualization").size(14),
                tooltip_section,
            ]
            .spacing(14)
            .padding(10)
            .width(Length::Fill)
            .align_x(Alignment::Start),
            container(Space::new().width(Length::Fill).height(1)).style(style::split_ruler_container),
            row![
                Space::new().width(Length::Fill),
                sync_all_button(VisualConfig::Heatmap(cfg))
            ],
        ]
        .spacing(8),
    ))
    .width(Length::Shrink)
    .padding(12)
    .max_width(340)
    .style(style::chart_modal)
    .into()
}

pub fn timesales_cfg_view<'a>(
    cfg: timeandsales::Config,
    pane: pane_grid::Pane,
) -> Element<'a, Message> {
    let trade_size_slider = {
        let filter = cfg.trade_size_filter;

        labeled_slider(
            "Trade",
            0.0..=50000.0,
            filter,
            move |value| {
                Message::VisualConfigChanged(
                    Some(pane),
                    VisualConfig::TimeAndSales(timeandsales::Config {
                        trade_size_filter: value,
                        buffer_filter: cfg.buffer_filter,
                        stacked_bar_ratio: cfg.stacked_bar_ratio,
                        show_usd_size: cfg.show_usd_size,
                        display_style: cfg.display_style,
                    }),
                )
            },
            |value| format!("${}", format_with_commas(*value)),
            Some(500.0),
        )
    };

    let buffer_size_slider = {
        let buffer = cfg.buffer_filter as f32;

        labeled_slider(
            "Buffer",
            100.0..=2000.0,
            buffer,
            move |value| {
                Message::VisualConfigChanged(
                    Some(pane),
                    VisualConfig::TimeAndSales(timeandsales::Config {
                        trade_size_filter: cfg.trade_size_filter,
                        buffer_filter: value as usize,
                        stacked_bar_ratio: cfg.stacked_bar_ratio,
                        show_usd_size: cfg.show_usd_size,
                        display_style: cfg.display_style,
                    }),
                )
            },
            |value| format!("{}", *value as usize),
            Some(50.0),
        )
    };

    let stacked_bar_picklist = pick_list(
        data::chart::timeandsales::StackedBarRatio::ALL,
        Some(cfg.stacked_bar_ratio),
        move |ratio| {
            Message::VisualConfigChanged(
                Some(pane),
                VisualConfig::TimeAndSales(timeandsales::Config {
                    trade_size_filter: cfg.trade_size_filter,
                    buffer_filter: cfg.buffer_filter,
                    stacked_bar_ratio: ratio,
                    show_usd_size: cfg.show_usd_size,
                    display_style: cfg.display_style,
                }),
            )
        },
    );

    let display_style_picklist = pick_list(
        data::chart::timeandsales::DisplayStyle::ALL,
        Some(cfg.display_style),
        move |style| {
            Message::VisualConfigChanged(
                Some(pane),
                VisualConfig::TimeAndSales(timeandsales::Config {
                    trade_size_filter: cfg.trade_size_filter,
                    buffer_filter: cfg.buffer_filter,
                    stacked_bar_ratio: cfg.stacked_bar_ratio,
                    show_usd_size: cfg.show_usd_size,
                    display_style: style,
                }),
            )
        },
    );

    let usd_size_checkbox = iced::widget::checkbox("Show USD size", cfg.show_usd_size)
        .on_toggle(move |value| {
            Message::VisualConfigChanged(
                Some(pane),
                VisualConfig::TimeAndSales(timeandsales::Config {
                    trade_size_filter: cfg.trade_size_filter,
                    buffer_filter: cfg.buffer_filter,
                    stacked_bar_ratio: cfg.stacked_bar_ratio,
                    show_usd_size: value,
                    display_style: cfg.display_style,
                }),
            )
        });

    container(scrollable_content(
        column![
            column![text("Size filter").size(14), trade_size_slider]
                .spacing(12)
                .padding(10)
                .align_x(Alignment::Center),
            column![
                row![
                    tooltip(
                        text("Buffer size").size(14),
                        Some("Amount of trades will be stored before cleanup."),
                        TooltipPosition::Right,
                    ),
                    Space::new().width(Length::Fill),
                ]
                .align_y(Alignment::Center),
                buffer_size_slider,
            ]
                .spacing(12)
                .padding(10)
                .align_x(Alignment::Center),
            column![text("Histogram ratio").size(14), stacked_bar_picklist]
                .spacing(12)
                .padding(10)
                .align_x(Alignment::Center),
            column![text("Display style").size(14), display_style_picklist]
                .spacing(12)
                .padding(10)
                .align_x(Alignment::Center),
            column![text("Display options").size(14), usd_size_checkbox]
                .spacing(12)
                .padding(10)
                .align_x(Alignment::Center),
            row![
                Space::new().width(Length::Fill),
                sync_all_button(VisualConfig::TimeAndSales(cfg))
            ],
        ]
        .spacing(8),
    ))
    .width(Length::Shrink)
    .padding(12)
    .max_width(420)
    .style(style::chart_modal)
    .into()
}

fn sync_all_button<'a>(config: VisualConfig) -> Element<'a, Message> {
    container(tooltip(
        button("Sync all")
            .on_press(Message::VisualConfigChanged(None, config))
            .padding(8),
        Some("Apply configuration to similar panes"),
        TooltipPosition::Top,
    ))
    .padding(16)
    .into()
}

pub fn kline_cfg_view<'a>(
    study_config: &'a study::ChartStudy,
    kind: &'a KlineChartKind,
    pane: pane_grid::Pane,
) -> Element<'a, Message> {
    match kind {
        KlineChartKind::Candles => container(text(
            "This chart type doesn't have any configurations, WIP...",
        ))
        .padding(16)
        .width(Length::Shrink)
        .max_width(500)
        .style(style::chart_modal)
        .into(),
        KlineChartKind::Footprint { clusters, studies } => {
            let cluster_picklist =
                pick_list(ClusterKind::ALL, Some(clusters), move |new_cluster_kind| {
                    Message::ClusterKindSelected(pane, new_cluster_kind)
                });

            let study_cfg = study_config
                .view(studies)
                .map(move |msg| Message::StudyConfigurator(pane, msg));

            container(scrollable_content(
                column![
                    column![text("Clustering type").size(14), cluster_picklist].spacing(4),
                    column![text("Footprint studies").size(14), study_cfg].spacing(4),
                ]
                .spacing(20)
                .padding(16)
                .align_x(Alignment::Start),
            ))
            .width(Length::Shrink)
            .max_width(320)
            .padding(16)
            .style(style::chart_modal)
            .into()
        }
    }
}

pub mod study {
    use crate::style::{self, Icon, icon_text};
    use data::chart::kline::FootprintStudy;
use iced::{
    Element, padding, Length,
        widget::{button, column, container, row, slider, text, Space},
};

    #[derive(Debug, Clone, Copy)]
    pub enum Message {
        CardToggled(FootprintStudy),
        StudyToggled(FootprintStudy, bool),
        StudyValueChanged(FootprintStudy),
    }

    pub enum Action {
        ToggleStudy(FootprintStudy, bool),
        ConfigureStudy(FootprintStudy),
    }

    #[derive(Default)]
    pub struct ChartStudy {
        expanded_card: Option<FootprintStudy>,
    }

    impl ChartStudy {
        pub fn new() -> Self {
            Self {
                expanded_card: None,
            }
        }

        pub fn update(&mut self, message: Message) -> Option<Action> {
            match message {
                Message::CardToggled(study) => {
                    let should_collapse = self
                        .expanded_card
                        .as_ref()
                        .is_some_and(|expanded| expanded.is_same_type(&study));

                    if should_collapse {
                        self.expanded_card = None;
                    } else {
                        self.expanded_card = Some(study);
                    }
                }
                Message::StudyToggled(study, is_checked) => {
                    return Some(Action::ToggleStudy(study, is_checked));
                }
                Message::StudyValueChanged(study) => {
                    return Some(Action::ConfigureStudy(study));
                }
            }

            None
        }

        pub fn view(&self, studies: &Vec<FootprintStudy>) -> Element<'_, Message> {
            let mut content = column![].spacing(4);

            for available_study in FootprintStudy::ALL {
                let (is_selected, study_config) = {
                    let mut is_selected = false;
                    let mut study_config = None;

                    for s in studies {
                        if s.is_same_type(&available_study) {
                            is_selected = true;
                            study_config = Some(*s);
                            break;
                        }
                    }
                    (is_selected, study_config)
                };

                content =
                    content.push(self.create_study_row(available_study, is_selected, study_config));
            }

            content.into()
        }

        fn create_study_row(
            &self,
            study: FootprintStudy,
            is_selected: bool,
            study_config: Option<FootprintStudy>,
        ) -> Element<'_, Message> {
            let checkbox = iced::widget::checkbox(study.to_string(), is_selected)
                .on_toggle(move |checked| Message::StudyToggled(study, checked));

            let mut checkbox_row = row![checkbox, Space::new().width(Length::Fill)]
                .height(36)
                .align_y(iced::Alignment::Center)
                .padding(4)
                .spacing(4);

            let is_expanded = self
                .expanded_card
                .as_ref()
                .is_some_and(|expanded| expanded.is_same_type(&study));

            if is_selected {
                checkbox_row = checkbox_row.push(
                    button(icon_text(Icon::Cog, 12))
                        .on_press(Message::CardToggled(study))
                        .style(move |theme, status| {
                            style::button::transparent(theme, status, is_expanded)
                        }),
                );
            }

            let mut column = column![checkbox_row].padding(padding::left(4));

            if is_expanded && study_config.is_some() {
                let config = study_config.unwrap();

                match config {
                    FootprintStudy::NPoC { lookback } => {
                        let slider_ui = slider(10.0..=400.0, lookback as f32, move |new_value| {
                            let updated_study = FootprintStudy::NPoC {
                                lookback: new_value as usize,
                            };
                            Message::StudyValueChanged(updated_study)
                        })
                        .step(10.0);

                        column = column.push(
                            column![text(format!("Lookback: {lookback} datapoints")), slider_ui]
                                .padding(8)
                                .spacing(4),
                        );
                    }
                    FootprintStudy::Imbalance {
                        threshold,
                        color_scale,
                        ignore_zeros,
                    } => {
                        let qty_threshold = {
                            let info_text = text(format!("Ask:Bid threshold: {threshold}%"));

                            let threshold_slider =
                                slider(100.0..=800.0, threshold as f32, move |new_value| {
                                    let updated_study = FootprintStudy::Imbalance {
                                        threshold: new_value as usize,
                                        color_scale,
                                        ignore_zeros,
                                    };
                                    Message::StudyValueChanged(updated_study)
                                })
                                .step(25.0);

                            column![info_text, threshold_slider,].padding(8).spacing(4)
                        };

                        let color_scaling = {
                            let color_scale_enabled = color_scale.is_some();
                            let color_scale_value = color_scale.unwrap_or(100);

                            let color_scale_checkbox = iced::widget::checkbox(
                                "Dynamic color scaling",
                                color_scale_enabled,
                            )
                            .on_toggle(move |is_enabled| {
                                let updated_study = FootprintStudy::Imbalance {
                                    threshold,
                                    color_scale: if is_enabled {
                                        Some(color_scale_value)
                                    } else {
                                        None
                                    },
                                    ignore_zeros,
                                };
                                Message::StudyValueChanged(updated_study)
                            });

                            if color_scale_enabled {
                                let scaling_slider = column![
                                    text(format!("Opaque color at: {color_scale_value}x")),
                                    slider(
                                        50.0..=2000.0,
                                        color_scale_value as f32,
                                        move |new_value| {
                                            let updated_study = FootprintStudy::Imbalance {
                                                threshold,
                                                color_scale: Some(new_value as usize),
                                                ignore_zeros,
                                            };
                                            Message::StudyValueChanged(updated_study)
                                        }
                                    )
                                    .step(50.0)
                                ]
                                .spacing(2);

                                column![color_scale_checkbox, scaling_slider]
                                    .padding(8)
                                    .spacing(8)
                            } else {
                                column![color_scale_checkbox].padding(8)
                            }
                        };

                        let ignore_zeros_checkbox = {
                            let cbox = iced::widget::checkbox("Ignore zeros", ignore_zeros)
                                .on_toggle(move |is_checked| {
                                    let updated_study = FootprintStudy::Imbalance {
                                        threshold,
                                        color_scale,
                                        ignore_zeros: is_checked,
                                    };
                                    Message::StudyValueChanged(updated_study)
                                });

                            column![cbox].padding(8).spacing(4)
                        };

                        column = column.push(
                            column![
                                qty_threshold,
                                container(Space::new().width(Length::Fill).height(1)).style(style::split_ruler_container),
                                color_scaling,
                                container(Space::new().width(Length::Fill).height(1)).style(style::split_ruler_container),
                                ignore_zeros_checkbox,
                            ]
                            .padding(8)
                            .spacing(4),
                        );
                    }
                }
            }

            container(column).style(style::modal_container).into()
        }
    }
}
