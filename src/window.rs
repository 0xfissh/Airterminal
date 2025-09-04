use std::collections::HashMap;

use data::layout::WindowSpec;
use iced::{Point, Size, Subscription, Task, window, keyboard};

pub use iced::window::{Id, Position, Settings, close, open};
use iced_futures::MaybeSend;

#[derive(Debug, Clone, Copy)]
pub struct Window {
    pub id: Id,
    pub position: Option<Point>,
}

impl Window {
    pub fn new(id: Id) -> Self {
        Self { id, position: None }
    }
}

pub fn default_size() -> Size {
    WindowSpec::default().size()
}

#[derive(Debug, Clone)]
pub enum Event {
    CloseRequested(window::Id),
    KeyboardShortcut(KeyboardShortcut),
}

#[derive(Debug, Clone)]
pub enum KeyboardShortcut {
    // File operations
    NewLayout,
    OpenSettings,
    SaveLayout,
    
    // Navigation
    NextLayout,
    PreviousLayout,
    
    // View operations
    ToggleFullscreen,
    ZoomIn,
    ZoomOut,
    ResetZoom,
    
    // Pane operations
    TogglePaneMaximize,
    SplitPane,
    ClonePane,
    ResetPane,
    
    // Autoscale operations
    CenterLatest,
    FitToVisible,
    
    // Application
    Quit,
    
    // Theme
    ToggleTheme,
    
    // Data
    OpenDataFolder,
    

}

pub fn events() -> Subscription<Event> {
    iced::event::listen_with(filtered_events)
}

fn filtered_events(
    event: iced::Event,
    status: iced::event::Status,
    window: window::Id,
) -> Option<Event> {
    match &event {
        iced::Event::Window(iced::window::Event::CloseRequested) => {
            Some(Event::CloseRequested(window))
        }
        iced::Event::Keyboard(keyboard::Event::KeyPressed { 
            key, 
            modifiers, 
            .. 
        }) => {
            // Handle keyboard shortcuts
            match key {
                // Cmd/Ctrl + N - New Layout
                keyboard::Key::Character(c) if c.as_str() == "n" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::NewLayout))
                }
                
                // Cmd/Ctrl + , - Open Settings
                keyboard::Key::Character(c) if c.as_str() == "," && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::OpenSettings))
                }
                
                // Cmd/Ctrl + S - Save Layout
                keyboard::Key::Character(c) if c.as_str() == "s" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::SaveLayout))
                }
                
                // Cmd/Ctrl + ] - Next Layout (alternative to Tab)
                keyboard::Key::Character(c) if c.as_str() == "]" && modifiers.command() && !modifiers.shift() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::NextLayout))
                }
                
                // Cmd/Ctrl + [ - Previous Layout (alternative to Shift+Tab)
                keyboard::Key::Character(c) if c.as_str() == "[" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::PreviousLayout))
                }
                
                // Cmd/Ctrl + F - Toggle Fullscreen (alternative to F11)
                keyboard::Key::Character(c) if c.as_str() == "f" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::ToggleFullscreen))
                }
                
                // F - Toggle Pane Maximize/Restore (no modifiers) - only if not captured by another widget
                keyboard::Key::Character(c) if c.as_str() == "f" && !modifiers.command() && !modifiers.shift() && !modifiers.alt() && !modifiers.control() && matches!(status, iced::event::Status::Ignored) => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::TogglePaneMaximize))
                }
                
                // C - Center Latest (no modifiers) - only if not captured by another widget
                keyboard::Key::Character(c) if c.as_str() == "c" && !modifiers.command() && !modifiers.shift() && !modifiers.alt() && !modifiers.control() && matches!(status, iced::event::Status::Ignored) => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::CenterLatest))
                }
                
                // A - Fit To Visible (just A, no modifiers) - only if not captured by another widget
                keyboard::Key::Character(c) if c.as_str() == "a" && !modifiers.command() && !modifiers.shift() && !modifiers.alt() && !modifiers.control() && matches!(status, iced::event::Status::Ignored) => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::FitToVisible))
                }
                
                // Cmd/Ctrl + Plus - Zoom In
                keyboard::Key::Character(c) if (c.as_str() == "+" || c.as_str() == "=") && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::ZoomIn))
                }
                
                // Cmd/Ctrl + Minus - Zoom Out
                keyboard::Key::Character(c) if c.as_str() == "-" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::ZoomOut))
                }
                
                // Cmd/Ctrl + 0 - Reset Zoom
                keyboard::Key::Character(c) if c.as_str() == "0" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::ResetZoom))
                }
                
                // Cmd/Ctrl + Shift + Enter - Clone Pane
                keyboard::Key::Named(keyboard::key::Named::Enter)
                    if modifiers.command() && modifiers.shift() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::ClonePane))
                }

                // Cmd/Ctrl + Enter - Split Pane
                keyboard::Key::Named(keyboard::key::Named::Enter) if modifiers.command() && !modifiers.shift() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::SplitPane))
                }
                
                // Cmd/Ctrl + R - Reset Pane
                keyboard::Key::Character(c) if c.as_str() == "r" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::ResetPane))
                }

                // Cmd/Ctrl + Q - Quit
                keyboard::Key::Character(c) if c.as_str() == "q" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::Quit))
                }
                
                // Cmd/Ctrl + T - Toggle Theme
                keyboard::Key::Character(c) if c.as_str() == "t" && modifiers.command() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::ToggleTheme))
                }
                
                // Cmd/Ctrl + Shift + D - Open Data Folder
                keyboard::Key::Character(c) if c.as_str() == "d" && modifiers.command() && modifiers.shift() => {
                    Some(Event::KeyboardShortcut(KeyboardShortcut::OpenDataFolder))
                }
                
                _ => None,
            }
        }
        _ => None,
    }
}

pub fn collect_window_specs<M, F>(window_ids: Vec<window::Id>, message: F) -> Task<M>
where
    F: Fn(HashMap<window::Id, WindowSpec>) -> M + Send + 'static,
    M: MaybeSend + 'static,
{
    // Create a task that collects specs for each window
    let window_spec_tasks: Vec<Task<(window::Id, (Option<Point>, Size))>> = window_ids
        .into_iter()
        .map(|window_id| {
            // Map both tasks to produce an enum or tuple to distinguish them
            let pos_task: Task<(Option<Point>, Option<Size>)> =
                iced::window::get_position(window_id).map(|pos| (pos, None));

            let size_task: Task<(Option<Point>, Option<Size>)> =
                iced::window::get_size(window_id).map(|size| (None, Some(size)));

            Task::batch(vec![pos_task, size_task])
                .collect()
                .map(move |results| {
                    let position = results.iter().find_map(|(pos, _)| *pos);
                    let size = results
                        .iter()
                        .find_map(|(_, size)| *size)
                        .unwrap_or_else(|| Size::new(1024.0, 768.0));

                    (window_id, (position, size))
                })
        })
        .collect();

    // Batch all window tasks together and collect results
    Task::batch(window_spec_tasks)
        .collect()
        .map(move |results| {
            let specs: HashMap<window::Id, WindowSpec> = results
                .into_iter()
                .filter_map(|(id, (pos, size))| {
                    pos.map(|position| (id, WindowSpec::from((&position, &size))))
                })
                .collect();

            message(specs)
        })
}

#[cfg(target_os = "linux")]
pub fn settings() -> Settings {
    Settings {
        min_size: Some(Size::new(800.0, 600.0)),
        ..Default::default()
    }
}

#[cfg(target_os = "macos")]
pub fn settings() -> Settings {
    use iced::window;

    Settings {
        platform_specific: window::settings::PlatformSpecific {
            title_hidden: true,
            titlebar_transparent: true,
            fullsize_content_view: true,
        },
        min_size: Some(Size::new(800.0, 600.0)),
        ..Default::default()
    }
}

#[cfg(target_os = "windows")]
pub fn settings() -> Settings {
    Settings {
        min_size: Some(Size::new(800.0, 600.0)),
        ..Default::default()
    }
}
