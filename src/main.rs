#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod chart;
mod layout;
mod logger;
mod modal;
mod screen;
mod style;
mod widget;
mod window;

// Import rustls for crypto provider initialization
use rustls::crypto::ring;

use data::config::theme::{default_theme, redshift_theme};
use data::{layout::WindowSpec, sidebar};
use modal::{LayoutManager, ThemeEditor, audio};
use modal::{dashboard_modal, main_dialog_modal};
use screen::dashboard::{self, Dashboard};
use style::Icon;
use widget::{
    confirm_dialog_container,
    toast::{self, Toast},
    tooltip,
};

use iced::{
    Alignment, Element, Subscription, Task, padding,
    widget::{
        button, column, container, pane_grid, pick_list, row, text,
        tooltip::Position as TooltipPosition,
    },
};
use std::{
    borrow::Cow,
    collections::HashMap,
    time::{Duration, Instant},
    vec,
};
use supabase::{AuthState, SupabaseClientWrapper, SupabaseError, TrialStatus, SubscriptionCheckResult};

fn main() {
    // Initialize rustls crypto provider
    ring::default_provider().install_default().expect("Failed to install rustls crypto provider");

    logger::setup(cfg!(debug_assertions)).expect("Failed to initialize logger");

    std::thread::spawn(data::cleanup_old_market_data);

    // Clean up any sensitive configuration files that shouldn't be in the data directory
    std::thread::spawn(data::cleanup_sensitive_files);

    let _ = iced::daemon(Airterminal::new, Airterminal::update, Airterminal::view)
        .settings(iced::Settings {
            antialiasing: true,
            fonts: vec![
                Cow::Borrowed(style::AZERET_MONO_BYTES),
                Cow::Borrowed(style::ICONS_BYTES),
            ],
            default_text_size: iced::Pixels(12.0),
            ..Default::default()
        })
        .title(Airterminal::title)
        .theme(Airterminal::theme)
        .scale_factor(Airterminal::scale_factor)
        .subscription(Airterminal::subscription)
        .run();
}

struct Airterminal {
    main_window: window::Window,
    sidebar: dashboard::Sidebar,
    layout_manager: LayoutManager,
    theme_editor: ThemeEditor,
    audio_stream: audio::AudioStream,
    confirm_dialog: Option<(String, Box<Message>)>,
    scale_factor: data::ScaleFactor,
    timezone: data::UserTimezone,
    theme: data::Theme,
    notifications: Vec<Toast>,
    
    // Supabase related state
    supabase_client: Option<SupabaseClientWrapper>,
    auth_state: AuthState,
    is_pro: bool,
    trial_status: Option<TrialStatus>,
    show_login_view: bool,
    
    // Login form state
    email_input: String,
    password_input: String,
    password_visible: bool,
    login_in_progress: bool,
    login_error_message: Option<String>,
    
    // User profile data
    user_name: Option<String>,
    user_email: Option<String>,
    
    // Detailed subscription information
    subscription_status: Option<String>,
    subscription_id: Option<String>,
    next_billing_date: Option<chrono::DateTime<chrono::Utc>>,
    copperx_status: Option<supabase::CopperXSubscriptionStatus>,
    
    // Update-related state
    update_available: Option<supabase::UpdateInfo>,
    update_downloading: bool,
    update_download_path: Option<std::path::PathBuf>,
    
    // Initialization state
    is_initializing: bool,
    
    // Geoblocking detection
    show_geoblock_warning: bool,
    geoblock_error_count: usize,
}

#[derive(Debug, Clone)]
enum Message {
    LoadLayout(layout::Layout),
    Sidebar(dashboard::sidebar::Message),
    MarketWsEvent(exchange::Event),
    Dashboard(Option<uuid::Uuid>, dashboard::Message),
    Tick(Instant),
    WindowEvent(window::Event),
    ExitRequested(HashMap<window::Id, WindowSpec>),

    DataFolderRequested,
    ThemeSelected(data::Theme),
    ScaleFactorChanged(data::ScaleFactor),
    SetTimezone(data::UserTimezone),
    ToggleTradeFetch(bool),

    ToggleDialogModal(Option<(String, Box<Message>)>),
    AddNotification(Toast),
    DeleteNotification(usize),
    ThemeEditor(modal::theme_editor::Message),
    Layouts(modal::layout_manager::Message),
    AudioStream(modal::audio::Message),
    
    // Keyboard shortcuts
    KeyboardShortcut(window::KeyboardShortcut),
    
    // Supabase related messages
    SupabaseInit(Result<SupabaseClientWrapper, SupabaseError>),
    LoginWithEmail(String, String),
    UpdateEmailField(String),
    UpdatePasswordField(String),
    ResetLoginState,
    SetLoginError(String),
    AuthStateChanged(AuthState),
    ComprehensiveSubscriptionChecked(Result<SubscriptionCheckResult, SupabaseError>),
    Logout,
    
    // User profile related messages
    FetchUserProfile,
    UpdateUserProfile(Option<String>, Option<String>), // name, email
    
    // App update related messages
    CheckForUpdate,
    UpdateCheckComplete(Result<supabase::UpdateInfo, SupabaseError>),
    DownloadUpdate(String), // download_url
    UpdateDownloaded(Result<std::path::PathBuf, SupabaseError>),
    ApplyUpdate(std::path::PathBuf),
    DismissUpdate,
    TogglePasswordVisibility,
    UpdateSubscriptionDetails(String, Option<String>, Option<chrono::DateTime<chrono::Utc>>),
    
    // Copy to clipboard
    CopyToClipboard(String),
    
    // Open URL in browser
    OpenUrl(String),
    
    // Manual save state
    SaveState,
    SaveStateWithSpecs(HashMap<window::Id, WindowSpec>),
    
    // Geoblocking detection
    DismissGeoblockWarning,
}

impl Airterminal {
    fn new() -> (Self, Task<Message>) {
        let saved_state = layout::load_saved_state();

        let (main_window_id, open_main_window) = {
            let (position, size) = saved_state.window();

            let config = window::Settings {
                size,
                position,
                exit_on_close_request: false,
                ..window::settings()
            };

            window::open(config)
        };

        let load_layout = Task::done(Message::LoadLayout(
            saved_state.layout_manager.active_layout(),
        ));
        let (sidebar, launch_sidebar) = dashboard::Sidebar::new(&saved_state);

        // Synchronously check for existing session to avoid login screen flash
        let (initial_auth_state, initial_show_login_view) = match supabase::fast_load_session() {
            Ok(Some(session)) => {
                if session.should_use() {
                    log::info!("Found valid session during initialization, user should be logged in");
                    (AuthState::Authenticated(session.user_id), false)
                } else {
                    log::info!("Found expired session during initialization, user needs to log in");
                    (AuthState::Unauthenticated, true)
                }
            }
            Ok(None) => {
                log::info!("No session found during initialization, user needs to log in");
                (AuthState::Unauthenticated, true)
            }
            Err(e) => {
                log::warn!("Error loading session during initialization: {}", e);
                (AuthState::Unauthenticated, true)
            }
        };

        (
            Self {
                main_window: window::Window::new(main_window_id),
                layout_manager: saved_state.layout_manager,
                theme_editor: ThemeEditor::new(saved_state.custom_theme),
                audio_stream: audio::AudioStream::new(saved_state.audio_cfg),
                sidebar,
                confirm_dialog: None,
                timezone: saved_state.timezone,
                scale_factor: saved_state.scale_factor,
                theme: saved_state.theme,
                notifications: vec![],
                
                // Supabase related state - use the initial auth state we determined
                supabase_client: None,
                auth_state: initial_auth_state,
                is_pro: false,
                trial_status: None,
                show_login_view: initial_show_login_view,
                
                // Login form state
                email_input: String::new(),
                password_input: String::new(),
                password_visible: false,
                login_in_progress: false,
                login_error_message: None,
                
                // User profile data
                user_name: None,
                user_email: None,
                
                // Detailed subscription information
                subscription_status: None,
                subscription_id: None,
                next_billing_date: None,
                copperx_status: None,
                
                // Update-related state
                update_available: None,
                update_downloading: false,
                update_download_path: None,
                
                // Initialization state
                is_initializing: true,
                
                // Geoblocking detection
                show_geoblock_warning: false,
                geoblock_error_count: 0,
            },
            open_main_window
                .then(|_| Task::none())
                .chain(Task::batch(vec![
                    load_layout,
                    launch_sidebar.map(Message::Sidebar),
                    Task::done(Message::SetTimezone(saved_state.timezone)),
                    // Initialize Supabase client (fast)
                    Task::perform(
                        supabase::get_supabase_client_fast(),
                        Message::SupabaseInit
                    ),
                ])),
        )
    }

    fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::MarketWsEvent(event) => {
                let main_window_id = self.main_window.id;
                let timezone = self.timezone; // Copy the timezone before borrowing
                let dashboard = self.active_dashboard_mut();

                match event {
                    exchange::Event::Connected(exchange, _) => {
                        log::info!("a stream connected to {exchange} WS");
                        // Reset geoblocking detection on successful connection
                        if self.geoblock_error_count > 0 || self.show_geoblock_warning {
                            log::info!("Resetting geoblocking detection due to successful connection to {}", exchange);
                            self.geoblock_error_count = 0;
                            // Don't auto-dismiss the warning here, let user manually dismiss it
                        }
                    }
                    exchange::Event::Disconnected(exchange, reason) => {
                        log::info!("a stream disconnected from {exchange} WS: {reason:?}");
                    }
                    exchange::Event::DepthReceived(
                        stream,
                        depth_update_t,
                        depth,
                        trades_buffer,
                    ) => {
                        let task = dashboard
                            .update_depth_and_trades(
                                &stream,
                                depth_update_t,
                                &depth,
                                &trades_buffer,
                                main_window_id,
                                timezone,
                            )
                            .map(move |msg| Message::Dashboard(None, msg));

                        if let Err(err) = self.audio_stream.try_play_sound(&stream, &trades_buffer)
                        {
                            log::error!("Failed to play sound: {err}");
                        }

                        return task;
                    }
                    exchange::Event::KlineReceived(stream, kline) => {
                        return dashboard
                            .update_latest_klines(&stream, &kline, main_window_id)
                            .map(move |msg| Message::Dashboard(None, msg));
                    }
                }
            }
            Message::Tick(now) => {
                let main_window_id = self.main_window.id;

                return self
                    .active_dashboard_mut()
                    .tick(now, main_window_id)
                    .map(move |msg| Message::Dashboard(None, msg));
            }
            Message::WindowEvent(event) => match event {
                window::Event::CloseRequested(window) => {
                    let main_window = self.main_window.id;
                    let dashboard = self.active_dashboard_mut();

                    if window != main_window {
                        dashboard.popout.remove(&window);
                        return window::close(window);
                    }

                    let mut opened_windows = dashboard
                        .popout
                        .keys()
                        .copied()
                        .collect::<Vec<window::Id>>();

                    opened_windows.push(main_window);

                    return window::collect_window_specs(opened_windows, Message::ExitRequested);
                }
                window::Event::KeyboardShortcut(shortcut) => {
                    return Task::done(Message::KeyboardShortcut(shortcut));
                }
            },
            Message::ExitRequested(windows) => {
                self.active_dashboard_mut()
                    .popout
                    .iter_mut()
                    .for_each(|(id, (_, window_spec))| {
                        if let Some(new_window_spec) = windows.get(id) {
                            *window_spec = *new_window_spec;
                        }
                    });

                let mut ser_layouts = vec![];

                for id in &self.layout_manager.layout_order {
                    if let Some((layout, dashboard)) = self.layout_manager.get_layout(*id) {
                        let serialized_dashboard = data::Dashboard::from(dashboard);

                        ser_layouts.push(data::Layout {
                            name: layout.name.clone(),
                            dashboard: serialized_dashboard,
                        });
                    }
                }

                let layouts = data::Layouts {
                    layouts: ser_layouts,
                    active_layout: self.layout_manager.active_layout().name.clone(),
                };

                let main_window = windows
                    .iter()
                    .find(|(id, _)| **id == self.main_window.id)
                    .map(|(_, spec)| *spec);

                let audio_cfg = data::AudioStream::from(&self.audio_stream);

                let layout = data::State::from_parts(
                    layouts,
                    self.theme.clone(),
                    self.theme_editor.custom_theme.clone().map(data::Theme),
                    self.sidebar.favorited_tickers(),
                    main_window,
                    self.timezone,
                    self.sidebar.state,
                    self.scale_factor,
                    audio_cfg,
                );

                match serde_json::to_string(&layout) {
                    Ok(layout_str) => {
                        let file_name = data::SAVED_STATE_PATH;

                        if let Err(e) = data::write_json_to_file(&layout_str, file_name) {
                            log::error!("Failed to write layout state to file: {}", e);
                        } else {
                            log::info!("Successfully wrote layout state to {file_name}");
                        }
                    }
                    Err(e) => log::error!("Failed to serialize layout: {}", e),
                }

                return iced::exit();
            }
            Message::ThemeSelected(theme) => {
                self.theme = theme.clone();
            }
            Message::Dashboard(id, message) => {
                let main_window = self.main_window;
                let layout_id = id.unwrap_or(self.layout_manager.active_layout().id);

                if let Some(dashboard) = self.layout_manager.mut_dashboard(&layout_id) {
                    let (main_task, event) = dashboard.update(message, &main_window, &layout_id);

                    let additional_task = match event {
                        Some(dashboard::Event::DistributeFetchedData {
                            layout_id,
                            pane_id,
                            data,
                            stream,
                        }) => dashboard
                            .distribute_fetched_data(main_window.id, pane_id, data, stream)
                            .map(move |msg| Message::Dashboard(Some(layout_id), msg)),
                        Some(dashboard::Event::Notification(toast)) => {
                            // Check if this notification might be due to geoblocking
                            if toast.status() == widget::toast::Status::Danger {
                                let error_message = format!("{} {}", toast.title(), toast.body()).to_lowercase();
                                let is_potential_geoblock = error_message.contains("rate limit") ||
                                    error_message.contains("429") ||
                                    error_message.contains("418") ||
                                    error_message.contains("403") ||
                                    error_message.contains("forbidden") ||
                                    error_message.contains("blocked") ||
                                    error_message.contains("invalid request") ||
                                    error_message.contains("fetch error");
                                
                                if is_potential_geoblock {
                                    self.geoblock_error_count += 1;
                                    
                                    // Show warning after 3 consecutive potential geoblock errors
                                    if self.geoblock_error_count >= 3 && !self.show_geoblock_warning {
                                        self.show_geoblock_warning = true;
                                        log::warn!("Potential geoblocking detected from dashboard after {} errors", self.geoblock_error_count);
                                    }
                                } else {
                                    // Reset counter if we get non-geoblock errors
                                    self.geoblock_error_count = 0;
                                }
                            }
                            
                            Task::done(Message::AddNotification(toast))
                        }
                        None => Task::none(),
                    };

                    return main_task
                        .map(move |msg| Message::Dashboard(Some(layout_id), msg))
                        .chain(additional_task);
                }
            }
            Message::SetTimezone(tz) => {
                self.timezone = tz;
            }
            Message::ScaleFactorChanged(value) => {
                self.scale_factor = value;
            }
            Message::ToggleTradeFetch(checked) => {
                self.layout_manager
                    .iter_dashboards_mut()
                    .for_each(|dashboard| {
                        dashboard.toggle_trade_fetch(checked, &self.main_window);
                    });

                if checked {
                    self.confirm_dialog = None;
                }
            }
            Message::ToggleDialogModal(dialog) => {
                self.confirm_dialog = dialog;
            }
            Message::Layouts(message) => {
                let action = self.layout_manager.update(message);

                match action {
                    Some(modal::layout_manager::Action::Select(layout)) => {
                        let active_popout_keys = self
                            .active_dashboard()
                            .popout
                            .keys()
                            .copied()
                            .collect::<Vec<_>>();

                        let window_tasks = Task::batch(
                            active_popout_keys
                                .iter()
                                .map(|&popout_id| window::close(popout_id))
                                .collect::<Vec<_>>(),
                        )
                        .then(|_: Task<window::Id>| Task::none());

                        return window::collect_window_specs(
                            active_popout_keys,
                            dashboard::Message::SavePopoutSpecs,
                        )
                        .map(move |msg| Message::Dashboard(None, msg))
                        .chain(window_tasks)
                        .chain(Task::done(Message::LoadLayout(layout)));
                    }
                    None => {}
                }
            }
            Message::LoadLayout(layout) => match self.layout_manager.set_active_layout(layout) {
                Ok(dashboard) => {
                    return dashboard
                        .load_layout()
                        .map(move |msg| Message::Dashboard(None, msg));
                }
                Err(err) => {
                    return Task::done(Message::AddNotification(Toast::error(format!(
                        "Failed to load layout: {err}"
                    ))));
                }
            },
            Message::AddNotification(toast) => {
                self.notifications.push(toast);
            }
            Message::DeleteNotification(index) => {
                if index < self.notifications.len() {
                    self.notifications.remove(index);
                }
            }
            Message::AudioStream(message) => self.audio_stream.update(message),
            Message::DataFolderRequested => {
                if let Err(err) = data::open_data_folder() {
                    return Task::done(Message::AddNotification(Toast::error(format!(
                        "Failed to open data folder: {err}",
                    ))));
                }
            }
            Message::ThemeEditor(msg) => {
                let action = self.theme_editor.update(msg, &self.theme.clone().into());

                match action {
                    Some(modal::theme_editor::Action::Exit) => {
                        self.sidebar.set_menu(Some(sidebar::Menu::Settings));
                    }
                    Some(modal::theme_editor::Action::UpdateTheme(theme)) => {
                        self.theme = data::Theme(theme);

                        let main_window = self.main_window.id;

                        self.active_dashboard_mut()
                            .invalidate_all_panes(main_window);
                    }
                    None => {}
                }
            }
            Message::Sidebar(message) => {
                let (task, action) = self.sidebar.update(message);

                // Check if the account menu was just activated and we need to fetch user profile
                let fetch_profile_task = if let Some(menu) = self.sidebar.active_menu() {
                    if menu == sidebar::Menu::Account && 
                       self.user_email.is_none() && 
                       self.user_name.is_none() {
                        Some(Task::done(Message::FetchUserProfile))
                    } else {
                        None
                    }
                } else {
                    None
                };

                match action {
                    Some(dashboard::sidebar::Action::TickerSelected(
                        ticker_info,
                        exchange,
                        content,
                    )) => {
                        let main_window_id = self.main_window.id;

                        let task = self.active_dashboard_mut().init_pane_task(
                            main_window_id,
                            ticker_info,
                            exchange,
                            &content,
                        );

                        return task.map(move |msg| Message::Dashboard(None, msg));
                    }
                    Some(dashboard::sidebar::Action::ErrorOccurred(err)) => {
                        // Check if this error might be due to geoblocking
                        let error_message = err.to_string().to_lowercase();
                        let is_potential_geoblock = error_message.contains("rate limit") ||
                            error_message.contains("429") ||
                            error_message.contains("418") ||
                            error_message.contains("403") ||
                            error_message.contains("forbidden") ||
                            error_message.contains("blocked") ||
                            error_message.contains("invalid request") ||
                            error_message.contains("fetch error");
                        
                        if is_potential_geoblock {
                            self.geoblock_error_count += 1;
                            
                            // Show warning after 3 consecutive potential geoblock errors
                            if self.geoblock_error_count >= 3 && !self.show_geoblock_warning {
                                self.show_geoblock_warning = true;
                                log::warn!("Potential geoblocking detected after {} errors", self.geoblock_error_count);
                            }
                        } else {
                            // Reset counter if we get non-geoblock errors
                            self.geoblock_error_count = 0;
                        }
                        
                        self.notifications.push(Toast::error(err.to_string()));
                    }
                    None => {}
                }

                let sidebar_task = task.map(Message::Sidebar);
                
                if let Some(fetch_task) = fetch_profile_task {
                    return Task::batch(vec![sidebar_task, fetch_task]);
                } else {
                    return sidebar_task;
                }
            }
            Message::SupabaseInit(result) => {
                match result {
                    Ok(client) => {
                        self.supabase_client = Some(client.clone());
                        self.is_initializing = false;
                        
                        // Check for app updates right after initializing Supabase
                        let update_task = Task::done(Message::CheckForUpdate);
                        
                        // Load session and check authentication state
                        let auth_task = match &self.auth_state {
                            AuthState::Authenticated(user_id) => {
                                // We already have a valid session from initialization, 
                                // load it into the client
                                let client_clone = client.clone();
                                let user_id_clone = *user_id;
                                Task::perform(
                                    async move {
                                        // Load the existing session into the client
                                        match client_clone.load_existing_session().await {
                                            Ok(true) => {
                                                log::info!("Successfully loaded existing session into client");
                                                AuthState::Authenticated(user_id_clone)
                                            }
                                            Ok(false) => {
                                                log::warn!("Failed to load existing session, user may need to re-login");
                                                AuthState::Unauthenticated
                                            }
                                            Err(e) => {
                                                log::error!("Error loading existing session: {}", e);
                                                AuthState::Unauthenticated
                                            }
                                        }
                                    },
                                    Message::AuthStateChanged
                                )
                            }
                            _ => {
                                // No valid session from initialization, try to load one
                                Task::perform(
                                    async move {
                                        match client.load_existing_session().await {
                                            Ok(true) => {
                                                // Successfully loaded a session, get the user ID
                                                match client.get_user_id().await {
                                                    Ok(Some(user_id)) => AuthState::Authenticated(user_id),
                                                    _ => AuthState::Unauthenticated,
                                                }
                                            }
                                            _ => AuthState::Unauthenticated,
                                        }
                                    },
                                    Message::AuthStateChanged
                                )
                            }
                        };
                        
                        return Task::batch(vec![update_task, auth_task]);
                    }
                    Err(e) => {
                        self.auth_state = AuthState::Error(e.to_string());
                        self.is_initializing = false;
                        
                        // Add more detailed error information for debugging
                        let mut error_msg = format!("Supabase initialization failed: {}", e);
                        
                        // Add information about current working directory
                        if let Ok(cwd) = std::env::current_dir() {
                            error_msg.push_str(&format!("\nCurrent directory: {}", cwd.display()));
                        }
                        
                        // Add information about executable path
                        if let Ok(exe_path) = std::env::current_exe() {
                            error_msg.push_str(&format!("\nExecutable path: {}", exe_path.display()));
                        }
                        
                        return Task::done(Message::AddNotification(Toast::error(error_msg)));
                    }
                }
            }
            Message::LoginWithEmail(email, password) => {
                // Validate email and password before attempting login
                if email.trim().is_empty() {
                    self.login_error_message = Some("Email cannot be empty".to_string());
                    return Task::none();
                }
                
                if !email.contains('@') {
                    self.login_error_message = Some("Invalid email format".to_string());
                    return Task::none();
                }
                
                if password.trim().is_empty() {
                    self.login_error_message = Some("Password cannot be empty".to_string());
                    return Task::none();
                }
                
                if let Some(client) = &self.supabase_client {
                    self.login_in_progress = true;
                    // Clear any previous error message
                    self.login_error_message = None;
                    
                    let client = client.clone();
                    let credentials = supabase::EmailCredentials {
                        email,
                        password,
                    };
                    
                    return Task::perform(
                        async move {
                            supabase::login_with_email(&client, &credentials).await
                        },
                        |result| match result {
                            Ok(auth_state) => Message::AuthStateChanged(auth_state),
                            Err(e) => {
                                // Store the error message for display in the UI
                                let error_msg = format!("Login failed: {}", e);
                                Message::SetLoginError(error_msg)
                            }
                        }
                    ).chain(Task::done(Message::ResetLoginState));
                } else {
                    self.login_error_message = Some("Supabase client not initialized".to_string());
                    return Task::done(Message::AddNotification(Toast::error(
                        "Supabase client not initialized".to_string()
                    )));
                }
            }
            Message::AuthStateChanged(state) => {
                self.auth_state = state.clone();
                // Reset login form state
                self.login_in_progress = false;
                // Clear error message on successful login
                self.login_error_message = None;
                // Reset login input fields
                self.email_input = String::new();
                self.password_input = String::new();
                // Reset user profile data
                self.user_name = None;
                self.user_email = None;
                
                if let AuthState::Authenticated(user_id) = state {
                    if let Some(client) = &self.supabase_client {
                        // Hide login view if it was showing
                        self.show_login_view = false;
                        
                        // Clone for each task to avoid borrow issues
                        let client1 = client.clone();
                        let client2 = client.clone();
                        
                        // Log app launch
                        let log_task = Task::perform(
                            async move {
                                supabase::log_app_launch(&client1).await
                            },
                            |result| match result {
                                Ok(_) => Message::AddNotification(Toast::info(
                                    "Login successful".to_string()
                                )),
                                Err(e) => Message::AddNotification(Toast::error(
                                    format!("Failed to log usage: {}", e)
                                )),
                            }
                        );
                        
                        // Use comprehensive subscription check instead of separate checks
                        let subscription_task = Task::perform(
                            async move {
                                supabase::check_user_subscription_status(&client2, user_id).await
                            },
                            Message::ComprehensiveSubscriptionChecked
                        );
                        
                        return Task::batch(vec![log_task, subscription_task]);
                    }
                } else if let AuthState::Unauthenticated = state {
                    // Maybe show a message that user needs to log in
                }
                
                return Task::none();
            }
            Message::ComprehensiveSubscriptionChecked(result) => {
                match result {
                    Ok(subscription_result) => {
                        self.is_pro = subscription_result.is_pro;
                        self.trial_status = Some(subscription_result.trial_status.clone());
                        self.copperx_status = subscription_result.copperx_status.clone();
                        
                        // Also fetch detailed subscription information for the account modal
                        if let AuthState::Authenticated(user_id) = &self.auth_state {
                            if let Some(client) = &self.supabase_client {
                                let client_clone = client.clone();
                                let user_id_clone = *user_id;
                                
                                // Fetch detailed subscription information
                                let fetch_details_task = Task::perform(
                                    async move {
                                        supabase::fetch_latest_user_subscription(&client_clone, user_id_clone).await
                                    },
                                    |result| match result {
                                        Ok(Some(subscription)) => Message::UpdateSubscriptionDetails(
                                            subscription.status,
                                            subscription.payment_processor_subscription_id,
                                            subscription.next_billing_date,
                                        ),
                                        Ok(None) => Message::UpdateSubscriptionDetails(
                                            "No subscription".to_string(),
                                            None,
                                            None,
                                        ),
                                        Err(_) => Message::UpdateSubscriptionDetails(
                                            "Unknown".to_string(),
                                            None,
                                            None,
                                        ),
                                    }
                                );
                                
                                let status_message = if subscription_result.is_pro {
                                    if let Some(copperx_status) = &subscription_result.copperx_status {
                                        format!("Pro subscription active (CopperX: {:?})", copperx_status)
                                    } else {
                                        "Pro subscription active".to_string()
                                    }
                                } else {
                                    match subscription_result.trial_status {
                                        TrialStatus::Active { days_remaining } => {
                                            format!("Trial active: {} days remaining", days_remaining)
                                        }
                                        TrialStatus::NotStarted => "Trial not started".to_string(),
                                        TrialStatus::Expired => "Trial expired".to_string(),
                                    }
                                };
                                
                                // Show different UI if subscription is needed
                                if subscription_result.needs_subscription {
                                    // This will cause the view to show the subscription needed screen
                                    log::info!("User needs subscription - trial expired and no active subscription");
                                }
                        
                                return Task::batch(vec![
                                    fetch_details_task,
                                    Task::done(Message::AddNotification(Toast::info(status_message)))
                                ]);
                            }
                        }
                        
                        let status_message = if subscription_result.is_pro {
                            if let Some(copperx_status) = &subscription_result.copperx_status {
                                format!("Pro subscription active (CopperX: {:?})", copperx_status)
                            } else {
                                "Pro subscription active".to_string()
                            }
                        } else {
                            match subscription_result.trial_status {
                                TrialStatus::Active { days_remaining } => {
                                    format!("Trial active: {} days remaining", days_remaining)
                                }
                                TrialStatus::NotStarted => "Trial not started".to_string(),
                                TrialStatus::Expired => "Trial expired".to_string(),
                            }
                        };
                        
                        // Show different UI if subscription is needed
                        if subscription_result.needs_subscription {
                            // This will cause the view to show the subscription needed screen
                            log::info!("User needs subscription - trial expired and no active subscription");
                        }
                        
                        return Task::done(Message::AddNotification(Toast::info(status_message)));
                    }
                    Err(e) => {
                        log::warn!("Failed to check comprehensive subscription status: {}", e);
                        // Fallback to showing trial not started
                        self.trial_status = Some(TrialStatus::NotStarted);
                        return Task::done(Message::AddNotification(Toast::error(
                            format!("Failed to check subscription status: {}", e)
                        )));
                    }
                }
            }
            Message::Logout => {
                if let Some(client) = &self.supabase_client {
                    let client = client.clone();
                    return Task::perform(
                        async move {
                            match supabase::logout(&client).await {
                                Ok(_) => Ok(()),
                                Err(e) => Err(e.to_string()),
                            }
                        },
                        |result| {
                            match result {
                                Ok(_) => Message::AuthStateChanged(AuthState::Unauthenticated),
                                Err(e) => Message::AddNotification(Toast::error(format!("Logout failed: {}", e))),
                            }
                        }
                    );
                } else {
                    return Task::none();
                }
            }
            Message::FetchUserProfile => {
                if let Some(client) = &self.supabase_client {
                    let client = client.clone();
                    return Task::perform(
                        async move {
                            let name_result = client.get_user_name().await;
                            let email_result = client.get_user_email().await;
                            
                            let name = match name_result {
                                Ok(name) if !name.is_empty() => Some(name),
                                _ => None,
                            };
                            
                            let email = match email_result {
                                Ok(email) if !email.is_empty() => Some(email),
                                _ => None,
                            };
                            
                            (name, email)
                        },
                        |(name, email)| Message::UpdateUserProfile(name, email)
                    );
                } else {
                    return Task::done(Message::AddNotification(Toast::error("Supabase client not initialized".to_string())));
                }
            }
            Message::UpdateUserProfile(name, email) => {
                // Update the local state with user profile data
                self.user_name = name;
                self.user_email = email;
                return Task::none();
            }
            Message::ResetLoginState => {
                self.login_in_progress = false;
                // Don't clear error message here, as we want to display it to the user
                return Task::none();
            }
            Message::SetLoginError(error) => {
                self.login_error_message = Some(error);
                return Task::none();
            }
            Message::UpdateEmailField(value) => {
                self.email_input = value;
                // Clear error message if field is now valid
                if let Some(error) = &self.login_error_message {
                    if error.contains("Email") && !self.email_input.trim().is_empty() && self.email_input.contains('@') {
                        self.login_error_message = None;
                    }
                }
                return Task::none();
            }
            Message::UpdatePasswordField(value) => {
                self.password_input = value;
                // Clear error message if field is now valid
                if let Some(error) = &self.login_error_message {
                    if error.contains("Password") && !self.password_input.trim().is_empty() {
                        self.login_error_message = None;
                    }
                }
                return Task::none();
            }
            Message::CheckForUpdate => {
                if let Some(client) = &self.supabase_client {
                    let client = client.clone();
                    return Task::perform(
                        async move {
                            supabase::check_for_update(&client).await
                        },
                        Message::UpdateCheckComplete
                    );
                }
                return Task::none();
            }
            Message::UpdateCheckComplete(result) => {
                match result {
                    Ok(update_info) => {
                        if update_info.is_update_available {
                            self.update_available = Some(update_info.clone());
                            return Task::done(Message::AddNotification(Toast::info(
                                format!("New version {} available!", update_info.version)
                            )));
                        }
                    }
                    Err(e) => {
                        log::warn!("Failed to check for updates: {}", e);
                    }
                }
                return Task::none();
            }
            Message::DownloadUpdate(download_url) => {
                if let Some(client) = &self.supabase_client {
                    self.update_downloading = true;
                    let client = client.clone();
                    return Task::perform(
                        async move {
                            supabase::download_update(&client, &download_url).await
                        },
                        Message::UpdateDownloaded
                    )
                    .chain(Task::done(Message::AddNotification(Toast::info(
                        "Downloading update...".to_string()
                    ))));
                }
                return Task::none();
            }
            Message::UpdateDownloaded(result) => {
                self.update_downloading = false;
                match result {
                    Ok(path) => {
                        self.update_download_path = Some(path.clone());
                        return Task::done(Message::AddNotification(Toast::info(
                            "Update downloaded. Ready to install.".to_string()
                        )));
                    }
                    Err(e) => {
                        return Task::done(Message::AddNotification(Toast::error(
                            format!("Failed to download update: {}", e)
                        )));
                    }
                }
            }
            Message::ApplyUpdate(path) => {
                // Apply the update and restart the app
                if let Some(client) = &self.supabase_client {
                    match supabase::apply_update(client, &path) {
                        Ok(_) => {
                            // The app will exit as part of apply_update
                            return Task::none();
                        }
                        Err(e) => {
                            return Task::done(Message::AddNotification(Toast::error(
                                format!("Failed to apply update: {}", e)
                            )));
                        }
                    }
                } else {
                    return Task::done(Message::AddNotification(Toast::error(
                        "Cannot apply update: Supabase client not available".to_string()
                    )));
                }
            }
            Message::DismissUpdate => {
                self.update_available = None;
                self.update_download_path = None;
                return Task::none();
            }
            Message::TogglePasswordVisibility => {
                self.password_visible = !self.password_visible;
            }
            Message::UpdateSubscriptionDetails(status, subscription_id, next_billing_date) => {
                self.subscription_status = Some(status);
                self.subscription_id = subscription_id;
                self.next_billing_date = next_billing_date;
                return Task::none();
            }
            Message::CopyToClipboard(text) => {
                // Use iced's built-in clipboard write functionality
                return iced::clipboard::write::<Message>(text.clone())
                    .map(|_| Message::AddNotification(Toast::info("Copied to clipboard".to_string())));
            }
            Message::OpenUrl(url) => {
                // Open URL in default browser
                if let Err(e) = open::that(&url) {
                    return Task::done(Message::AddNotification(Toast::error(
                        format!("Failed to open URL: {}", e)
                    )));
                } else {
                    return Task::done(Message::AddNotification(Toast::info(
                        "Opening in browser...".to_string()
                    )));
                }
            }
            Message::SaveState => {
                // Collect window specifications first, then save
                let main_window = self.main_window.id;
                let dashboard = self.active_dashboard_mut();
                
                let mut opened_windows = dashboard
                    .popout
                    .keys()
                    .copied()
                    .collect::<Vec<window::Id>>();
                
                opened_windows.push(main_window);
                
                return window::collect_window_specs(opened_windows, Message::SaveStateWithSpecs);
            }
            Message::SaveStateWithSpecs(windows) => {
                // Update popout window specs
                self.active_dashboard_mut()
                    .popout
                    .iter_mut()
                    .for_each(|(id, (_, window_spec))| {
                        if let Some(new_window_spec) = windows.get(id) {
                            *window_spec = *new_window_spec;
                        }
                    });

                let mut ser_layouts = vec![];

                for id in &self.layout_manager.layout_order {
                    if let Some((layout, dashboard)) = self.layout_manager.get_layout(*id) {
                        let serialized_dashboard = data::Dashboard::from(dashboard);

                        ser_layouts.push(data::Layout {
                            name: layout.name.clone(),
                            dashboard: serialized_dashboard,
                        });
                    }
                }

                let layouts = data::Layouts {
                    layouts: ser_layouts,
                    active_layout: self.layout_manager.active_layout().name.clone(),
                };

                let main_window = windows
                    .iter()
                    .find(|(id, _)| **id == self.main_window.id)
                    .map(|(_, spec)| *spec);

                let audio_cfg = data::AudioStream::from(&self.audio_stream);

                let layout = data::State::from_parts(
                    layouts,
                    self.theme.clone(),
                    self.theme_editor.custom_theme.clone().map(data::Theme),
                    self.sidebar.favorited_tickers(),
                    main_window,
                    self.timezone,
                    self.sidebar.state,
                    self.scale_factor,
                    audio_cfg,
                );

                match serde_json::to_string(&layout) {
                    Ok(layout_str) => {
                        let file_name = data::SAVED_STATE_PATH;

                        if let Err(e) = data::write_json_to_file(&layout_str, file_name) {
                            log::error!("Failed to write layout state to file: {}", e);
                            return Task::done(Message::AddNotification(Toast::error(format!(
                                "Failed to save layout: {}", e
                            ))));
                        } else {
                            log::info!("Successfully wrote layout state to {file_name}");
                            return Task::done(Message::AddNotification(Toast::success("Layout saved successfully".to_string())));
                        }
                    }
                    Err(e) => {
                        log::error!("Failed to serialize layout: {}", e);
                        return Task::done(Message::AddNotification(Toast::error(format!(
                            "Failed to serialize layout: {}", e
                        ))));
                    }
                }
            }
            Message::KeyboardShortcut(shortcut) => {
                use window::KeyboardShortcut;
                
                match shortcut {
                    KeyboardShortcut::NewLayout => {
                        // Create a new layout
                        return Task::done(Message::Layouts(
                            modal::layout_manager::Message::AddLayout
                        ));
                    }
                    KeyboardShortcut::OpenSettings => {
                        // Open settings sidebar
                        return Task::done(Message::Sidebar(
                            dashboard::sidebar::Message::ToggleSidebarMenu(Some(sidebar::Menu::Settings))
                        ));
                    }
                    KeyboardShortcut::SaveLayout => {
                        // Trigger the actual save functionality
                        return Task::done(Message::SaveState);
                    }
                    KeyboardShortcut::NextLayout => {
                        // Switch to next layout
                        if let Some(next_layout) = self.layout_manager.next_layout() {
                            return Task::done(Message::LoadLayout(next_layout));
                        }
                    }
                    KeyboardShortcut::PreviousLayout => {
                        // Switch to previous layout
                        if let Some(prev_layout) = self.layout_manager.previous_layout() {
                            return Task::done(Message::LoadLayout(prev_layout));
                        }
                    }
                    KeyboardShortcut::ToggleFullscreen => {
                        // Toggle fullscreen mode
                        return iced::window::toggle_maximize(self.main_window.id);
                    }
                    KeyboardShortcut::TogglePaneMaximize => {
                        // Toggle maximize/restore for the currently focused pane
                        let dashboard = self.active_dashboard();
                        if let Some((window_id, pane_id)) = dashboard.focus {
                            // Check if the pane is currently maximized
                            let is_maximized = if window_id == self.main_window.id {
                                dashboard.panes.maximized().is_some()
                            } else {
                                // For popout windows, we don't support maximize/restore
                                false
                            };
                            
                            if window_id == self.main_window.id {
                                let message = if is_maximized {
                                    dashboard::pane::Message::Restore
                                } else {
                                    dashboard::pane::Message::MaximizePane(pane_id)
                                };
                                
                                return Task::done(Message::Dashboard(
                                    None,
                                    dashboard::Message::Pane(window_id, message)
                                ));
                            }
                        } else {
                            // No pane is focused, show a notification
                            return Task::done(Message::AddNotification(
                                Toast::info("No pane selected. Click on a pane first.".to_string())
                            ));
                        }
                    }
                    KeyboardShortcut::CenterLatest => {
                        // Apply CenterLatest autoscale to the focused chart pane
                        let dashboard = self.active_dashboard();
                        if let Some((window_id, pane_id)) = dashboard.focus {
                            return Task::done(Message::Dashboard(
                                None,
                                dashboard::Message::Pane(
                                    window_id,
                                    dashboard::pane::Message::ChartInteraction(
                                        pane_id,
                                        crate::chart::Message::SetAutoscale(data::chart::Autoscale::CenterLatest)
                                    )
                                )
                            ));
                        } else {
                            return Task::done(Message::AddNotification(
                                Toast::info("No pane selected. Click on a chart pane first.".to_string())
                            ));
                        }
                    }
                    KeyboardShortcut::FitToVisible => {
                        // Apply FitToVisible autoscale to the focused chart pane
                        let dashboard = self.active_dashboard();
                        if let Some((window_id, pane_id)) = dashboard.focus {
                            return Task::done(Message::Dashboard(
                                None,
                                dashboard::Message::Pane(
                                    window_id,
                                    dashboard::pane::Message::ChartInteraction(
                                        pane_id,
                                        crate::chart::Message::SetAutoscale(data::chart::Autoscale::FitToVisible)
                                    )
                                )
                            ));
                        } else {
                            return Task::done(Message::AddNotification(
                                Toast::info("No pane selected. Click on a chart pane first.".to_string())
                            ));
                        }
                    }
                    KeyboardShortcut::ZoomIn => {
                        // Increase scale factor
                        let current_value: f64 = self.scale_factor.into();
                        if current_value < data::config::MAX_SCALE {
                            return Task::done(Message::ScaleFactorChanged((current_value + 0.1).into()));
                        }
                    }
                    KeyboardShortcut::ZoomOut => {
                        // Decrease scale factor
                        let current_value: f64 = self.scale_factor.into();
                        if current_value > data::config::MIN_SCALE {
                            return Task::done(Message::ScaleFactorChanged((current_value - 0.1).into()));
                        }
                    }
                    KeyboardShortcut::ResetZoom => {
                        // Reset scale factor to 1.0
                        return Task::done(Message::ScaleFactorChanged(1.0.into()));
                    }
                    KeyboardShortcut::SplitPane => {
                        // Split the currently focused pane horizontally
                        let dashboard = self.active_dashboard();
                        if let Some((window_id, pane_id)) = dashboard.focus {
                            return Task::done(Message::Dashboard(
                                None,
                                dashboard::Message::Pane(
                                    window_id,
                                    dashboard::pane::Message::SplitPane(
                                        pane_grid::Axis::Horizontal,
                                        pane_id,
                                    )
                                )
                            ));
                        } else {
                            return Task::done(Message::AddNotification(
                                Toast::info("No pane selected. Click on a pane first.".to_string())
                            ));
                        }
                    }
                    KeyboardShortcut::ClonePane => {
                        // Clone the currently focused pane and split horizontally
                        let dashboard = self.active_dashboard();
                        if let Some((window_id, pane_id)) = dashboard.focus {
                            return Task::done(Message::Dashboard(
                                None,
                                dashboard::Message::Pane(
                                    window_id,
                                    dashboard::pane::Message::ClonePane(pane_id),
                                ),
                            ));
                        } else {
                            return Task::done(Message::AddNotification(
                                Toast::info("No pane selected. Click on a pane first.".to_string()),
                            ));
                        }
                    }
                    KeyboardShortcut::ResetPane => {
                        // Reset the currently focused pane
                        let dashboard = self.active_dashboard();
                        if let Some((window_id, pane_id)) = dashboard.focus {
                            return Task::done(Message::Dashboard(
                                None,
                                dashboard::Message::Pane(
                                    window_id,
                                    dashboard::pane::Message::ReplacePane(pane_id)
                                )
                            ));
                        } else {
                            return Task::done(Message::AddNotification(
                                Toast::info("No pane selected. Click on a pane first.".to_string())
                            ));
                        }
                    }

                    KeyboardShortcut::Quit => {
                        // Quit the application
                        let main_window = self.main_window.id;
                        let dashboard = self.active_dashboard_mut();
                        
                        let mut opened_windows = dashboard
                            .popout
                            .keys()
                            .copied()
                            .collect::<Vec<window::Id>>();
                        
                        opened_windows.push(main_window);
                        
                        return window::collect_window_specs(opened_windows, Message::ExitRequested);
                    }
                    KeyboardShortcut::ToggleTheme => {
                        // Cycle through all available themes (including custom ones)
                        let mut themes: Vec<iced::Theme> = iced_core::Theme::ALL.to_vec();

                        let default_theme = iced_core::Theme::Custom(default_theme().into());
                        themes.push(default_theme);

                        let redshift_theme = iced_core::Theme::Custom(redshift_theme().into());
                        themes.push(redshift_theme);

                        if let Some(custom_theme) = &self.theme_editor.custom_theme {
                            themes.push(custom_theme.clone());
                        }

                        let current_theme = &self.theme.0;
                        if let Some((idx, _)) = themes.iter().enumerate().find(|(_, t)| t == &current_theme) {
                            let next_idx = (idx + 1) % themes.len();
                            let next_theme = data::Theme(themes[next_idx].clone());
                            return Task::done(Message::ThemeSelected(next_theme));
                        } else if let Some(first) = themes.first().cloned() {
                            return Task::done(Message::ThemeSelected(data::Theme(first)));
                        }
                    }
                    KeyboardShortcut::OpenDataFolder => {
                        // Open data folder
                        return Task::done(Message::DataFolderRequested);
                    }
                }
            }
            Message::DismissGeoblockWarning => {
                self.show_geoblock_warning = false;
                self.geoblock_error_count = 0;
                return Task::none();
            }
        }
        Task::none()
    }

    fn view(&self, id: window::Id) -> Element<'_, Message> {
        // Show loading screen during initialization
        if self.is_initializing {
            return self.view_loading_screen();
        }
        
        // Show login view if needed
        if self.show_login_view || matches!(self.auth_state, AuthState::Unauthenticated) {
            return self.view_login_screen();
        }
        
        // Show geoblocking warning if detected
        if self.show_geoblock_warning {
            return self.view_geoblock_warning();
        }
        
        // If there's an update available and we should show the dialog (only for logged in users)
        if self.update_available.is_some() && !self.update_downloading {
            return self.view_update_modal();
        }
        
        // Check for pending or past due subscription status
        if let Some(copperx_status) = &self.copperx_status {
            if matches!(copperx_status, supabase::CopperXSubscriptionStatus::Pending | supabase::CopperXSubscriptionStatus::PastDue | supabase::CopperXSubscriptionStatus::Unpaid | supabase::CopperXSubscriptionStatus::Canceled) {
                return self.view_subscription_pending();
            }
        }
        
        // Check if trial expired or subscription needed
        if !self.is_pro {
            if let Some(TrialStatus::Expired) = self.trial_status {
                return self.view_subscription_needed();
            }
        }
        
        let dashboard = self.active_dashboard();
        let sidebar_pos = self.sidebar.position();

        let content = if id == self.main_window.id {
            let sidebar_view = self
                .sidebar
                .view(self.audio_stream.volume())
                .map(Message::Sidebar);

            let dashboard_view = dashboard
                .view(&self.main_window, self.timezone)
                .map(move |msg| Message::Dashboard(None, msg));

            let header_title = {
                #[cfg(target_os = "macos")]
                {
                    iced::widget::center(
                        text("AIRTERMINAL")
                            .font(iced::Font {
                                weight: iced::font::Weight::Bold,
                                ..Default::default()
                            })
                            .size(16)
                            .style(style::title_text),
                    )
                    .height(20)
                    .align_y(Alignment::Center)
                    .padding(padding::top(4))
                }
                #[cfg(not(target_os = "macos"))]
                {
                    column![]
                }
            };

            let base = column![
                header_title,
                match sidebar_pos {
                    sidebar::Position::Left => row![sidebar_view, dashboard_view,],
                    sidebar::Position::Right => row![dashboard_view, sidebar_view],
                }
                .spacing(4)
                .padding(8),
            ];

            if let Some(menu) = self.sidebar.active_menu() {
                self.view_with_modal(base.into(), dashboard, menu)
            } else {
                base.into()
            }
        } else {
            container(
                dashboard
                    .view_window(id, &self.main_window, self.timezone)
                    .map(move |msg| Message::Dashboard(None, msg)),
            )
            .padding(padding::top(style::TITLE_PADDING_TOP))
            .into()
        };

        toast::Manager::new(
            content,
            &self.notifications,
            match sidebar_pos {
                sidebar::Position::Left => Alignment::End,
                sidebar::Position::Right => Alignment::Start,
            },
            Message::DeleteNotification,
        )
        .into()
    }

    fn theme(&self, _window: window::Id) -> iced_core::Theme {
        self.theme.clone().into()
    }

    fn title(&self, _window: window::Id) -> String {
        format!("Airterminal [{}]", self.layout_manager.active_layout().name)
    }

    fn scale_factor(&self, _window: window::Id) -> f64 {
        self.scale_factor.into()
    }

    fn subscription(&self) -> Subscription<Message> {
        let window_events = window::events().map(Message::WindowEvent);
        let sidebar = self.sidebar.subscription().map(Message::Sidebar);

        let exchange_streams = self
            .active_dashboard()
            .market_subscriptions()
            .map(Message::MarketWsEvent);

        let tick = iced::time::every(Duration::from_millis(100)).map(Message::Tick);

        // Global keyboard handler for "/" key and Backspace key
        let global_keyboard = iced::event::listen_with(|event, _status, _window| {
            match event {
                iced::Event::Keyboard(iced::keyboard::Event::KeyPressed { key, modifiers, .. }) => {
                    match &key {
                        iced::keyboard::Key::Character(c) => {
                            if c.as_str() == "/" && !modifiers.command() && !modifiers.shift() && !modifiers.alt() && !modifiers.control() {
                                // Always handle "/" key regardless of which widget has focus
                                return Some(Message::Sidebar(
                                    dashboard::sidebar::Message::TickersTable(
                                        dashboard::tickers_table::Message::ToggleTable
                                    )
                                ));
                            }
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Backspace) => {
                            // Handle Backspace key globally for tickers table
                            return Some(Message::Sidebar(
                                dashboard::sidebar::Message::TickersTable(
                                    dashboard::tickers_table::Message::KeyboardNavigate(key, modifiers)
                                )
                            ));
                        }
                        iced::keyboard::Key::Named(iced::keyboard::key::Named::Escape) => {
                            // Handle Escape key globally for tickers table
                            return Some(Message::Sidebar(
                                dashboard::sidebar::Message::TickersTable(
                                    dashboard::tickers_table::Message::KeyboardNavigate(key, modifiers)
                                )
                            ));
                        }
                        _ => {}
                    }
                    None
                }
                _ => None,
            }
        });

        // Put global keyboard first for higher priority
        Subscription::batch(vec![global_keyboard, exchange_streams, sidebar, window_events, tick])
    }

    fn active_dashboard(&self) -> &Dashboard {
        self.layout_manager
            .active_dashboard()
            .expect("No active dashboard")
    }

    fn active_dashboard_mut(&mut self) -> &mut Dashboard {
        self.layout_manager
            .active_dashboard_mut()
            .expect("No active dashboard")
    }

    fn view_with_modal<'a>(
        &'a self,
        base: Element<'a, Message>,
        dashboard: &'a Dashboard,
        menu: sidebar::Menu,
    ) -> Element<'a, Message> {
        let sidebar_pos = self.sidebar.position();

        match menu {
            sidebar::Menu::Settings => {
                let settings_modal = {
                    let theme_picklist = {
                        let mut themes: Vec<iced::Theme> = iced_core::Theme::ALL.to_vec();

                        let default_theme = iced_core::Theme::Custom(default_theme().into());
                        themes.push(default_theme);

                        let redshift_theme = iced_core::Theme::Custom(redshift_theme().into());
                        themes.push(redshift_theme);

                        if let Some(custom_theme) = &self.theme_editor.custom_theme {
                            themes.push(custom_theme.clone());
                        }

                        pick_list(themes, Some(self.theme.0.clone()), |theme| {
                            Message::ThemeSelected(data::Theme(theme))
                        })
                    };

                    let toggle_theme_editor = button(text("Theme editor")).on_press(
                        Message::Sidebar(dashboard::sidebar::Message::ToggleSidebarMenu(Some(
                            sidebar::Menu::ThemeEditor,
                        ))),
                    );

                    let timezone_picklist = pick_list(
                        [data::UserTimezone::Utc, data::UserTimezone::Local],
                        Some(self.timezone),
                        Message::SetTimezone,
                    );

                    let sidebar_pos = pick_list(
                        [sidebar::Position::Left, sidebar::Position::Right],
                        Some(sidebar_pos),
                        |pos| {
                            Message::Sidebar(dashboard::sidebar::Message::SetSidebarPosition(pos))
                        },
                    );

                    let scale_factor = {
                        let current_value: f64 = self.scale_factor.into();

                        let decrease_btn = if current_value > data::config::MIN_SCALE {
                            button(text("-"))
                                .on_press(Message::ScaleFactorChanged((current_value - 0.1).into()))
                        } else {
                            button(text("-"))
                        };

                        let increase_btn = if current_value < data::config::MAX_SCALE {
                            button(text("+"))
                                .on_press(Message::ScaleFactorChanged((current_value + 0.1).into()))
                        } else {
                            button(text("+"))
                        };

                        container(
                            row![
                                decrease_btn,
                                text(format!("{:.0}%", current_value * 100.0)).size(14),
                                increase_btn,
                            ]
                            .align_y(Alignment::Center)
                            .spacing(8)
                            .padding(4),
                        )
                        .style(style::modal_container)
                    };

                    let trade_fetch_checkbox = {
                        let is_active = exchange::fetcher::is_trade_fetch_enabled();

                        let checkbox = iced::widget::checkbox("Fetch trades (Binance)", is_active)
                            .on_toggle(|checked| {
                                if checked {
                                    Message::ToggleDialogModal(Some((
                                        "This might be unreliable and take some time to complete"
                                            .to_string(),
                                        Box::new(Message::ToggleTradeFetch(true)),
                                    )))
                                } else {
                                    Message::ToggleTradeFetch(false)
                                }
                            });

                        tooltip(
                            checkbox,
                            Some("Try to fetch trades for footprint charts"),
                            TooltipPosition::Top,
                        )
                    };

                    let open_data_folder = {
                        let button =
                            button(text("Open data folder")).on_press(Message::DataFolderRequested);

                        tooltip(
                            button,
                            Some("Open the folder where the data & config is stored"),
                            TooltipPosition::Top,
                        )
                    };

                    container(
                        column![
                            column![open_data_folder,].spacing(8),
                            column![text("Sidebar position").size(14), sidebar_pos,].spacing(8),
                            column![text("Time zone").size(14), timezone_picklist,].spacing(8),
                            column![text("Theme").size(14), theme_picklist,].spacing(8),
                            column![text("Interface scale").size(14), scale_factor,].spacing(8),
                            column![
                                text("Experimental").size(14),
                                trade_fetch_checkbox,
                                toggle_theme_editor
                            ]
                            .spacing(8),
                        ]
                        .spacing(20),
                    )
                    .align_x(Alignment::Start)
                    .max_width(400)
                    .padding(24)
                    .style(style::dashboard_modal)
                };

                let (align_x, padding) = match sidebar_pos {
                    sidebar::Position::Left => (Alignment::Start, padding::left(44).bottom(4)),
                    sidebar::Position::Right => (Alignment::End, padding::right(44).bottom(4)),
                };

                let base_content = dashboard_modal(
                    base,
                    settings_modal,
                    Message::Sidebar(dashboard::sidebar::Message::ToggleSidebarMenu(None)),
                    padding,
                    Alignment::End,
                    align_x,
                );

                if let Some((dialog, on_confirm)) = &self.confirm_dialog {
                    let dialog_content = confirm_dialog_container(
                        dialog,
                        *on_confirm.to_owned(),
                        Message::ToggleDialogModal(None),
                    );

                    main_dialog_modal(
                        base_content,
                        dialog_content,
                        Message::ToggleDialogModal(None),
                    )
                } else {
                    base_content
                }
            }
            sidebar::Menu::Layout => {
                let main_window = self.main_window.id;

                let pane = if let Some(focus) = dashboard.focus {
                    focus.1
                } else {
                    *dashboard.panes.iter().next().unwrap().0
                };

                let reset_pane_button = tooltip(
                    button(text("Reset").align_x(Alignment::Center))
                        .width(iced::Length::Fill)
                        .on_press(Message::Dashboard(
                            None,
                            dashboard::Message::Pane(
                                main_window,
                                dashboard::pane::Message::ReplacePane(pane),
                            ),
                        )),
                    Some("Reset selected pane [cmd + R]"),
                    TooltipPosition::Top,
                );
                let split_pane_button = tooltip(
                    button(text("Split").align_x(Alignment::Center))
                        .width(iced::Length::Fill)
                        .on_press(Message::Dashboard(
                            None,
                            dashboard::Message::Pane(
                                main_window,
                                dashboard::pane::Message::SplitPane(
                                    pane_grid::Axis::Horizontal,
                                    pane,
                                ),
                            ),
                        )),
                    Some("Split selected pane [cmd + enter]"),
                    TooltipPosition::Top,
                );

                let manage_layout_modal = {
                    container(
                        column![
                            column![
                                text("Panes").size(14),
                                if dashboard.focus.is_some() {
                                    {
                        let clone_pane_button = tooltip(
                                            button(text("Clone").align_x(Alignment::Center))
                                                .width(iced::Length::Fill)
                                                .on_press(Message::Dashboard(
                                                    None,
                                                    dashboard::Message::Pane(
                                                        main_window,
                                                        dashboard::pane::Message::ClonePane(pane),
                                                    ),
                                                )),
                            Some("Clone selected pane [cmd + shift + enter]"),
                                            TooltipPosition::Top,
                                        );
                                        row![reset_pane_button, split_pane_button, clone_pane_button]
                                            .padding(padding::left(8).right(8))
                                            .spacing(8)
                                    }
                                } else {
                                    row![text("No pane selected"),]
                                },
                            ]
                            .align_x(Alignment::Center)
                            .spacing(8),
                            iced::widget::horizontal_rule(1.0).style(style::split_ruler),
                            self.layout_manager.view().map(Message::Layouts),
                        ]
                        .align_x(Alignment::Center)
                        .spacing(20),
                    )
                    .width(280)
                    .padding(24)
                    .style(style::dashboard_modal)
                };

                let (align_x, padding) = match sidebar_pos {
                    sidebar::Position::Left => (Alignment::Start, padding::left(44).top(40)),
                    sidebar::Position::Right => (Alignment::End, padding::right(44).top(40)),
                };

                dashboard_modal(
                    base,
                    manage_layout_modal,
                    Message::Sidebar(dashboard::sidebar::Message::ToggleSidebarMenu(None)),
                    padding,
                    Alignment::Start,
                    align_x,
                )
            }
            sidebar::Menu::Audio => {
                let (align_x, padding) = match sidebar_pos {
                    sidebar::Position::Left => (Alignment::Start, padding::left(44).top(76)),
                    sidebar::Position::Right => (Alignment::End, padding::right(44).top(76)),
                };

                let depth_streams_list = dashboard.streams.depth_streams(None);

                dashboard_modal(
                    base,
                    self.audio_stream
                        .view(depth_streams_list)
                        .map(Message::AudioStream),
                    Message::Sidebar(dashboard::sidebar::Message::ToggleSidebarMenu(None)),
                    padding,
                    Alignment::Start,
                    align_x,
                )
            }
            sidebar::Menu::ThemeEditor => {
                let (align_x, padding) = match sidebar_pos {
                    sidebar::Position::Left => (Alignment::Start, padding::left(44).bottom(4)),
                    sidebar::Position::Right => (Alignment::End, padding::right(44).bottom(4)),
                };

                dashboard_modal(
                    base,
                    self.theme_editor
                        .view(&self.theme.0)
                        .map(Message::ThemeEditor),
                    Message::Sidebar(dashboard::sidebar::Message::ToggleSidebarMenu(None)),
                    padding,
                    Alignment::End,
                    align_x,
                )
            }
            sidebar::Menu::Account => {
                let (align_x, padding) = match sidebar_pos {
                    sidebar::Position::Left => (Alignment::Start, padding::left(44).bottom(4)),
                    sidebar::Position::Right => (Alignment::End, padding::right(44).bottom(4)),
                };

                dashboard_modal(
                    base,
                    self.view_account_modal(),
                    Message::Sidebar(dashboard::sidebar::Message::ToggleSidebarMenu(None)),
                    padding,
                    Alignment::End,
                    align_x,
                )
            }
        }
    }

    fn view_loading_screen(&self) -> Element<'_, Message> {
        // Logo/branding at the top
        let logo = text("AIRTERMINAL")
            .size(32)
            .font(iced::Font {
                weight: iced::font::Weight::Bold,
                ..Default::default()
            })
            .style(style::title_text)
            .align_x(Alignment::Center);
            
        let loading_text = text("Loading...")
            .size(16)
            .align_x(Alignment::Center);
            
        let loading_content = column![
            logo,
            iced::widget::Space::with_height(20),
            loading_text,
        ]
        .spacing(10)
        .max_width(400)
        .align_x(Alignment::Center);
        
        // Create a centered container for the loading screen
        let loading_container = container(loading_content)
            .padding(30)
            .width(iced::Length::Shrink)
            .style(style::login_container);
        
        // Use iced's center function which automatically centers horizontally and vertically
        iced::widget::center(loading_container).into()
    }

    fn view_login_screen(&self) -> Element<'_, Message> {
        // Logo/branding at the top
        let logo = text("AIRTERMINAL")
            .size(32)
            .font(iced::Font {
                weight: iced::font::Weight::Bold,
                ..Default::default()
            })
            .style(style::title_text)
            .align_x(Alignment::Center);
            
        let title = text("Welcome Back")
            .size(28)
            .align_x(Alignment::Center);
            
        let subtitle = text("Please log in to your account")
            .size(16)
            .align_x(Alignment::Center);
        
        // Email input with icon and container
        let email_input = container(
            row![
                style::icon_text(Icon::Envelope, 14)
                    .width(24)
                    .align_x(Alignment::Center),
                iced::widget::text_input("Email", &self.email_input)
                    .on_input(Message::UpdateEmailField)
                    .padding(12)
                    .width(iced::Length::Fill)
            ]
            .spacing(8)
            .align_y(Alignment::Center)
        )
        .padding(5)
        .style(style::input_container);
            
        // Password input with icon and container
        let password_input = container(
            row![
                style::icon_text(Icon::Lock, 14)
                    .width(24)
                    .align_x(Alignment::Center),
                iced::widget::text_input("Password", &self.password_input)
                    .on_input(Message::UpdatePasswordField)
                    .secure(!self.password_visible)
                    .padding(12)
                    .width(iced::Length::Fill),
                button(
                    style::icon_text(
                        if self.password_visible { Icon::EyeSlash } else { Icon::Eye },
                        14
                    )
                )
                .on_press(Message::TogglePasswordVisibility)
                .padding(12)
                .style(|theme, status| style::button::transparent(theme, status, false))
            ]
            .spacing(8)
            .align_y(Alignment::Center)
        )
        .padding(5)
        .style(style::input_container);
        
        // Reset password link
        let reset_password_link = button(
            text("Reset password?")
                .size(14)
                .style(|theme: &iced::Theme| {
                    let palette = theme.extended_palette();
                    iced::widget::text::Style {
                        color: Some(palette.primary.base.color),
                    }
                })
        )
        .on_press(Message::OpenUrl("https://www.airterminal.xyz/auth/forgot-password".to_string()))
        .style(|theme, status| style::button::transparent(theme, status, false))
        .padding(0);
            
        // Error message (if any)
        let error_message = if let Some(error) = &self.login_error_message {
            container(
                row![
                    style::icon_text(Icon::Close, 14)
                        .width(16)
                        .align_x(Alignment::Center),
                    text(error)
                        .size(14)
                        .style(|theme: &iced::Theme| {
                            let palette = theme.extended_palette();
                            iced::widget::text::Style {
                                color: Some(palette.danger.base.color),
                            }
                        })
                ]
                .spacing(8)
                .align_y(Alignment::Center)
            )
            .width(iced::Length::Fill)
            .padding(8)
            .style(style::error_container)
        } else if let AuthState::Error(error) = &self.auth_state {
            container(
                row![
                    style::icon_text(Icon::Close, 14)
                        .width(16)
                        .align_x(Alignment::Center),
                    text(error)
                        .size(14)
                        .style(|theme: &iced::Theme| {
                            let palette = theme.extended_palette();
                            iced::widget::text::Style {
                                color: Some(palette.danger.base.color),
                            }
                        })
                ]
                .spacing(8)
                .align_y(Alignment::Center)
            )
            .width(iced::Length::Fill)
            .padding(8)
            .style(style::error_container)
        } else {
            container(text("")).width(iced::Length::Fill)
        };
        
        // Email login button with better styling
        let email_login_button = {
            let button_content = if self.login_in_progress {
                row![
                    style::icon_text(Icon::Loader, 14)
                        .width(16)
                        .align_x(Alignment::Center),
                    text("Logging in...")
                        .size(16)
                        .width(iced::Length::Fill)
                        .align_x(Alignment::Center)
                ]
                .spacing(10)
                .align_y(Alignment::Center)
            } else {
                row![
                    text("Login with Email")
                        .size(16)
                        .width(iced::Length::Fill)
                        .align_x(Alignment::Center)
                ]
                .align_y(Alignment::Center)
            };
            
            if !self.login_in_progress {
                button(button_content)
                    .padding(16)
                    .width(iced::Length::Fill)
                    .on_press(Message::LoginWithEmail(
                        self.email_input.clone(), 
                        self.password_input.clone()
                    ))
                    .style(style::primary_button)
            } else {
                button(button_content)
                    .padding(16)
                    .width(iced::Length::Fill)
                    .style(|_theme, _status| style::disabled_button(_theme))
            }
        };
        
        // Main login form content
        let login_form = column![
            logo,
            iced::widget::Space::with_height(20),
            title,
            iced::widget::Space::with_height(10),
            subtitle,
            iced::widget::Space::with_height(40),
            email_input,
            iced::widget::Space::with_height(15),
            password_input,
            iced::widget::Space::with_height(8),
            container(reset_password_link)
                .width(iced::Length::Fill)
                .align_x(Alignment::End),
            iced::widget::Space::with_height(15),
            error_message,
            iced::widget::Space::with_height(25),
            email_login_button,
        ]
        .spacing(5)
        .max_width(400)
        .align_x(Alignment::Center);
        
        // Create a centered container for the login form
        let login_container = container(login_form)
            .padding(30)
            .width(iced::Length::Shrink)
            .style(style::login_container);
        
        // Use iced's center function which automatically centers horizontally and vertically
        iced::widget::center(login_container).into()
    }

    fn view_update_modal(&self) -> Element<'_, Message> {
        if let Some(update_info) = &self.update_available {
            let title = text(format!("Update Available: v{}", update_info.version))
                .size(18)
                .font(iced::Font {
                    weight: iced::font::Weight::Bold,
                    ..Default::default()
                })
                .align_x(Alignment::Center);
                
            let release_notes = text(&update_info.release_notes)
                .size(14)
                .align_x(Alignment::Center);
                
            let button_row = if let Some(path) = &self.update_download_path {
                // Downloaded and ready to install
                if update_info.update_type == supabase::UpdateType::Force {
                    row![
                        button("Install Now")
                            .on_press(Message::ApplyUpdate(path.clone()))
                            .padding(10)
                            .style(style::primary_button),
                    ]
                } else {
                    row![
                        button("Install Now")
                            .on_press(Message::ApplyUpdate(path.clone()))
                            .padding(10)
                            .style(style::primary_button),
                        button("Later")
                            .on_press(Message::DismissUpdate)
                            .padding(10)
                            .style(|theme, status| style::button::transparent(theme, status, false)),
                    ]
                }
            } else if self.update_downloading {
                // Currently downloading
                row![
                    text("Downloading...").size(14),
                ]
            } else {
                // Available but not yet downloaded
                if update_info.update_type == supabase::UpdateType::Force {
                    row![
                        button("Download")
                            .on_press(Message::DownloadUpdate(update_info.download_url.clone()))
                            .padding(10)
                            .style(style::primary_button),
                    ]
                } else {
                    row![
                        button("Download")
                            .on_press(Message::DownloadUpdate(update_info.download_url.clone()))
                            .padding(10)
                            .style(style::primary_button),
                        button("Later")
                            .on_press(Message::DismissUpdate)
                            .padding(10)
                            .style(|theme, status| style::button::transparent(theme, status, false)),
                    ]
                }
            };
            
            let update_content = column![
                title,
                iced::widget::Space::with_height(10),
                release_notes,
                iced::widget::Space::with_height(20),
                button_row.spacing(10),
            ]
            .spacing(5)
            .max_width(400)
            .align_x(Alignment::Center);
            
            // Create a centered container for the update form
            let update_container = container(update_content)
                .padding(20)
                .width(iced::Length::Shrink)
                .style(style::dashboard_modal);
            
            // Use iced's center function which automatically centers horizontally and vertically
            iced::widget::center(update_container).into()
        } else {
            // No update available, return empty element
            iced::widget::Space::new(iced::Length::Fill, iced::Length::Fill).into()
        }
    }

    fn view_subscription_pending(&self) -> Element<'_, Message> {
        let status_text = match &self.copperx_status {
            Some(supabase::CopperXSubscriptionStatus::Pending) => "Subscription Pending",
            Some(supabase::CopperXSubscriptionStatus::PastDue) => "Payment Past Due",
            Some(supabase::CopperXSubscriptionStatus::Unpaid) => "Payment Required",
            Some(supabase::CopperXSubscriptionStatus::Canceled) => "Subscription Canceled",
            _ => "Subscription Issue", // fallback, shouldn't happen
        };
        
        let status_description = match &self.copperx_status {
            Some(supabase::CopperXSubscriptionStatus::Pending) => 
                "Your subscription is pending payment. Please complete your payment to continue using AirTerminal.",
            Some(supabase::CopperXSubscriptionStatus::PastDue) => 
                "Your subscription payment is past due. Please update your payment method to continue using AirTerminal.",
            Some(supabase::CopperXSubscriptionStatus::Unpaid) => 
                "Your subscription is unpaid. Please make a payment to continue using AirTerminal.",
            Some(supabase::CopperXSubscriptionStatus::Canceled) => 
                "Your subscription has been canceled. Please reactivate your subscription to continue using AirTerminal.",
            _ => "There is an issue with your subscription. Please check your account to continue using AirTerminal.",
        };

        let title = text(status_text)
            .size(28)
            .align_x(Alignment::Center)
            .style(|theme: &iced::Theme| {
                let palette = theme.extended_palette();
                iced::widget::text::Style {
                    color: Some(palette.danger.base.color),
                }
            });
            
        let subtitle = text(status_description)
            .size(16)
            .align_x(Alignment::Center);
            
        let account_button = button(
            row![
                style::icon_text(Icon::Link, 14),
                text("Go to Account Page").size(16)
            ]
            .spacing(8)
            .align_y(Alignment::Center)
        )
        .padding(12)
        .on_press(Message::OpenUrl("https://www.airterminal.xyz/account".to_string()))
        .style(style::primary_button);
            
        let logout_button = button(
            row![
                style::icon_text(Icon::Close, 12),
                text("Logout").size(16)
            ]
            .spacing(4)
            .align_y(Alignment::Center)
        )
        .padding(10)
        .on_press(Message::Logout)
        .style(|theme, status| style::button::transparent(theme, status, false));
            
        iced::widget::center(
            column![
                title,
                iced::widget::Space::with_height(20),
                subtitle,
                iced::widget::Space::with_height(30),
                account_button,
                iced::widget::Space::with_height(15),
                logout_button,
            ]
            .spacing(10)
            .max_width(450)
            .align_x(Alignment::Center)
        )
        .into()
    }

    fn view_subscription_needed(&self) -> Element<'_, Message> {
        let title = text("Subscription Required")
            .size(28)
            .align_x(Alignment::Center);
            
        let subtitle = text("Your trial has expired. Please subscribe to continue using AirTerminal.")
            .size(16)
            .align_x(Alignment::Center);
            
        let subscription_button = button(
            row![
                style::icon_text(Icon::Star, 14),
                text("Subscribe Now").size(16)
            ]
            .spacing(6)
            .align_y(Alignment::Center)
        )
        .padding(10)
        .on_press(Message::OpenUrl("https://www.airterminal.xyz/pricing".to_string()))
        .style(style::primary_button);
            
        let logout_button = button(
            row![
                style::icon_text(Icon::Close, 12),
                text("Logout").size(16)
            ]
            .spacing(4)
            .align_y(Alignment::Center)
        )
        .padding(10)
        .on_press(Message::Logout)
        .style(|theme, status| style::button::transparent(theme, status, false));
            
        iced::widget::center(
            column![
                title,
                iced::widget::Space::with_height(20),
                subtitle,
                iced::widget::Space::with_height(30),
                subscription_button,
                iced::widget::Space::with_height(10),
                logout_button,
            ]
            .spacing(10)
            .max_width(400)
            .align_x(Alignment::Center)
        )
        .into()
    }

    fn view_geoblock_warning(&self) -> Element<'_, Message> {
        let title = text("Geographic Restriction Detected")
            .size(28)
            .align_x(Alignment::Center)
            .style(|theme: &iced::Theme| {
                let palette = theme.extended_palette();
                iced::widget::text::Style {
                    color: Some(palette.danger.base.color),
                }
            });
            
        let subtitle = text("Your requests are being blocked by the exchange APIs")
            .size(16)
            .align_x(Alignment::Center);
            
        let explanation = text(
            "AirTerminal has detected multiple failed requests that suggest your location may be \
            geographically restricted by cryptocurrency exchanges like Binance and Bybit. \
            These exchanges often block access from certain countries or IP addresses."
        )
        .size(14)
        .align_x(Alignment::Center);
        
        let solution_title = text("Recommended Solutions:")
            .size(16)
            .font(iced::Font {
                weight: iced::font::Weight::Bold,
                ..Default::default()
            })
            .align_x(Alignment::Start);
            
        let solution_points = column![
            row![
                text("•").size(14).width(20),
                text("Use a VPN service to connect through an unrestricted location").size(14)
            ].spacing(4),
            row![
                text("•").size(14).width(20),
                text("Try connecting from a different country (Asia, EU regions often work)").size(14)
            ].spacing(4),
            row![
                text("•").size(14).width(20),
                text("Check if your current IP is in a restricted region").size(14)
            ].spacing(4),
            row![
                text("•").size(14).width(20),
                text("Contact your network administrator if using corporate internet").size(14)
            ].spacing(4),
        ]
        .spacing(8);
        
        let technical_info = text(
            "Note: This is not a bug or technical issue. Exchanges implement rate limiting and geographic restrictions based on local regulations. Using a VPN to access services that comply with your local laws is generally acceptable."
        )
        .size(12)
        .align_x(Alignment::Center)
        .style(|theme: &iced::Theme| {
            let palette = theme.extended_palette();
            iced::widget::text::Style {
                color: Some(palette.background.weak.text),
            }
        });
        
        let support_button = button(
            row![
                style::icon_text(Icon::Link, 14),
                text("Get Support").size(14)
            ]
            .spacing(6)
            .align_y(Alignment::Center)
        )
        .padding(10)
        .on_press(Message::OpenUrl("https://t.me/Airterminalxyz".to_string()))
        .style(style::primary_button);
        
        let dismiss_button = button(
            row![
                style::icon_text(Icon::Eye, 12),
                text("Continue Anyway").size(14)
            ]
            .spacing(4)
            .align_y(Alignment::Center)
        )
        .padding(8)
        .on_press(Message::DismissGeoblockWarning)
        .style(|theme, status| style::button::transparent(theme, status, false));
        
        let content = column![
            title,
            iced::widget::Space::with_height(20),
            subtitle,
            iced::widget::Space::with_height(20),
            explanation,
            iced::widget::Space::with_height(25),
            solution_title,
            iced::widget::Space::with_height(10),
            solution_points,
            iced::widget::Space::with_height(20),
            technical_info,
            iced::widget::Space::with_height(30),
            row![
                support_button,
                iced::widget::Space::with_width(10),
                dismiss_button,
            ]
            .align_y(Alignment::Center),
        ]
        .spacing(5)
        .max_width(500)
        .align_x(Alignment::Center);
        
        // Create a centered container for the warning
        let warning_container = container(content)
            .padding(30)
            .width(iced::Length::Shrink)
            .style(style::dashboard_modal);
        
        // Use iced's center function which automatically centers horizontally and vertically
        iced::widget::center(warning_container).into()
    }

    fn view_account_modal(&self) -> Element<'_, Message> {
        match &self.auth_state {
            AuthState::Authenticated(user_id) => {
                // User header section
                let user_id_string = user_id.to_string();
                let formatted_id = format!("{}...{}", &user_id_string[..8], &user_id_string[user_id_string.len()-8..]);
                
                let user_header = column![
            row![
                        style::icon_text(Icon::User, 16)
                            .width(20)
                            .align_x(Alignment::Center),
                text("Account Information")
                            .size(16)
                    .font(iced::Font {
                        weight: iced::font::Weight::Bold,
                        ..Default::default()
                    })
            ]
            .spacing(8)
                    .align_y(Alignment::Center),
                    iced::widget::Space::with_height(6),
                    container(
                        column![
                            // User ID row with copy button
                            row![
                                text("User ID:").size(13).width(80),
                                text(formatted_id).size(13),
                                iced::widget::Space::with_width(iced::Length::Fill),
                                button(style::icon_text(Icon::Clone, 12))
                                    .on_press(Message::CopyToClipboard(user_id_string.clone()))
                                    .padding(4)
                                    .style(|theme, status| style::button::transparent(theme, status, false))
                            ].spacing(6).align_y(Alignment::Center),
                            // Name row
                            row![
                                text("Name:").size(13).width(80),
                                text(self.user_name.as_deref().unwrap_or("Loading...")).size(13),
                            ].spacing(6).align_y(Alignment::Center),
                            // Email row
                            row![
                                text("Email:").size(13).width(80),
                                text(self.user_email.as_deref().unwrap_or("Loading...")).size(13),
                            ].spacing(6).align_y(Alignment::Center),
                        ]
                        .spacing(6)
                    )
                    .padding(10)
                    .style(|theme: &iced::Theme| {
                        let palette = theme.extended_palette();
                        container::Style {
                            background: Some(palette.background.weak.color.into()),
                            border: iced::Border {
                                color: palette.background.strong.color,
                                width: 1.0,
                                radius: iced::border::Radius::from(4.0),
                            },
                            ..Default::default()
                        }
                    })
                ]
                .spacing(4);

                // Subscription section
                let subscription_section = {
                    let subscription_header = row![
                        style::icon_text(Icon::Cog, 16)
                            .width(20)
                            .align_x(Alignment::Center),
                        text("Subscription Details")
                            .size(16)
                            .font(iced::Font {
                                weight: iced::font::Weight::Bold,
                                ..Default::default()
                            })
            ]
            .spacing(8)
                    .align_y(Alignment::Center);

                    let mut subscription_details = column![];

                    // Main status row
                    let status_text = if self.is_pro {
                        "Pro Subscription"
            } else {
                    match &self.trial_status {
                Some(TrialStatus::Active { days_remaining }) => {
                                // Special handling for active trial
                                let trial_content = column![
                                    row![
                                        text("Plan:").size(13).width(80),
                                        text("Trial Period").size(13).font(iced::Font {
                                            weight: iced::font::Weight::Bold,
                                            ..Default::default()
                                        }).style(|theme: &iced::Theme| {
                                            let palette = theme.extended_palette();
                                            iced::widget::text::Style {
                                                color: Some(palette.primary.base.color),
                                            }
                                        }),
                                    ].spacing(6).align_y(Alignment::Center),
                                    row![
                                        text("Days Remaining:").size(13).width(80),
                                        text(format!("{} days", days_remaining)).size(13),
                                    ].spacing(6).align_y(Alignment::Center),
                                    iced::widget::Space::with_height(6),
                button(
                    row![
                        style::icon_text(Icon::Star, 12),
                                            text("Upgrade to Pro").size(13)
                    ]
                    .spacing(4)
                    .align_y(Alignment::Center)
                )
                .padding(8)
                                    .width(iced::Length::Fill)
                                    .on_press(Message::OpenUrl("https://www.airterminal.xyz/pricing".to_string()))
                .style(style::primary_button)
            ]
                                .spacing(6);
                                
                                return container(
                                    column![
                                        subscription_header,
                                        iced::widget::Space::with_height(6),
                                        container(trial_content)
                                            .padding(10)
                                            .style(|theme: &iced::Theme| {
                                                let palette = theme.extended_palette();
                                                container::Style {
                                                    background: Some(palette.background.weak.color.into()),
                                                    border: iced::Border {
                                                        color: palette.background.strong.color,
                                                        width: 1.0,
                                                        radius: iced::border::Radius::from(4.0),
                                                    },
                                                    ..Default::default()
                                                }
                                            }),
                                        iced::widget::Space::with_height(12),
                                        // Actions section for trial
                button(
                    row![
                                                style::icon_text(Icon::Close, 12),
                                                text("Logout").size(13)
                    ]
                    .spacing(4)
                    .align_y(Alignment::Center)
                )
                .padding(8)
                                        .width(iced::Length::Fill)
                                        .on_press(Message::Logout)
                .style(|theme, status| style::button::transparent(theme, status, false))
                                    ]
                                    .spacing(4)
                                )
                                .align_x(Alignment::Start)
                                .max_width(400)
                                .padding(24)
                                .style(style::dashboard_modal)
                                .into();
                            }
                            Some(TrialStatus::Expired) => "Trial Expired",
                            Some(TrialStatus::NotStarted) | None => "Trial Not Started",
                        }
                    };

                    let status_style = |theme: &iced::Theme| {
                        let palette = theme.extended_palette();
                        if self.is_pro {
                            iced::widget::text::Style {
                                color: Some(palette.success.base.color),
                            }
        } else {
                            iced::widget::text::Style {
                                color: Some(palette.danger.base.color),
                            }
                        }
                    };

                    subscription_details = subscription_details.push(
                    row![
                            text("Plan:").size(13).width(80),
                            text(status_text).size(13).font(iced::Font {
                                weight: iced::font::Weight::Bold,
                                ..Default::default()
                            }).style(status_style),
                        ].spacing(6).align_y(Alignment::Center)
                    );
                
                    // Status (CopperX status) if available
                    if let Some(ref copperx_status) = self.copperx_status {
                        subscription_details = subscription_details.push(
                    row![
                                text("Status:").size(13).width(80),
                                text(format!("{:?}", copperx_status)).size(13),
                            ].spacing(6).align_y(Alignment::Center)
                        );
                    }

                    // Subscription ID if available with copy button
                    if let Some(ref sub_id) = self.subscription_id {
                        let formatted_sub_id = if sub_id.len() > 20 {
                            format!("{}...{}", &sub_id[..8], &sub_id[sub_id.len()-8..])
                        } else {
                            sub_id.clone()
                        };
                        subscription_details = subscription_details.push(
                    row![
                                text("Subscription ID:").size(13).width(80),
                                text(formatted_sub_id).size(13),
                                iced::widget::Space::with_width(iced::Length::Fill),
                                button(style::icon_text(Icon::Clone, 12))
                                    .on_press(Message::CopyToClipboard(sub_id.clone()))
                                    .padding(4)
                                    .style(|theme, status| style::button::transparent(theme, status, false))
                            ].spacing(6).align_y(Alignment::Center)
                        );
                    }

                    // Next billing date if available
                    if let Some(ref next_billing) = self.next_billing_date {
                        subscription_details = subscription_details.push(
                        row![
                                text("Next Billing:").size(13).width(80),
                                text(next_billing.format("%B %d, %Y").to_string()).size(13),
                            ].spacing(6).align_y(Alignment::Center)
                        );
                    }
                
                    // Add upgrade button for non-pro users (but not for active trial, handled above)
                    if !self.is_pro && !matches!(self.trial_status, Some(TrialStatus::Active { .. })) {
                        subscription_details = subscription_details.push(iced::widget::Space::with_height(6));
                        subscription_details = subscription_details.push(
                            button(
                                row![
                                    style::icon_text(Icon::Star, 12),
                                    text("Upgrade to Pro").size(13)
                                ]
                                .spacing(4)
                                .align_y(Alignment::Center)
                            )
                            .padding(8)
                            .width(iced::Length::Fill)
                            .on_press(Message::OpenUrl("https://www.airterminal.xyz/pricing".to_string()))
                            .style(style::primary_button)
                        );
                    }

                    column![
                        subscription_header,
                        iced::widget::Space::with_height(6),
                        container(subscription_details.spacing(6))
                            .padding(10)
                            .style(|theme: &iced::Theme| {
                                let palette = theme.extended_palette();
                                container::Style {
                                    background: Some(palette.background.weak.color.into()),
                                    border: iced::Border {
                                        color: palette.background.strong.color,
                                        width: 1.0,
                                        radius: iced::border::Radius::from(4.0),
                                    },
                                    ..Default::default()
                                }
                            })
                    ]
                    .spacing(4)
                };

                // Actions section
                let actions_section = button(
            row![
                style::icon_text(Icon::Close, 12),
                        text("Logout").size(13)
            ]
            .spacing(4)
            .align_y(Alignment::Center)
        )
        .padding(8)
                .width(iced::Length::Fill)
        .on_press(Message::Logout)
        .style(|theme, status| style::button::transparent(theme, status, false));

                // Main content
                let content = column![
                    user_header,
                    iced::widget::Space::with_height(12),
                    subscription_section,
                    iced::widget::Space::with_height(12),
                    actions_section,
                ]
                .spacing(4);

                container(content)
                    .align_x(Alignment::Start)
        .max_width(400)
                    .padding(24)
                    .style(style::dashboard_modal)
                    .into()
            },
            _ => {
                // This shouldn't happen since the account button should only be visible when logged in
                let content = column![
                    text("Not logged in").size(14),
                    iced::widget::Space::with_height(8),
                    button("Log in")
                        .padding(8)
                        .on_press(Message::LoginWithEmail(
                            self.email_input.clone(), 
                            self.password_input.clone()
                        ))
                ]
                .spacing(6)
        .align_x(Alignment::Center);

        container(content)
                    .align_x(Alignment::Start)
                    .max_width(400)
            .padding(24)
            .style(style::dashboard_modal)
            .into()
            }
        }
    }
}
