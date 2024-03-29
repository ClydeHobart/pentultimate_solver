use {
    self::input::InputToggles,
    crate::{
        app::prelude::*,
        preferences::prelude::*,
        preferences::Update,
        prelude::*,
        puzzle::transformation::{Addr, GenusIndex, GenusIndexType, Library},
        tools::prelude::*,
        ui::input::{
            FileAction, FileActionType, KeyPressAction, PendingActions, PuzzleAction,
            PuzzleActionType,
        },
    },
    bevy::{app::CoreStage, prelude::*, window::WindowMode as BevyWindowMode},
    bevy_egui::{EguiContext as BevyEguiContext, EguiSystem},
    bevy_inspector_egui::{Context, Inspectable},
    egui::{
        Align2, Area, CentralPanel, Color32, Context as EguiContext, Grid, ScrollArea, Stroke,
        Style, TopBottomPanel, Ui, Vec2, Window as EguiWindow,
    },
    serde::Deserialize,
    std::{
        boxed::Box,
        ffi::OsStr,
        fs::{create_dir_all, read_dir, DirEntry, Metadata, ReadDir},
        io::Result as IoResult,
        mem::swap,
        ops::Range,
        path::Path,
        sync::Mutex,
        time::SystemTime,
    },
};

pub mod camera;
pub mod input;

/// Copied from bevy_window::window::WindowMode
#[derive(Clone, Copy, Deserialize, Inspectable, PartialEq)]
pub enum WindowMode {
    /// Creates a window that uses the given size
    Windowed,
    /// Creates a borderless window that uses the full size of the screen
    BorderlessFullscreen,
    /// Creates a fullscreen window that will render at desktop resolution. The app will use the
    /// closest supported size from the given size and scale it to fit the screen.
    SizedFullscreen,
    /// Creates a fullscreen window that uses the maximum supported size
    Fullscreen,
}

impl Default for WindowMode {
    fn default() -> Self {
        Self::Windowed
    }
}

impl From<WindowMode> for BevyWindowMode {
    fn from(window_mode: WindowMode) -> Self {
        match window_mode {
            WindowMode::Windowed => Self::Windowed,
            WindowMode::BorderlessFullscreen => Self::BorderlessFullscreen,
            WindowMode::SizedFullscreen => Self::SizedFullscreen,
            WindowMode::Fullscreen => Self::Fullscreen,
        }
    }
}

#[derive(Clone, Default, Deserialize, Inspectable, PartialEq)]
pub struct UIData {
    window_mode: WindowMode,
}

impl UIData {
    fn update_window_mode(&self, windows: &mut Windows) {
        debug_expect_some!(windows.get_primary_mut(), |window: &mut Window| {
            window.set_mode(self.window_mode.into());
            window.set_maximized(true);
        });
    }
}

impl Update for UIData {
    fn update(&self, other: &Self, world: &mut World, _: &Preferences) {
        if self != other {
            debug_expect_some!(world.get_resource_mut::<Windows>(), |mut windows: Mut<
                Windows,
            >| {
                self.update_window_mode(windows.as_mut());
            });
        }
    }
}

pub enum View {
    Main,
    Preferences(Box<Preferences>),
}

pub struct UIPlugin;

impl UIPlugin {
    pub fn startup(mut preferences: ResMut<Preferences>, mut windows: ResMut<Windows>) {
        *preferences = Preferences::from_file_or_default(STRING_DATA.files.preferences.as_ref());
        preferences.ui.update_window_mode(windows.as_mut());
        warn_expect_ok!(create_dir_all(&STRING_DATA.files.saves));
    }

    fn run(world: &mut World) {
        world.resource_scope(
            |world: &mut World, mut bevy_egui_context: Mut<BevyEguiContext>| {
                let mut context: Context =
                    Context::new_world_access(Some(bevy_egui_context.ctx_mut()), world);

                Self::render_menu(&mut context);

                match *warn_expect_some!(context.world().unwrap().get_resource::<View>(), return) {
                    View::Main => Self::render_main(&mut context),
                    View::Preferences(_) => {
                        Self::render_preferences(&mut context);
                    }
                }
            },
        );
    }

    fn render_menu(context: &mut Context) {
        TopBottomPanel::top("Menu").show(context.ui_ctx.unwrap(), |ui: &mut Ui| {
            egui::menu::bar(ui, |ui: &mut Ui| {
                ui.menu_button("File", |ui: &mut Ui| {
                    ui.group(|ui: &mut Ui| {
                        let world: &mut World = unsafe { context.world_mut() }.unwrap();
                        let mut close_menu: bool = false;

                        world.resource_scope(|world: &mut World, mut view: Mut<View>| {
                            if ui.button("Preferences").clicked() {
                                Self::on_preferences_clicked(world, &mut view);
                                close_menu = true;
                            }
                        });

                        world.resource_scope(
                            |world: &mut World, mut input_state: Mut<InputState>| {
                                let input_state: &mut InputState = &mut input_state;

                                ui.set_enabled(!input_state.has_active_action());

                                if ui.button("Reset").clicked() {
                                    Self::on_reset_clicked(world, input_state);
                                    close_menu = true;
                                }

                                if ui.button("Randomize").clicked() {
                                    Self::on_randomize_clicked(world, input_state);
                                    close_menu = true;
                                }

                                if ui.button("Solve").clicked() {
                                    Self::on_solve_clicked(world, input_state);
                                    close_menu = true;
                                }

                                if ui.button("Save Puzzle State").clicked() {
                                    Self::on_save_clicked(input_state);
                                    close_menu = true;
                                }

                                if ui.button("Load Puzzle State").clicked() {
                                    Self::on_load_clicked(input_state);
                                    close_menu = true;
                                }
                            },
                        );

                        if ui.button("Quit").clicked() {
                            exit_app(world);
                        }

                        if close_menu {
                            ui.close_menu();
                        }
                    });
                });
            });
        });
    }

    fn on_preferences_clicked(world: &World, view: &mut View) {
        *view = View::Preferences(Box::new(
            world
                .get_resource::<Preferences>()
                .map_or_else(Preferences::default, Preferences::clone),
        ));
    }

    fn on_reset_clicked(world: &mut World, input_state: &mut InputState) {
        if let (Some(camera_orientation), Some(preferences)) = (
            world.query::<CameraComponents>().iter(world).next().map(
                |camera_components_item: CameraComponentsItem| -> Quat {
                    camera_components_item.transform.rotation
                },
            ),
            world.get_resource::<Preferences>(),
        ) {
            input_state.puzzle_action = Some(PuzzleAction {
                action_type: PuzzleActionType::Reset,
                current_action: None,
                pending_actions: Some(Box::<PendingActions>::new(PendingActions::reset(
                    &camera_orientation,
                    preferences.speed.animation.clone(),
                ))),
            });
        }
    }

    fn on_randomize_clicked(world: &mut World, input_state: &mut InputState) {
        if let (Some(camera_orientation), Some(preferences), Some(extended_puzzle_state)) = (
            world
                .query::<(&CameraComponent, &Transform)>()
                .iter(world)
                .next()
                .map(|(_, transform): (&CameraComponent, &Transform)| -> Quat {
                    transform.rotation
                }),
            world.get_resource::<Preferences>(),
            world.get_resource::<ExtendedPuzzleState>(),
        ) {
            input_state.puzzle_action = Some(PuzzleAction {
                action_type: PuzzleActionType::Randomize,
                current_action: None,
                pending_actions: Some(Box::<PendingActions>::new(PendingActions::randomize(
                    preferences,
                    &extended_puzzle_state.puzzle_state,
                    &camera_orientation,
                ))),
            });
        }
    }

    fn on_solve_clicked(world: &mut World, input_state: &mut InputState) {
        world.resource_scope(|world: &mut World, mut solver: Mut<Solver>| {
            if let Some(extended_puzzle_state) = world.get_resource::<ExtendedPuzzleState>() {
                solver.init(&extended_puzzle_state.puzzle_state);
                input_state.is_solving = true;
            }
        });
    }

    fn on_save_clicked(input_state: &mut InputState) {
        input_state.file_action = Some(FileAction::new(
            Mutex::new(Box::pin(async {
                rfd::AsyncFileDialog::new()
                    .set_title("Save Puzzle State")
                    .set_directory(
                        Path::new(&STRING_DATA.files.saves)
                            .canonicalize()
                            .unwrap_or_default(),
                    )
                    .set_file_name(&format!(
                        "puzzleState{}",
                        read_dir(&STRING_DATA.files.saves).map_or(0_usize, ReadDir::count)
                            + 1_usize,
                    ))
                    .add_filter("Serialized Format", SerFmt::file_extensions())
                    .save_file()
                    .await
            })),
            FileActionType::Save,
        ));
    }

    fn on_load_clicked(input_state: &mut InputState) {
        input_state.file_action = Some(FileAction::new(
            Mutex::new(Box::pin(async {
                rfd::AsyncFileDialog::new()
                    .set_title("Load Puzzle State")
                    .set_directory(
                        Path::new(&STRING_DATA.files.saves)
                            .canonicalize()
                            .unwrap_or_default(),
                    )
                    .set_file_name(&read_dir(&STRING_DATA.files.saves).map_or(
                        "puzzleState1".into(),
                        |read_dir: ReadDir| -> String {
                            read_dir
                                // Filter to accepted file types
                                .filter(|dir_entry: &IoResult<DirEntry>| -> bool {
                                    dir_entry.is_ok()
                                        && SerFmt::file_extensions().contains(
                                            &Path::new(&dir_entry.as_ref().unwrap().file_name())
                                                .extension()
                                                .and_then(OsStr::to_str)
                                                .unwrap_or(""),
                                        )
                                })
                                // Find the most recent file
                                .max_by_key(|dir_entry: &IoResult<DirEntry>| -> SystemTime {
                                    dir_entry
                                        .as_ref()
                                        .unwrap()
                                        .metadata()
                                        .ok()
                                        .as_ref()
                                        .map(Metadata::created)
                                        .map(Result::ok)
                                        .unwrap_or_default()
                                        .unwrap_or(SystemTime::UNIX_EPOCH)
                                })
                                // Convert to usable type
                                .map(Result::ok)
                                .unwrap_or_default()
                                .as_ref()
                                .map(DirEntry::file_name)
                                .as_deref()
                                .and_then(OsStr::to_str)
                                .unwrap_or("puzzleState1")
                                .into()
                        },
                    ))
                    .add_filter("Serialized Format", SerFmt::file_extensions())
                    .pick_file()
                    .await
            })),
            FileActionType::Load,
        ));
    }

    fn render_main(context: &mut Context) {
        Self::render_input_state(&mut context.with_id(0_u64));
        Self::render_action_stack(&mut context.with_id(1_u64));
        Self::render_tools(&mut context.with_id(2_u64));
    }

    fn render_input_state(context: &mut Context) {
        const OFFSET: f32 = 20.0_f32;
        const INPUT_STATE_OFFSET: Vec2 = Vec2::new(OFFSET, -OFFSET);

        let world: &World = warn_expect_some!(context.world(), return);
        let input_state: &InputState =
            warn_expect_some!(world.get_resource::<InputState>(), return);
        let preferences: &Preferences =
            warn_expect_some!(world.get_resource::<Preferences>(), return);

        Area::new("InputState")
            .anchor(Align2::LEFT_BOTTOM, INPUT_STATE_OFFSET)
            .show(context.ui_ctx.unwrap(), |ui: &mut Ui| {
                use GenusIndexType as GIT;

                let toggles: &InputToggles = &input_state.toggles;
                let genus_index: GIT = toggles.genus_index.into();

                for family_index in 0_usize..Library::get_family_count() {
                    let genus_range: Range<GIT> = Library::get_family_genus_range(family_index);

                    if genus_range.contains(&genus_index) {
                        ui.colored_label(Color32::WHITE, format!("{:?}", toggles.genus_index));
                    } else {
                        ui.colored_label(
                            Color32::GRAY,
                            format!("{:?}", GenusIndex::try_from(genus_range.start).unwrap()),
                        );
                    }
                }

                Grid::new("ModifierTable").show(ui, |ui: &mut Ui| {
                    use KeyPressAction as KPA;

                    let input: &InputData = &preferences.input;

                    ui.end_row();

                    let mut modifier_row =
                        |modifier: bool,
                         kpa: KPA,
                         name: &str,
                         requiring_genus: Option<GenusIndex>| {
                            let text_stroke: &mut Stroke =
                                &mut ui.visuals_mut().widgets.noninteractive.fg_stroke;

                            if modifier {
                                text_stroke.color = Color32::WHITE;
                                text_stroke.width = 1.5_f32;
                            } else {
                                text_stroke.color = Color32::GRAY;
                                text_stroke.width = 1.0_f32;
                            }

                            ui.label(format!("{:?}", input.key_presses[kpa]));
                            ui.label(format!(
                                "{}{}",
                                name,
                                if requiring_genus.is_some()
                                    && requiring_genus.unwrap() == input_state.toggles.genus_index
                                {
                                    " (required by genus)"
                                } else {
                                    ""
                                }
                            ));
                            ui.end_row();
                        };

                    modifier_row(
                        toggles.enable_recentering,
                        KPA::EnableRecentering,
                        "Enable Recentering",
                        Some(GenusIndex::REORIENTATION),
                    );
                    modifier_row(
                        toggles.enable_modifiers,
                        KPA::EnableModifiers,
                        "Enable Modifiers",
                        Some(GenusIndex::SIMPLE),
                    );
                    modifier_row(toggles.rotate_twice, KPA::RotateTwice, "Rotate Twice", None);
                    modifier_row(
                        toggles.counter_clockwise,
                        KPA::CounterClockwise,
                        "Counter Clockwise",
                        None,
                    );
                    modifier_row(
                        toggles.alt_hemi,
                        KPA::AlternateHemisphere,
                        "Alternate Hemisphere",
                        None,
                    );
                });
            });
    }

    fn render_action_stack(context: &mut Context) {
        const OFFSET: f32 = 20.0_f32;
        const ACTION_STACK_OFFSET: Vec2 = Vec2::new(-OFFSET, -OFFSET);

        let extended_puzzle_state: &ExtendedPuzzleState = warn_expect_some!(
            context
                .world()
                .and_then(World::get_resource::<ExtendedPuzzleState>),
            return
        );
        let max_height: f32 = context.ui_ctx.unwrap().available_rect().height() - 2.0_f32 * OFFSET;

        Area::new("ActionStack")
            .anchor(Align2::RIGHT_BOTTOM, ACTION_STACK_OFFSET)
            .show(context.ui_ctx.unwrap(), |ui: &mut Ui| {
                ScrollArea::vertical()
                    .max_height(max_height)
                    // This isn't reliable, but if it was, it'd be better than always_show_scroll()
                    // .min_scrolled_height(max_height)
                    .always_show_scroll(!extended_puzzle_state.actions.is_empty())
                    .stick_to_bottom()
                    .show(ui, |ui: &mut Ui| {
                        Grid::new("ActionStackGrid")
                            .min_col_width(0.0_f32)
                            .spacing(ui.spacing().item_spacing * Vec2::Y)
                            .show(ui, |ui: &mut Ui| {
                                let stack_debug: Option<&Stack> = context
                                    .world()
                                    .and_then(World::get_resource::<Preferences>)
                                    .and_then(|preferences: &Preferences| -> Option<&ToolDataBox> {
                                        preferences.tools.get_tool_data(Tool::Stack)
                                    })
                                    .and_then(|tool_data_box: &ToolDataBox| -> Option<&Stack> {
                                        tool_data_box.as_any().downcast_ref::<Stack>()
                                    });

                                for (action_index, action) in
                                    extended_puzzle_state.actions.iter().enumerate()
                                {
                                    let camera_half_addr: HalfAddr = action.camera_start;
                                    let transformation: FullAddr =
                                        action.transformation - camera_half_addr;
                                    let transformation_half_addr: HalfAddr =
                                        *transformation.get_half_addr();

                                    ui.visuals_mut().widgets.noninteractive.fg_stroke.color =
                                        if action_index + 1_usize
                                            == extended_puzzle_state.curr_action
                                        {
                                            Color32::WHITE
                                        } else {
                                            Color32::GRAY
                                        };

                                    if let Some(stack_debug) = stack_debug {
                                        ui.label(
                                            match (
                                                action_index == stack_debug.min_focus_index,
                                                action_index == stack_debug.max_focus_index,
                                            ) {
                                                (true, true) => "= ",
                                                (true, false) => "≥ ",
                                                (false, true) => "≤ ",
                                                (false, false) => "",
                                            },
                                        );
                                    }

                                    ui.label(transformation.try_get_genus_index().map_or_else(
                                        || -> String { "[INVALID]".into() },
                                        |genus_index: GenusIndex| -> String {
                                            format!("{:?} ", genus_index)
                                        },
                                    ));

                                    if transformation_half_addr.is_valid() {
                                        ui.label(format!(
                                            "({}, ",
                                            transformation_half_addr.get_species_index()
                                        ));
                                        ui.label(format!(
                                            "{}) ",
                                            transformation_half_addr.get_organism_index()
                                        ));
                                    } else {
                                        ui.label("(-1, ");
                                        ui.label("-1) ");
                                    }

                                    if camera_half_addr.is_valid() {
                                        ui.label(format!(
                                            "@ ({}, ",
                                            camera_half_addr.get_species_index()
                                        ));
                                        ui.label(format!(
                                            "{})",
                                            camera_half_addr.get_organism_index()
                                        ));
                                    } else {
                                        ui.label("(-1, ");
                                        ui.label("-1)");
                                    }

                                    if let Some(stack_debug) = stack_debug {
                                        if stack_debug.print_true_action {
                                            let transformation: FullAddr = action.transformation;
                                            let transformation_half_addr: HalfAddr =
                                                *transformation.get_half_addr();

                                            ui.label(format!(
                                                " (== {}",
                                                transformation.try_get_genus_index().map_or_else(
                                                    || -> String { "[INVALID]".into() },
                                                    |genus_index: GenusIndex| -> String {
                                                        format!("{:?} ", genus_index)
                                                    }
                                                )
                                            ));

                                            if transformation_half_addr.is_valid() {
                                                ui.label(format!(
                                                    "({}, ",
                                                    transformation_half_addr.get_species_index()
                                                ));
                                                ui.label(format!(
                                                    "{}) ",
                                                    transformation_half_addr.get_organism_index()
                                                ));
                                            } else {
                                                ui.label("(-1, ");
                                                ui.label("-1)");
                                            }
                                        }
                                    }

                                    ui.end_row();
                                }
                            });
                    });
            });
    }

    fn render_tools(context: &mut Context) {
        const OFFSET: f32 = 20.0_f32;
        const TOOLS_OFFSET: Vec2 = Vec2::new(OFFSET, OFFSET);
        const FILL_ALPHA: u8 = 0xF0_u8;

        let mut preferences: Mut<Preferences> = warn_expect_some!(
            unsafe { context.world_mut() }.and_then(World::get_resource_mut::<Preferences>),
            return
        );

        if !preferences.tools.should_render() {
            return;
        }

        const fn color32_from_gray_and_alpha<const A: u8>(gray: u8) -> Color32 {
            let premultiplied_gray: u8 = (gray as f32 * A as f32 / u8::MAX as f32) as u8;

            Color32::from_rgba_premultiplied(
                premultiplied_gray,
                premultiplied_gray,
                premultiplied_gray,
                A,
            )
        }

        let egui_context: &EguiContext = context.ui_ctx.unwrap();
        let prev_style: Style = (*egui_context.style()).clone();
        let mut curr_style: Style = prev_style.clone();

        macro_rules! style {
            ($($color_type:ident, $color32_func:path =>
                $($color_sub_type:ident, $gray:expr, $field:expr),*);*) => {
                $(
                    {
                        enum $color_type {
                            $($color_sub_type),*
                        }

                        const COUNT: usize = {
                            let mut count: usize = 0_usize;

                            $(
                                count += 1_usize;
                                ignore!($gray);
                            )*

                            count
                        };

                        const COLORS: [Color32; COUNT] = [
                            $(
                                ($color32_func)($gray),
                            )*
                        ];

                        $(
                            $field = COLORS[$color_type::$color_sub_type as usize];
                        )*
                    }
                )*
            }
        }

        style!(
            Fill, color32_from_gray_and_alpha::<FILL_ALPHA> =>
                WidgetsNoninteractiveBg, 27_u8, curr_style.visuals.widgets.noninteractive.bg_fill,
                WidgetsInactiveBg,       48_u8, curr_style.visuals.widgets.inactive.bg_fill,
                WidgetsHoveredBg,        70_u8, curr_style.visuals.widgets.hovered.bg_fill,
                WidgetsActiveBg,         55_u8, curr_style.visuals.widgets.active.bg_fill,
                WidgetsOpenBg,           27_u8, curr_style.visuals.widgets.open.bg_fill,
                FaintBgColor,            24_u8, curr_style.visuals.faint_bg_color,
                ExtremeBgColor,          10_u8, curr_style.visuals.extreme_bg_color,
                CodeBgColor,             64_u8, curr_style.visuals.code_bg_color;
            Stroke, Color32::from_gray =>
                NoninteractiveBg,  60_u8, curr_style.visuals.widgets.noninteractive.bg_stroke.color,
                NoninteractiveFg, 140_u8, curr_style.visuals.widgets.noninteractive.fg_stroke.color,
                InactiveBg,         0_u8, curr_style.visuals.widgets.inactive.bg_stroke.color,
                InactiveFg,       180_u8, curr_style.visuals.widgets.inactive.fg_stroke.color,
                HoveredBg,        150_u8, curr_style.visuals.widgets.hovered.bg_stroke.color,
                HoveredFg,        240_u8, curr_style.visuals.widgets.hovered.fg_stroke.color,
                ActiveBg,         255_u8, curr_style.visuals.widgets.active.bg_stroke.color,
                ActiveFg,         255_u8, curr_style.visuals.widgets.active.fg_stroke.color,
                OpenBg,            60_u8, curr_style.visuals.widgets.open.bg_stroke.color,
                OpenFg,           210_u8, curr_style.visuals.widgets.open.fg_stroke.color
        );

        curr_style.visuals.window_shadow = epaint::Shadow::default();
        curr_style.wrap = Some(false);
        egui_context.set_style(curr_style);
        EguiWindow::new("Tools")
            .anchor(Align2::LEFT_TOP, TOOLS_OFFSET)
            .title_bar(false)
            .show(egui_context, |ui: &mut Ui| {
                preferences.tools.render(ui, context);
            });
        context.ui_ctx.unwrap().set_style(prev_style);
    }

    fn render_preferences(context: &mut Context) {
        let egui_context: &EguiContext = warn_expect_some!(context.ui_ctx, return);
        let world: &mut World = warn_expect_some!(unsafe { context.world_mut() }, return);

        if !world.contains_resource::<Preferences>() || !world.contains_resource::<View>() {
            return;
        }

        enum ClosingAction {
            None,
            Reset,
            Save,
            Cancel,
        }

        let mut closing_action: ClosingAction = ClosingAction::None;

        world.resource_scope(|world: &mut World, mut view: Mut<View>| {
            if let View::Preferences(preferences) = &mut (*view) {
                CentralPanel::default().show(egui_context, |ui: &mut Ui| {
                    ScrollArea::vertical().show(ui, |ui: &mut Ui| {
                        preferences.ui(ui, (), &mut context.with_id(0_u64));
                        ui.horizontal(|ui: &mut Ui| {
                            if ui.button("Reset").clicked() {
                                closing_action = ClosingAction::Reset;
                            }

                            if ui.button("Save").clicked() {
                                closing_action = ClosingAction::Save;
                            }

                            if ui.button("Cancel").clicked() {
                                closing_action = ClosingAction::Cancel;
                            }
                        });
                    });
                });

                match closing_action {
                    ClosingAction::Reset => {
                        **preferences = Preferences::default();
                    }
                    ClosingAction::Save => {
                        world.resource_scope(
                            |world: &mut World, mut res_preferences: Mut<Preferences>| {
                                preferences.update(&*res_preferences, world, preferences);
                                swap(&mut *res_preferences, &mut *preferences);
                            },
                        );

                        *view = View::Main;
                    }
                    ClosingAction::Cancel => {
                        *view = View::Main;
                    }
                    _ => {}
                }
            }
        });
    }
}

impl Plugin for UIPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(Preferences::default())
            .insert_resource(View::Main)
            .insert_resource(Msaa { samples: 4 })
            .insert_resource(WindowDescriptor {
                title: STRING_DATA.misc.app_title.clone(),
                ..WindowDescriptor::default()
            })
            .add_plugin(CameraPlugin)
            .add_plugin(InputPlugin)
            .add_startup_system(Self::startup.after(TransformationPlugin::startup))
            .add_system_to_stage(
                CoreStage::PostUpdate,
                Self::run
                    .exclusive_system()
                    .before(EguiSystem::ProcessOutput),
            );
    }
}
