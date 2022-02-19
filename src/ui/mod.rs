use {
	self::input::InputToggles,
	crate::{
		prelude::*,
		app::prelude::*,
		preferences::Update,
		puzzle::transformation::{
			Addr,
			Type,
			TYPE_COUNT
		},
		ui::input::{
			FileAction,
			FileActionType,
			PuzzleActionType,
			PuzzleAction,
			PendingActions
		}
	},
	bevy::{
		prelude::*,
		app::CoreStage,
		input::keyboard::KeyCode as BevyKeyCode
	},
	bevy_egui::EguiContext,
	bevy_inspector_egui::{
		Context,
		Inspectable
	},
	egui::{
		self,
		Color32,
		CtxRef,
		Ui,
		Vec2
	},
	std::{
		boxed::Box,
		ffi::{
			OsStr,
			OsString
		},
		fs::{
			DirEntry,
			Metadata,
			ReadDir,
			create_dir_all,
			read_dir
		},
		io::Result as IoResult,
		path::Path,
		sync::Mutex,
		time::SystemTime
	}
};

#[cfg(debug_assertions)]
use crate::debug::prelude::{
	DebugMode,
	DebugModeDataBox,
	Stack
};

pub mod camera;
pub mod input;

#[derive(PartialEq)]
pub enum View {
	Main,
	Preferences(Box<Preferences>)
}

pub struct UIPlugin;

impl UIPlugin {
	fn startup(mut windows: ResMut<Windows>) -> () {
		debug_expect_some!(windows.get_primary_mut(), |window: &mut Window| -> () { window.set_maximized(true); });
		log_result_err!(create_dir_all(&STRING_DATA.files.saves));
	}

	fn run(world: &mut World) -> () {
		world.resource_scope(|world: &mut World, mut egui_context: Mut<EguiContext>| -> () {
			let mut context: Context = Context::new_world_access(Some(egui_context.ctx_mut()), world);

			Self::render_menu(&mut context);

			match *log_option_none!(context.world().unwrap().get_resource::<View>()) {
				View::Main => { Self::render_main(&mut context) },
				View::Preferences(_) => { Self::render_preferences(&mut context); }
			}
		});
	}

	fn render_menu(context: &mut Context) -> () {
		egui::TopBottomPanel::top("Menu").show(context.ui_ctx.unwrap(), |ui: &mut Ui| -> () {
			egui::menu::bar(ui, |ui: &mut Ui| -> () {
				ui.menu_button("File", |ui: &mut egui::Ui| -> () {
					ui.group(|ui: &mut Ui| -> () {
						let world: &mut World = unsafe { context.world_mut() }.unwrap();

						world.resource_scope(|world: &mut World, mut view: Mut<View>| -> () {
							if ui.button("Preferences").clicked() {
								Self::on_preferences_clicked(world, &mut *view);
							}
						});

						world.resource_scope(|world: &mut World, mut input_state: Mut<InputState>| -> () {
							let input_state: &mut InputState = &mut *input_state;

							ui.set_enabled(!input_state.has_active_action());

							if ui.button("Reset").clicked() {
								Self::on_reset_clicked(world, input_state);
							}

							if ui.button("Randomize").clicked() {
								Self::on_randomize_clicked(world, input_state);
							}

							if ui.button("Save Puzzle State").clicked() {
								Self::on_save_clicked(input_state);
							}

							if ui.button("Load Puzzle State").clicked() {
								Self::on_load_clicked(input_state);
							}
						});
					});
				});
			});
		});
	}

	fn on_preferences_clicked(world: &World, view: &mut View) -> () {
		*view = View::Preferences(
			Box::new(world
				.get_resource::<Preferences>()
				.map_or_else(Preferences::default, Preferences::clone)
		));
	}

	fn on_reset_clicked(world: &mut World, input_state: &mut InputState) -> () {
		if let (
			Some(camera_orientation),
			Some(preferences)
		) = (
			world
				.query::<CameraTuple>()
				.iter(world)
				.next()
				.map(|(_, transform): CameraTuple| -> Quat {
					transform.rotation
				}
			),
			world.get_resource::<Preferences>(),
		) {
			input_state.puzzle_action = Some(PuzzleAction {
				action_type: PuzzleActionType::Reset,
				current_action: None,
				pending_actions: Some(Box::<PendingActions>::new(
					PendingActions::reset(
						&camera_orientation,
						preferences.speed.animation.clone()
					)
				))
			});
		}
	}

	fn on_randomize_clicked(world: &mut World, input_state: &mut InputState) -> () {
		if let (
			Some(camera_orientation),
			Some(preferences),
			Some(extended_puzzle_state)
		) = (
			world
				.query::<(&CameraComponent, &Transform)>()
				.iter(world)
				.next()
				.map(|(_, transform): (&CameraComponent, &Transform)| -> Quat {
					transform.rotation
				}
			),
			world.get_resource::<Preferences>(),
			world.get_resource::<ExtendedPuzzleState>()
		) {
			input_state.puzzle_action = Some(PuzzleAction {
				action_type: PuzzleActionType::Randomize,
				current_action: None,
				pending_actions: Some(Box::<PendingActions>::new(
					PendingActions::randomize(
						preferences,
						&extended_puzzle_state.puzzle_state,
						&camera_orientation
					)
				))
			});
		}
	}

	fn on_save_clicked(input_state: &mut InputState) -> () {
		input_state.file_action = Some(FileAction::new(
			Mutex::new(Box::pin(async {
				rfd::AsyncFileDialog::new()
					.set_title("Save Puzzle State")
					.set_directory(
						Path::new(&STRING_DATA.files.saves)
							.canonicalize()
							.unwrap_or_default()
					)
					.set_file_name(&format!(
						"puzzleState{}",
						read_dir(&STRING_DATA.files.saves)
							.map_or(0_usize, ReadDir::count)
							+ 1_usize,
					))
					.add_filter(
						"Serialized Format",
						SerFmt::file_extensions())
					.save_file()
					.await
			})),
			FileActionType::Save
		));
	}

	fn on_load_clicked(input_state: &mut InputState) -> () {
		input_state.file_action = Some(FileAction::new(
			Mutex::new(Box::pin(async {
				rfd::AsyncFileDialog::new()
					.set_title("Load Puzzle State")
					.set_directory(Path::new(&STRING_DATA.files.saves).canonicalize().unwrap_or_default())
					.set_file_name(&read_dir(&STRING_DATA.files.saves)
						.map_or(
							"puzzleState1".into(),
							|read_dir: ReadDir| -> String {
								read_dir
									// Filter to accepted file types
									.filter(|dir_entry: &IoResult<DirEntry>| -> bool {
										dir_entry.is_ok()
											&& SerFmt::file_extensions()
												.contains(&Path::new(&dir_entry.as_ref().unwrap().file_name())
													.extension().and_then(OsStr::to_str).unwrap_or("")
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
									.as_ref()
									.map(OsString::as_os_str)
									.and_then(OsStr::to_str)
									.unwrap_or("puzzleState1")
									.into()
							}
						)
					)
					.add_filter(
						"Serialized Format",
						SerFmt::file_extensions())
					.pick_file()
					.await
			})),
			FileActionType::Load
		));
	}

	fn render_main(context: &mut Context) -> () {
		Self::render_input_state(&mut context.with_id(0_u64));
		Self::render_action_stack(&mut context.with_id(1_u64));

		#[cfg(debug_assertions)]
		Self::render_debug_modes(&mut context.with_id(2_u64));
	}

	fn render_input_state(context: &mut Context) -> () {
		const OFFSET: f32 = 20.0_f32;
		const INPUT_STATE_OFFSET: Vec2 = Vec2::new(OFFSET, -OFFSET);

		let world: &World = log_option_none!(context.world());
		let input_state: &InputState = log_option_none!(world.get_resource::<InputState>());
		let preferences: &Preferences = log_option_none!(world.get_resource::<Preferences>());

		egui::Area::new("InputState")
			.anchor(egui::Align2::LEFT_BOTTOM, INPUT_STATE_OFFSET)
			.show(context.ui_ctx.unwrap(), |ui: &mut Ui| -> () {
				let toggles: &InputToggles = &input_state.toggles;

				egui::Grid::new("ModifierTable").show(ui, |ui: &mut Ui| -> () {
					let input: &InputData = &preferences.input;

					macro_rules! modifier_row {
						($toggle:ident, $modifier_name:expr) => {
							let text_stroke: &mut egui::Stroke = &mut ui.visuals_mut().widgets.noninteractive.fg_stroke;

							if toggles.$toggle {
								text_stroke.color = Color32::WHITE;
								text_stroke.width = 1.5_f32;
							} else {
								text_stroke.color = Color32::GRAY;
								text_stroke.width = 1.0_f32;
							}

							ui.label(format!("[{:?}]", BevyKeyCode::from(input.$toggle)));
							ui.label($modifier_name);
							ui.end_row();
						}
					}

					modifier_row!(enable_modifiers,		"Enable Modifiers");
					modifier_row!(rotate_twice,			"Rotate Twice");
					modifier_row!(counter_clockwise,	"Counter Clockwise");
					modifier_row!(alt_hemi,				"Alt. Hemi.");
					modifier_row!(disable_recentering,	"Disable Recentering");

					ui.end_row();
				});

				for transformation_type_u8 in 0_u8 .. TYPE_COUNT as u8 {
					ui.visuals_mut().widgets.noninteractive.fg_stroke.color = if toggles.transformation_type as u8
						== transformation_type_u8
					{
						Color32::WHITE
					} else {
						Color32::GRAY
					};

					ui.label(format!("{:?}", TransformationType::try_from(transformation_type_u8).unwrap()));
				}
			});
	}

	fn render_action_stack(context: &mut Context) -> () {
		const OFFSET: f32 = 20.0_f32;
		const ACTION_STACK_OFFSET: Vec2 = Vec2::new(-OFFSET, -OFFSET);

		let extended_puzzle_state: &ExtendedPuzzleState = log_option_none!(
			context.world().and_then(World::get_resource::<ExtendedPuzzleState>)
		);

		egui::Area::new("ActionStack")
			.anchor(egui::Align2::RIGHT_BOTTOM, ACTION_STACK_OFFSET)
			.show(context.ui_ctx.unwrap(), |ui: &mut Ui| -> () {
				egui::ScrollArea::vertical().show(ui, |ui: &mut Ui| -> () {
					egui::Grid::new("ActionStackGrid")
						.min_col_width(0.0_f32)
						.spacing(ui.spacing().item_spacing * Vec2::Y)
						.show(ui, |ui: &mut Ui| -> () {
							#[cfg(debug_assertions)]
							let stack_debug: Option<&Stack> = context
								.world()
								.and_then(World::get_resource::<Preferences>)
								.and_then(|preferences: &Preferences| -> Option<&DebugModeDataBox> {
									preferences.debug_modes.get_debug_mode_data(DebugMode::Stack)
								})
								.and_then(|debug_mode_data_box: &DebugModeDataBox| -> Option<&Stack> {
									debug_mode_data_box.as_any().downcast_ref::<Stack>()
								});

							for (action_index, action)
								in extended_puzzle_state.actions.iter().enumerate()
							{
								let camera_half_addr:			HalfAddr = *action.camera_start();
								let transformation:				FullAddr = *action.transformation() - camera_half_addr;
								let transformation_half_addr:	HalfAddr = *transformation.get_half_addr();

								ui.visuals_mut().widgets.noninteractive.fg_stroke.color =
									if action_index as i32 == extended_puzzle_state.curr_action {
										Color32::WHITE
									} else {
										Color32::GRAY
									};

								#[cfg(debug_assertions)]
								if let Some(stack_debug) = stack_debug {
									ui.label(match (
										action_index == stack_debug.min_simplification_index.0,
										action_index == stack_debug.max_simplification_index.0
									) {
										(true,	true)	=> "= ",
										(true,	false)	=> "≥ ",
										(false,	true)	=> "≤ ",
										(false,	false)	=> ""
									});
								}

								ui.label(transformation
									.get_page_index_type()
									.map_or_else(
										|| -> String { "[INVALID]".into() },
										|transformation_type: Type| -> String {
											format!("{:?} ", transformation_type)
										}
									));
								ui.label(format!("({}, ", transformation_half_addr.get_line_index()));
								ui.label(format!("{}) ", transformation_half_addr.get_word_index()));
								ui.label(format!("@ ({}, ", camera_half_addr.get_line_index()));
								ui.label(format!("{})", camera_half_addr.get_word_index()));
								ui.end_row();
							}
						});
				});
			});
	}

	#[cfg(debug_assertions)]
	fn render_debug_modes(context: &mut Context) -> () {
		const OFFSET: f32 = 20.0_f32;
		const DEBUG_MODES_OFFSET: Vec2 = Vec2::new(OFFSET, OFFSET);
		const FILL_ALPHA: u8 = 0xC0_u8;

		let mut preferences: Mut<Preferences> = log_option_none!(
			unsafe { context.world_mut() }.and_then(World::get_resource_mut::<Preferences>)
		);

		if !preferences.debug_modes.should_render() {
			return;
		}

		const fn color32_from_gray_and_alpha<const A: u8>(gray: u8) -> Color32 {
			let premultiplied_gray: u8 = (gray as f32 * A as f32 / u8::MAX as f32) as u8;

			Color32::from_rgba_premultiplied(
				premultiplied_gray,
				premultiplied_gray,
				premultiplied_gray,
				A
			)
		}

		let ctx_ref: &CtxRef = context.ui_ctx.unwrap();
		let prev_style: egui::Style = (*ctx_ref.style()).clone();
		let mut curr_style: egui::Style = prev_style.clone();

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
								$gray;
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
				WidgetsNoninteractiveBg,	27_u8,	curr_style.visuals.widgets.noninteractive.bg_fill,
				WidgetsInactiveBg,			48_u8,	curr_style.visuals.widgets.inactive.bg_fill,
				WidgetsHoveredBg,			70_u8,	curr_style.visuals.widgets.hovered.bg_fill,
				WidgetsActiveBg,			55_u8,	curr_style.visuals.widgets.active.bg_fill,
				WidgetsOpenBg,				27_u8,	curr_style.visuals.widgets.open.bg_fill,
				FaintBgColor,				24_u8,	curr_style.visuals.faint_bg_color,
				ExtremeBgColor,				10_u8,	curr_style.visuals.extreme_bg_color,
				CodeBgColor,				64_u8,	curr_style.visuals.code_bg_color;
			Stroke, Color32::from_gray =>
				NoninteractiveBg,			060_u8,	curr_style.visuals.widgets.noninteractive.bg_stroke.color,
				NoninteractiveFg,			140_u8,	curr_style.visuals.widgets.noninteractive.fg_stroke.color,
				InactiveBg,					000_u8,	curr_style.visuals.widgets.inactive.bg_stroke.color,
				InactiveFg,					180_u8,	curr_style.visuals.widgets.inactive.fg_stroke.color,
				HoveredBg,					150_u8,	curr_style.visuals.widgets.hovered.bg_stroke.color,
				HoveredFg,					240_u8,	curr_style.visuals.widgets.hovered.fg_stroke.color,
				ActiveBg,					255_u8,	curr_style.visuals.widgets.active.bg_stroke.color,
				ActiveFg,					255_u8,	curr_style.visuals.widgets.active.fg_stroke.color,
				OpenBg,						060_u8,	curr_style.visuals.widgets.open.bg_stroke.color,
				OpenFg,						210_u8,	curr_style.visuals.widgets.open.fg_stroke.color
		);

		curr_style.visuals.window_shadow = epaint::Shadow::default();
		curr_style.wrap = Some(false);
		ctx_ref.set_style(curr_style);
		egui::Window::new("DebugModes")
			.anchor(egui::Align2::LEFT_TOP, DEBUG_MODES_OFFSET)
			.title_bar(false)
			.show(ctx_ref, |ui: &mut Ui| -> () {
				preferences.debug_modes.render(ui, context);
			});
		context.ui_ctx.unwrap().set_style(prev_style);
	}

	fn render_preferences(context: &mut Context) -> () {
		let ctx_ref: &CtxRef = log_option_none!(context.ui_ctx);
		let world: &mut World = log_option_none!(unsafe { context.world_mut() });

		if !world.contains_resource::<Preferences>() || !world.contains_resource::<View>() {
			return;
		}

		enum ClosingAction {
			None,
			Reset,
			Save,
			Cancel
		}

		let mut closing_action: ClosingAction = ClosingAction::None;

		world.resource_scope(|world: &mut World, mut view: Mut<View>| -> () {
			if let View::Preferences(preferences) = &mut (*view) {
				egui::CentralPanel::default()
					.show(ctx_ref, |ui: &mut egui::Ui| -> () {
						egui::ScrollArea::vertical().show(ui, |ui: &mut Ui| -> () {
							preferences.ui(ui, (), &mut context.with_id(0_u64));
							ui.horizontal(|ui: &mut Ui| -> () {
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
					},
					ClosingAction::Save => {
						world.resource_scope(|world: &mut World, mut res_preferences: Mut<Preferences>| -> () {
							*res_preferences = (**preferences).clone();
							(*res_preferences).update(world);
						});

						*view = View::Main;
					},
					ClosingAction::Cancel => {
						*view = View::Main;
					},
					_ => {}
				}
			}
		});
	}
}

impl Plugin for UIPlugin {
	fn build(&self, app: &mut App) {
		app
			.insert_resource(Preferences::from_file_or_default(&STRING_DATA.files.preferences))
			.insert_resource(View::Main)
			.insert_resource(Msaa { samples: 4 })
			.insert_resource(WindowDescriptor {
				title: STRING_DATA.misc.app_title.clone(),
				.. WindowDescriptor::default()
			})
			.add_plugin(CameraPlugin)
			.add_plugin(InputPlugin)
			.add_startup_system(Self::startup.system())
			.add_system_to_stage(CoreStage::Last, Self::run.exclusive_system());
	}
}