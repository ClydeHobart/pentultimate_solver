use {
	self::input::InputToggles,
	crate::{
		prelude::*,
		app::prelude::*,
		preferences::Update,
		puzzle::transformation::TYPE_COUNT
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
		Ui,
		Vec2
	},
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
		log_option_none!(windows.get_primary_mut()).set_maximized(true);
	}

	fn run(world: &mut World) -> () {
		world.resource_scope(|world: &mut World, egui_context: Mut<EguiContext>| -> () {
			Self::render_menu(&egui_context, world);

			match *log_option_none!(world.get_resource::<View>()) {
				View::Main => { Self::render_main(&egui_context, world) },
				View::Preferences(_) => { Self::render_preferences(&egui_context, world); }
			}
		});
	}

	fn render_menu(egui_context: &EguiContext, world: &mut World) -> () {
		egui::TopBottomPanel::top("Menu").show(egui_context.ctx(), |ui: &mut Ui| -> () {
			egui::menu::bar(ui, |ui: &mut Ui| -> () {
				egui::menu::menu(ui, "File", |ui: &mut egui::Ui| -> () {
					ui.group(|ui: &mut Ui| -> () {
						world.resource_scope(|world: &mut World, mut view: Mut<View>| -> () {
							ui.set_enabled(matches!(*view, View::Main));
	
							if ui.button("Preferences").clicked() {
								*view = View::Preferences(
									Box::new(world
										.get_resource::<Preferences>()
										.map_or_else(Preferences::default, Preferences::clone)
								));
							}

							
	
							ui.set_enabled(false);
	
							if ui.button("Save Puzzle State").clicked() {
								unreachable!();
							}
						});
					});
				});
			});
		});
	}

	fn render_main(egui_context: &EguiContext, world: &mut World) -> () {
		const OFFSET: f32 = 20.0_f32;
		const INPUT_STATE_OFFSET: Vec2 = Vec2::new(OFFSET, -OFFSET);
		const ACTION_STACK_OFFSET: Vec2 = Vec2::new(-OFFSET, -OFFSET);

		let extended_puzzle_state: &ExtendedPuzzleState = log_option_none!(world.get_resource::<ExtendedPuzzleState>());
		let input_state: &InputState = log_option_none!(world.get_resource::<InputState>());
		let preferences: &Preferences = log_option_none!(world.get_resource::<Preferences>());

		egui::Area::new("InputState")
			.anchor(egui::Align2::LEFT_BOTTOM, INPUT_STATE_OFFSET)
			.show(egui_context.ctx(), |ui: &mut Ui| -> () {
				let toggles: &InputToggles = &input_state.toggles;

				// ui.style_mut().visuals.widgets.noninteractive.bg_fill = Color32::TRANSPARENT;
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

		egui::Area::new("ActionStack")
			.anchor(egui::Align2::RIGHT_BOTTOM, ACTION_STACK_OFFSET)
			.show(egui_context.ctx(), |ui: &mut Ui| -> () {
				// ui.label("Action Stack");
				egui::ScrollArea::auto_sized().show(ui, |ui: &mut Ui| -> () {
					for (action_index, action) in extended_puzzle_state.actions.iter().enumerate() {
						ui.visuals_mut().widgets.noninteractive.fg_stroke.color = if action_index as i32 == extended_puzzle_state.curr_action {
							Color32::WHITE
						} else {
							Color32::GRAY
						};
						ui.label(format!("{:?}", action.transformation()));
					}
				});
			});

		#[cfg(debug_assertions)]
		{
			world.resource_scope(|world: &mut World, mut preferences: Mut<Preferences>| -> () {
				if preferences.debug_modes.should_render() {
					enum Fill {
						WidgetsNoninteractiveBg,
						WidgetsInactiveBg,
						WidgetsHoveredBg,
						WidgetsActiveBg,
						WidgetsOpenBg,
						FaintBgColor,
						ExtremeBgColor,
						CodeBgColor
					}
					enum Stroke {
						NoninteractiveBg,
						NoninteractiveFg,
						InactiveBg,
						InactiveFg,
						HoveredBg,
						HoveredFg,
						ActiveBg,
						ActiveFg,
						OpenBg,
						OpenFg,
					}

					const FILL_COUNT: usize = 8_usize;
					const FILL_GRAYS: [u8; FILL_COUNT] = [
						27_u8,	// WidgetsNoninteractiveBg
						48_u8,	// WidgetsInactive
						70_u8,	// WidgetsHovered
						55_u8,	// WidgetsActive
						27_u8,	// WidgetsOpenBg
						24_u8,	// FaintBgColor
						10_u8,	// ExtremeBgColor
						64_u8	// CodeBgColor
					];
					const fn color32_from_gray_and_alpha(gray: u8, alpha: u8) -> Color32 {
						let premultiplied_gray: u8 = (gray as f32 * alpha as f32 / u8::MAX as f32) as u8;

						Color32::from_rgba_premultiplied(
							premultiplied_gray,
							premultiplied_gray,
							premultiplied_gray,
							alpha
						)
					}
					const FILL_ALPHA: u8 = 0xC0_u8;
					const FILL_COLORS: [Color32; FILL_COUNT] = [
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::WidgetsNoninteractiveBg	as usize], FILL_ALPHA),
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::WidgetsInactiveBg			as usize], FILL_ALPHA),
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::WidgetsHoveredBg			as usize], FILL_ALPHA),
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::WidgetsActiveBg			as usize], FILL_ALPHA),
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::WidgetsOpenBg				as usize], FILL_ALPHA),
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::FaintBgColor				as usize], FILL_ALPHA),
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::ExtremeBgColor				as usize], FILL_ALPHA),
						color32_from_gray_and_alpha(FILL_GRAYS[Fill::CodeBgColor				as usize], FILL_ALPHA)
					];
					const STROKE_COUNT: usize = 10_usize;
					const STROKE_GRAYS: [u8; STROKE_COUNT] = [
						60_u8,	// NoninteractiveBg
						140_u8,	// NoninteractiveFg
						0_u8,	// InactiveBg
						180_u8,	// InactiveFg
						150_u8,	// HoveredBg
						240_u8,	// HoveredFg
						255_u8,	// ActiveBg
						255_u8,	// ActiveFg
						60_u8,	// OpenBg
						210_u8	// OpenFg
					];
					const STROKE_COLORS: [Color32; STROKE_COUNT] = [
						Color32::from_gray(STROKE_GRAYS[Stroke::NoninteractiveBg	as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::NoninteractiveFg	as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::InactiveBg			as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::InactiveFg			as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::HoveredBg			as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::HoveredFg			as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::ActiveBg			as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::ActiveFg			as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::OpenBg				as usize]),
						Color32::from_gray(STROKE_GRAYS[Stroke::OpenFg				as usize])
					];
					const WINDOW_SHADOW: epaint::Shadow = epaint::Shadow {
						extrusion: 0.0_f32,
						color: Color32::TRANSPARENT
					};
					const DEBUG_MODES_OFFSET: Vec2 = Vec2::new(OFFSET, OFFSET);

					let prev_style: egui::Style = (*egui_context.ctx().style()).clone();
					let mut curr_style: egui::Style = prev_style.clone();

					curr_style.visuals.widgets.noninteractive.bg_fill			= FILL_COLORS[Fill::WidgetsNoninteractiveBg as usize];
					curr_style.visuals.widgets.noninteractive.bg_stroke.color	= STROKE_COLORS[Stroke::NoninteractiveBg as usize];
					curr_style.visuals.widgets.noninteractive.fg_stroke.color	= STROKE_COLORS[Stroke::NoninteractiveFg as usize];
					curr_style.visuals.widgets.inactive.bg_fill					= FILL_COLORS[Fill::WidgetsInactiveBg as usize];
					curr_style.visuals.widgets.inactive.bg_stroke.color			= STROKE_COLORS[Stroke::InactiveBg as usize];
					curr_style.visuals.widgets.inactive.fg_stroke.color			= STROKE_COLORS[Stroke::InactiveFg as usize];
					curr_style.visuals.widgets.hovered.bg_fill					= FILL_COLORS[Fill::WidgetsHoveredBg as usize];
					curr_style.visuals.widgets.hovered.bg_stroke.color			= STROKE_COLORS[Stroke::HoveredBg as usize];
					curr_style.visuals.widgets.hovered.fg_stroke.color			= STROKE_COLORS[Stroke::HoveredFg as usize];
					curr_style.visuals.widgets.active.bg_fill					= FILL_COLORS[Fill::WidgetsActiveBg as usize];
					curr_style.visuals.widgets.active.bg_stroke.color			= STROKE_COLORS[Stroke::ActiveBg as usize];
					curr_style.visuals.widgets.active.fg_stroke.color			= STROKE_COLORS[Stroke::ActiveFg as usize];
					curr_style.visuals.widgets.open.bg_fill						= FILL_COLORS[Fill::WidgetsOpenBg as usize];
					curr_style.visuals.widgets.open.bg_stroke.color				= STROKE_COLORS[Stroke::OpenBg as usize];
					curr_style.visuals.widgets.open.fg_stroke.color				= STROKE_COLORS[Stroke::OpenFg as usize];
					curr_style.visuals.faint_bg_color							= FILL_COLORS[Fill::FaintBgColor as usize];
					curr_style.visuals.extreme_bg_color							= FILL_COLORS[Fill::ExtremeBgColor as usize];
					curr_style.visuals.code_bg_color							= FILL_COLORS[Fill::CodeBgColor as usize];
					curr_style.visuals.window_shadow							= WINDOW_SHADOW;
					curr_style.wrap = Some(false);
					egui_context.ctx().set_style(curr_style);
					egui::Window::new("DebugModes")
						.anchor(egui::Align2::LEFT_TOP, DEBUG_MODES_OFFSET)
						.title_bar(false)
						.show(egui_context.ctx(), |ui: &mut Ui| -> () {
							preferences.debug_modes.render(egui_context, ui, world);
						});
					egui_context.ctx().set_style(prev_style);
				}
			});
		}
	}

	fn render_preferences(egui_context: &EguiContext, world: &mut World) -> () {
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
					.show(egui_context.ctx(), |ui: &mut egui::Ui| -> () {
						egui::ScrollArea::auto_sized().show(ui, |ui: &mut Ui| -> () {
							preferences.ui(ui, (), &Context::new(egui_context.ctx(), world));
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
	fn build(&self, app: &mut AppBuilder) {
		app
			.insert_resource(from_ron_or_default::<Preferences>(&STRING_DATA.files.preferences))
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