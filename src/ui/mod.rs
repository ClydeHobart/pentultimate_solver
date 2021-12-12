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
								let preferences: View = View::Preferences(
									Box::new(world
										.get_resource::<Preferences>()
										.map_or(
											Preferences::default(),
											|preferences: &Preferences| -> Preferences {
												preferences.clone()
											}
										)
								));
	
								*view = preferences;
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
		let extended_puzzle_state: &ExtendedPuzzleState = log_option_none!(world.get_resource::<ExtendedPuzzleState>());
		let input_state: &InputState = log_option_none!(world.get_resource::<InputState>());
		let preferences: &Preferences = log_option_none!(world.get_resource::<Preferences>());

		egui::Area::new("InputState")
			.anchor(egui::Align2::LEFT_BOTTOM, Vec2::new(30.0_f32, -30.0_f32))
			.show(egui_context.ctx(), |ui: &mut Ui| -> () {
				let toggles: &InputToggles = &input_state.toggles;

				ui.style_mut().visuals.widgets.noninteractive.bg_fill = Color32::TRANSPARENT;
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

					modifier_row!(rotate_twice,			"Rotate Twice");
					modifier_row!(counter_clockwise,	"Counter Clockwise");
					modifier_row!(alt_hemi,				"Alt. Hemi.");
					modifier_row!(
						disable_recentering,
						if matches!(toggles.transformation_type, TransformationType::Reorientation) {
							"Enable Modifiers"
						} else {
							"Disable Recentering"
						}
					);

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
			.anchor(egui::Align2::RIGHT_BOTTOM, Vec2::new(-30.0_f32, -30.0_f32))
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