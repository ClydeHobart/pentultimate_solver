#[allow(unused_imports)]
use {
	std::sync::{
		Arc,
		Mutex
	},
	bevy::prelude::*,
	bevy_egui::{
		EguiContext,
		EguiPlugin
	},
	egui::{
		style::Spacing,
		Button,
		CentralPanel,
		Color32,
		ComboBox,
		Grid,
		RichText,
		Ui,
		Vec2
	},
	lazy_static::*
};

struct DebugState {
	index: u32,
	error_message: bool
}

static mut DEBUG_STATE: DebugState = DebugState {
	index: 0_u32,
	error_message: false
};

fn run(mut egui_context: ResMut<EguiContext>) {
	let debug_state: &mut DebugState = unsafe { &mut DEBUG_STATE };

	CentralPanel::default().show(egui_context.ctx_mut(), |ui: &mut Ui| {
		const MIN_COL_WIDTH_SCALE: f32 = 2.0_f32;
		let col_width: f32 = ui.spacing().interact_size.x * MIN_COL_WIDTH_SCALE;
		Grid::new(0_u64)
			.num_columns(1_usize)
			.min_col_width(2.0_f32 * col_width)
			.show(ui, |ui: &mut Ui| {
				const STRS: [&str; 4_usize] = [
					"Alt",
					"Ctrl",
					"Shift",
					"Win"
				];

				ui.centered_and_justified(|ui: &mut Ui| {
					ComboBox::from_id_source(0_u64)
						.width(2.0_f32 * col_width)
						.selected_text(STRS[debug_state.index as usize].to_string())
						.show_ui(ui, |ui: &mut Ui| {
							for index in 0_u32 .. STRS.len() as u32 {
								if ui.selectable_label(index == debug_state.index, format!("{}", index)).clicked() {
									debug_state.index = index;
								}
							}
						});
				});

				ui.end_row();

				Grid::new(1_u64)
					.num_columns(2_usize)
					.min_col_width(col_width)
					.show(ui, |ui: &mut Ui| {
						for modifier in 0_usize .. STRS.len() {
							let mut button = |
								ui: &mut Ui,
								s: &str
							| {
								// ui.centered_and_justified(|ui: &mut Ui| -> () {
									if ui.add_sized(
										ui.spacing().interact_size
											* Vec2::new(MIN_COL_WIDTH_SCALE, 1.0_f32),
										Button::new(format!("{}{}", s, STRS[modifier]))
									).clicked() {
										debug_state.error_message = !debug_state.error_message;
									}
								// });
							};
		
							button(ui, "L");
							button(ui, "R");
							ui.end_row();
						}
					});

				ui.end_row();

				if debug_state.error_message {
					ui.colored_label(Color32::RED, "Error message, error error");
					ui.end_row();
				}
			});
	});
}

fn main() { App::new().add_plugins(DefaultPlugins).add_plugin(EguiPlugin).add_system(run).run(); }