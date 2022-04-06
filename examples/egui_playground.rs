use {
	bevy::prelude::*,
	bevy_egui::{
		EguiContext,
		EguiPlugin
	},
	egui::{
		style::Spacing,
		Color32,
		RichText,
		Ui,
		Vec2
	}
};

fn run(mut egui_context: ResMut<EguiContext>) -> () {
	egui::CentralPanel::default().show(egui_context.ctx_mut(), |ui: &mut Ui| -> () {
		let spacing: &mut Spacing = ui.spacing_mut();

		spacing.item_spacing = Vec2::ZERO;
		spacing.interact_size = Vec2::ZERO;

		for y in 0_u32 .. 10_u32 {
			let ten_y: u32 = 10_u32 * y;
			let y_is_even: bool = y & 1_u32 == 0_u32;

			ui.horizontal(|ui: &mut Ui| -> () {
				for x in 0_u32 .. 10_u32 {
					let x_is_even: bool = x & 1_u32 == 0_u32;
					let color: Color32 = if x_is_even && y_is_even {
						Color32::RED
					} else if x_is_even || y_is_even {
						Color32::BLUE
					} else {
						ui.style().visuals.text_color()
					};

					ui.label(RichText::new(format!("{:02}", ten_y + x)).monospace().color(color));
				}
			});
		}
	});
}

fn main() -> () { App::new().add_plugins(DefaultPlugins).add_plugin(EguiPlugin).add_system(run.system()).run(); }