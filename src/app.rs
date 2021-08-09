use {
	crate::{
		colors::ColorsPlugin,
		piece::PiecePlugin,
		puzzle::PuzzlePlugin,
		ui::UIPlugin
	},
	bevy::{
		prelude::*,
		app::PluginGroupBuilder
	}
};


struct AppPlugin;

impl Plugin for AppPlugin {
	fn build(&self, app: &mut AppBuilder) {
		app
			.add_plugins(DefaultPlugins);
	}
}

struct AppPluginGroup;

impl PluginGroup for AppPluginGroup {
	fn build(&mut self, group: &mut PluginGroupBuilder) {
		group
			.add(ColorsPlugin)
			.add(PiecePlugin)
			.add(PuzzlePlugin)
			.add(UIPlugin)
			.add(AppPlugin);
	}
}

pub fn main() -> () {
	App::build().add_plugins(AppPluginGroup).run();
}