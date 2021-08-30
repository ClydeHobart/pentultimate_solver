use {
	crate::{
		colors::ColorsPlugin,
		math::polyhedra::data::DataPlugin,
		piece::PiecePlugin,
		puzzle::{
			PuzzlePlugin,
			TransformationPlugin
		},
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
	fn build(&mut self, group: &mut PluginGroupBuilder) -> () {
		group
			.add(DataPlugin)
			.add(TransformationPlugin)
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