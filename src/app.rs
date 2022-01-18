use {
	crate::{
		math::polyhedra::data::DataPlugin,
		piece::PiecePlugin,
		preferences::colors::ColorsPlugin,
		puzzle::{
			PuzzlePlugin,
			TransformationPlugin
		},
		ui::UIPlugin
	},
	bevy::{
		prelude::*,
		app::PluginGroupBuilder
	},
	bevy_inspector_egui::bevy_egui::EguiPlugin
};

pub mod prelude {
	pub use crate::{
		math::polyhedra::data::{
			Data as PolyhedraData,
			DataPlugin as PolyhedraDataPlugin,
		},
		piece::{
			PieceComponent,
			PieceLibrary,
			PiecePlugin
		},
		preferences::{
			colors::{
				ColorData,
				ColorsPlugin
			},
			Preferences
		},
		puzzle::{
			transformation::{
				Library as TransformationLibrary,
				Type as TransformationType
			},
			ExtendedPuzzleState,
			PuzzlePlugin,
		},
		ui::{
			camera::{
				CameraComponent,
				CameraPlugin,
				CameraQuery,
				CameraQueryNT
			},
			input::{
				InputData,
				InputPlugin,
				InputState
			},
			UIPlugin
		}
	};
}

struct AppPlugin;

impl Plugin for AppPlugin {
	fn build(&self, app: &mut AppBuilder) {
		app
			.add_plugins(DefaultPlugins)
			.add_plugin(EguiPlugin);
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