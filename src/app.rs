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
	}
};

pub mod prelude {
	pub use crate::{
		math::polyhedra::data::{
			DataLibrary as PolyhedraDataLibrary,
			DataPlugin as PolyhedraDataPlugin,
		},
		piece::{
			PieceComponent,
			PieceLibrary,
			PiecePlugin
		},
		preferences::colors::{
			ColorData,
			ColorsPlugin
		},
		puzzle::{
			ExtendedPuzzleState,
			PuzzlePlugin,
			TransformationLibraryRef
		},
		ui::{
			camera::CameraComponent,
			input::InputData,
			UIPlugin
		}
	};
}

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