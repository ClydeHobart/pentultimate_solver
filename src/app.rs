use {
	self::prelude::*,
	bevy::{
		prelude::*,
		app::PluginGroupBuilder
	},
	bevy_inspector_egui::bevy_egui::EguiPlugin,
	serde::{
		Deserialize,
		Serialize
	}
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
			PiecePlugin,
			PieceQuery
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
				FullAddr,
				HalfAddr,
				Library as TransformationLibrary,
				Type as TransformationType
			},
			ExtendedPuzzleState,
			PuzzlePlugin,
			TransformationPlugin
		},
		ui::{
			camera::{
				CameraComponent,
				CameraPlugin,
				CameraQuery,
				CameraQueryMut,
				CameraQueryNTMut,
				CameraQueryNT,
				CameraTuple,
				CameraTupleMut,
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

#[derive(Deserialize, Serialize)]
struct SaveState {
	pub extended_puzzle_state:	ExtendedPuzzleState,
	pub input_state:			InputState,
	pub camera:					HalfAddr
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
			.add(PolyhedraDataPlugin)
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