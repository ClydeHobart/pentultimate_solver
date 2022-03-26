use {
	crate::prelude::set_rust_log_env_var,
	self::prelude::*,
	bevy::{
		prelude::*,
		app::PluginGroupBuilder,
		log::LogPlugin
	},
	bevy_inspector_egui::bevy_egui::EguiPlugin,
	serde::{
		Deserialize,
		Serialize
	}
};

pub mod prelude {
	pub use crate::{
		app::SaveState,
		math::polyhedra::data::{
			Data as PolyhedraData,
			DataPlugin as PolyhedraDataPlugin,
		},
		piece::{
			PieceComponent,
			PieceLibrary,
			PiecePlugin,
			PieceQuery,
			PieceQueryState,
			PieceTuple
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
				GenusIndex
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
				CameraQueryNT,
				CameraQueryNTMut,
				CameraQueryState,
				CameraQueryStateMut,
				CameraQueryStateNT,
				CameraQueryStateNTMut,
				CameraTuple,
				CameraTupleMut,
			},
			input::{
				InputData,
				InputPlugin,
				InputState,
				InputToggles
			},
			UIPlugin
		}
	};
}

#[derive(Deserialize, Serialize)]
pub struct SaveState {
	pub extended_puzzle_state:	ExtendedPuzzleState,
	pub input_toggles:			InputToggles,
	pub camera:					HalfAddr
}

struct AppPlugin;

impl Plugin for AppPlugin {
	fn build(&self, app: &mut App) {
		app
			.add_plugins_with(DefaultPlugins, |group: &mut PluginGroupBuilder| -> &mut PluginGroupBuilder {
				group.disable::<LogPlugin>()
			})
			.add_plugin(EguiPlugin);
	}
}

struct AppPluginGroup;

impl PluginGroup for AppPluginGroup {
	fn build(&mut self, group: &mut PluginGroupBuilder) -> () {
		group
			.add(LogPlugin)
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
	set_rust_log_env_var();

	#[cfg(debug_assertions)]
	std::env::set_var("RUST_BACKTRACE", "1");

	App::new().add_plugins(AppPluginGroup).run();
}