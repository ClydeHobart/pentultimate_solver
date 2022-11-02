use {
    self::prelude::*,
    crate::prelude::set_rust_log_env_var,
    bevy::{app::PluginGroupBuilder, log::LogPlugin, prelude::*},
    bevy_inspector_egui::bevy_egui::EguiPlugin,
    bevy_prototype_debug_lines::DebugLinesPlugin,
    serde::{Deserialize, Serialize},
};

pub mod prelude {
    pub use crate::{
        app::SaveState,
        math::polyhedra::data::{Data as PolyhedraData, DataPlugin as PolyhedraDataPlugin},
        piece::{
            PieceComponent, PieceComponents, PieceComponentsMut, PieceLibrary, PiecePlugin,
            PieceQuery, PieceQueryMut,
        },
        preferences::{colors::ColorsPlugin, Preferences},
        puzzle::{
            solver::{Solver, SolverPlugin},
            transformation::{
                Addr, FullAddr, GenusIndex, HalfAddr, Library as TransformationLibrary,
                TransformationPlugin,
            },
            ExtendedPuzzleState, InflatedPuzzleState, PuzzlePlugin, PuzzlePluginGroup,
        },
        ui::{
            camera::{
                CameraComponent, CameraComponents, CameraComponentsItem, CameraComponentsMut,
                CameraPlugin, CameraQuery, CameraQueryMut, CameraQueryMutNT, CameraQueryMutState,
                CameraQueryMutStateNT, CameraQueryNT, CameraQueryState, CameraQueryStateNT,
            },
            input::{InputPlugin, InputState, InputToggles},
            UIPlugin,
        },
    };
}

#[derive(Deserialize, Serialize)]
pub struct SaveState {
    pub extended_puzzle_state: ExtendedPuzzleState,
    pub input_toggles: InputToggles,
    pub camera: HalfAddr,
}

struct AppPlugin;

impl Plugin for AppPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins_with(
            DefaultPlugins,
            |group: &mut PluginGroupBuilder| -> &mut PluginGroupBuilder {
                group.disable::<LogPlugin>()
            },
        )
        .add_plugin(EguiPlugin)
        .add_plugin(DebugLinesPlugin::default());
    }
}

struct AppPluginGroup;

impl PluginGroup for AppPluginGroup {
    fn build(&mut self, group: &mut PluginGroupBuilder) -> () {
        group
            .add(LogPlugin)
            .add(PolyhedraDataPlugin)
            .add(PuzzlePluginGroup)
            .add(ColorsPlugin)
            .add(PiecePlugin)
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
