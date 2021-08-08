use bevy::app::PluginGroupBuilder;

use {
	crate::{
		prelude::*,
		colors::ColorsPlugin,
		piece::PiecePlugin,
		puzzle::PuzzlePlugin,
		strings::STRING_DATA
	},
	bevy::{
		prelude::*,
		render::camera::PerspectiveProjection as BevyPerspectiveProjection
	},
	serde::Deserialize
};


#[derive(Clone, Copy, Deserialize)]
struct PerspectiveProjection {
	fov:			f32,
	aspect_ratio:	f32,
	near:			f32,
	far:			f32,
}

impl From<PerspectiveProjection> for BevyPerspectiveProjection {
	fn from(persp_proj: PerspectiveProjection) -> Self {
		Self {
			fov:			persp_proj.fov,
			aspect_ratio:	persp_proj.aspect_ratio,
			near:			persp_proj.near,
			far:			persp_proj.far
		}
	}
}

impl From<BevyPerspectiveProjection> for PerspectiveProjection {
	fn from(bevy_persp_proj: BevyPerspectiveProjection) -> Self {
		Self {
			fov:			bevy_persp_proj.fov,
			aspect_ratio:	bevy_persp_proj.aspect_ratio,
			near:			bevy_persp_proj.near,
			far:			bevy_persp_proj.far
		}
	}
}

impl Default for PerspectiveProjection {
	fn default() -> Self {
		BevyPerspectiveProjection::default().into()
	}
}

#[derive(Deserialize)]
struct LightAndCameraData {
	light_pos: [f32; 3],
	camera_pos: [f32; 3],
	persp_proj: PerspectiveProjection
}

impl Default for LightAndCameraData {
	fn default() -> Self {
		Self {
			light_pos:	[5.0, 5.0, 5.0],
			camera_pos:	[5.0, -5.0, 3.0],
			persp_proj: PerspectiveProjection::default()
		}
	}
}

struct AppPlugin;

impl AppPlugin {
	fn startup_app(
		mut commands: Commands,
		light_and_camera_data: Res<LightAndCameraData>,
		mut meshes: ResMut<Assets<Mesh>>,
		mut materials: ResMut<Assets<StandardMaterial>>,
		mut windows: ResMut<Windows>
	) -> () {
		let cube_mesh_handle: Handle<Mesh> = meshes.add(Mesh::from(shape::Cube {size: 1.0_f32}));

		log_option_none!(windows.get_primary_mut()).set_maximized(true);
		commands.spawn_bundle(PbrBundle {
			mesh: cube_mesh_handle.clone(),
			material: materials.add(Color::RED.into()),
			transform: Transform::from_matrix(Mat4::from_scale(Vec3::new(10.0_f32, 0.125_f32, 0.125_f32))),
			.. Default::default()
		});
		commands.spawn_bundle(PbrBundle {
			mesh: cube_mesh_handle.clone(),
			material: materials.add(Color::GREEN.into()),
			transform: Transform::from_matrix(Mat4::from_scale(Vec3::new(0.125_f32, 10.0_f32, 0.125_f32))),
			.. Default::default()
		});
		commands.spawn_bundle(PbrBundle {
			mesh: cube_mesh_handle.clone(),
			material: materials.add(Color::BLUE.into()),
			transform: Transform::from_matrix(Mat4::from_scale(Vec3::new(0.125_f32, 0.125_f32, 10.0_f32))),
			.. Default::default()
		});
		commands.spawn_bundle(LightBundle {
			transform: Transform::from_translation(Vec3::from(light_and_camera_data.light_pos.clone())),
			.. Default::default()
		});
		commands.spawn_bundle(PerspectiveCameraBundle {
			transform: Transform::from_translation(Vec3::from(light_and_camera_data.camera_pos.clone())).looking_at(Vec3::ZERO, Vec3::Z),
			perspective_projection: light_and_camera_data.persp_proj.into(),
			.. Default::default()
		});
	}
}

impl Plugin for AppPlugin {
	fn build(&self, app: &mut AppBuilder) {
		app
			.insert_resource(Msaa { samples: 4 })
			.insert_resource(WindowDescriptor {
				title: STRING_DATA.misc.app_title.clone(),
				.. WindowDescriptor::default()
			})
			.insert_resource(from_ron_or_default::<LightAndCameraData>(&STRING_DATA.files.light_and_camera_data))
			.add_plugins(DefaultPlugins)
			.add_startup_system(Self::startup_app
				.system()
				.after(STRING_DATA.labels.puzzle.as_ref())
			);
	}
}

struct AppPluginGroup;

impl PluginGroup for AppPluginGroup {
	fn build(&mut self, group: &mut PluginGroupBuilder) {
		group
			.add(AppPlugin)
			.add(ColorsPlugin)
			.add(PiecePlugin)
			.add(PuzzlePlugin);
	}
}

pub fn main() -> () {
	App::build().add_plugins(AppPluginGroup).run();
}