use bevy::app;

use {
	crate::{
		prelude::*,
		colors,
		piece,
		puzzle
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

pub fn build_app(app_builder: &mut AppBuilder) -> () {
	const LIGHT_AND_CAMERA_DATA_FILE: &str = "lightAndCameraData.ron";

	app_builder
		.insert_resource(Msaa { samples: 4 })
		.add_plugins(DefaultPlugins)
		.insert_resource(log_error_result!(from_ron::<LightAndCameraData>(LIGHT_AND_CAMERA_DATA_FILE), LightAndCameraData::default()))
		.add_startup_system(startup_app
			.system()
			.after("puzzle::startup_app()")
		);
	colors::build_app(app_builder);
	piece::build_app(app_builder);
	puzzle::build_app(app_builder);
}

fn startup_app(
	mut commands: Commands,
	light_and_camera_data: Res<LightAndCameraData>,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<StandardMaterial>>
) -> () {
	let cube_mesh_handle: Handle<Mesh> = meshes.add(Mesh::from(shape::Cube {size: 1.0_f32}));

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