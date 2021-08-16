use {
	crate::{
		prelude::*,
		math::polyhedra::{
			data::Data,
			Polyhedron
		},
		piece::Type
	},
	bevy::{
		prelude::*,
		render::camera::PerspectiveProjection as BevyPerspectiveProjection
	},
	serde::Deserialize
};

#[derive(Deserialize)]
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
			light_pos:	[5.0, 0.0, 3.0],
			camera_pos:	[5.0, 0.0, 0.0],
			persp_proj: PerspectiveProjection::default()
		}
	}
}

pub struct CameraComponent;

pub struct CameraPlugin;

impl CameraPlugin {
	fn startup_app(
		mut commands: Commands
	) -> () {
		commands
			.spawn_bundle((
				CameraComponent,
				Transform::from_rotation(Self::get_quat_for_face(Type::Pentagon.index_offset())),
				GlobalTransform::default()
			))
			.with_children(|child_builder: &mut ChildBuilder| -> () {
				let light_and_camera_data: LightAndCameraData = from_ron_or_default::<LightAndCameraData>(&STRING_DATA.files.light_and_camera_data);

				child_builder.spawn_bundle(LightBundle {
					transform: Transform::from_translation(Vec3::from(light_and_camera_data.light_pos)),
					.. Default::default()
				});
				child_builder.spawn_bundle(PerspectiveCameraBundle {
					transform: Transform::from_translation(Vec3::from(light_and_camera_data.camera_pos)).looking_at(Vec3::ZERO, Vec3::Y),
					perspective_projection: light_and_camera_data.persp_proj.into(),
					.. Default::default()
				});
			});
	}

	fn get_quat_for_face(face_index: usize) -> Quat {
		Data::get(Polyhedron::Icosidodecahedron).map_or_else(
			|| -> Quat {
				log::warn!(target: log_path!("get_quat_for_face()"), "Data::get(Polyhedron::Icosidodecahedron) was None, returning Quat::IDENTITY for index {}", face_index);

				Quat::IDENTITY
			},
			|icosidodecahedron_data: &Data| -> Quat {
				icosidodecahedron_data.faces[face_index].quat
			}
		)
	}
}

impl Plugin for CameraPlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app.add_startup_system(Self::startup_app.system());
	}
}