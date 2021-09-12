use {
	crate::{
		prelude::*,
		math::polyhedra::{
			data::Data,
			Polyhedron
		},
		piece::Type,
		preferences::Preferences
	},
	bevy::{
		prelude::*,
		render::camera::PerspectiveProjection as BevyPerspectiveProjection
	},
	serde::Deserialize
};

define_struct_with_default!(
	#[derive(Clone, Deserialize)]
	pub PerspectiveProjection {
		fov:			f32	= 0.5_f32,
		aspect_ratio:	f32	= 1.0_f32,
		near:			f32	= 1.0_f32,
		far:			f32	= 1000.0_f32,
	}
);

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

define_struct_with_default!(
	#[derive(Deserialize)]
	pub LightAndCameraData {
		pub light_pos:	[f32; 3]				= [0.0, 1.0, 10.0],
		pub camera_pos:	[f32; 3]				= [0.0, 0.0, 10.0],
		pub persp_proj:	PerspectiveProjection	= PerspectiveProjection::default(),
	}
);

pub struct CameraComponent;

pub struct CameraPlugin;

impl CameraPlugin {
	fn startup_app(
		mut commands: Commands,
		preferences: Res<Preferences>
	) -> () {
		commands
			.spawn_bundle((
				CameraComponent,
				Transform::from_rotation(Self::get_quat_for_face(Type::Pentagon.index_offset())),
				GlobalTransform::default()
			))
			.with_children(|child_builder: &mut ChildBuilder| -> () {
				let light_and_camera_data: &LightAndCameraData = &preferences.light_and_camera;

				child_builder.spawn_bundle(LightBundle {
					transform: Transform::from_translation(Vec3::from(light_and_camera_data.light_pos)),
					.. Default::default()
				});
				child_builder.spawn_bundle(PerspectiveCameraBundle {
					transform: Transform::from_translation(Vec3::from(light_and_camera_data.camera_pos)).looking_at(Vec3::ZERO, Vec3::Y),
					perspective_projection: light_and_camera_data.persp_proj.clone().into(),
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