use {
	crate::{
		prelude::*,
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		piece::Type,
		preferences::{
			Preferences,
			Update
		},
		puzzle::{
			consts::PENTAGON_SIDE_COUNT,
			transformation::HalfAddr
		}
	},
	super::input::InputState,
	bevy::{
		prelude::*,
		render::camera::PerspectiveProjection as BevyPerspectiveProjection
	},
	bevy_inspector_egui::Inspectable,
	serde::Deserialize
};

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct PerspectiveProjection {
		fov:			f32	= 0.5_f32,
		aspect_ratio:	f32	= 1.0_f32,
		near:			f32	= 1.0_f32,
		far:			f32	= 1000.0_f32
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
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct LightAndCameraData {
		pub light_pos:	[f32; 3]				= [0.0_f32, 0.0_f32, 10.0_f32],
		pub camera_pos:	[f32; 3]				= [0.0_f32, 0.0_f32, 10.0_f32],
		#[inspectable(collapse)]
		pub persp_proj:	PerspectiveProjection	= PerspectiveProjection::default()
	}
);

impl Update for LightAndCameraData {
	fn update(&self, world: &mut World) -> () {
		if let Some((_, mut transform)) = world
			.query::<(&bevy::pbr::Light, &mut Transform)>()
			.iter_mut(world)
			.next()
		{
			transform.translation = Vec3::from(self.light_pos);
		}

		if let Some((
			mut transform,
			mut bevy_perspective_projection
		)) = world
			.query::<(&mut Transform, &mut BevyPerspectiveProjection)>()
			.iter_mut(world)
			.next()
		{
			transform.translation = Vec3::from(self.camera_pos);
			*bevy_perspective_projection = self.persp_proj.clone().into();
		}
	}
}

pub struct CameraComponent;
pub type CameraTupleMut<'a, 'b> = (&'a CameraComponent, &'b mut Transform);
pub type CameraQueryMut<'a, 'b, 'c> = Query<'a, CameraTupleMut<'b, 'c>>;
pub struct CameraQueryNTMut<'a, 'b: 'a, 'c: 'b, 'd: 'b>(pub &'a mut CameraQueryMut<'b, 'c, 'd>);

impl<'a, 'b, 'c,'d> CameraQueryNTMut<'a, 'b, 'c, 'd> {
	pub fn orientation<T, F: FnOnce(Option<&mut Quat>) -> T>(&mut self, f: F) -> T {
		match self.0.iter_mut().next() {
			Some((_, mut transform)) => {
				f(Some(&mut transform.rotation))
			},
			None => {
				f(None)
			}
		}
	}
}

pub type CameraTuple<'a, 'b> = (&'a CameraComponent, &'b Transform);
pub type CameraQuery<'a, 'b, 'c> = Query<'a, CameraTuple<'b, 'c>>;
pub struct CameraQueryNT<'a, 'b: 'a, 'c: 'b, 'd: 'b>(pub &'a CameraQuery<'b, 'c, 'd>);

impl<'a, 'b, 'c,'d> CameraQueryNT<'a, 'b, 'c, 'd> {
	pub fn orientation<T, F: FnOnce(Option<&Quat>) -> T>(&self, f: F) -> T {
		match self.0.iter().next() {
			Some((_, transform)) => {
				f(Some(&transform.rotation))
			},
			None => {
				f(None)
			}
		}
	}
}

pub struct CameraPlugin;

impl CameraPlugin {
	fn startup(
		mut commands: Commands,
		preferences: Res<Preferences>
	) -> () {
		commands
			.spawn_bundle((
				CameraComponent,
				Transform::from_rotation(Data::get(Polyhedron::Icosidodecahedron).faces[Type::Pentagon.index_offset()].quat),
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

	fn run(
		input_state: Res<InputState>,
		mut camera_query: CameraQueryMut
	) -> () {
		log_option_none!(camera_query.iter_mut().next()).1.rotate(input_state.camera_rotation);
	}

	pub fn compute_camera_addr(quat: &Quat) -> HalfAddr {
		Data::get(Polyhedron::Icosidodecahedron).get_pos_and_rot(
			&quat,
			Some(Box::new(|face_data: &FaceData| -> bool {
				face_data.get_size() == PENTAGON_SIDE_COUNT
			}))
		).into()
	}
}

impl Plugin for CameraPlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app
			.add_startup_system(Self::startup.system())
			.add_system(Self::run
				.system()
				.label(STRING_DATA.labels.camera_run.as_ref())
				.after(STRING_DATA.labels.puzzle_run.as_ref())
			);
	}
}