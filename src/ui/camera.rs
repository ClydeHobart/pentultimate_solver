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
		pub light_pos:			[f32; 3]				= [0.0_f32, 0.0_f32, 10.0_f32],
		pub camera_pos:			[f32; 3]				= [0.0_f32, 0.0_f32, 10.0_f32],
		#[inspectable(collapse)]
		pub persp_proj:			PerspectiveProjection	= PerspectiveProjection::default(),
		#[inspectable(min = Some(-5.0_f32), max = Some(5.0_f32))]
		pub light_illuminance:	f32						= 4.0_f32
	}
);

impl Update for LightAndCameraData {
	fn update(&self, world: &mut World) -> () {
		if let Some((mut directional_light, mut transform)) = world
			.query::<(&mut bevy::pbr::DirectionalLight, &mut Transform)>()
			.iter_mut(world)
			.next()
		{
			directional_light.illuminance = 10.0_f32.powf(self.light_illuminance);
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

#[derive(Component)]
pub struct CameraComponent;

pub type CameraTuple<'cc, 't> = (&'cc CameraComponent, &'t Transform);
pub type CameraQueryState<'cc, 't> = QueryState<CameraTuple<'cc, 't>>;
pub type CameraQuery<'world, 'state, 'cc, 't> = Query<'world, 'state, CameraTuple<'cc, 't>>;
pub struct CameraQueryNT<'field, 'world, 'state, 'cc, 't>(pub &'field CameraQuery<'world, 'state, 'cc, 't>);

impl<'field, 'world, 'state, 'cc, 't> CameraQueryNT<'field, 'world, 'state, 'cc, 't> {
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

pub type CameraTupleMut<'cc, 't> = (&'cc CameraComponent, &'t mut Transform);
pub type CameraQueryStateMut<'cc, 't> = QueryState<CameraTupleMut<'cc, 't>>;
pub type CameraQueryMut<'world, 'state, 'cc, 't> = Query<'world, 'state, CameraTupleMut<'cc, 't>>;
pub struct CameraQueryNTMut<'field, 'world, 'state, 'cc, 't>(pub &'field mut CameraQueryMut<'world, 'state, 'cc, 't>);

impl<'field, 'world, 'state, 'cc, 't> CameraQueryNTMut<'field, 'world, 'state, 'cc, 't> {
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

				child_builder.spawn_bundle(DirectionalLightBundle {
					directional_light: DirectionalLight {
						illuminance: 10.0_f32.powf(light_and_camera_data.light_illuminance),
						.. DirectionalLight::default()
					},
					transform: Transform::from_translation(Vec3::from(light_and_camera_data.light_pos))
						.looking_at(Vec3::ZERO, Vec3::Y),
					.. DirectionalLightBundle::default()
				});
				child_builder.spawn_bundle(PerspectiveCameraBundle {
					perspective_projection: light_and_camera_data.persp_proj.clone().into(),
					transform: Transform::from_translation(Vec3::from(light_and_camera_data.camera_pos))
						.looking_at(Vec3::ZERO, Vec3::Y),
					.. PerspectiveCameraBundle::default()
				});
			});
	}

	fn run(
		input_state: Res<InputState>,
		mut camera_query: CameraQueryMut
	) -> () {
		if input_state.has_camera_rotation {
			CameraQueryNTMut(&mut camera_query).orientation(|rotation: Option<&mut Quat>| -> () {
				if let Some(rotation) = rotation {
					*rotation *= input_state.camera_rotation;
				}
			});
		}
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
	fn build(&self, app: &mut App) -> () {
		app
			.add_startup_system(Self::startup.system())
			.add_system(Self::run
				.system()
				.label(STRING_DATA.labels.camera_run.as_ref())
				.after(STRING_DATA.labels.puzzle_run.as_ref())
			);
	}
}