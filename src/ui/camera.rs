use {
	bevy::{
		ecs::query::WorldQuery,
		prelude::*,
		render::camera::PerspectiveProjection as BevyPerspectiveProjection
	},
	bevy_inspector_egui::Inspectable,
	serde::Deserialize,
	crate::{
		app::prelude::*,
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		piece::{
			consts::*,
			Type
		},
		preferences::{
			Preferences,
			Update
		},
		prelude::*,
		puzzle::transformation::HalfAddr
	},
	super::input::InputState
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
	fn update(&self, other: &Self, world: &mut World, _: &Preferences) -> () {
		if self != other {
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
}

#[derive(Component)]
pub struct CameraComponent;

#[derive(WorldQuery)]
pub struct CameraComponents<'c> {
	pub camera_component:	&'c CameraComponent,
	pub transform:			&'c Transform,
}

pub type CameraQuery<'world, 'state, 'c> = Query<'world, 'state, CameraComponents<'c>>;
pub struct CameraQueryNT<'field, 'world, 'state, 'c>(pub &'field CameraQuery<'world, 'state, 'c>);

impl<'field, 'world, 'state, 'c> CameraQueryNT<'field, 'world, 'state, 'c> {
	pub fn orientation<T, F: FnOnce(Option<&Quat>) -> T>(&self, f: F) -> T {
		match self.0.iter().next() {
			Some(camera_components_item) => {
				f(Some(&camera_components_item.transform.rotation))
			},
			None => {
				f(None)
			}
		}
	}
}

pub type CameraQueryState<'c> = QueryState<CameraComponents<'c>>;
pub struct CameraQueryStateNT<'field, 'c>(pub &'field mut CameraQueryState<'c>);

impl<'field, 'c> CameraQueryStateNT<'field, 'c> {
	pub fn orientation<T, F: FnOnce(Option<&Quat>) -> T>(&mut self, world: &World, f: F) -> T {
		match self.0.iter(world).next() {
			Some(camera_components_item) => {
				f(Some(&camera_components_item.transform.rotation))
			},
			None => {
				f(None)
			}
		}
	}
}

#[derive(WorldQuery)]
#[world_query(mutable)]
pub struct CameraComponentsMut<'c> {
	camera_component:	&'c CameraComponent,
	transform:			&'c mut Transform,
}

pub type CameraQueryMut<'world, 'state, 'c> = Query<'world, 'state, CameraComponentsMut<'c>>;
pub struct CameraQueryMutNT<'field, 'world, 'state, 'c>(pub &'field mut CameraQueryMut<'world, 'state, 'c>);

impl<'field, 'world, 'state, 'c> CameraQueryMutNT<'field, 'world, 'state, 'c> {
	pub fn orientation<T, F: FnOnce(Option<&mut Quat>) -> T>(&mut self, f: F) -> T {
		match self.0.iter_mut().next() {
			Some(mut camera_components_mut_item) => {
				f(Some(&mut camera_components_mut_item.transform.rotation))
			},
			None => {
				f(None)
			}
		}
	}
}

pub type CameraQueryMutState<'c> = QueryState<CameraComponentsMut<'c>>;
pub struct CameraQueryMutStateNT<'field, 'c>(pub &'field mut CameraQueryMutState<'c>);

impl<'field, 'c> CameraQueryMutStateNT<'field, 'c> {
	pub fn orientation<T, F: FnOnce(Option<&mut Quat>) -> T>(&mut self, world: &mut World, f: F) -> T {
		match self.0.iter_mut(world).next() {
			Some(mut camera_components_mut_item) => {
				f(Some(&mut camera_components_mut_item.transform.rotation))
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
			CameraQueryMutNT(&mut camera_query).orientation(|rotation: Option<&mut Quat>| -> () {
				if let Some(rotation) = rotation {
					*rotation *= input_state.camera_rotation;
				}
			});
		}
	}

	pub fn compute_camera_addr(quat: &Quat) -> HalfAddr {
		Data::get(Polyhedron::Icosidodecahedron).get_pos_and_rot(
			&quat,
			Some(&|face_index: usize, _: &FaceData| -> bool {
				PENTAGON_PIECE_RANGE.contains(&face_index)
			})
		).into()
	}
}

impl Plugin for CameraPlugin {
	fn build(&self, app: &mut App) -> () {
		app
			.add_startup_system(Self::startup.after(PolyhedraDataPlugin::startup))
			.add_system(Self::run.after(PuzzlePlugin::run));
	}
}