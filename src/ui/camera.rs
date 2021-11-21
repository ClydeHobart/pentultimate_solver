use {
	crate::{
		prelude::*,
		app::prelude::*,
		math::polyhedra::data::{
			Data,
			FaceData
		},
		piece::Type,
		preferences::{
			Preferences,
			Update
		},
		puzzle::{
			consts::PENTAGON_SIDE_COUNT,
			inflated::Animation as PuzzleAnimation,
			transformation::{
				Addr,
				GetWord
			}
		}
	},
	super::{
		input::{
			InputState,
			PendingAction
		},
		View
	},
	std::time::{
		Duration,
		Instant
	},
	bevy::{
		prelude::*,
		input::mouse::{
			MouseMotion,
			MouseWheel
		},
		render::camera::PerspectiveProjection as BevyPerspectiveProjection
	},
	bevy_inspector_egui::Inspectable,
	serde::Deserialize
};

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
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
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub LightAndCameraData {
		pub light_pos:	[f32; 3]				= [0.0_f32, 0.0_f32, 10.0_f32],
		pub camera_pos:	[f32; 3]				= [0.0_f32, 0.0_f32, 10.0_f32],
		#[inspectable(collapse)]
		pub persp_proj:	PerspectiveProjection	= PerspectiveProjection::default(),
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

pub struct Animation {
	pub puzzle_animation: PuzzleAnimation,
	pub start_quat: Quat
}

#[derive(Default)]
pub struct CameraComponent {
	pub animation: Option<Animation>,
	pub prev_addr: Addr
}

pub struct CameraPlugin;

impl CameraPlugin {
	fn startup(
		mut commands: Commands,
		polyhedra_data_library: Res<PolyhedraDataLibrary>,
		preferences: Res<Preferences>
	) -> () {
		commands
			.spawn_bundle((
				CameraComponent::default(),
				Transform::from_rotation(polyhedra_data_library.icosidodecahedron.faces[Type::Pentagon.index_offset()].quat),
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
		view: Res<View>,
		time: Res<Time>,
		mouse_button_input: Res<Input<MouseButton>>,
		transformation_library: Res<TransformationLibraryRef>,
		polyhedra_data_library: Res<PolyhedraDataLibrary>,
		preferences: Res<Preferences>,
		input_state: Res<InputState>,
		mut mouse_motion_events: EventReader<MouseMotion>,
		mut mouse_wheel_events: EventReader<MouseWheel>,
		mut camera_component_query: Query<(&mut CameraComponent, &mut Transform)>
	) -> () {
		if let View::Main = *view {} else { return; }

		let time_delta: f32 = time.delta_seconds();
		let mouse_wheel_delta: f32 = mouse_wheel_events
			.iter()
			.map(|mouse_wheel: &MouseWheel| -> f32 {
				mouse_wheel.y
			})
			.sum::<f32>() / time_delta;
		let (mut camera_component, mut transform): (Mut<CameraComponent>, Mut<Transform>) = log_option_none!(camera_component_query.iter_mut().next());
		let mut rotated_or_panned: bool = false;

		if mouse_wheel_delta.abs() > f32::EPSILON {
			const ROLL_SCALING_FACTOR: f32 = 5_000.0_f32;

			transform.rotate(Quat::from_rotation_z(mouse_wheel_delta / ROLL_SCALING_FACTOR * preferences.speed.roll_speed as f32));
			rotated_or_panned = true;
		}

		if mouse_button_input.pressed(MouseButton::Middle) {
			let mouse_motion_delta: Vec2 = mouse_motion_events
				.iter()
				.map(|mouse_motion: &MouseMotion| -> &Vec2 {
					&mouse_motion.delta
				})
				.sum::<Vec2>() / time_delta;

			if !mouse_motion_delta.abs_diff_eq(Vec2::ZERO, f32::EPSILON) {
				let pan_direction: Vec3 = Vec3::new(mouse_motion_delta.x, -mouse_motion_delta.y, 0.0_f32);
				let axis: Vec3 = pan_direction.cross(Vec3::Z).normalize();

				const PAN_SCALING_FACTOR: f32 = 500_000.0_f32;

				transform.rotate(Quat::from_axis_angle(axis, mouse_motion_delta.length() / PAN_SCALING_FACTOR * preferences.speed.pan_speed as f32));
				rotated_or_panned = true;
			}
		}

		if let Some(animation) = &camera_component.animation {
			if rotated_or_panned {
				camera_component.animation = None;
			} else if warn_expect!(animation.puzzle_animation.addr.is_valid_with_mask(Addr::from((None, Some(0), Some(0))))) {
				let now: Instant = Instant::now();
				let end_quat: Quat = transformation_library.orientation_data.get_word(animation.puzzle_animation.addr).quat;
	
				transform.rotation = if animation.puzzle_animation.is_done_at_time(&now) {
					end_quat
				} else {
					animation.start_quat.short_slerp(end_quat, animation.puzzle_animation.s_at_time(&now))
				};
			}
		} else if matches!(input_state.pending_action, PendingAction::RecenterCamera) {
			camera_component.animation = Some(Animation {
				puzzle_animation: PuzzleAnimation {
					addr: Self::compute_camera_addr(&polyhedra_data_library.icosidodecahedron, &transform.rotation),
					start: Instant::now(),
					duration: Duration::from_millis(preferences.speed.rotation_millis as u64)
				},
				start_quat: transform.rotation
			})
		}
	}

	pub fn compute_camera_addr(icosidodecahedron_data: &Data, quat: &Quat) -> Addr {
		let (line_index, word_index): (usize, usize) = icosidodecahedron_data.get_pos_and_rot(
			&quat,
			Some(Box::new(|face_data: &FaceData| -> bool {
				face_data.get_size() == PENTAGON_SIDE_COUNT
			}))
		);

		Addr::from((None, Some(line_index), Some(word_index)))
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