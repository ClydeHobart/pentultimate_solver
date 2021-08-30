use {
	crate::prelude::*,
	self::camera::{
		CameraComponent,
		CameraPlugin
	},
	bevy::{
		prelude::*,
		input::mouse::{
			MouseMotion,
			MouseWheel
		}
	},
};

mod camera;

#[derive(Clone, Copy)]
pub enum Key {
	Key1,
	Key2,
	Key3,
	Key4,
	Key5,
	Key6
}

pub struct UIPlugin;

impl UIPlugin {
	fn startup_app(
		mut windows: ResMut<Windows>
	) -> () {
		log_option_none!(windows.get_primary_mut()).set_maximized(true);
	}

	fn run_app(
		time: Res<Time>,
		mouse_button_input: Res<Input<MouseButton>>,
		mut mouse_motion_events: EventReader<MouseMotion>,
		mut mouse_wheel_events: EventReader<MouseWheel>,
		mut camera_component_query: Query<(&CameraComponent, &mut Transform)>
	) -> () {
		let time_delta: f32 = time.delta_seconds();
		let mouse_wheel_delta: f32 = mouse_wheel_events
			.iter()
			.map(|mouse_wheel: &MouseWheel| -> f32 {
				mouse_wheel.y
			})
			.sum::<f32>() / time_delta;

		if mouse_wheel_delta.abs() > f32::EPSILON {
			const ROLL_SCALING_FACTOR: f32 = 100.0_f32;

			for (_, mut transform) in camera_component_query.iter_mut() {
				transform.rotate(Quat::from_rotation_z(mouse_wheel_delta / ROLL_SCALING_FACTOR));
			}
		}

		// TODO: account for screen size in mouse wheel/pan scrolling

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
	
				const PAN_SCALING_FACTOR: f32 = 10_000.0_f32;
	
				for (_, mut transform) in camera_component_query.iter_mut() {
					transform.rotate(Quat::from_axis_angle(axis, mouse_motion_delta.length() / PAN_SCALING_FACTOR));
				}
			}
		}
	}
}

impl Plugin for UIPlugin {
	fn build(&self, app: &mut AppBuilder) {
		app
			.insert_resource(Msaa { samples: 4 })
			.insert_resource(WindowDescriptor {
				title: STRING_DATA.misc.app_title.clone(),
				.. WindowDescriptor::default()
			})
			.add_plugin(CameraPlugin)
			.add_startup_system(Self::startup_app.system())
			.add_system(Self::run_app.system());
	}
}