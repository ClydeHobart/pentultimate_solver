use {
	crate::prelude::*,
	serde::Deserialize,
	bevy::prelude::World,
	bevy_inspector_egui::Inspectable
};

pub use {
	crate::{
		ui::{
			camera::LightAndCameraData,
			input::InputData
		}
	},
	self::colors::ColorData
};

pub mod colors;

pub trait Update {
	fn update(&self, world: &mut World) -> ();
}

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub SpeedData {
		#[inspectable(min = 125, max = 2000)]
		pub rotation_millis:					u32		= 250_u32,

		#[inspectable(min = 1, max = 100)]
		pub pan_speed:							u32		= 50_u32,

		#[inspectable(min = 1, max = 100)]
		pub roll_speed:							u32		= 50_u32,

		pub uniform_transformation_duration:	bool	= false,
	}
);

#[derive(Clone, Default, Deserialize, Inspectable, PartialEq)]
pub struct Preferences {
	#[inspectable(collapse)]
	pub color:				ColorData,
	#[inspectable(collapse)]
	pub input:				InputData,
	#[inspectable(collapse)]
	pub light_and_camera:	LightAndCameraData,
	#[inspectable(collapse)]
	pub speed:				SpeedData,
}

impl Update for Preferences {
	fn update(&self, world: &mut World) -> () {
		self.color.update(world);
		self.light_and_camera.update(world);
	}
}