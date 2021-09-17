use {
	crate::prelude::*,
	serde::Deserialize
};

pub use {
	crate::{
		ui::{
			camera::LightAndCameraData,
			input::InputData
		}
	},
	self::colors::ColorDataTyped
};

pub mod colors;

define_struct_with_default!(
	#[derive(Deserialize)]
	pub SpeedData {
		pub rotation_millis:					u32		= 500_u32,
		pub pan_speed:							u32		= 50_u32,
		pub roll_speed:							u32		= 50_u32,
		pub uniform_transformation_duration:	bool	= false,
	}
);

#[derive(Default, Deserialize)]
pub struct Preferences {
	pub color:				ColorDataTyped,
	pub input:				InputData,
	pub light_and_camera:	LightAndCameraData,
	pub speed:				SpeedData,
}