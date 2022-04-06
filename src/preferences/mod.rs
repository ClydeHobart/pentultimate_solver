use {
	crate::{
		prelude::*,
		puzzle::transformation::GenusIndexBitArray,
		tools::ToolsData
	},
	bevy::prelude::World,
	bevy_inspector_egui::Inspectable,
	serde::Deserialize
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

#[derive(Clone, Deserialize, Inspectable, PartialEq)]
pub enum RandomizationType {
	FromCurrent,
	FromSolved,
	FromSolvedNoStack
}

#[derive(Clone, Deserialize, Inspectable, PartialEq)]
pub struct FileMenuData {
	#[inspectable(collapse)]
	pub random_transformation_genera:	GenusIndexBitArray,
	#[inspectable(min = 1_u8, max = 100_u8)]
	pub random_transformation_count:	u8,
	pub randomization_type:				RandomizationType
}

impl Default for FileMenuData {
	fn default() -> Self { Self {
		random_transformation_genera:	GenusIndexBitArray::default(),
		random_transformation_count:	30_u8,
		randomization_type:				RandomizationType::FromSolved
	} }
}



define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct AnimationSpeedData {
		#[inspectable(min = 0_u32, max = 2000_u32)]
		pub rotation_millis:					u32		= 250_u32,

		pub uniform_transformation_duration:	bool	= false,

		pub animate_undo_and_redo:				bool	= false
	}
);

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct CameraSpeedData {
		#[inspectable(min = 1_u32, max = 100_u32)]
		pub pan_speed:							u32		= 50_u32,

		#[inspectable(min = 1_u32, max = 100_u32)]
		pub roll_speed:							u32		= 50_u32
	}
);

#[derive(Clone, Default, Deserialize, Inspectable, PartialEq)]
pub struct SpeedData {
	#[inspectable(collapse)]
	pub camera:							CameraSpeedData,

	#[inspectable(collapse)]
	pub animation:						AnimationSpeedData,

	#[inspectable(min = 1.0_f32, max = 60.0_f32)]
	pub solver_cycle_duration_millis:	f32
}

#[derive(Clone, Default, Deserialize, Inspectable, PartialEq)]
pub struct Preferences {
	#[inspectable(collapse)]
	pub color:				ColorData,
	#[inspectable(collapse)]
	pub file_menu:			FileMenuData,
	#[inspectable(collapse)]
	pub input:				InputData,
	#[inspectable(collapse)]
	pub light_and_camera:	LightAndCameraData,
	#[inspectable(collapse)]
	pub speed:				SpeedData,
	#[inspectable(collapse)]
	pub tools:				ToolsData,
}

impl Update for Preferences {
	fn update(&self, world: &mut World) -> () {
		self.color.update(world);
		self.light_and_camera.update(world);
	}
}