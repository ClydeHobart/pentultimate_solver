use {
	crate::{
		prelude::*,
		puzzle::transformation::{
			Type,
			TYPE_COUNT
		},
		util::inspectable_bit_array::InspectableBitArray,
		impl_deserialize_and_inspectable_for_inspectable_bit_array_wrapper
	},
	bevy::prelude::World,
	bevy_inspector_egui::{
		Context,
		Inspectable
	},
	bit_field::BitArray,
	egui::Ui,
	serde::{
		Deserialize,
		Deserializer
	}
};

#[cfg(debug_assertions)]
use crate::debug::DebugModes;

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

#[derive(Clone, PartialEq)]
pub struct RandomTransformationTypes(pub InspectableBitArray<u16, 1_usize>);

impl RandomTransformationTypes {
	fn bit_index_to_transformation_type() -> Option<Box<dyn Fn(usize) -> String>> {
		Some(Box::new(|bit_index: usize| -> String {
			if let Ok(transformation_type) = if let Ok(bit_index_u8) = u8::try_from(bit_index) {
				Type::try_from(bit_index_u8)
			} else {
				Err(())
			} {
				format!("{:?}", transformation_type)
			} else {
				"[OUT OF RANGE]".into()
			}
		}))
	}
}

impl Default for RandomTransformationTypes {
	fn default() -> Self { Self (InspectableBitArray::<u16, 1_usize>([1_u16 << Type::Simple as usize])) }
}

impl_deserialize_and_inspectable_for_inspectable_bit_array_wrapper!(
	Type,
	RandomTransformationTypes,
	InspectableBitArray<u16, 1_usize>
);

#[derive(Clone, Deserialize, Inspectable, PartialEq)]
pub enum RandomizationType {
	FromCurrent,
	FromSolved,
	FromSolvedNoStack
}

#[derive(Clone, Deserialize, Inspectable, PartialEq)]
pub struct FileMenuData {
	#[inspectable(length = TYPE_COUNT, fetch_label = RandomTransformationTypes::bit_index_to_transformation_type(), collapse)]
	pub random_transformation_types:	RandomTransformationTypes,
	#[inspectable(min = 1, max = 100)]
	pub random_transformation_count:	u8,
	pub randomization_type:				RandomizationType
}

impl Default for FileMenuData {
	fn default() -> Self { Self {
		random_transformation_types:	RandomTransformationTypes::default(),
		random_transformation_count:	30_u8,
		randomization_type:				RandomizationType::FromSolved
	} }
}



define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct AnimationSpeedData {
		#[inspectable(min = 0, max = 2000)]
		pub rotation_millis:					u32		= 250_u32,

		pub uniform_transformation_duration:	bool	= false,

		pub animate_undo_and_redo:				bool	= false
	}
);

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct CameraSpeedData {
		#[inspectable(min = 1, max = 100)]
		pub pan_speed:							u32		= 50_u32,

		#[inspectable(min = 1, max = 100)]
		pub roll_speed:							u32		= 50_u32
	}
);

#[derive(Clone, Default, Deserialize, Inspectable, PartialEq)]
pub struct SpeedData {
	#[inspectable(collapse)]
	pub camera:		CameraSpeedData,

	#[inspectable(collapse)]
	pub animation:	AnimationSpeedData,
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

	#[cfg(debug_assertions)]
	#[inspectable(collapse)]
	#[serde(skip, default = "DebugModes::default")]
	pub debug_modes:		DebugModes,
}

impl Update for Preferences {
	fn update(&self, world: &mut World) -> () {
		self.color.update(world);
		self.light_and_camera.update(world);
	}
}