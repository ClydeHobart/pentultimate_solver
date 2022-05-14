use {
	bevy::prelude::World,
	bevy_inspector_egui::Inspectable,
	serde::Deserialize,
	crate::{
		prelude::*,
		puzzle::transformation::GenusIndex
	},
	self::prelude::*
};

pub mod colors;

pub mod prelude {
	pub use {
		crate::{
			piece::Design,
			puzzle::transformation::GenusIndexBitArray,
			tools::ToolsData,
			ui::{
				camera::LightAndCameraData,
				input::InputData
			}
		},
		super::{
			colors::{
				ColAndMat,
				ColorData,
				ColorDataWithMat
			},
			FileMenuData,
			Preferences,
			RandomizationParams,
			RandomizationType,
			Update
		}
	};
}

pub trait Update: PartialEq + Sized {
	fn update(&self, other: &Self, world: &mut World, preferences: &Preferences) -> ();
}

#[derive(Clone, Deserialize, Inspectable, PartialEq)]
pub enum RandomizationType {
	FromCurrent,
	FromSolved,
	FromSolvedNoStack
}

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct RandomizationParams {
		#[inspectable(collapse)]
		pub random_transformation_genera:	GenusIndexBitArray	=
			GenusIndexBitArray::from([GenusIndex::SIMPLE].as_slice()),
		#[inspectable(min = 1_u8, max = 100_u8)]
		pub random_transformation_count:	u8					= 30_u8,
		pub randomization_type:				RandomizationType	= RandomizationType::FromSolved
	}
);

#[derive(Clone, Default, Deserialize, Inspectable, PartialEq)]
pub struct FileMenuData {
	pub randomization_params: RandomizationParams
}

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct PuzzleData {
		pub color:	ColorData	= ColorData::default(),
		pub design:	Design		= Design::CustomSuperDodecahedron
	}
);

impl Update for PuzzleData {
	fn update(&self, other: &Self, world: &mut World, preferences: &Preferences) -> () {
		self.color.colors_with_mat.update(&other.color.colors_with_mat, world, preferences);
		self.design.update(&other.design, world, preferences);
	}
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
		pub pan_speed:	u32	= 50_u32,

		#[inspectable(min = 1_u32, max = 100_u32)]
		pub roll_speed:	u32	= 50_u32
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
	pub file_menu:			FileMenuData,
	#[inspectable(collapse)]
	pub input:				InputData,
	#[inspectable(collapse)]
	pub light_and_camera:	LightAndCameraData,
	#[inspectable(collapse)]
	pub puzzle:				PuzzleData,
	#[inspectable(collapse)]
	pub speed:				SpeedData,
	#[inspectable(collapse)]
	pub tools:				ToolsData,
}

impl Update for Preferences {
	fn update(&self, other: &Self, world: &mut World, preferences: &Preferences) -> () {
		self.light_and_camera.update(&other.light_and_camera, world, preferences);
		self.puzzle.update(&other.puzzle, world, preferences);
	}
}