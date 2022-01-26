use {
	crate::prelude::*,
	serde::Deserialize
};

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Files<String> {
		pub preferences					= "preferences.ron",
		pub piece_library_data			= "pieceLibraryData.ron",
		pub rust_log					= "RUST_LOG.ron",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Labels<String> {
		pub camera_run					= "CameraPlugin::run()",
		pub color_data_startup			= "ColorData::startup()",
		pub input_run					= "InputPlugin::run()",
		pub piece_library_startup		= "PieceLibrary::startup()",
		pub puzzle_run					= "PuzzlePlugin::run()",
		pub puzzle_startup				= "PuzzlePlugin::startup()",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Misc<String> {
		pub app_title					= "Pentultimate Solver",
	}
);

#[cfg(debug_assertions)]
pub mod debug {
	use super::*;

	define_struct_with_default!(
		#[derive(Deserialize)]
		pub Debug<String> {
			pub debug_modes				= "debugModes.ron",
		}
	);

	impl Debug {
		pub fn default() -> Self { from_file_or_default("debugStringData.ron") }
	}
}

#[cfg(test)]
pub mod test {
	use super::*;

	define_struct_with_default!(
		#[derive(Deserialize)]
		pub Test<String> {
			pub reorientation_tests		= "reorientationTests.ron",
		}
	);

	impl Test {
		pub fn default() -> Self { from_file_or_default("testStringData.ron") }
	}
}

#[derive(Default, Deserialize)]
pub struct StringData {
	pub files:	Files,
	pub labels:	Labels,
	pub misc:	Misc,

	#[cfg(debug_assertions)]
	#[serde(skip, default = "debug::Debug::default")]
	pub debug:	debug::Debug,

	#[cfg(test)]
	#[serde(skip, default = "test::Test::default")]
	pub tests:	test::Test,
}

lazy_static!{
	pub static ref STRING_DATA: StringData = from_file_or_default("stringData.ron");
}