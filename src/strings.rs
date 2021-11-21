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

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Tests<String> {
		pub reorientation_tests			= "reorientationTests.ron",
	}
);

#[derive(Default, Deserialize)]
pub struct StringData {
	pub files:	Files,
	pub labels:	Labels,
	pub misc:	Misc,

	#[cfg(test)]
	pub tests:	Tests,
}

lazy_static!{
	pub static ref STRING_DATA: StringData = from_ron_or_default("stringData.ron");
}