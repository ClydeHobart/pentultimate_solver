use {
	crate::prelude::*,
	serde::Deserialize
};

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Files<String> {
		pub preferences				= "preferences.ron",
		pub piece_library_data		= "pieceLibraryData.ron",
		pub rust_log				= "RUST_LOG.ron",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Labels<String> {
		pub color_data_typed		= "ColorDataTyped::startup_app()",
		pub piece_library			= "PieceLibrary::startup_app()",
		pub puzzle					= "puzzle::startup_app()",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Misc<String> {
		pub app_title				= "Pentultimate Solver",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Tests<String> {
		pub reorientation_tests		= "reorientationTests.ron",
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