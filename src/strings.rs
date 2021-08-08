use {
	crate::prelude::*,
	serde::Deserialize
};

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Files<String> {
		pub color_data				= "colorData.ron",
		pub light_and_camera_data	= "lightAndCameraData.ron",
		pub piece_library_data		= "pieceLibraryData.ron",
		pub rust_log				= "RUST_LOG.ron",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Labels<String> {
		pub color_data_typed	= "ColorDataTyped::startup_app()",
		pub piece_library		= "PieceLibrary::startup_app()",
		pub puzzle				= "puzzle::startup_app()",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub Misc<String> {
		pub app_title	= "Pentultimate Solver",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub StringData {
		pub files:	Files	= Files::default(),
		pub labels:	Labels	= Labels::default(),
		pub misc:	Misc	= Misc::default(),
	}
);

lazy_static!{
	pub static ref STRING_DATA: StringData = from_ron_or_default("stringData.ron");
}