use {
	crate::prelude::*,
	serde::Deserialize,
	std::concat
};

macro_rules! file_concat { ($first:expr $(, $other:expr)*) => { concat!($first $(, "/", $other)*) } }
macro_rules! root { ($($file:expr)?) => { file_concat!("." $(, $file)?) } }
	macro_rules! assets { ($($file:expr)?) => { file_concat!(root!("assets") $(, $file)?) } }
		macro_rules! config { ($($file:expr),*) => { file_concat!(assets!("config") $(, $file)?) } }
	macro_rules! saves { ($($file:expr)?) => { file_concat!(root!("saves") $(, $file)?) } }

define_struct_with_default!(
	#[derive(Deserialize)]
	pub struct Files<String> {
		pub library						= config!("library.ron"),
		pub preferences					= config!("preferences.ron"),
		pub piece_library_data			= config!("pieceLibraryData.ron"),
		pub rust_log					= config!("rustLog.ron"),
		pub saves						= saves!()
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub struct Labels<String> {
		pub camera_run					= "CameraPlugin::run()",
		pub color_data_startup			= "ColorData::startup()",
		pub input_run					= "InputPlugin::run()",
		pub piece_library_startup		= "PieceLibrary::startup()",
		pub puzzle_run					= "PuzzlePlugin::run()",
		pub puzzle_startup				= "PuzzlePlugin::startup()",
		pub solver_update				= "SolverPlugin::update()",
		pub solver_post_update			= "SolverPlugin::post_update()",
		pub solver_startup				= "SolverPlugin::startup()",
		pub transformation_startup		= "TransformationPlugin::startup()",
		pub ui_startup					= "UIPlugin::startup()",
	}
);

define_struct_with_default!(
	#[derive(Deserialize)]
	pub struct Misc<String> {
		pub app_title					= "Pentultimate Solver"
	}
);

#[cfg(test)]
pub mod test {
	use super::*;

	define_struct_with_default!(
		#[derive(Deserialize)]
		pub struct Test<String> {
			pub reorientation_tests		= config!("reorientationTests.ron")
		}
	);

	impl Test { pub fn default() -> Self { Self::from_file_or_default(config!("testStringData.ron")) } }
}

#[derive(Default, Deserialize)]
pub struct StringData {
	pub files:	Files,
	pub labels:	Labels,
	pub misc:	Misc,

	#[cfg(test)]
	#[serde(skip, default = "test::Test::default")]
	pub tests:	test::Test,
}

lazy_static!{
	pub static ref STRING_DATA: StringData = StringData::from_file_or_default(config!("stringData.ron"));
}