use {
	crate::{
		prelude::*,
		math::polyhedra::Polyhedron
	},
	std::collections::HashMap,
	bevy::prelude::*,
	serde::Deserialize
};

impl FromAlt<&Color> for String {
	fn from_alt(color: &Color) -> String {
		let rgba_f32s: [f32; 4] = color.as_rgba_f32();
		let mut rgba_u8s: [u8; 4] = [0_u8; 4];

		for (index, rgba_f32) in rgba_f32s.iter().enumerate() {
			rgba_u8s[index] = (rgba_f32 * u8::MAX as f32).round() as u8;
		}

		let rgba_u32: u32 = u32::from_be_bytes(rgba_u8s);

		if rgba_u32 & u8::MAX as u32 == u8::MAX as u32 {
			format!("{:06X}", rgba_u32 >> u8::BITS)
		} else {
			format!("{:08X}", rgba_u32)
		}
	}
}

impl FromAlt<&String> for Color {
	fn from_alt(string: &String) -> Self {
		Color::hex(string.as_str()).unwrap_or(Default::default())
	}
}

#[derive(Deserialize, Debug)]
struct ColorDataWithoutMat<T> {

	#[serde(skip)]
	phantom: std::marker::PhantomData<T>
}

impl<T, U> FromAlt<ColorDataWithoutMat<U>> for ColorDataWithoutMat<T> {
	fn from_alt(_: ColorDataWithoutMat<U>) -> Self {
		Default::default()
	}
}

impl<T> Default for ColorDataWithoutMat<T> {
	fn default() -> Self {
		ColorDataWithoutMat::<T> { phantom: Default::default() }
	}
}

#[derive(Deserialize, Debug)]
pub struct ColorDataWithMat<T> {
	pub polyhedron_to_colors: HashMap<Polyhedron, Vec<T>>,
	pub base_color: T
}

impl Default for ColorDataWithMat<String> {
	fn default() -> Self {
		ColorDataWithMat::<String> {
			polyhedron_to_colors: [
				(
					Polyhedron::Dodecahedron,
					vec![
						"FFFFFF",
						"C0C0C0",
						"FF0000",
						"FF8000",
						"0000FF",
						"80C0FF",
						"008000",
						"80FF00",
						"FFFF00",
						"FFFFA0",
						"800080",
						"FF80FF"
					]
						.iter()
						.map(|slice: &&str| -> String { (*slice).into() })
						.collect()
				)
			]
				.iter()
				.cloned()
				.collect(),
			base_color: "000000".into()
		}
	}
}

impl<T, U> FromAlt<ColorDataWithMat<U>> for ColorDataWithMat<T>
	where
		T: for<'a> FromAlt<&'a U>
{
	fn from_alt(data: ColorDataWithMat<U>) -> Self {
		ColorDataWithMat::<T> {
			polyhedron_to_colors: data.polyhedron_to_colors
				.iter()
				.map(|(polyhedron, colors)| -> (Polyhedron, Vec<T>) {
					(
						*polyhedron,
						colors
							.iter()
							.map(|color: &U| -> T {
								T::from_alt(color)
							})
							.collect()
					)
				})
				.collect(),
			base_color: T::from_alt(&data.base_color)
		}
	}
}

pub type MatHdl = Handle<StandardMaterial>;

#[derive(Deserialize, Debug)]
pub struct ColorData<T> {
	colors_without_mat: ColorDataWithoutMat<T>,
	pub colors_with_mat: ColorDataWithMat<T>,

	#[serde(skip)]
	pub mats: Option<ColorDataWithMat<MatHdl>>
}

impl<T, U> FromAlt<ColorData<U>> for ColorData<T>
	where
		T : for<'a> FromAlt<&'a U>
{
	fn from_alt(data: ColorData<U>) -> Self {
		ColorData::<T> {
			colors_without_mat: ColorDataWithoutMat::<T>::from_alt(data.colors_without_mat),
			colors_with_mat: ColorDataWithMat::<T>::from_alt(data.colors_with_mat),
			mats: None
		}
	}
}

#[derive(Deserialize, Debug)]
pub enum ColorDataTyped {
	Strings(ColorData<String>),
	Colors(ColorData<Color>)
}

impl Default for ColorDataTyped {
	fn default() -> Self {
		Self::Colors(ColorData::<Color>::from_alt(
			ColorData::<String> {
				colors_without_mat: Default::default(),
				colors_with_mat: Default::default(),
				mats: None
			}
		))
	}
}

impl ColorDataTyped {
	pub fn convert(self) -> Self {
		match self {
			ColorDataTyped::Strings(data) => ColorDataTyped::Colors(ColorData::<Color>::from_alt(data)),
			ColorDataTyped::Colors(data) => ColorDataTyped::Strings(ColorData::<String>::from_alt(data))
		}
	}

	pub fn as_strings(self) -> Self {
		match self {
			ColorDataTyped::Strings(_) => self,
			ColorDataTyped::Colors(_) => self.convert()
		}
	}

	pub fn as_colors(self) -> Self {
		match self {
			ColorDataTyped::Strings(_) => self.convert(),
			ColorDataTyped::Colors(_) => self
		}
	}

	pub fn try_get_strings(&self) -> Option<&ColorData<String>> {
		match self {
			ColorDataTyped::Strings(strings) => Some(strings),
			ColorDataTyped::Colors(_) => None
		}
	}

	pub fn try_get_colors(&self) -> Option<&ColorData<Color>> {
		match self {
			ColorDataTyped::Strings(_) => None,
			ColorDataTyped::Colors(colors) => Some(colors)
		}
	}

	fn startup_app(
		mut color_data: ResMut<ColorData<Color>>,
		mut materials: ResMut<Assets<StandardMaterial>>
	) -> () {
		color_data.mats = Some(
			ColorDataWithMat::<Handle<StandardMaterial>> {
				polyhedron_to_colors: color_data.colors_with_mat.polyhedron_to_colors
					.iter()
					.map(|(polyhedron, colors)| -> (Polyhedron, Vec<MatHdl>) {
						(
							*polyhedron,
							colors
								.iter()
								.map(|color| -> MatHdl { materials.add((*color).into()) })
								.collect()
						)
					})
					.collect(),
				base_color: materials.add(color_data.colors_with_mat.base_color.into())
			}
		);
	}

	fn build_app(app_builder: &mut AppBuilder) -> () {
		const COLOR_DATA_FILE: &str = "colorData.ron";

		app_builder
			.insert_resource::<ColorData<Color>>(log_error_result!(from_ron::<ColorDataTyped>(COLOR_DATA_FILE), ColorDataTyped::default()).into())
			.add_startup_system(ColorDataTyped::startup_app.system().label("ColorDataTyped::startup_app()"));
	}
}

impl From<ColorDataTyped> for ColorData<String> {
	fn from(color_data_typed: ColorDataTyped) -> Self {
		match color_data_typed.as_strings() {
			ColorDataTyped::Strings(color_data) => color_data,
			ColorDataTyped::Colors(_) => { panic!(); }
		}
	}
}

impl From<ColorDataTyped> for ColorData<Color> {
	fn from(color_data_typed: ColorDataTyped) -> Self {
		match color_data_typed.as_colors() {
			ColorDataTyped::Strings(_) => { panic!(); },
			ColorDataTyped::Colors(color_data) => color_data,
		}
	}
}

pub fn build_app(app_builder: &mut AppBuilder) -> () {
	ColorDataTyped::build_app(app_builder);
}