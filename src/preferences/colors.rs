use {
	crate::{
		prelude::*,
		math::polyhedra::Polyhedron,
		strings::STRING_DATA
	},
	super::Preferences,
	self::traits::*,
	std::collections::HashMap,
	bevy::prelude::*,
	serde::Deserialize
};

pub mod traits {
	use super::ColorData;

	pub trait AsTypeInternal<T> {
		fn as_internal			(self)		-> Self;
	}
	
	pub trait IsTypeInternal<T> where Self: TryGetTypeInternal<T> {
		fn is_internal			(&self)		-> bool																{ self.try_get_internal().is_some() }
	}
	
	pub trait SetTypeInternal<T> where Self: AsTypeInternal<T> + Clone + IsTypeInternal<T> + Sized {
		fn set_internal			(&mut self)	-> ()																{ if !self.is_internal() { *self = self.clone().as_internal(); }}
	}
	
	pub trait TryGetTypeInternal<T> {
		fn try_get_internal		(&self)		-> Option<&ColorData<T>>											{ None }
		fn try_get_mut_internal	(&mut self)	-> Option<&mut ColorData<T>>										{ None }
	}
	
	pub trait AsType where Self: Sized {
		fn as_type<T>			(self)		-> Self							where Self: AsTypeInternal<T>		{ self.as_internal() }
	}
	
	pub trait IsType {
		fn is<T>				(&self)		-> bool							where Self: IsTypeInternal<T>		{ self.is_internal() }
	}
	
	pub trait SetType {
		fn set<T>				(&mut self)	-> ()							where Self: SetTypeInternal<T>		{ self.set_internal() }
	}
	
	pub trait TryGetType {
		fn try_get<T>			(&self)		-> Option<&ColorData<T>>		where Self: TryGetTypeInternal<T>	{ self.try_get_internal() }
		fn try_get_mut<T>		(&mut self)	-> Option<&mut ColorData<T>>	where Self: TryGetTypeInternal<T>	{ self.try_get_mut_internal() }
	}
}

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

#[derive(Clone, Deserialize, Debug)]
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

#[derive(Clone, Deserialize, Debug)]
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
						"FFFFFF", // White
						"0000FF", // Blue
						"80C0FF", // Light Blue
						"808080", // Gray
						"008000", // Dark Green
						"800080", // Purple
						"FF80FF", // Pink
						"80FF00", // Light Green
						"FF0000", // Red
						"FFFFA0", // Light Yellow
						"FFFF00", // Yellow
						"FF8000", // Orange
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

#[derive(Clone, Deserialize, Debug)]
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

#[derive(Clone, Deserialize, Debug)]
pub enum ColorDataTyped {
	Strings(ColorData<String>),
	Colors(ColorData<Color>)
}

impl ColorDataTyped {
	pub fn convert(self) -> Self {
		match self {
			ColorDataTyped::Strings(data) => ColorDataTyped::Colors(ColorData::<Color>::from_alt(data)),
			ColorDataTyped::Colors(data) => ColorDataTyped::Strings(ColorData::<String>::from_alt(data))
		}
	}

	fn startup_app(
		mut preferences: ResMut<Preferences>,
		mut materials: ResMut<Assets<StandardMaterial>>
	) -> () {
		preferences.color.set::<Color>();

		let color_data: &mut ColorData<Color> = preferences.color.try_get_mut::<Color>().unwrap();

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

impl AsTypeInternal<Color> for ColorDataTyped {
	fn as_internal(self) -> Self {
		match self {
			ColorDataTyped::Strings(_) => self.convert(),
			ColorDataTyped::Colors(_) => self
		}
	}
}

impl AsTypeInternal<String> for ColorDataTyped {
	fn as_internal(self) -> Self {
		match self {
			ColorDataTyped::Strings(_) => self,
			ColorDataTyped::Colors(_) => self.convert()
		}
	}
}

impl TryGetTypeInternal<Color> for ColorDataTyped {
	fn try_get_internal(&self) -> Option<&ColorData<Color>> {
		match self {
			ColorDataTyped::Strings(_) => None,
			ColorDataTyped::Colors(colors) => Some(colors)
		}
	}

	fn try_get_mut_internal(&mut self) -> Option<&mut ColorData<Color>> {
		match self {
			ColorDataTyped::Strings(_) => None,
			ColorDataTyped::Colors(colors) => Some(colors)
		}
	}
}

impl TryGetTypeInternal<String> for ColorDataTyped {
	fn try_get_internal(&self) -> Option<&ColorData<String>> {
		match self {
			ColorDataTyped::Strings(strings) => Some(strings),
			ColorDataTyped::Colors(_) => None
		}
	}

	fn try_get_mut_internal(&mut self) -> Option<&mut ColorData<String>> {
		match self {
			ColorDataTyped::Strings(strings) => Some(strings),
			ColorDataTyped::Colors(_) => None
		}
	}
}

impl IsTypeInternal<Color>		for ColorDataTyped {}
impl IsTypeInternal<String>		for ColorDataTyped {}
impl SetTypeInternal<Color>		for ColorDataTyped {}
impl SetTypeInternal<String>	for ColorDataTyped {}

impl AsType						for ColorDataTyped {}
impl IsType						for ColorDataTyped {}
impl SetType					for ColorDataTyped {}
impl TryGetType					for ColorDataTyped {}

impl<T> From<ColorDataTyped> for ColorData<T>
	where
		ColorDataTyped: AsType + AsTypeInternal<T> + TryGetType + TryGetTypeInternal<T>,
		T: Clone
{
	fn from(color_data_typed: ColorDataTyped) -> Self { color_data_typed.as_type::<T>().try_get::<T>().unwrap().clone() }
}

pub struct ColorsPlugin;

impl Plugin for ColorsPlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app
			.add_startup_system(ColorDataTyped::startup_app.system().label(STRING_DATA.labels.color_data_typed_startup.as_ref()));
	}
}