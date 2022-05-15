use {
	bevy::{
		prelude::*,
		render::color::{
			Color as BevyColor,
			HexColorError
		}
	},
	bevy_inspector_egui::{
		Context,
		Inspectable
	},
	egui::{
		Grid,
		Ui
	},
	serde::{
		de::{
			Error,
			Visitor
		},
		Deserialize,
		Deserializer
	},
	crate::{
		math::polyhedra::Polyhedron,
		preferences::Update,
		prelude::*,
		ui::UIPlugin,
		util::inspectable_bin_map::*
	},
	super::Preferences
};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Color(pub BevyColor);

impl Color {
	fn as_rgba_f32s(&self) -> [f32; 4] {
		self.0.as_rgba_f32()
	}

	fn as_rgba_u8s(&self) -> [u8; 4] {
		let rgba_f32s: [f32; 4] = self.as_rgba_f32s();
		let mut rgba_u8s: [u8; 4] = [0_u8; 4];

		for (index, rgba_f32) in rgba_f32s.iter().enumerate() {
			rgba_u8s[index] = (rgba_f32 * u8::MAX as f32).round() as u8;
		}

		rgba_u8s
	}
}

struct ColorVisitor;

impl<'de> Visitor<'de> for ColorVisitor {
	type Value = BevyColor;

	fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(formatter, "a hex string in \"RGB\", \"RGBA\", \"RRGGBB\", or \"RRGGBBAA\" format in sRGB colorspace")
	}

	fn visit_str<E: Error>(self, v: &str) -> Result<Self::Value, E> {
		BevyColor::hex(v).map_err(|hex_color_error: HexColorError| -> E {
			E::custom(format!("{:?}", hex_color_error))
		})
	}
}

impl<'de> Deserialize<'de> for Color {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		deserializer.deserialize_str(ColorVisitor).map(|bevy_color: BevyColor| -> Self {
			Self(bevy_color)
		})
	}
}

impl From<&str> for Color {
	fn from(v: &str) -> Self {
		Self(BevyColor::hex(v).unwrap_or(BevyColor::default()))
	}
}

impl From<BevyColor> for Color {
	fn from(bevy_color: BevyColor) -> Self { Self(bevy_color) }
}

impl From<Color> for u32 {
	fn from(color: Color) -> Self {
		u32::from_be_bytes(color.as_rgba_u8s())
	}
}

impl Inspectable for Color {
	type Attributes = ();

	fn ui(&mut self, ui: &mut Ui, _: Self::Attributes, _: &mut Context) -> bool {
		let mut rgba_u8s: [u8; 4] = self.as_rgba_u8s();
		let changed: bool = ui.color_edit_button_srgba_unmultiplied(&mut rgba_u8s).changed();

		if changed {
			self.0 = BevyColor::rgba_u8(rgba_u8s[0], rgba_u8s[1], rgba_u8s[2], rgba_u8s[3]);
		}

		changed
	}
}

#[derive(Clone, Debug, Default, Deserialize, Inspectable, PartialEq)]
struct ColorDataWithoutMat {}

pub type MatHdl = Handle<StandardMaterial>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ColAndMat {
	pub col: Color,
	pub mat: MatHdl
}

impl ColAndMat {
	fn set_hdl(&mut self, materials: &mut Assets<StandardMaterial>) -> () {
		self.mat = materials.add(StandardMaterial {
			base_color: self.col.0,
			perceptual_roughness: 0.5_f32,
			reflectance: 0.35_f32,
			.. StandardMaterial::default()
		});
	}
}

impl<'de> Deserialize<'de> for ColAndMat {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		deserializer.deserialize_str(ColorVisitor).map(|bevy_color: BevyColor| -> Self {
			Self {
				col: bevy_color.into(),
				mat: MatHdl::default()
			}
		})
	}
}

impl From<Color> for ColAndMat {
	fn from(col: Color) -> Self {
		Self {
			col,
			.. Self::default()
		}
	}
}

impl Inspectable for ColAndMat {
	type Attributes = <Color as Inspectable>::Attributes;

	fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, context: &mut Context) -> bool {
		self.col.ui(ui, options, context)
	}
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
pub struct ColorDataWithMat {
	pub polyhedron_to_colors: Vec<(Polyhedron, Vec<ColAndMat>)>,
	pub base_color: ColAndMat
}

impl Default for ColorDataWithMat {
	fn default() -> Self {
		ColorDataWithMat {
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
						.map(|slice: &&str| -> ColAndMat { Color::from(*slice).into() })
						.collect()
				)
			]
				.iter()
				.cloned()
				.collect(),
			base_color: Color::from("000000").into()
		}
	}
}

impl Inspectable for ColorDataWithMat {
	type Attributes = ();

	fn ui(&mut self, ui: &mut Ui, _: Self::Attributes, context: &mut Context) -> bool {
		let mut changed: bool = false;

		ui.vertical_centered(|ui: &mut Ui| -> () {
			Grid::new(context.id()).show(ui, |ui: &mut Ui| -> () {
				ui.label("polyhedron_to_colors");

				let mut inspectable_bin_map: InspectableBinMapMut<(Polyhedron, Vec<ColAndMat>)> =
					self.polyhedron_to_colors.as_inspectable_bin_map_mut();

				inspectable_bin_map.ui(ui, Default::default(), &mut context.with_id(0_u64));
				ui.end_row();
				ui.label("base_color");
				changed |= <ColAndMat as Inspectable>::ui(
					&mut self.base_color,
					ui,
					Default::default(),
					&mut context.with_id(1_u64)
				);
				ui.end_row();
			});
		});

		changed
	}
}

impl Update for ColorDataWithMat {
	fn update(&self, other: &Self, world: &mut World, _: &Preferences) -> () {
		if self != other {
			let mut mat_assets: Mut<Assets<StandardMaterial>> = warn_expect_some!(
				world.get_resource_mut::<Assets<StandardMaterial>>(),
				return
			);
	
			for (_, col_and_mats) in &self.polyhedron_to_colors {
				for col_and_mat in col_and_mats {
					if let Some(mat) = mat_assets.get_mut(&col_and_mat.mat) {
						mat.base_color = col_and_mat.col.0;
					}
				}
			}
	
			if let Some(base_mat) = mat_assets.get_mut(&self.base_color.mat) {
				base_mat.base_color = self.base_color.col.0;
			}
		}
	}
}

#[derive(Clone, Debug, Default, Deserialize, Inspectable, PartialEq)]
pub struct ColorData {
	#[inspectable(collapse)]
	colors_without_mat: ColorDataWithoutMat,
	#[inspectable(collapse)]
	pub colors_with_mat: ColorDataWithMat
}

impl ColorData {
	fn startup_app(
		mut preferences: ResMut<Preferences>,
		mut materials: ResMut<Assets<StandardMaterial>>
	) -> () {
		let colors_with_mat: &mut ColorDataWithMat = &mut preferences.puzzle.color.colors_with_mat;
		let materials: &mut Assets<StandardMaterial> = &mut *materials;

		for (_, col_and_hdls) in &mut colors_with_mat.polyhedron_to_colors {
			for col_and_hdl in col_and_hdls {
				col_and_hdl.set_hdl(materials);
			}
		}

		colors_with_mat.base_color.set_hdl(materials);
	}
}

pub struct ColorsPlugin;

impl Plugin for ColorsPlugin {
	fn build(&self, app: &mut App) -> () {
		app
			.add_startup_system(ColorData::startup_app.after(UIPlugin::startup));
	}
}