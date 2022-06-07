use {
	std::mem::transmute,
	bevy_inspector_egui::Inspectable,
	serde::{
		Deserialize,
		Serialize
	}
};

pub mod data;
pub mod properties;

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Inspectable, Ord, PartialEq, PartialOrd, Serialize)]
#[repr(u8)]
pub enum Polyhedron {
	Icosahedron,
	Dodecahedron,
	Icosidodecahedron,
	RhombicTriacontahedron
}

impl Polyhedron {
	pub fn dual(self) -> Self {
		match self {
			Self::Icosahedron				=> Self::Dodecahedron,
			Self::Dodecahedron				=> Self::Icosahedron,
			Self::Icosidodecahedron			=> Self::RhombicTriacontahedron,
			Self::RhombicTriacontahedron	=> Self::Icosidodecahedron,
		}
	}

	#[inline(always)]
	pub const fn properties(self) -> &'static properties::Properties { properties::Properties::get(self) }
}

impl Default for Polyhedron {
	fn default() -> Self { Self::Icosidodecahedron }
}

pub struct PolyhedronOption(pub Option<Polyhedron>);

impl From<u8> for PolyhedronOption {
	fn from(polyhedron: u8) -> Self {
		Self(if polyhedron <= Polyhedron::RhombicTriacontahedron as u8 {
			Some(unsafe {transmute::<u8, Polyhedron>(polyhedron)})
		} else {
			None
		})
	}
}