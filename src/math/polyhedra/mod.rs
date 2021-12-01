use {
	bevy_inspector_egui::Inspectable,
	serde::Deserialize
};

pub mod data;
pub mod properties;

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Inspectable, Ord, PartialEq, PartialOrd)]
pub enum Polyhedron {
	Icosahedron,
	Dodecahedron,
	Icosidodecahedron,
	RhombicTriacontahedron
}

impl Polyhedron {
	pub fn dual(&self) -> Self {
		match self {
			Polyhedron::Icosahedron				=> Polyhedron::Dodecahedron,
			Polyhedron::Dodecahedron			=> Polyhedron::Icosahedron,
			Polyhedron::Icosidodecahedron		=> Polyhedron::RhombicTriacontahedron,
			Polyhedron::RhombicTriacontahedron	=> Polyhedron::Icosidodecahedron,
		}
	}
}

impl Default for Polyhedron {
	fn default() -> Self { Self::Icosidodecahedron }
}