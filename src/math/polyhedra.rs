pub mod properties;

pub mod data;

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum Polyhedron {
	Invalid,
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
			Polyhedron::Invalid					=> Polyhedron::Invalid
		}
	}

	pub fn is_valid(&self) -> bool {
		*self != Polyhedron::Invalid
	}

	pub fn is_invalid(&self) -> bool {
		*self == Polyhedron::Invalid
	}
}

impl Default for Polyhedron {
	fn default() -> Self {
		Self::Invalid
	}
}

impl From<u8> for Polyhedron {
	fn from(val: u8) -> Self {
		unsafe { std::mem::transmute(crate::clamp!(val, 0, Polyhedron::RhombicTriacontahedron as u8)) }
	}
}