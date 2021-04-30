use crate::math;

use super::super::*;
use bevy::math::const_vec3;

#[derive(Clone, Copy, Debug, PartialEq)]
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

pub struct Properties {
	pub polyhedron:		Polyhedron,
	pub vert_count:		usize,
	pub edge_count:		usize,
	pub face_count:		usize,
	pub base_vectors:	&'static [bevy::math::Vec3],
	pub edge_length:	f32
}

impl Properties {
	pub fn get(polyhedron: Polyhedron) -> Option<&'static Self> {
		match polyhedron {
			Polyhedron::Icosahedron				=> Some(&ICOSAHEDRON),
			Polyhedron::Dodecahedron			=> Some(&DODECAHEDRON),
			Polyhedron::Icosidodecahedron		=> Some(&ICOSIDODECAHEDRON),
			Polyhedron::RhombicTriacontahedron	=> Some(&RHOMBIC_TRIACONTAHEDRON),
			_									=> None
		}
	}
}

pub const ICOSAHEDRON: Properties = Properties {
	polyhedron:		Polyhedron::Icosahedron,
	vert_count:		12,
	edge_count:		30,
	face_count:		20,
	base_vectors:	&[
		const_vec3!([
			PHI as f32,
			1.0,
			0.0
		])
	],
	edge_length:	2.0
};

pub const DODECAHEDRON: Properties = Properties {
	polyhedron:		Polyhedron::Dodecahedron,
	vert_count:		ICOSAHEDRON.face_count,
	edge_count:		ICOSAHEDRON.edge_count,
	face_count:		ICOSAHEDRON.vert_count,
	base_vectors:	&[
		const_vec3!([
			PHI as f32,
			0.0,
			ONE_OVER_PHI as f32
		]), const_vec3!([
			1.0,
			1.0,
			1.0
		])
	],
	edge_length:	(2.0 * ONE_OVER_PHI) as f32
};

pub const ICOSIDODECAHEDRON: Properties = Properties {
	polyhedron:		Polyhedron::Icosidodecahedron,
	vert_count:		ICOSAHEDRON.edge_count,
	edge_count:		2 * ICOSAHEDRON.edge_count,
	face_count:		ICOSAHEDRON.face_count + DODECAHEDRON.face_count,
	base_vectors:	&[
		const_vec3!([
			PHI as f32,
			0.0,
			0.0
		]), const_vec3!([
			(0.5 * PHI * PHI) as f32,
			0.5,
			(0.5 * PHI) as f32
		])
	],
	edge_length:	1.0
};

pub const RHOMBIC_TRIACONTAHEDRON: Properties = Properties {
	polyhedron:		Polyhedron::RhombicTriacontahedron,
	vert_count:		ICOSIDODECAHEDRON.face_count,
	edge_count:		ICOSIDODECAHEDRON.edge_count,
	face_count:		ICOSIDODECAHEDRON.vert_count,
	base_vectors:	&[
		ICOSAHEDRON.base_vectors[0],
		DODECAHEDRON.base_vectors[0],
		DODECAHEDRON.base_vectors[1]
	],
	edge_length:	math::const_sqrt_f64(1.0 + ONE_OVER_PHI * ONE_OVER_PHI) as f32
};