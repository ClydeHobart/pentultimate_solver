use crate::math::{
	*,
	polyhedra::{
		data::{
			Data,
			VertexData
		},
		Polyhedron
	}
};

pub enum Type {
	Pentagon,
	Triangle
}

type Tri = [Vec3; 3];

impl From<Type> for Option<gltf_json::Root> {
	fn from(piece_type: Type) -> Self {
		return match piece_type {
			Type::Pentagon => {
				let icosidodecahedron_data:			&Data				= Data::get(Polyhedron::Icosidodecahedron)?;
				let icosidodecahedron_verts:		&Vec<VertexData>	= &icosidodecahedron_data.verts;
				let icosidodecahedron_vert_indices:	&Vec<usize>			= &icosidodecahedron_data.vert_indices;

				let normal:							Vec3				= Data::get(Polyhedron::Icosahedron)?.verts[0].norm;

				let mut vertices: Vec<Vec3> = Vec::with_capacity(/* 5 + 5 + 5 + 1 == */ 16);
				let mut center_sum: Vec3 = Vec3::ZERO;

				for vert_indices_index in icosidodecahedron_data.faces[0].range.clone() {
					let vert: Vec3 = icosidodecahedron_verts[icosidodecahedron_vert_indices[vert_indices_index]].vec;

					center_sum += vert;
					vertices.push(vert);
				}

				for vert_index in 0usize .. 5 {
					vertices.push((vertices[vert_index] + vertices[(vert_index + 1) % 5]) * 0.5f32);
				}

				for vert_index in 0usize .. 5 {
					vertices.push((ONE_OVER_PHI as f32) * vertices[vert_index] + (ONE_OVER_PHI_SQUARED as f32) * vertices[(vert_index + 2) % 5]);
				}

				vertices.push(center_sum * 0.2f32);

				None
			},
			Type::Triangle => {
				None
			}
		}
	}
}