use {
	crate::{
		prelude::*,
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		puzzle::consts::*
	},
	bevy::{
		prelude::*,
		input::keyboard::KeyCode
	},
	serde::Deserialize
};

pub fn generate_default_positions() -> [usize; HALF_PENTAGON_PIECE_COUNT] {
	let mut positions: [usize; HALF_PENTAGON_PIECE_COUNT] = [0_usize; HALF_PENTAGON_PIECE_COUNT];
	let icosidodecahedron_data: &Data = match Data::get(Polyhedron::Icosidodecahedron) {
		Some(icosidodecahedron_data) => icosidodecahedron_data,
		None => {
			return positions;
		}
	};
	let face_0_data: &FaceData = &icosidodecahedron_data.faces[0_usize];
	let face_1_norm: Vec3 = icosidodecahedron_data.faces[1_usize].norm;

	for rotation in 3_u32 ..= 7_u32 {
		positions[rotation as usize - 2] = icosidodecahedron_data.get_closest_face_index(&face_0_data.get_rotation_quat(rotation % PENTAGON_SIDE_COUNT as u32).mul_vec3(face_1_norm), None);
	}

	positions
}

define_struct_with_default!(
	#[derive(Deserialize)]
	pub InputData {
		pub default_positions:	[usize; HALF_PENTAGON_PIECE_COUNT]		= generate_default_positions(),
		pub rotation_keys:		[KeyCode; HALF_PENTAGON_PIECE_COUNT]	= [KeyCode::Numpad0, KeyCode::Numpad1, KeyCode::Numpad4, KeyCode::Numpad5, KeyCode::Numpad6, KeyCode::Numpad3],
		pub rotate_twice:		KeyCode									= KeyCode::D,
		pub counter_clockwise:	KeyCode									= KeyCode::S,
		pub alt_hemi:			KeyCode									= KeyCode::A,
	}
);