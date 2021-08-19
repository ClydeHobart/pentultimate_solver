use {
	crate::{
		prelude::*,
		get_data,
		colors::{
			ColorDataWithMat,
			ColorData,
			MatHdl
		},
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			properties::ICOSIDODECAHEDRON,
			Polyhedron
		},
		piece::{
			PieceLibrary,
			PiecePair,
			Type
		},
		strings::STRING_DATA
	},
	self::consts::{
		PENTAGON_PIECE_COUNT,
		PENTAGON_SIDE_COUNT,
		PENTAGON_SIDE_COUNT_F32,
		PIECE_COUNT
	},
	std::{
		f32::consts::TAU,
		mem::{
			size_of,
			transmute
		}
	},
	bevy::prelude::*,
	libc::{
		c_void,
		memcmp
	},
	rand::prelude::*
};

pub use {
	deflated::PuzzleState as DeflatedPuzzleState,
	inflated::PuzzleState as InflatedPuzzleState
};

const_assert!(ICOSIDODECAHEDRON.face_count <= u32::BITS as usize);

#[derive(Clone, Copy)]
struct HemisphereMask(u32);

impl HemisphereMask {
	fn new(pentagon: usize) -> Self {
		HemisphereMask({
			if pentagon >= Type::Pentagon.instance_count() {
				log::warn!("HemisphereMask::new({}) was called, but {} is an invalid pentagon index", pentagon, pentagon);

				0_u32
			} else if let Some(data) = Data::get(Polyhedron::Icosidodecahedron) {
				let faces: &Vec<FaceData> = &data.faces;
				let face_normal: Vec3 = faces[pentagon].norm;

				let mut hemisphere_mask: u32 = 0_u32;

				for piece_index in 0_usize .. ICOSIDODECAHEDRON.face_count {
					hemisphere_mask |= ((faces[piece_index].norm.dot(face_normal) > 0.0_f32) as u32) << piece_index;
				}

				hemisphere_mask
			} else {
				log::warn!("Couldn't get Icosidodecahedron Data");

				0_u32
			}
		})
	}

	fn includes_piece(&self, piece_index: usize) -> bool {
		self.0 & (1_u32 << piece_index) != 0_u32
	}
}

mod consts {
	use super::Type;

	pub const PENTAGON_PIECE_COUNT:		usize	= Type::Pentagon.instance_count();				// 12
	pub const TRIANGLE_PIECE_COUNT:		usize	= Type::Triangle.instance_count();				// 20
	pub const PIECE_COUNT:				usize	= PENTAGON_PIECE_COUNT + TRIANGLE_PIECE_COUNT;	// 32
	pub const PENTAGON_SIDE_COUNT:		usize	= Type::Pentagon.side_count();					// 5
	pub const TRIANGLE_SIDE_COUNT:		usize	= Type::Triangle.side_count();					// 3
	pub const PENTAGON_PIECE_COUNT_F32:	f32		= PENTAGON_PIECE_COUNT as f32;
	pub const TRIANGLE_PIECE_COUNT_F32:	f32		= TRIANGLE_PIECE_COUNT as f32;
	pub const PENTAGON_SIDE_COUNT_F32:	f32		= PENTAGON_SIDE_COUNT as f32;
	pub const TRIANGLE_SIDE_COUNT_F32:	f32		= TRIANGLE_SIDE_COUNT as f32;
	pub const PENTAGON_SIDE_COUNT_U16:	u16		= PENTAGON_SIDE_COUNT as u16;
	pub const TRIANGLE_SIDE_COUNT_U16:	u16		= TRIANGLE_SIDE_COUNT as u16;
	pub const PENTAGON_SIDE_COUNT_U8:	u8		= PENTAGON_SIDE_COUNT as u8;
	pub const TRIANGLE_SIDE_COUNT_U8:	u8		= TRIANGLE_SIDE_COUNT as u8;
}

// Compressed version for smaller memory footprint when keeping track of multiple states
pub mod deflated {
	use super::{
		*,
		consts::*
	};

	pub type PieceState = u8;

	#[derive(Debug)]
	#[repr(align(32))]
	pub struct PuzzleState {
		pub pents:	[PieceState; PENTAGON_PIECE_COUNT],
		pub tris:	[PieceState; TRIANGLE_PIECE_COUNT]
	}

	assert_eq_size!(PuzzleState, [PieceState; PIECE_COUNT]);

	impl Default for PuzzleState {
		fn default() -> Self {
			assert_eq_size!(PuzzleState, [u8; PIECE_COUNT]);

			unsafe {
				transmute::<[u8; PIECE_COUNT], Self>([0_u8; PIECE_COUNT])
			}
		}
	}

	impl From<&inflated::PuzzleState> for PuzzleState {
		#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
		fn from(inflated_puzzle_state: &inflated::PuzzleState) -> Self {
			use crate::util::simd;

			crate::util_simd_deflate_deflate_puzzle_state!(inflated_puzzle_state, inflated::PuzzleState, PuzzleState)
		}

		#[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
		fn from(inflated_puzzle_state: &inflated::PuzzleState) -> Self {
			let pos: &inflated::PuzzleStateComponent = &inflated_puzzle_state.pos;
			let rot: &inflated::PuzzleStateComponent = &inflated_puzzle_state.rot;

			let mut deflated_puzzle_state: Self = Self::default();

			for pent_index in 0_usize .. PENTAGON_PIECE_COUNT {
				deflated_puzzle_state.pents[pent_index] = (pos.pents[pent_index] * PENTAGON_SIDE_COUNT as u16 + rot.pents[pent_index]) as u8;
			}

			for tri_index in 0_usize .. TRIANGLE_PIECE_COUNT {
				deflated_puzzle_state.tris[tri_index] = (pos.tris[tri_index] * TRIANGLE_SIDE_COUNT as u16 + rot.tris[tri_index]) as u8;
			}

			deflated_puzzle_state
		}
	}

	impl From<&mut ThreadRng> for PuzzleState {
		fn from(thread_rng: &mut ThreadRng) -> Self {
			let mut puzzle_state: PuzzleState = PuzzleState::default();

			for pent_index in 0_usize .. PENTAGON_PIECE_COUNT {
				puzzle_state.pents[pent_index] = (thread_rng.gen::<f32>() * PENTAGON_PIECE_COUNT_F32) as u8 * PENTAGON_SIDE_COUNT_U8 + (thread_rng.gen::<f32>() * PENTAGON_SIDE_COUNT_F32) as u8;
			}

			for tri_index in 0_usize .. TRIANGLE_PIECE_COUNT {
				puzzle_state.tris[tri_index] = (thread_rng.gen::<f32>() * TRIANGLE_PIECE_COUNT_F32) as u8 * TRIANGLE_SIDE_COUNT_U8 + (thread_rng.gen::<f32>() * TRIANGLE_SIDE_COUNT_F32) as u8;
			}

			puzzle_state
		}
	}

	impl PartialEq for PuzzleState {
		fn eq(&self, other: &Self) -> bool {
			unsafe { memcmp(self as *const Self as *const c_void, other as *const Self as *const c_void, size_of::<Self>()) == 0 }
		}
	}
}

// Decompressed version for utilization of AVX2 SIMD instructions
pub mod inflated {
	use super::{
		*,
		consts::*
	};

	pub type PieceStateComponent = u16;

	#[derive(Debug)]
	#[repr(align(32))]
	pub struct PuzzleStateComponent {
		pub pents:	[PieceStateComponent; PENTAGON_PIECE_COUNT],
		pub tris:	[PieceStateComponent; TRIANGLE_PIECE_COUNT]
	}

	assert_eq_size!(PuzzleStateComponent, [PieceStateComponent; PIECE_COUNT]);

	#[derive(Debug)]
	#[repr(align(32))]
	pub struct PuzzleState {
		pub pos: PuzzleStateComponent,
		pub rot: PuzzleStateComponent
	}

	assert_eq_size!(PuzzleState, [PuzzleStateComponent; 2]);

	impl Default for PuzzleState {
		fn default() -> Self {
			assert_eq_size!(PuzzleState, [u16; 2 * PIECE_COUNT]);

			unsafe {
				transmute::<[u16; 2 * PIECE_COUNT], Self>([0_u16; 2 * PIECE_COUNT])
			}
		}
	}

	impl From<&deflated::PuzzleState> for PuzzleState {
		#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
		fn from(deflated_puzzle_state: &deflated::PuzzleState) -> Self {
			use crate::util::simd;

			crate::util_simd_inflate_inflate_puzzle_state!(deflated_puzzle_state, deflated::PuzzleState, PuzzleState)
		}

		#[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
		fn from(deflated_puzzle_state: &deflated::PuzzleState) -> Self {
			const PENTAGON_SIDE_COUNT_U16: u16 = PENTAGON_SIDE_COUNT as u16;
			const TRIANGLE_SIDE_COUNT_U16: u16 = TRIANGLE_SIDE_COUNT as u16;

			let mut inflated_puzzle_state: Self = Self::default();
			let pos: &mut PuzzleStateComponent = &mut inflated_puzzle_state.pos;
			let rot: &mut PuzzleStateComponent = &mut inflated_puzzle_state.rot;

			for pent_index in 0_usize .. PENTAGON_PIECE_COUNT {
				let deflated_piece_state: u16 = deflated_puzzle_state.pents[pent_index] as u16;
				let pent_pos: u16 = deflated_piece_state / PENTAGON_SIDE_COUNT_U16;

				pos.pents[pent_index] = pent_pos;
				rot.pents[pent_index] = deflated_piece_state - pent_pos * PENTAGON_SIDE_COUNT_U16;
			}

			for tri_index in 0_usize .. TRIANGLE_PIECE_COUNT {
				let deflated_piece_state: u16 = deflated_puzzle_state.tris[tri_index] as u16;
				let tri_pos: u16 = deflated_piece_state / TRIANGLE_SIDE_COUNT_U16;

				pos.tris[tri_index] = tri_pos;
				rot.tris[tri_index] = deflated_piece_state - tri_pos * TRIANGLE_SIDE_COUNT_U16;
			}

			inflated_puzzle_state
		}
	}

	impl From<&mut ThreadRng> for PuzzleState {
		fn from(thread_rng: &mut ThreadRng) -> Self {
			let mut puzzle_state: PuzzleState = PuzzleState::default();

			for pent_index in 0_usize .. PENTAGON_PIECE_COUNT {
				puzzle_state.pos.pents[pent_index] = (thread_rng.gen::<f32>() * PENTAGON_PIECE_COUNT_F32) as u16;
				puzzle_state.rot.pents[pent_index] = (thread_rng.gen::<f32>() * PENTAGON_SIDE_COUNT_F32) as u16;
			}

			for tri_index in 0_usize .. TRIANGLE_PIECE_COUNT {
				puzzle_state.pos.tris[tri_index] = (thread_rng.gen::<f32>() * TRIANGLE_PIECE_COUNT_F32) as u16;
				puzzle_state.rot.tris[tri_index] = (thread_rng.gen::<f32>() * TRIANGLE_SIDE_COUNT_F32) as u16;
			}

			puzzle_state
		}
	}

	impl PartialEq for PuzzleState {
		fn eq(&self, other: &Self) -> bool {
			unsafe { memcmp(self as *const Self as *const c_void, other as *const Self as *const c_void, size_of::<Self>()) == 0 }
		}
	}
}

type Transformation = inflated::PuzzleState;

pub struct TransformationLibrary {
	hemi_masks:			[HemisphereMask; PENTAGON_PIECE_COUNT],
	transformations:	[[Transformation; PENTAGON_SIDE_COUNT]; PENTAGON_PIECE_COUNT]
}

impl TransformationLibrary {
	fn new() -> LogErrorResult<Self> {
		let icosidodecahedron_data: &Data = option_to_result!(Data::get(Polyhedron::Icosidodecahedron))?;

		let mut transformation_library: TransformationLibrary = unsafe { std::mem::MaybeUninit::<TransformationLibrary>::zeroed().assume_init() };

		for transformation_index in 0_usize .. transformation_library.transformations.len() {
			let hemi_mask: HemisphereMask = HemisphereMask::new(transformation_index);
			let transformation_axis: Vec3 = icosidodecahedron_data.faces[transformation_index].norm;

			transformation_library.hemi_masks[transformation_index] = hemi_mask;

			for turn_index in 0_usize .. transformation_library.transformations[transformation_index].len() {
				use inflated::PieceStateComponent;

				const TURN_ANGLE: f32 = TAU / PENTAGON_SIDE_COUNT_F32; // TODO: this is not correct for triangles!

				let transformation_quat: Quat = Quat::from_axis_angle(transformation_axis, turn_index as f32 * -TURN_ANGLE);
				let transformation: &mut Transformation = &mut transformation_library.transformations[transformation_index][turn_index];

				let populate_transformation = |piece_type: Type, pos: &mut [PieceStateComponent], rot: &mut [PieceStateComponent]| -> () {
					for piece_index in 0_usize .. piece_type.instance_count() {
						let offset_piece_index: usize = piece_index + piece_type.index_offset();

						if hemi_mask.includes_piece(offset_piece_index) {
							let result_piece_quat: Quat = transformation_quat * icosidodecahedron_data.faces[offset_piece_index].quat;
							let result_offset_piece_index: usize = icosidodecahedron_data.get_closest_face_index(&(result_piece_quat * Vec3::Z));
							let position: PieceStateComponent = (result_offset_piece_index - piece_type.index_offset()) as PieceStateComponent;
							let result_face: &FaceData = &icosidodecahedron_data.faces[result_offset_piece_index];
							let collapsed_result_y: Vec3 = Mat4::look_at_rh(Vec3::ZERO, -result_face.norm, result_face.norm.cross(result_face.quat * Vec3::Y)).transform_vector3(result_piece_quat * Vec3::Y);
							let rotation: PieceStateComponent = ((TAU - collapsed_result_y.y.atan2(collapsed_result_y.x)) * piece_type.side_count() as f32 / TAU).round() as PieceStateComponent % piece_type.side_count() as PieceStateComponent;

							pos[piece_index] = position;
							rot[piece_index] = rotation;
						} else {
							pos[piece_index] = piece_index as PieceStateComponent;
							rot[piece_index] = 0_u16 /* as PieceStateComponent */;
						}
					}
				};

				populate_transformation(Type::Pentagon, &mut transformation.pos.pents, &mut transformation.rot.pents);
				populate_transformation(Type::Triangle, &mut transformation.pos.tris, &mut transformation.rot.tris);
			}
		}

		Ok(transformation_library)
	}
}

pub struct PuzzlePlugin;

impl PuzzlePlugin {
	fn startup_app(
		mut commands: Commands,
		piece_library: Res<PieceLibrary>,
		color_data: Res<ColorData<Color>>
	) -> () {
		log_result_err!(Self::startup_app_internal(&mut commands, &piece_library, &color_data));
	}

	fn startup_app_internal(
		commands: &mut Commands,
		piece_library: &Res<PieceLibrary>,
		color_data: &Res<ColorData<Color>>
	) -> LogErrorResult {
		let piece_pair: &PiecePair = match piece_library.pieces.get(&piece_library.data.default_design) {
			Some(piece_pair) => piece_pair,
			None => {
				return Err(log_error!(
					Level::Error,
					format!("No PiecePair available for Design {:?}",
						piece_library.data.default_design
					)
				))
			}
		};
		let (base_mat, color_mats): (&MatHdl, &Vec<MatHdl>) = {
			let mats: &ColorDataWithMat<MatHdl> = match color_data.mats.as_ref() {
				Some(mats) => mats,
				None => {
					return Err(log_error!(
						Level::Error,
						format!("ColorData had None for mats")
					));
				}
			};

			(
				&mats.base_color,
				match mats.polyhedron_to_colors.get(&piece_library.data.default_design.as_polyhedron()) {
					Some(color_mats) => color_mats,
					None => {
						return Err(log_error!(
							Level::Error,
							format!("No color materials available for Design {:?}'s Polyhedron, {:?}",
								piece_library.data.default_design,
								piece_library.data.default_design.as_polyhedron()
							)
						));
					}
				}
			)
		};
		let faces: &Vec<FaceData> = &get_data!(Icosidodecahedron).faces;
		let param_bundle: (&MatHdl, &Vec<MatHdl>, &Vec<FaceData>) = (base_mat, color_mats, faces);

		piece_pair.add_entities(commands, &param_bundle);

		Ok(())
	}

}

impl Plugin for PuzzlePlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app.add_startup_system(Self::startup_app
			.system()
			.label(STRING_DATA.labels.puzzle.as_ref())
			.after(STRING_DATA.labels.piece_library.as_ref())
			.after(STRING_DATA.labels.color_data_typed.as_ref())
		);
	}
}

#[cfg(test)]
mod tests {
	use {
		super::{*,
			deflated::PuzzleState as DeflatedPuzzleState,
			inflated::PuzzleState as InflatedPuzzleState,
			consts::*
		},
		std::fmt::Debug
	};

	fn test_conversion<T, U>(original: &T) -> bool
		where
			T: for<'a> From<&'a U> + PartialEq + Debug,
			U: for<'b> From<&'b T> + Debug
	{
		let converted:		U = U::from(original);
		let reconverted:	T = T::from(&converted);

		if *original == reconverted {
			true
		} else {
			error_expr!(original, converted, reconverted);

			false
		}
	}

	#[test]
	fn test_default_conversions() -> () {
		init_log!();

		assert!(test_conversion::<DeflatedPuzzleState, InflatedPuzzleState>(&DeflatedPuzzleState::default()));
		assert!(test_conversion::<InflatedPuzzleState, DeflatedPuzzleState>(&InflatedPuzzleState::default()));
	}

	#[test]
	fn test_base_case_conversions() -> () {
		init_log!();

		let mut deflated_puzzle_state: DeflatedPuzzleState = DeflatedPuzzleState::default();
		let mut inflated_puzzle_state: InflatedPuzzleState = InflatedPuzzleState::default();

		for pent_index in 0_usize .. PENTAGON_PIECE_COUNT {
			let pent_index_u8: u8 = pent_index as u8;
			let rot: u8 = pent_index_u8 % PENTAGON_SIDE_COUNT as u8;

			deflated_puzzle_state.pents[pent_index] = pent_index_u8 * PENTAGON_SIDE_COUNT as u8 + rot;
			inflated_puzzle_state.pos.pents[pent_index] = pent_index_u8 as u16;
			inflated_puzzle_state.rot.pents[pent_index] = rot as u16;
		}

		for tri_index in 0_usize .. TRIANGLE_PIECE_COUNT {
			let tri_index_u8: u8 = tri_index as u8;
			let rot: u8 = tri_index_u8 % TRIANGLE_SIDE_COUNT as u8;

			deflated_puzzle_state.tris[tri_index] = tri_index_u8 * TRIANGLE_SIDE_COUNT as u8 + rot;
			inflated_puzzle_state.pos.tris[tri_index] = tri_index_u8 as u16;
			inflated_puzzle_state.rot.tris[tri_index] = rot as u16;
		}

		assert!(test_conversion::<DeflatedPuzzleState, InflatedPuzzleState>(&deflated_puzzle_state));
		assert!(test_conversion::<InflatedPuzzleState, DeflatedPuzzleState>(&inflated_puzzle_state));
	}

	#[test]
	fn test_random_conversions() -> () {
		const RANDOM_TEST_COUNT: u32 = 500_u32;

		init_log!();

		let mut thread_rng: ThreadRng = rand::thread_rng();

		for _ in 0_u32 .. RANDOM_TEST_COUNT {
			assert!(test_conversion::<DeflatedPuzzleState, InflatedPuzzleState>(&DeflatedPuzzleState::from(&mut thread_rng)));
			assert!(test_conversion::<InflatedPuzzleState, DeflatedPuzzleState>(&InflatedPuzzleState::from(&mut thread_rng)));
		}
	}
}

pub fn main() -> () {
	if let Ok(transformation_library) = TransformationLibrary::new() {
		debug_expr!(transformation_library.transformations[0]);
	}
}