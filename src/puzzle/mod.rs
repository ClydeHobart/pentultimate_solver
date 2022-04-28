use {
	std::{
		fmt::{
			Debug,
			Error,
			Formatter,
			Write
		},
		mem::{
			size_of,
			take,
			transmute
		},
		ops::{
			Add,
			AddAssign,
			Range,
			Sub
		}
	},
	bevy::{
		app::PluginGroupBuilder,
		prelude::*
	},
	bevy_inspector_egui::Inspectable,
	libc::{
		c_void,
		memcmp
	},
	rand::prelude::*,
	serde::{
		Deserialize,
		Serialize
	},
	crate::{
		app::prelude::*,
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		piece::{
			PieceLibrary,
			PiecePair,
			Type
		},
		preferences::{
			colors::{
				ColAndMat,
				ColorDataWithMat
			},
			Preferences,
			RandomizationType
		},
		prelude::*,
		util::inspectable_bin_map::*,
		max
	},
	self::{
		consts::*,
		transformation::{
			Action,
			Addr,
			FullAddr,
			GenusIndexBitArray,
			HalfAddr,
			Library,
			Transformation
		}
	}
};

pub use {
	deflated::PuzzleState as DeflatedPuzzleState,
	inflated::{
		ExtendedPuzzleState,
		PieceStateComponent as InflatedPieceStateComponent,
		PuzzleState as InflatedPuzzleState,
		PuzzleStateComponent as InflatedPuzzleStateComponent
	}
};

pub mod explorer;
pub mod solver;
pub mod tools;
pub mod transformation;

pub mod consts {
	use {
		super::{
			inflated::PieceStateComponent as IPSC,
			Type,
			max
		},
		std::ops::Range
	};

	pub trait PolygonVertexCounts: Sized{
		const PENTAGON_VERTEX_COUNT: Self;
		const TRIANGLE_VERTEX_COUNT: Self;
	}

	impl PolygonVertexCounts for usize {
		const PENTAGON_VERTEX_COUNT: Self = Type::Pentagon.vertex_count();
		const TRIANGLE_VERTEX_COUNT: Self = Type::Triangle.vertex_count();
	}

	impl PolygonVertexCounts for f32 {
		const PENTAGON_VERTEX_COUNT: Self = Type::Pentagon.vertex_count() as Self;
		const TRIANGLE_VERTEX_COUNT: Self = Type::Triangle.vertex_count() as Self;
	}

	impl PolygonVertexCounts for IPSC {
		const PENTAGON_VERTEX_COUNT: Self = Type::Pentagon.vertex_count() as Self;
		const TRIANGLE_VERTEX_COUNT: Self = Type::Triangle.vertex_count() as Self;
	}

	pub const PENTAGON_PIECE_COUNT:			usize			= Type::Pentagon.instance_count();				// 12
	pub const TRIANGLE_PIECE_COUNT:			usize			= Type::Triangle.instance_count();				// 20
	pub const PIECE_COUNT:					usize			= PENTAGON_PIECE_COUNT + TRIANGLE_PIECE_COUNT;	// 32
	pub const PENTAGON_INDEX_OFFSET:		usize			= Type::Pentagon.index_offset();				// 0
	pub const TRIANGLE_INDEX_OFFSET:		usize			= Type::Triangle.index_offset();				// 12
	pub const ROTATION_BIT_COUNT:			u32				= usize::BITS - max!(usize::PENTAGON_VERTEX_COUNT, usize::TRIANGLE_VERTEX_COUNT).leading_zeros(); // 3
	pub const ROTATION_BIT_MASK:			IPSC			= ((1 as IPSC) << ROTATION_BIT_COUNT) - 1;		// 0b111
	pub const PENTAGON_PIECE_COUNT_F32:		f32				= PENTAGON_PIECE_COUNT as f32;
	pub const TRIANGLE_PIECE_COUNT_F32:		f32				= TRIANGLE_PIECE_COUNT as f32;
	pub const PIECE_COUNT_F32:				f32				= PENTAGON_PIECE_COUNT_F32 + TRIANGLE_PIECE_COUNT_F32;
	pub const ZERO_IPSC:					IPSC			= 0 as IPSC;
	pub const PENTAGON_PIECE_RANGE:			Range<usize>	= PENTAGON_INDEX_OFFSET .. PENTAGON_INDEX_OFFSET + PENTAGON_PIECE_COUNT;
	pub const TRIANGLE_PIECE_RANGE:			Range<usize>	= TRIANGLE_INDEX_OFFSET .. TRIANGLE_INDEX_OFFSET + TRIANGLE_PIECE_COUNT;
	pub const PIECE_RANGE:					Range<usize>	= 0_usize .. PIECE_COUNT;
	pub const HALF_PENTAGON_PIECE_COUNT:	usize			= PENTAGON_PIECE_COUNT / 2;
}

// Compressed version for smaller memory footprint when keeping track of multiple states
pub mod deflated {
	use super::*;

	pub type PieceState = u8;

	#[derive(Debug, Eq, Hash)]
	#[repr(align(32))]
	pub struct PuzzleState {
		pub pieces: [PieceState; PIECE_COUNT]
	}

	impl PuzzleState {
		const SOLVED_STATE: PuzzleState = unsafe { transmute::<[u128; 2_usize], PuzzleState>([
			0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01_00_u128 << ROTATION_BIT_COUNT,
			0x1F_1E_1D_1C_1B_1A_19_18_17_16_15_14_13_12_11_10_u128 << ROTATION_BIT_COUNT
		]) };

		pub fn naive_deflation(inflated_puzzle_state: &inflated::PuzzleState) -> Self {
			Self::naive_deflation_inline(inflated_puzzle_state)
		}

		#[inline]
		fn naive_deflation_inline(inflated_puzzle_state: &inflated::PuzzleState) -> Self {
			let pos: &inflated::PuzzleStateComponent = &inflated_puzzle_state.pos;
			let rot: &inflated::PuzzleStateComponent = &inflated_puzzle_state.rot;

			let mut deflated_puzzle_state: Self = Self::default();

			for piece_index in PIECE_RANGE {
				deflated_puzzle_state.pieces[piece_index] = (pos[piece_index] << ROTATION_BIT_COUNT | rot[piece_index]) as PieceState;
			}

			deflated_puzzle_state
		}
	}

	impl Default for PuzzleState { fn default() -> Self { Self::SOLVED_STATE } }

	impl From<&inflated::PuzzleState> for PuzzleState {
		#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
		fn from(inflated_puzzle_state: &inflated::PuzzleState) -> Self {
			use crate::util::simd;

			type InflatedPuzzleState = inflated::PuzzleState;

			crate::util_simd_deflated_deflate_puzzle_state!(inflated_puzzle_state, InflatedPuzzleState, PuzzleState)
		}

		#[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
		fn from(inflated_puzzle_state: &inflated::PuzzleState) -> Self {
			Self::naive_deflation_inline(inflated_puzzle_state)
		}
	}

	impl From<&mut ThreadRng> for PuzzleState {
		fn from(thread_rng: &mut ThreadRng) -> Self {
			let mut puzzle_state: PuzzleState = PuzzleState::default();

			for pent_index in PENTAGON_PIECE_RANGE {
				puzzle_state.pieces[pent_index] = ((thread_rng.gen::<f32>() * PENTAGON_PIECE_COUNT_F32) as PieceState)
					<< ROTATION_BIT_COUNT
					| (thread_rng.gen::<f32>() * f32::PENTAGON_VERTEX_COUNT)
					as PieceState;
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				puzzle_state.pieces[tri_index] = ((thread_rng.gen::<f32>() * TRIANGLE_PIECE_COUNT_F32) as PieceState)
					<< ROTATION_BIT_COUNT
					| (thread_rng.gen::<f32>() * f32::TRIANGLE_VERTEX_COUNT)
					as PieceState;
			}

			puzzle_state
		}
	}

	impl FromAlt<&inflated::PuzzleState> for PuzzleState {
		fn from_alt(inflated_puzzle_state: &inflated::PuzzleState) -> Self {
			Self::naive_deflation(inflated_puzzle_state)
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
	use super::*;

	pub type PieceStateComponent = u32;

	pub type PuzzleStateComponent = [PieceStateComponent; PIECE_COUNT];
	pub type PosAndRot<'p, 'r> = (&'p PuzzleStateComponent, &'r PuzzleStateComponent);
	pub type MutPosAndRot<'p, 'r> = (&'p mut PuzzleStateComponent, &'r mut PuzzleStateComponent);

	#[derive(Clone, Deserialize, Serialize)]
	#[repr(align(32))]
	pub struct PuzzleState {
		pub pos: PuzzleStateComponent,	// An array representing the current position of the piece with the given index
		pub rot: PuzzleStateComponent	// An array representing the cumulative rotation of the piece with the given index
	}

	assert_eq_size!(PuzzleState, [PuzzleStateComponent; 2]);

	macro_rules! puzzle_state_add {
		($src_state:ident, $transformation:ident, $dest_state:ident) => {
			use PieceStateComponent as PSC;

			// let transformation_pos: &PuzzleStateComponent = &$transformation.pos;
			// let transformation_rot: &PuzzleStateComponent = &$transformation.rot;
			let (transformation_pos, transformation_rot) = $transformation.arrays();

			for pent_index in PENTAGON_PIECE_RANGE {
				let curr_pos: usize = $src_state.pos[pent_index] as usize;

				$dest_state.pos[pent_index] = transformation_pos[curr_pos];
				$dest_state.rot[pent_index] = {
					let rot_sum: PSC = $src_state.rot[pent_index] + transformation_rot[curr_pos];

					if rot_sum >= PSC::PENTAGON_VERTEX_COUNT {
						rot_sum - PSC::PENTAGON_VERTEX_COUNT
					} else {
						rot_sum
					}
				};
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				let curr_pos: usize = $src_state.pos[tri_index] as usize;

				$dest_state.pos[tri_index] = transformation_pos[curr_pos];
				$dest_state.rot[tri_index] = {
					let rot_sum: PieceStateComponent = $src_state.rot[tri_index] + transformation_rot[curr_pos];

					if rot_sum >= PSC::TRIANGLE_VERTEX_COUNT {
						rot_sum - PSC::TRIANGLE_VERTEX_COUNT
					} else {
						rot_sum
					}
				};
			}
		};
	}

	impl PuzzleState {
		const SOLVED_COMPONENT:	PuzzleStateComponent = [
			0x00_u32,	0x01_u32,	0x02_u32,	0x03_u32,	0x04_u32,	0x05_u32,	0x06_u32,	0x07_u32,
			0x08_u32,	0x09_u32,	0x0A_u32,	0x0B_u32,	0x0C_u32,	0x0D_u32,	0x0E_u32,	0x0F_u32,
			0x10_u32,	0x11_u32,	0x12_u32,	0x13_u32,	0x14_u32,	0x15_u32,	0x16_u32,	0x17_u32,
			0x18_u32,	0x19_u32,	0x1A_u32,	0x1B_u32,	0x1C_u32,	0x1D_u32,	0x1E_u32,	0x1F_u32
		];
		const ZERO_COMPONENT: PuzzleStateComponent = [0_u32; PIECE_COUNT];
		pub const SOLVED_STATE: PuzzleState = PuzzleState {
			pos: Self::SOLVED_COMPONENT,
			rot: Self::ZERO_COMPONENT
		};
		pub const ZERO: PuzzleState = PuzzleState {
			pos: Self::ZERO_COMPONENT,
			rot: Self::ZERO_COMPONENT
		};

		pub fn half_addr(&self, piece_index: usize) -> HalfAddr {
			HalfAddr::new(self.pos[piece_index] as usize, self.rot[piece_index] as usize)
		}

		pub fn full_addr(&self, piece_index: usize) -> FullAddr {
			self.half_addr(piece_index).as_reorientation()
		}

		pub fn arrays(&self) -> PosAndRot {
			(&self.pos, &self.rot)
		}

		pub fn arrays_mut(&mut self) -> MutPosAndRot {
			(&mut self.pos, &mut self.rot)
		}

		pub fn debug_strings(&self) -> (String, String) {
			let mut pos_string: String = "[".into();
			let mut rot_string: String = "[".into();

			for pent_index in PENTAGON_PIECE_RANGE {
				write!(pos_string, "{: >3}", &format!("{:#X}", self.pos[pent_index])[2 ..]).unwrap();
				write!(rot_string, "{: >3}", &format!("{:#X}", self.rot[pent_index])[2 ..]).unwrap();
			}

			pos_string += " |";
			rot_string += " |";

			for tri_index in TRIANGLE_PIECE_RANGE {
				write!(pos_string, "{: >3}", &format!("{:#X}", self.pos[tri_index])[2 ..]).unwrap();
				write!(rot_string, "{: >3}", &format!("{:#X}", self.rot[tri_index])[2 ..]).unwrap();
			}

			pos_string += " ]";
			rot_string += " ]";

			(pos_string, rot_string)
		}

		pub fn invert_position(pos_index: usize) -> usize {
			pos_index ^ 0b11_usize
		}

		pub fn is_solved(&self) -> bool { *self == Self::SOLVED_STATE }

		pub fn is_standardized(&self) -> bool {
			self.pos[0_usize] == 0 as PieceStateComponent && self.rot[0_usize] == 0 as PieceStateComponent
		}

		pub fn is_valid(&self) -> bool {
			use PieceStateComponent as PSC;
			const_assert!(PIECE_COUNT <= PSC::BITS as usize);

			let mut present_positions:	u32 = 0_u32;
			let mut tri_rot_sum:		PSC = 0 as PSC;

			for pent_index in PENTAGON_PIECE_RANGE {
				let pos: PSC = self.pos[pent_index];

				if !PENTAGON_PIECE_RANGE.contains(&(pos as usize))
					|| self.rot[pent_index] >= PSC::PENTAGON_VERTEX_COUNT
				{
					return false;
				}

				present_positions |= 1_u32 << pos as u32;
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				let pos: PieceStateComponent = self.pos[tri_index];

				if !TRIANGLE_PIECE_RANGE.contains(&(pos as usize))
					|| self.rot[tri_index] >= PSC::TRIANGLE_VERTEX_COUNT
				{
					return false;
				}

				present_positions |= 1_u32 << pos as u32;
				tri_rot_sum += self.rot[tri_index];
			}

			present_positions == u32::MAX && tri_rot_sum % PSC::TRIANGLE_VERTEX_COUNT == 0
		}

		pub fn naive_add(&self, transformation: &Transformation) -> Self {
			Self::naive_add_inline(&self, transformation)
		}

		#[inline]
		fn naive_add_inline(&self, transformation: &Transformation) -> Self {
			let mut dest_state: PuzzleState = PuzzleState::default();

			puzzle_state_add!(self, transformation, dest_state);

			dest_state
		}

		pub fn naive_add_assign(&mut self, transformation: &Transformation) -> () {
			Self::naive_add_assign_inline(self, transformation);
		}

		#[inline]
		fn naive_add_assign_inline(&mut self, transformation: &Transformation) -> () {
			puzzle_state_add!(self, transformation, self);
		}

		pub fn naive_inflation(deflated_puzzle_state: &deflated::PuzzleState) -> Self {
			Self::naive_inflation_inline(deflated_puzzle_state)
		}

		#[inline]
		fn naive_inflation_inline(deflated_puzzle_state: &deflated::PuzzleState) -> Self {
			let mut inflated_puzzle_state: Self = Self::default();
			let pos: &mut PuzzleStateComponent = &mut inflated_puzzle_state.pos;
			let rot: &mut PuzzleStateComponent = &mut inflated_puzzle_state.rot;

			for piece_index in PIECE_RANGE {
				let deflated_piece_state: PieceStateComponent = deflated_puzzle_state
					.pieces
					[piece_index]
					as PieceStateComponent;

				pos[piece_index] = deflated_piece_state >> ROTATION_BIT_COUNT;
				rot[piece_index] = deflated_piece_state & ROTATION_BIT_MASK;
			}

			inflated_puzzle_state
		}

		pub fn standardization_half_addr(&self) -> HalfAddr { *self.standardization_full_addr().get_half_addr() }

		pub fn standardization_full_addr(&self) -> FullAddr {
			self.full_addr(PENTAGON_INDEX_OFFSET).get_inverse_addr()
		}

		pub fn standardize(&mut self) -> &mut Self {
			*self += self.standardization_full_addr();

			self
		}

		pub fn update_pieces(&self, pieces_query: &mut PieceQuery) -> () {
			for (piece_component, mut transform) in pieces_query.iter_mut() {
				transform.rotation = *self.half_addr(piece_component.index).get_orientation().unwrap();
			}
		}
	}

	impl<'a, 'b> Add<&'b Transformation> for &'a PuzzleState {
		type Output = PuzzleState;

		#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
		fn add(self: &'a PuzzleState, transformation: &'b Transformation) -> Self::Output {
			use crate::util::simd;

			crate::util_simd_inflated_add!(self, PuzzleState, transformation, Transformation)
		}

		#[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
		fn add(self: &'a PuzzleState, transformation: &'b Transformation) -> Self::Output {
			PuzzleState::naive_add_inline(self, transformation)
		}
	}

	impl<'a> Add<FullAddr> for &'a PuzzleState {
		type Output = PuzzleState;

		fn add(self, rhs: FullAddr) -> Self::Output {
			if let Some(transformation) = rhs.get_transformation() {
				self + transformation
			} else {
				self.clone()
			}
		}
	}

	impl<'a> AddAssign<&'a Transformation> for PuzzleState {
		#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
		fn add_assign(self: &mut PuzzleState, transformation: &'a Transformation) -> () {
			use crate::util::simd;

			crate::util_simd_inflated_add_assign!(self, PuzzleState, transformation, Transformation)
		}

		#[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
		fn add_assign(self: &mut PuzzleState, transformation: &'a Transformation) -> () {
			Self::naive_add_assign_inline(self, transformation);
		}
	}

	impl AddAssign<HalfAddr> for PuzzleState {
		fn add_assign(&mut self, rhs: HalfAddr) -> () {
			if let Some(transformation) = rhs.as_reorientation().get_transformation() {
				*self += transformation;
			}
		}
	}

	impl AddAssign<FullAddr> for PuzzleState {
		fn add_assign(&mut self, rhs: FullAddr) -> () {
			if let Some(transformation) = rhs.get_transformation() {
				*self += transformation;
			}
		}
	}

	impl Debug for PuzzleState {
		fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), Error> {
			let (pos_string, rot_string): (String, String) = self.debug_strings();

			formatter
				.debug_struct("PuzzleState")
				.field("pos", &pos_string)
				.field("rot", &rot_string)
				.finish()
		}
	}

	impl Default for PuzzleState { fn default() -> Self { Self::SOLVED_STATE } }

	impl From<&deflated::PuzzleState> for PuzzleState {
		#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
		fn from(deflated_puzzle_state: &deflated::PuzzleState) -> Self {
			use crate::util::simd;

			crate::util_simd_inflated_inflate_puzzle_state!(deflated_puzzle_state, deflated::PuzzleState, PuzzleState)
		}

		#[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
		fn from(deflated_puzzle_state: &deflated::PuzzleState) -> Self {
			Self::naive_inflation_inline(deflated_puzzle_state)
		}
	}

	impl From<&mut ThreadRng> for PuzzleState {
		fn from(thread_rng: &mut ThreadRng) -> Self {
			use PieceStateComponent as PSC;

			let mut puzzle_state: PuzzleState = PuzzleState::default();

			for pent_index in PENTAGON_PIECE_RANGE {
				puzzle_state.pos[pent_index] = (thread_rng.gen::<f32>() * PENTAGON_PIECE_COUNT_F32) as PSC;
				puzzle_state.rot[pent_index] = (thread_rng.gen::<f32>() * f32::PENTAGON_VERTEX_COUNT) as PSC;
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				puzzle_state.pos[tri_index] = (thread_rng.gen::<f32>() * TRIANGLE_PIECE_COUNT_F32) as PSC;
				puzzle_state.rot[tri_index] = (thread_rng.gen::<f32>() * f32::TRIANGLE_VERTEX_COUNT) as PSC;
			}

			puzzle_state
		}
	}

	impl FromAlt<&deflated::PuzzleState> for PuzzleState {
		fn from_alt(deflated_puzzle_state: &deflated::PuzzleState) -> Self {
			Self::naive_inflation(deflated_puzzle_state)
		}
	}

	impl PartialEq for PuzzleState {
		fn eq(&self, other: &Self) -> bool {
			unsafe { memcmp(self as *const Self as *const c_void, other as *const Self as *const c_void, size_of::<Self>()) == 0 }
		}
	}

	impl<'a, 'b> Sub<&'b PuzzleState> for &'a PuzzleState {
		type Output = Transformation;

		#[must_use]
		fn sub(self, prev_state: &'b PuzzleState) -> Self::Output {
			use PieceStateComponent as PSC;

			let mut transformation: Transformation = Transformation::default();
			let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) = transformation.arrays_mut();

			for pent_index in PENTAGON_PIECE_RANGE {
				pos_array[prev_state.pos[pent_index] as usize] = self.pos[pent_index];
				rot_array[prev_state.pos[pent_index] as usize] = {
					let rot_sum: PSC = PSC::PENTAGON_VERTEX_COUNT + self.rot[pent_index] - prev_state.rot[pent_index];

					if rot_sum >= PSC::PENTAGON_VERTEX_COUNT {
						rot_sum - PSC::PENTAGON_VERTEX_COUNT
					} else {
						rot_sum
					}
				};
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				pos_array[prev_state.pos[tri_index] as usize] = self.pos[tri_index];
				rot_array[prev_state.pos[tri_index] as usize] = {
					let rot_sum: PSC = PSC::TRIANGLE_VERTEX_COUNT + self.rot[tri_index] - prev_state.rot[tri_index];

					if rot_sum >= PSC::TRIANGLE_VERTEX_COUNT {
						rot_sum - PSC::TRIANGLE_VERTEX_COUNT
					} else {
						rot_sum
					}
				};
			}

			transformation
		}
	}

	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct RandomizePuzzleStateParams {
		#[inspectable(collapse)]
		pub random_transformation_genera:	GenusIndexBitArray,
		#[inspectable(min = 1_u8, max = 100_u8)]
		pub random_transformation_count:	u8,
		pub randomization_type:				RandomizationType
	}

	#[derive(Clone, Default, Deserialize, Serialize)]
	pub struct ExtendedPuzzleState {
		pub puzzle_state:	PuzzleState,
		pub actions:		Vec<Action>,
		pub curr_action:	usize
	}

	impl ExtendedPuzzleState {
		pub fn can_reorient_actions(&self, range: &Range<usize>) -> bool {
			!self.sanitize_action_range(range).is_empty()
		}

		pub fn can_simplify_actions(&self, range: &Range<usize>) -> bool {
			self.actions[self.sanitize_action_range(range)].iter().any(|action: &Action| -> bool {
				action.transformation.try_get_genus_index().map(GenusIndex::is_complex).unwrap_or_default()
			})
		}

		pub fn actions_are_simplified(&self, range: &Range<usize>) -> bool {
			self.has_actions() && self.actions[self.sanitize_action_range(range)].iter().all(|action: &Action| -> bool {
				action.transformation.try_get_genus_index().map(GenusIndex::is_simple).unwrap_or_default()
			})
		}

		pub fn get_as_seed_simples(&self, range: &Range<usize>) -> Option<Vec<HalfAddr>> {
			if !self.actions_are_simplified(range) {
				return None;
			}

			let mut comprising_simples: Vec<HalfAddr> = Vec::<HalfAddr>::with_capacity(range.len());
			let mut cumulative_standardization: HalfAddr = HalfAddr::ORIGIN;

			for action in self.actions[self.sanitize_action_range(range)].iter() {
				comprising_simples.push(*(action.transformation - cumulative_standardization).get_half_addr());
				cumulative_standardization += action.standardization();
			}

			Some(comprising_simples)
		}

		pub fn has_actions(&self) -> bool { !self.actions.is_empty() }

		pub fn reorient_actions(&mut self, range: &Range<usize>, reorientation: HalfAddr, camera: &mut HalfAddr) -> () {
			let range: Range<usize> = self.sanitize_action_range(range);
			let reorient_actions_internal = |
				old_actions: &Vec<Action>,
				puzzle_state: &mut PuzzleState,
				actions: &mut Vec<Action>,
				curr_action: Option<&mut usize>
			| -> () {
				let reorientation_line_index: PieceStateComponent = reorientation.get_species_index()
					as PieceStateComponent;
	
				/* new_origin_word_offset may be >= WORD_COUNT, but any summation it's involved in will need to be %'ed
				anyway */
				let (new_origin_piece_index, new_origin_word_offset): (usize, usize) = puzzle_state
					.pos
					.iter()
					.enumerate()
					.find_map(|(piece_index, line_index): (usize, &PieceStateComponent)| -> Option<(usize, usize)> {
						if *line_index == reorientation_line_index {
							Some((
								piece_index,
								puzzle_state.rot[piece_index] as usize + reorientation.get_organism_index()
							))
						} else {
							None
						}
					})
					.unwrap_or_default();

				for old_action in old_actions[range.clone()].iter() {
					let action: Action = *old_action + {
						let new_origin: HalfAddr = puzzle_state.half_addr(new_origin_piece_index);
	
						HalfAddr::new(
							new_origin.get_species_index(),
							(new_origin.get_organism_index() + new_origin_word_offset) % Library::ORGANISMS_PER_SPECIES
						)
					};
	
					*puzzle_state += action.transformation;
					*puzzle_state += action.standardization();
					actions.push(action);
				}
	
				if let Some(curr_action) = curr_action {
					*curr_action = (*curr_action).min(range.end);
				}
			};

			if self.curr_action < range.start {
				// Don't bother modifying self.puzzle_state, since we'd need to revert back to the current state anyway
				let mut new_self: Self = Self {
					puzzle_state:	self.puzzle_state.clone(),
					actions:		self.get_allocated_actions(&range),
					curr_action:	self.curr_action
				};

				new_self.skip_to_action(range.start);
				reorient_actions_internal(&self.actions, &mut new_self.puzzle_state, &mut new_self.actions, None);
				self.actions = take::<Vec<Action>>(&mut new_self.actions);
			} else {
				let mut curr_action: usize = self.curr_action;
				let mut new_actions: Vec<Action> = self.get_allocated_actions(&range);

				self.skip_to_action(range.start);
				reorient_actions_internal(
					&self.actions,
					&mut self.puzzle_state,
					&mut new_actions,
					Some(&mut curr_action)
				);
				self.actions = take::<Vec<Action>>(&mut new_actions);
				self.skip_to_action(curr_action);
				*camera = self.actions[self.curr_action - 1_usize].get_standardized_camera_end();
			}
		}

		pub fn set_camera_start(&mut self, range: &Range<usize>, camera_start: HalfAddr) -> () {
			let range: Range<usize> = self.sanitize_action_range(range);

			for action in self.actions[range].iter_mut() {
				action.camera_start = camera_start;
			}
		}

		pub fn set_initial_camera_start(&mut self, range: &Range<usize>, mut camera_start: HalfAddr) -> () {
			let range: Range<usize> = self.sanitize_action_range(range);

			for action in self.actions[range].iter_mut() {
				action.camera_start = camera_start;
				camera_start = action.get_standardized_camera_end();
			}
		}

		pub fn simplify_actions(&mut self, range: &Range<usize>) -> () {
			let range: Range<usize> = self.sanitize_action_range(range);
			let mut actions: Vec<Action> = Vec::<Action>::with_capacity(self.actions.len());
			let mut curr_action: usize = self.curr_action;

			actions.resize(range.start, Action::default());
			actions[0_usize .. range.start].copy_from_slice(
				&self.actions[0_usize .. range.start]
			);

			for (slice_index, action) in self.actions[range.clone()].iter().enumerate() {
				let action_index: usize = slice_index + range.start;

				if action_index == self.curr_action {
					curr_action = actions.len();
				}

				if warn_expect!(action.is_valid()) {
					let genus_index: GenusIndex = action
						.transformation
						.try_get_genus_index()
						.unwrap();

					if genus_index.is_complex() {
						let comprising_simples: &[HalfAddr] = action.transformation.get_simple_slice();
						let mut camera_start: HalfAddr = action.camera_start;
						let mut cumulative_standardization: HalfAddr = HalfAddr::ORIGIN;

						actions.reserve(comprising_simples.len());

						for comprising_simple in action.transformation.get_simple_slice() {
							let transformation: FullAddr = comprising_simple.as_simple() + cumulative_standardization;
							let standardization: HalfAddr = transformation.standardization();

							actions.push(Action::new(transformation, camera_start));
							camera_start += standardization;
							cumulative_standardization += standardization;
						}
					} else {
						actions.push(*action);
					}
				}
			}

			if range.end <= self.curr_action {
				curr_action = actions.len() + self.curr_action - range.end;
			}

			let current_len: usize = actions.len();
			let remaining: Range<usize> = range.end .. self.actions.len();

			actions.resize(current_len + remaining.len(), Action::default());
			actions[current_len .. current_len + remaining.len()].copy_from_slice(&self.actions[remaining]);
			self.actions = actions;
			self.curr_action = curr_action;
		}

		fn get_allocated_actions(&self, range: &Range<usize>) -> Vec<Action> {
			let mut actions: Vec<Action> = Vec::<Action>::with_capacity(range.end);

			actions.resize(range.start, Action::default());
			actions[0 .. range.start].copy_from_slice(&self.actions[0 .. range.start]);

			actions
		}

		fn sanitize_action_range(&self, range: &Range<usize>) -> Range<usize> {
			range.start.min(self.actions.len()) .. range.end.min(self.actions.len())
		}

		fn skip_to_action(&mut self, action: usize) -> () {
			let action: usize = action.min(self.actions.len());

			if self.curr_action < action {
				for action in self.actions[self.curr_action .. action].iter() {
					self.puzzle_state += action.transformation;
					self.puzzle_state += action.standardization();
				}
			} else if self.curr_action > action {
				for action in self.actions[action .. self.curr_action].iter().rev() {
					let inverted_action: Action = action.invert();
	
					self.puzzle_state += inverted_action.transformation;
					self.puzzle_state += inverted_action.standardization();
				}
			}

			self.curr_action = action;
		}
	}
}

pub struct PuzzlePlugin;

impl PuzzlePlugin {
	fn startup(
		mut commands: Commands,
		piece_library: Res<PieceLibrary>,
		preferences: Res<Preferences>
	) -> () {
		warn_expect_ok!(Self::startup_internal(&mut commands, &piece_library, &preferences));
	}

	fn startup_internal(
		commands: &mut Commands,
		piece_library: &Res<PieceLibrary>,
		preferences: &Res<Preferences>
	) -> LogErrorResult {
		let color_data_with_mat: &ColorDataWithMat = &preferences.color.colors_with_mat;
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
		let inspectable_bin_map: InspectableBinMap<(Polyhedron, Vec<ColAndMat>)> =
			color_data_with_mat
				.polyhedron_to_colors
				.as_inspectable_bin_map();
		let (base_col_and_mat, col_and_mats): (&ColAndMat, &Vec<ColAndMat>) = {
			(
				&color_data_with_mat.base_color,
				match inspectable_bin_map.get(&piece_library.data.default_design.as_polyhedron()) {
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
		let faces: &Vec<FaceData> = &Data::get(Polyhedron::Icosidodecahedron).faces;
		let param_bundle: (&ColAndMat, &Vec<ColAndMat>, &Vec<FaceData>) = (base_col_and_mat, col_and_mats, faces);

		piece_pair.add_entities(commands, &param_bundle);

		Ok(())
	}

	fn run(
		mut extended_puzzle_state: ResMut<ExtendedPuzzleState>,
		mut input_state: ResMut<InputState>,
		mut queries: QuerySet<(
			CameraQueryStateMut,
			PieceQueryState
		)>
	) -> () {
		if input_state.puzzle_action.is_some()
			&& input_state.puzzle_action.as_mut().unwrap().update(&mut *extended_puzzle_state, &mut queries)
		{
			input_state.puzzle_action = None;
		}
	}
}

impl Plugin for PuzzlePlugin {
	fn build(&self, app: &mut App) -> () {
		app
			.insert_resource(ExtendedPuzzleState::default())
			.add_startup_system(Self::startup
				.system()
				.label(STRING_DATA.labels.puzzle_startup.as_ref())
				.after(STRING_DATA.labels.piece_library_startup.as_ref())
				.after(STRING_DATA.labels.color_data_startup.as_ref())
			)
			.add_system(Self::run
				.system()
				.label(STRING_DATA.labels.puzzle_run.as_ref())
			);
	}
}

pub struct PuzzlePluginGroup;

impl Plugin for PuzzlePluginGroup {
	fn build(&self, app: &mut App) -> () {
		app.add_plugins(Self);
	}
}

impl PluginGroup for PuzzlePluginGroup {
	fn build(&mut self, group: &mut PluginGroupBuilder) -> () {
		group
			.add(TransformationPlugin)
			.add(PuzzlePlugin)
			.add(SolverPlugin);
	}
}

#[cfg(test)]
mod tests {
	use {
		super::{
			*,
			deflated::{
				PieceState as DPS,
				PuzzleState as DeflatedPuzzleState
			},
			inflated::{
				PieceStateComponent as IPSC,
				PuzzleState as InflatedPuzzleState
			}
		},
		std::{
			any::type_name,
			fmt::Debug
		}
	};

	fn test_conversion<T, U>(original: &T, expectation: &U) -> bool
		where
			T: Debug,
			U: for<'a> From<&'a T> + for<'b> FromAlt<&'b T> + PartialEq + Debug
	{
		let converted: U = U::from(original);
		let converted_alt: U = U::from_alt(original);

		if converted != *expectation {
			log::error!("{}::from(original) != *expectation", type_name::<U>());
			error_expr!(original, converted, expectation);

			return false;
		}

		if converted != converted_alt {
			log::error!("{}::from(original) != {}::from_alt(original)", type_name::<U>(), type_name::<U>());
			error_expr!(original, converted, converted_alt);

			return false;
		}

		true
	}

	fn test_conversion_bidir<T, U>(original: &T) -> bool
		where
			T: for<'a> From<&'a U> + for<'b> FromAlt<&'b U> + PartialEq + Debug,
			U: for<'c> From<&'c T> + for<'d> FromAlt<&'d T> + PartialEq + Debug
	{
		let converted: U = U::from(original);
		let converted_alt: U = U::from_alt(original); 

		if converted != converted_alt {
			log::error!("{0}::from(original) != {0}::from_alt(original)", type_name::<U>());
			error_expr!(original, converted, converted_alt);

			return false;
		}

		let converted_converted: T = T::from(&converted);
		let converted_alt_converted: T = T::from(&converted_alt);
		let converted_converted_alt: T = T::from_alt(&converted);
		let converted_alt_converted_alt: T = T::from_alt(&converted_alt);

		if converted_converted != *original {
			log::error!("{}::from(&{}::from(original)) != *original", type_name::<T>(), type_name::<U>());
			error_expr!(original, converted, converted_converted);

			return false;
		}

		if converted_converted != converted_alt_converted {
			log::error!("{0}::from(&{1}::from(original)) != {0}::from(&{1}::from_alt(original))", type_name::<T>(), type_name::<U>());
			error_expr!(original, converted, converted_converted, converted_alt, converted_alt_converted);

			return false;
		}

		if converted_converted != converted_converted_alt {
			log::error!("{0}::from(&{1}::from(original)) != {0}::from_alt(&{1}::from(original))", type_name::<T>(), type_name::<U>());
			error_expr!(original, converted, converted_converted, converted_converted_alt);

			return false;
		}

		if converted_converted != converted_alt_converted_alt {
			log::error!("{0}::from(&{1}::from(original)) != {0}::from_alt(&{1}::from_alt(original))", type_name::<T>(), type_name::<U>());
			error_expr!(original, converted, converted_converted, converted_alt, converted_alt_converted_alt);

			return false;
		}

		true
	}

	#[test]
	fn test_default_conversions() -> () {
		init_env_logger();

		let deflated_puzzle_state: DeflatedPuzzleState = DeflatedPuzzleState::default();
		let inflated_puzzle_state: InflatedPuzzleState = InflatedPuzzleState::default();

	break_assert!(test_conversion(&deflated_puzzle_state, &inflated_puzzle_state));
	break_assert!(test_conversion(&inflated_puzzle_state, &deflated_puzzle_state));
	}

	#[test]
	fn test_base_case_conversions() -> () {
		init_env_logger();

		let mut deflated_puzzle_state: DeflatedPuzzleState = DeflatedPuzzleState::default();
		let mut inflated_puzzle_state: InflatedPuzzleState = InflatedPuzzleState::default();

		for piece_index in PIECE_RANGE {
			let piece_index_dps: DPS = piece_index as DPS;
			let rot: DPS = piece_index_dps % Type::from_index(piece_index).unwrap().vertex_count() as DPS;

			deflated_puzzle_state.pieces[piece_index] = piece_index_dps << ROTATION_BIT_COUNT | rot;
			inflated_puzzle_state.pos[piece_index] = piece_index_dps as IPSC;
			inflated_puzzle_state.rot[piece_index] = rot as IPSC;
		}

	break_assert!(test_conversion(&deflated_puzzle_state, &inflated_puzzle_state));
	break_assert!(test_conversion(&inflated_puzzle_state, &deflated_puzzle_state));
	}

	#[test]
	fn test_random_conversions() -> () {
		const RANDOM_TEST_COUNT: u32 = 500_u32;

		init_env_logger();

		let mut thread_rng: ThreadRng = rand::thread_rng();

		for _ in 0_u32 .. RANDOM_TEST_COUNT {
		break_assert!(test_conversion_bidir::<DeflatedPuzzleState, InflatedPuzzleState>(&DeflatedPuzzleState::from(&mut thread_rng)));
		break_assert!(test_conversion_bidir::<InflatedPuzzleState, DeflatedPuzzleState>(&InflatedPuzzleState::from(&mut thread_rng)));
		}
	}

	#[cfg(feature = "non_unit_tests")]
	#[test]
	fn test_puzzle_state_properties() -> () {
		use super::{
			inflated::PuzzleStateConsts,
			transformation::Genus
		};

		const ITERATION_COUNT: usize = 10000000_usize;

		init_env_logger();

		let simples:									&Page<Transformation>		= &TransformationLibrary::initialize_and_get().book_pack_data.trfm[TransformationType::Simple as usize];
		let mut correct_pos_pent_piece_count_counts:			[u32; PENTAGON_PIECE_COUNT]	= [0_u32; PENTAGON_PIECE_COUNT];
		let mut correct_pos_tri_piece_count_counts:				[u32; TRIANGLE_PIECE_COUNT]	= [0_u32; TRIANGLE_PIECE_COUNT];
		let mut correct_rot_pent_piece_count_counts:			[u32; PENTAGON_PIECE_COUNT]	= [0_u32; PENTAGON_PIECE_COUNT];
		let mut correct_rot_tri_piece_count_counts:				[u32; TRIANGLE_PIECE_COUNT]	= [0_u32; TRIANGLE_PIECE_COUNT];
		let mut correct_pos_and_rot_pent_piece_count_counts:	[u32; PENTAGON_PIECE_COUNT]	= [0_u32; PENTAGON_PIECE_COUNT];
		let mut correct_pos_and_rot_tri_piece_count_counts:		[u32; TRIANGLE_PIECE_COUNT]	= [0_u32; TRIANGLE_PIECE_COUNT];
		let mut all_pent_rot_sums_mod_5_are_zero:				bool						= true;
		let mut all_tri_rot_sums_mod_3_are_zero:				bool						= true;
		let mut thread_rng:										ThreadRng					= rand::thread_rng();
		let mut puzzle_state:									InflatedPuzzleState			= InflatedPuzzleState::SOLVED_STATE;

		for _ in 0_usize .. ITERATION_COUNT {
			puzzle_state += &simples[thread_rng.gen_range(PENTAGON_PIECE_RANGE)][thread_rng.gen_range(1_usize .. PENTAGON_VERTEX_COUNT)];

			let mut correct_pos_pent_piece_count:			u32 = 0_u32;
			let mut correct_pos_tri_piece_count:			u32 = 0_u32;
			let mut correct_rot_pent_piece_count:			u32 = 0_u32;
			let mut correct_rot_tri_piece_count:			u32 = 0_u32;
			let mut correct_pos_and_rot_pent_piece_count:	u32 = 0_u32;
			let mut correct_pos_and_rot_tri_piece_count:	u32 = 0_u32;
			let mut pent_rot_sum:							u32 = 0_u32;
			let mut tri_rot_sum:							u32 = 0_u32;

			for pent_index in PENTAGON_PIECE_RANGE {
				let correct_pos: bool = puzzle_state.pos[pent_index] == pent_index as IPSC;
				let correct_rot: bool = puzzle_state.rot[pent_index] == 0 as IPSC;

				correct_pos_pent_piece_count			+= correct_pos as u32;
				correct_rot_pent_piece_count			+= correct_rot as u32;
				correct_pos_and_rot_pent_piece_count	+= (correct_pos && correct_rot) as u32;
				pent_rot_sum							+= puzzle_state.rot[pent_index];
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				let correct_pos: bool = puzzle_state.pos[tri_index] == tri_index as IPSC;
				let correct_rot: bool = puzzle_state.rot[tri_index] == 0 as IPSC;

				correct_pos_tri_piece_count			+= correct_pos as u32;
				correct_rot_tri_piece_count			+= correct_rot as u32;
				correct_pos_and_rot_tri_piece_count	+= (correct_pos && correct_rot) as u32;
				tri_rot_sum							+= puzzle_state.rot[tri_index];
			}

			correct_pos_pent_piece_count_counts			[correct_pos_pent_piece_count			as usize]	+= 1;
			correct_pos_tri_piece_count_counts			[correct_pos_tri_piece_count			as usize]	+= 1;
			correct_rot_pent_piece_count_counts			[correct_rot_pent_piece_count			as usize]	+= 1;
			correct_rot_tri_piece_count_counts			[correct_rot_tri_piece_count			as usize]	+= 1;
			correct_pos_and_rot_pent_piece_count_counts	[correct_pos_and_rot_pent_piece_count	as usize]	+= 1;
			correct_pos_and_rot_tri_piece_count_counts	[correct_pos_and_rot_tri_piece_count	as usize]	+= 1;
			all_pent_rot_sums_mod_5_are_zero																&= pent_rot_sum	% PENTAGON_VERTEX_COUNT as u32 == 0_u32;
			all_tri_rot_sums_mod_3_are_zero																	&= tri_rot_sum	% TRIANGLE_VERTEX_COUNT as u32 == 0_u32;
		}

		trace_expr!(
			correct_pos_pent_piece_count_counts,
			correct_pos_tri_piece_count_counts,
			correct_rot_pent_piece_count_counts,
			correct_rot_tri_piece_count_counts,
			correct_pos_and_rot_pent_piece_count_counts,
			correct_pos_and_rot_tri_piece_count_counts,
			all_pent_rot_sums_mod_5_are_zero,
			all_tri_rot_sums_mod_3_are_zero
		);
	}
}