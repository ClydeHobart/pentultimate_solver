use {
	crate::{
		prelude::*,
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
			Preferences
		},
		util::inspectable_bin_map::*,
		max
	},
	self::{
		consts::*,
		transformation::{
			Action,
			FullAddr,
			HalfAddr,
			Transformation
		}
	},
	std::{
		fmt::{
			Debug,
			Error,
			Formatter,
			Write
		},
		mem::{
			size_of,
			transmute
		},
		ops::{
			Add,
			AddAssign,
			Sub
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
	inflated::{
		ExtendedPuzzleState,
		PieceStateComponent as InflatedPieceStateComponent,
		PuzzleState as InflatedPuzzleState,
		PuzzleStateComponent as InflatedPuzzleStateComponent,
		PuzzleStateConsts as InflatedPuzzleStateConsts
	},
	transformation::TransformationPlugin,
};

pub mod transformation;
pub mod explore;

pub mod consts {
	use {
		super::{
			inflated::PieceStateComponent as IPSC,
			Type,
			max
		},
		std::ops::Range
	};

	pub const PENTAGON_PIECE_COUNT:			usize			= Type::Pentagon.instance_count();				// 12
	pub const TRIANGLE_PIECE_COUNT:			usize			= Type::Triangle.instance_count();				// 20
	pub const PIECE_COUNT:					usize			= PENTAGON_PIECE_COUNT + TRIANGLE_PIECE_COUNT;	// 32
	pub const PENTAGON_SIDE_COUNT:			usize			= Type::Pentagon.side_count();					// 5
	pub const TRIANGLE_SIDE_COUNT:			usize			= Type::Triangle.side_count();					// 3
	pub const PENTAGON_INDEX_OFFSET:		usize			= Type::Pentagon.index_offset();				// 0
	pub const TRIANGLE_INDEX_OFFSET:		usize			= Type::Triangle.index_offset();				// 12
	pub const ROTATION_BIT_COUNT:			u32				= usize::BITS - max!(PENTAGON_SIDE_COUNT, TRIANGLE_SIDE_COUNT).leading_zeros(); // 3
	pub const ROTATION_BIT_MASK:			IPSC			= ((1 as IPSC) << ROTATION_BIT_COUNT) - 1;		// 0b111
	pub const PENTAGON_PIECE_COUNT_F32:		f32				= PENTAGON_PIECE_COUNT as f32;
	pub const TRIANGLE_PIECE_COUNT_F32:		f32				= TRIANGLE_PIECE_COUNT as f32;
	pub const PIECE_COUNT_F32:				f32				= PENTAGON_PIECE_COUNT_F32 + TRIANGLE_SIDE_COUNT_F32;
	pub const PENTAGON_SIDE_COUNT_F32:		f32				= PENTAGON_SIDE_COUNT as f32;
	pub const TRIANGLE_SIDE_COUNT_F32:		f32				= TRIANGLE_SIDE_COUNT as f32;
	pub const PENTAGON_SIDE_COUNT_IPSC:		IPSC			= PENTAGON_SIDE_COUNT as IPSC;
	pub const TRIANGLE_SIDE_COUNT_IPSC:		IPSC			= TRIANGLE_SIDE_COUNT as IPSC;
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
				puzzle_state.pieces[pent_index] = ((thread_rng.gen::<f32>() * PENTAGON_PIECE_COUNT_F32) as PieceState) << ROTATION_BIT_COUNT | (thread_rng.gen::<f32>() * PENTAGON_SIDE_COUNT_F32) as PieceState;
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				puzzle_state.pieces[tri_index] = ((thread_rng.gen::<f32>() * TRIANGLE_PIECE_COUNT_F32) as PieceState) << ROTATION_BIT_COUNT | (thread_rng.gen::<f32>() * TRIANGLE_SIDE_COUNT_F32) as PieceState;
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

	pub trait PuzzleStateComponentConsts {
		const SOLVED_STATE:	PuzzleStateComponent;
		const ZERO:			PuzzleStateComponent;
	}

	impl PuzzleStateComponentConsts for PuzzleStateComponent {
		const SOLVED_STATE:	PuzzleStateComponent = unsafe { transmute::<[u32; PIECE_COUNT], PuzzleStateComponent>([
			0x00,	0x01,	0x02,	0x03,	0x04,	0x05,	0x06,	0x07,
			0x08,	0x09,	0x0A,	0x0B,	0x0C,	0x0D,	0x0E,	0x0F,
			0x10,	0x11,	0x12,	0x13,	0x14,	0x15,	0x16,	0x17,
			0x18,	0x19,	0x1A,	0x1B,	0x1C,	0x1D,	0x1E,	0x1F
		]) };
		const ZERO:			PuzzleStateComponent = unsafe { transmute::<[u32; PIECE_COUNT], PuzzleStateComponent>([0_u32; PIECE_COUNT]) };
	}

	#[derive(Clone)]
	#[repr(align(32))]
	pub struct PuzzleState {
		pub pos: PuzzleStateComponent,	// An array representing the current position of the piece with the given index
		pub rot: PuzzleStateComponent	// An array representing the cumulative rotation of the piece with the given index
	}

	assert_eq_size!(PuzzleState, [PuzzleStateComponent; 2]);

	pub trait PuzzleStateConsts {
		const SOLVED_STATE: PuzzleState;
	}

	macro_rules! puzzle_state_add {
		($src_state:ident, $transformation:ident, $dest_state:ident) => {
			// let transformation_pos: &PuzzleStateComponent = &$transformation.pos;
			// let transformation_rot: &PuzzleStateComponent = &$transformation.rot;
			let (transformation_pos, transformation_rot) = $transformation.arrays();

			for pent_index in PENTAGON_PIECE_RANGE {
				let curr_pos: usize = $src_state.pos[pent_index] as usize;

				$dest_state.pos[pent_index] = transformation_pos[curr_pos];
				$dest_state.rot[pent_index] = {
					let rot_sum: PieceStateComponent = $src_state.rot[pent_index] + transformation_rot[curr_pos];

					if rot_sum >= PENTAGON_SIDE_COUNT_IPSC {
						rot_sum - PENTAGON_SIDE_COUNT_IPSC
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

					if rot_sum >= TRIANGLE_SIDE_COUNT_IPSC {
						rot_sum - TRIANGLE_SIDE_COUNT_IPSC
					} else {
						rot_sum
					}
				};
			}
		};
	}

	impl PuzzleState {
		pub fn half_addr(&self, piece_index: usize) -> HalfAddr {
			HalfAddr::new(self.pos[piece_index] as usize, self.rot[piece_index] as usize)
		}

		pub fn full_addr(&self, piece_index: usize) -> FullAddr {
			self.half_addr(piece_index).as_reorientation()
		}

		pub fn arrays(&self) -> (&PuzzleStateComponent, &PuzzleStateComponent) {
			(&self.pos, &self.rot)
		}

		pub fn arrays_mut(&mut self) -> (&mut PuzzleStateComponent, &mut PuzzleStateComponent) {
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

		pub fn is_standardized(&self) -> bool {
			self.pos[0_usize] == 0 as PieceStateComponent && self.rot[0_usize] == 0 as PieceStateComponent
		}

		pub fn is_valid(&self) -> bool {
			const_assert!(PIECE_COUNT <= u32::BITS as usize);

			let mut present_positions:	u32 = 0_u32;
			let mut tri_rot_sum:		PieceStateComponent = 0 as PieceStateComponent;

			for pent_index in PENTAGON_PIECE_RANGE {
				let pos: PieceStateComponent = self.pos[pent_index];

				if !PENTAGON_PIECE_RANGE.contains(&(pos as usize)) || self.rot[pent_index] >= PENTAGON_SIDE_COUNT_IPSC {
					return false;
				}

				present_positions |= 1_u32 << pos as u32;
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				let pos: PieceStateComponent = self.pos[tri_index];

				if !TRIANGLE_PIECE_RANGE.contains(&(pos as usize)) || self.rot[tri_index] >= TRIANGLE_SIDE_COUNT_IPSC {
					return false;
				}

				present_positions |= 1_u32 << pos as u32;
				tri_rot_sum += self.rot[tri_index];
			}

			present_positions == u32::MAX && tri_rot_sum % TRIANGLE_SIDE_COUNT_IPSC == 0
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
			self.full_addr(PENTAGON_INDEX_OFFSET).invert()
		}

		pub fn standardize(&mut self) -> &mut Self {
			*self += self.standardization_full_addr();

			self
		}
	}

	impl PuzzleStateConsts for PuzzleState {
		const SOLVED_STATE: PuzzleState = unsafe { std::mem::transmute::<[PuzzleStateComponent; 2], PuzzleState>([
			PuzzleStateComponent::SOLVED_STATE,
			PuzzleStateComponent::ZERO
		]) };
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
			if let Some(trfm) = rhs.trfm() { self + trfm } else { self.clone() }
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

	impl AddAssign<FullAddr> for PuzzleState {
		fn add_assign(&mut self, rhs: FullAddr) -> () {
			if let Some(trfm) = rhs.trfm() {
				*self += trfm;
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

	impl Default for PuzzleState {
		fn default() -> Self {
			assert_eq_size!(PuzzleState, [u32; 2 * PIECE_COUNT]);

			unsafe {
				transmute::<[u32; 2 * PIECE_COUNT], Self>([0_u32; 2 * PIECE_COUNT])
			}
		}
	}

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
			let mut puzzle_state: PuzzleState = PuzzleState::default();

			for pent_index in PENTAGON_PIECE_RANGE {
				puzzle_state.pos[pent_index] = (thread_rng.gen::<f32>() * PENTAGON_PIECE_COUNT_F32) as PieceStateComponent;
				puzzle_state.rot[pent_index] = (thread_rng.gen::<f32>() * PENTAGON_SIDE_COUNT_F32) as PieceStateComponent;
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				puzzle_state.pos[tri_index] = (thread_rng.gen::<f32>() * TRIANGLE_PIECE_COUNT_F32) as PieceStateComponent;
				puzzle_state.rot[tri_index] = (thread_rng.gen::<f32>() * TRIANGLE_SIDE_COUNT_F32) as PieceStateComponent;
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
			let mut transformation: Transformation = Transformation::default();
			let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) = transformation.arrays_mut();

			for pent_index in PENTAGON_PIECE_RANGE {
				pos_array[prev_state.pos[pent_index] as usize] = self.pos[pent_index];
				rot_array[prev_state.pos[pent_index] as usize] = {
					let rot_sum: PieceStateComponent = PENTAGON_SIDE_COUNT_IPSC + self.rot[pent_index] - prev_state.rot[pent_index];

					if rot_sum >= PENTAGON_SIDE_COUNT_IPSC {
						rot_sum - PENTAGON_SIDE_COUNT_IPSC
					} else {
						rot_sum
					}
				};
			}

			for tri_index in TRIANGLE_PIECE_RANGE {
				pos_array[prev_state.pos[tri_index] as usize] = self.pos[tri_index];
				rot_array[prev_state.pos[tri_index] as usize] = {
					let rot_sum: PieceStateComponent = TRIANGLE_SIDE_COUNT_IPSC + self.rot[tri_index] - prev_state.rot[tri_index];

					if rot_sum >= TRIANGLE_SIDE_COUNT_IPSC {
						rot_sum - TRIANGLE_SIDE_COUNT_IPSC
					} else {
						rot_sum
					}
				};
			}

			transformation
		}
	}

	#[repr(align(32))]
	pub struct ExtendedPuzzleState {
		pub puzzle_state:	PuzzleState,
		pub actions:		Vec<Action>,
		pub curr_action:	i32
	}

	impl ExtendedPuzzleState {

	}

	impl<'a> AddAssign<&'a Transformation> for ExtendedPuzzleState {
		fn add_assign(&mut self, transformation: &'a Transformation) -> () {
			self.puzzle_state += transformation;
		}
	}

	impl Default for ExtendedPuzzleState {
		fn default() -> Self {
			Self {
				puzzle_state:	PuzzleState::SOLVED_STATE,
				actions:		Vec::<Action>::new(),
				curr_action:	-1_i32
			}
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
		log_result_err!(Self::startup_internal(&mut commands, &piece_library, &preferences));
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
			Query<(&CameraComponent, &mut Transform)>,
			Query<(&PieceComponent, &mut Transform)>
		)>
	) -> () {
		if input_state.action.is_some()
			&& input_state.action.as_mut().unwrap().update(&mut *extended_puzzle_state, &mut queries)
		{
			input_state.action = None;
		}
	}
}

impl Plugin for PuzzlePlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
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

		assert!(test_conversion(&deflated_puzzle_state, &inflated_puzzle_state));
		assert!(test_conversion(&inflated_puzzle_state, &deflated_puzzle_state));
	}

	#[test]
	fn test_base_case_conversions() -> () {
		init_env_logger();

		let mut deflated_puzzle_state: DeflatedPuzzleState = DeflatedPuzzleState::default();
		let mut inflated_puzzle_state: InflatedPuzzleState = InflatedPuzzleState::default();

		for piece_index in PIECE_RANGE {
			let piece_index_dps: DPS = piece_index as DPS;
			let rot: DPS = piece_index_dps % Type::from_index(piece_index).unwrap().side_count() as DPS;

			deflated_puzzle_state.pieces[piece_index] = piece_index_dps << ROTATION_BIT_COUNT | rot;
			inflated_puzzle_state.pos[piece_index] = piece_index_dps as IPSC;
			inflated_puzzle_state.rot[piece_index] = rot as IPSC;
		}

		assert!(test_conversion(&deflated_puzzle_state, &inflated_puzzle_state));
		assert!(test_conversion(&inflated_puzzle_state, &deflated_puzzle_state));
	}

	#[test]
	fn test_random_conversions() -> () {
		const RANDOM_TEST_COUNT: u32 = 500_u32;

		init_env_logger();

		let mut thread_rng: ThreadRng = rand::thread_rng();

		for _ in 0_u32 .. RANDOM_TEST_COUNT {
			assert!(test_conversion_bidir::<DeflatedPuzzleState, InflatedPuzzleState>(&DeflatedPuzzleState::from(&mut thread_rng)));
			assert!(test_conversion_bidir::<InflatedPuzzleState, DeflatedPuzzleState>(&InflatedPuzzleState::from(&mut thread_rng)));
		}
	}

	#[cfg(feature = "non_unit_tests")]
	#[test]
	fn test_puzzle_state_properties() -> () {
		use super::{
			inflated::PuzzleStateConsts,
			transformation::Page
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
			puzzle_state += &simples[thread_rng.gen_range(PENTAGON_PIECE_RANGE)][thread_rng.gen_range(1_usize .. PENTAGON_SIDE_COUNT)];

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
			all_pent_rot_sums_mod_5_are_zero																&= pent_rot_sum	% PENTAGON_SIDE_COUNT as u32 == 0_u32;
			all_tri_rot_sums_mod_3_are_zero																	&= tri_rot_sum	% TRIANGLE_SIDE_COUNT as u32 == 0_u32;
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