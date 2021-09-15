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
				traits::*,
				ColorDataWithMat,
				ColorData,
				MatHdl
			},
			Preferences
		},
		ui::camera::Animation as CameraAnimation,
		get_data
	},
	self::{
		consts::*,
		inflated::{
			Animation,
			PuzzleStateComponent
		},
		transformation::{
			packs::*,
			Addr,
			Page,
			Transformation,
			Library as TransformationLibrary,
			Type as TransformationType,
			GetWord
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
		},
		time::{
			Duration,
			Instant
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
		PuzzleState as InflatedPuzzleState
	},
	transformation::{
		TransformationPlugin,
		LibraryRef as TransformationLibraryRef
	}
};

pub mod transformation;

pub mod consts {
	use {
		super::{
			inflated::PieceStateComponent as IPSC,
			Type
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
	pub const ROTATION_BIT_COUNT:			u32				= usize::BITS - (if PENTAGON_SIDE_COUNT > TRIANGLE_SIDE_COUNT { PENTAGON_SIDE_COUNT } else { TRIANGLE_SIDE_COUNT }).leading_zeros(); // 3
	pub const ROTATION_BIT_MASK:			IPSC			= ((1 as IPSC) << ROTATION_BIT_COUNT) - 1;		// 0b111
	pub const PENTAGON_PIECE_COUNT_F32:		f32				= PENTAGON_PIECE_COUNT as f32;
	pub const TRIANGLE_PIECE_COUNT_F32:		f32				= TRIANGLE_PIECE_COUNT as f32;
	pub const PENTAGON_SIDE_COUNT_F32:		f32				= PENTAGON_SIDE_COUNT as f32;
	pub const TRIANGLE_SIDE_COUNT_F32:		f32				= TRIANGLE_SIDE_COUNT as f32;
	pub const PENTAGON_SIDE_COUNT_IPSC:		IPSC			= PENTAGON_SIDE_COUNT as IPSC;
	pub const TRIANGLE_SIDE_COUNT_IPSC:		IPSC			= TRIANGLE_SIDE_COUNT as IPSC;
	pub const PENTAGON_PIECE_RANGE:			Range<usize>	= PENTAGON_INDEX_OFFSET .. PENTAGON_INDEX_OFFSET + PENTAGON_PIECE_COUNT;
	pub const TRIANGLE_PIECE_RANGE:			Range<usize>	= TRIANGLE_INDEX_OFFSET .. TRIANGLE_INDEX_OFFSET + TRIANGLE_PIECE_COUNT;
	pub const PIECE_RANGE:					Range<usize>	= 0_usize .. PIECE_COUNT;
	pub const HALF_PENTAGON_PIECE_COUNT:	usize			= PENTAGON_PIECE_COUNT / 2;
}

// Compressed version for smaller memory footprint when keeping track of multiple states
pub mod deflated {
	use super::*;

	pub type PieceState = u8;

	#[derive(Debug)]
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
	#[repr(align(256))]
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
				let deflated_piece_state: PieceStateComponent = deflated_puzzle_state.pieces[piece_index] as PieceStateComponent;

				pos[piece_index] = deflated_piece_state >> ROTATION_BIT_COUNT;
				rot[piece_index] = deflated_piece_state & ROTATION_BIT_MASK;
			}

			inflated_puzzle_state
		}

		pub fn standardize(&mut self) -> () {
			self.standardize_with_reorientation_page(&TransformationLibrary::get().book_pack_data.trfm[TransformationType::Reorientation as usize])
		}

		pub fn standardize_with_reorientation_page(&mut self, reorientation_page: &Page<Transformation>) -> () {
			*self += &reorientation_page[self.pos[PENTAGON_INDEX_OFFSET] as usize][self.rot[PENTAGON_INDEX_OFFSET] as usize];
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

	#[derive(Clone)]
	pub struct Animation {
		pub addr:		Addr,
		pub start:		Instant,
		pub duration:	Duration
	}

	impl Animation {
		pub fn is_done(&self) -> bool { self.is_done_at_time(&Instant::now()) }

		pub fn is_done_at_time(&self, time: &Instant) -> bool { self.start + self.duration <= *time }

		pub fn s(&self) -> f32 { self.s_at_time(&Instant::now()) }

		pub fn s_at_time(&self, time: &Instant) -> f32 { (*time - self.start).as_millis() as f32 / self.duration.as_millis() as f32 }
	}

	#[repr(align(32))]
	pub struct ExtendedPuzzleState {
		pub puzzle_state:	PuzzleState,
		pub pos_to_piece:	PuzzleStateComponent,
		pub animation:		Option<Animation>
	}

	impl ExtendedPuzzleState {

	}

	impl<'a> AddAssign<&'a Transformation> for ExtendedPuzzleState {
		fn add_assign(&mut self, transformation: &'a Transformation) -> () {
			self.puzzle_state += transformation;

			for piece_index in PIECE_RANGE {
				self.pos_to_piece[self.puzzle_state.pos[piece_index] as usize] = piece_index as PieceStateComponent;
			}
		}
	}

	impl Default for ExtendedPuzzleState {
		fn default() -> Self {
			Self {
				puzzle_state:	PuzzleState::SOLVED_STATE,
				pos_to_piece:	PuzzleStateComponent::SOLVED_STATE,
				animation:		None
			}
		}
	}
}

pub struct PuzzlePlugin;

impl PuzzlePlugin {
	fn startup_app(
		mut commands: Commands,
		piece_library: Res<PieceLibrary>,
		preferences: Res<Preferences>
	) -> () {
		log_result_err!(Self::startup_app_internal(&mut commands, &piece_library, &preferences));
	}

	fn startup_app_internal(
		commands: &mut Commands,
		piece_library: &Res<PieceLibrary>,
		preferences: &Res<Preferences>
	) -> LogErrorResult {
		let color_data: &ColorData<Color> = option_to_result!(preferences.color.try_get::<Color>())?;
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

	pub fn run_app(
		keyboard_input: Res<Input<KeyCode>>,
		preferences: Res<Preferences>,
		transformation_library: Res<TransformationLibraryRef>,
		polyhedra_data_library: Res<PolyhedraDataLibrary>,
		mut extended_puzzle_state: ResMut<ExtendedPuzzleState>,
		mut queries: QuerySet<(
			Query<(&mut CameraComponent, &mut Transform)>,
			Query<(&PieceComponent, &mut Transform)>
		)>
	) -> () {
		let input_data: &InputData = &preferences.input;

		if keyboard_input.just_pressed(KeyCode::Return) {
			const CAMERA_LOGIC_KEYS: [KeyCode; 6] = [KeyCode::Key0, KeyCode::Key9, KeyCode::Key8, KeyCode::Key7, KeyCode::Key6, KeyCode::Key5];
	
			let mut bit_field: i32 = 0_i32;

			for (index, key_code) in CAMERA_LOGIC_KEYS.iter().enumerate() {
				if keyboard_input.pressed(*key_code) {
					bit_field |= 1_i32 << index;
				}
			}

			if let Some((mut camera_component, _)) = queries.q0_mut().iter_mut().next() {
				camera_component.debug_camera_logic = bit_field;
			}
		}

		if let Some(animation) = &extended_puzzle_state.animation {
			let now: Instant = Instant::now();
			let word_pack: WordPack = transformation_library.book_pack_data.get_word_pack(animation.addr);

			if animation.is_done_at_time(&now) {
				// trace_expr!(extended_puzzle_state.puzzle_state);

				*extended_puzzle_state += word_pack.trfm;

				let reorientation_addr: Addr = Addr::from((
					TransformationType::Reorientation as usize,
					extended_puzzle_state.puzzle_state.pos[PENTAGON_INDEX_OFFSET] as usize,
					extended_puzzle_state.puzzle_state.rot[PENTAGON_INDEX_OFFSET] as usize
				));

				// trace_expr!(extended_puzzle_state.puzzle_state);

				*extended_puzzle_state += transformation_library.book_pack_data.trfm.get_word(reorientation_addr);

				// trace_expr!(reorientation_addr, transformation_library.book_pack_data.trfm.get_word(reorientation_addr), extended_puzzle_state.puzzle_state);

				for (piece_component, mut transform) in queries.q1_mut().iter_mut() {
					let piece_index: usize = piece_component.index;

					*transform = Transform::from_rotation(transformation_library.orientation_data[extended_puzzle_state.puzzle_state.pos[piece_index] as usize][extended_puzzle_state.puzzle_state.rot[piece_index] as usize].quat);
				}

				extended_puzzle_state.animation = None;

				// trace_expr!(extended_puzzle_state.puzzle_state.is_standardized());

				if let Some((mut camera_component, mut transform)) = queries.q0_mut().iter_mut().next() {
					let home_base_orientation:					Quat = transformation_library.orientation_data[PENTAGON_INDEX_OFFSET][PENTAGON_INDEX_OFFSET].quat;
					let part_1_addr:							Addr = camera_component.prev_addr;
					let inv_part_1_addr:						Addr = *transformation_library.book_pack_data.addr.get_word(part_1_addr);
					let part_2_addr:							Addr = reorientation_addr;
					let inv_part_1_rotation:					Quat = *transformation_library.book_pack_data.quat.get_word(inv_part_1_addr);
					let part_2_rotation:						Quat = *transformation_library.book_pack_data.quat.get_word(part_2_addr);
					let part_2_times_inv_part_1_rotation:		Quat = part_2_rotation * inv_part_1_rotation;
					let part_2_times_inv_part_1_orientation:	Quat = part_2_times_inv_part_1_rotation				* home_base_orientation;

					if camera_component.animation.is_some() {
						transform.rotation = part_2_times_inv_part_1_orientation;
						// transform.rotation = part_2_times_inv_part_1_conj_orientation;
						camera_component.animation = None;
					} else {
						let part_1_orientation:					Quat = transformation_library.orientation_data.get_word(part_1_addr).quat;
						let current_orientation:				Quat = transform.rotation;
						let new_addr:							Addr = CameraPlugin::compute_camera_addr(polyhedra_data_library.icosidodecahedron, &part_2_times_inv_part_1_orientation);
	
						let new_orientation:					Quat = transformation_library.orientation_data.get_word(new_addr).quat;
	
						trace_expr!(new_orientation, part_2_times_inv_part_1_orientation, new_orientation.dot(part_2_times_inv_part_1_orientation), new_orientation.conjugate(), part_2_times_inv_part_1_orientation.conjugate());
						let new_to_part_1_rotation:				Quat = part_1_orientation * new_orientation.conjugate();
						let rotation_2_b:						Quat = new_to_part_1_rotation.conjugate() * part_1_orientation * (new_to_part_1_rotation.conjugate() * current_orientation).conjugate();
	
						// new_orientation == part_2_times_inv_part_1_orientation
	
						transform.rotation = rotation_2_b.conjugate() * part_2_times_inv_part_1_orientation;
					}

					// let reorientation_quat_part_1: Quat = *transformation_library.book_pack_data.quat.get_word(camera_component.prev_addr); // The rotation from the camera address to the default (valid) address
					// let inv_reorientation_addr: Addr = *transformation_library.book_pack_data.addr.get_word(reorientation_addr); // I don't know why the inverse of the reorientation applied to the puzzle state is needed here, but it is
					// // let reorientation_quat_part_2: Quat = *transformation_library.book_pack_data.quat.get_word(reorientation_addr); // This is the one that makes the most sense?
					// let reorientation_quat_part_2: Quat = *transformation_library.book_pack_data.quat.get_word(inv_reorientation_addr); // Not actually sure, just giving this a try
					// let inv_reorientation_quat_full: Quat = (reorientation_quat_part_2 * reorientation_quat_part_1).conjugate(); // The rotation required to offset for the standardization, taking into account the camera's previous position

					// let home_base_orientation: Quat = transformation_library.orientation_data[PENTAGON_INDEX_OFFSET][PENTAGON_INDEX_OFFSET].quat;

					// let part_1_addr:		Addr = camera_component.prev_addr;
					// let inv_part_1_addr:	Addr = *transformation_library.book_pack_data.addr.get_word(part_1_addr);
					// let part_2_addr:		Addr = reorientation_addr;
					// let inv_part_2_addr:	Addr = *transformation_library.book_pack_data.addr.get_word(part_2_addr);

					// trace_expr!(0, part_1_addr, inv_part_1_addr, part_2_addr, inv_part_2_addr);

					// let part_1_rotation:		Quat = *transformation_library.book_pack_data.quat.get_word(part_1_addr);
					// let inv_part_1_rotation:	Quat = *transformation_library.book_pack_data.quat.get_word(inv_part_1_addr);
					// let part_2_rotation:		Quat = *transformation_library.book_pack_data.quat.get_word(part_2_addr);
					// let inv_part_2_rotation:	Quat = *transformation_library.book_pack_data.quat.get_word(inv_part_2_addr);

					// trace_expr!(inv_part_1_rotation, part_1_rotation.conjugate(), part_2_rotation, inv_part_2_rotation.conjugate());

					// let part_2_times_part_1_rotation:				Quat = part_2_rotation * part_1_rotation;
					// let part_2_times_part_1_conj_rotation:			Quat = part_2_times_part_1_rotation.conjugate();
					// let part_2_times_inv_part_1_rotation:			Quat = part_2_rotation * inv_part_1_rotation;
					// let part_2_times_inv_part_1_conj_rotation:		Quat = part_2_times_inv_part_1_rotation.conjugate();
					// let inv_part_2_times_part_1_rotation:			Quat = inv_part_2_rotation * part_1_rotation;
					// let inv_part_2_times_part_1_conj_rotation:		Quat = inv_part_2_times_part_1_rotation.conjugate();
					// let inv_part_2_times_inv_part_1_rotation:		Quat = inv_part_2_rotation * inv_part_1_rotation;
					// let inv_part_2_times_inv_part_1_conj_rotation:	Quat = inv_part_2_times_inv_part_1_rotation.conjugate();

					// let part_2_times_part_1_orientation:				Quat = part_2_times_part_1_rotation					* home_base_orientation;
					// let part_2_times_part_1_conj_orientation:			Quat = part_2_times_part_1_conj_rotation			* home_base_orientation;
					// let part_2_times_inv_part_1_orientation:			Quat = part_2_times_inv_part_1_rotation				* home_base_orientation;
					// let part_2_times_inv_part_1_conj_orientation:		Quat = part_2_times_inv_part_1_conj_rotation		* home_base_orientation;
					// let inv_part_2_times_part_1_orientation:			Quat = inv_part_2_times_part_1_rotation				* home_base_orientation;
					// let inv_part_2_times_part_1_conj_orientation:		Quat = inv_part_2_times_part_1_conj_rotation		* home_base_orientation;
					// let inv_part_2_times_inv_part_1_orientation:		Quat = inv_part_2_times_inv_part_1_rotation			* home_base_orientation;
					// let inv_part_2_times_inv_part_1_conj_orientation:	Quat = inv_part_2_times_inv_part_1_conj_rotation	* home_base_orientation;

					// let home_base_orientation: Quat = transformation_library.orientation_data[PENTAGON_INDEX_OFFSET][PENTAGON_INDEX_OFFSET].quat;
					// let part_1_addr:		Addr = camera_component.prev_addr;
					// let inv_part_1_addr:	Addr = *transformation_library.book_pack_data.addr.get_word(part_1_addr);
					// let part_2_addr:		Addr = reorientation_addr;
					// let inv_part_1_rotation:	Quat = *transformation_library.book_pack_data.quat.get_word(inv_part_1_addr);
					// let part_2_rotation:		Quat = *transformation_library.book_pack_data.quat.get_word(part_2_addr);
					// let part_2_times_inv_part_1_rotation:			Quat = part_2_rotation * inv_part_1_rotation;
					// let part_2_times_inv_part_1_orientation:			Quat = part_2_times_inv_part_1_rotation				* home_base_orientation;

					// if camera_component.animation.is_some() {
					// 	transform.rotation = part_2_times_inv_part_1_orientation;
					// 	// transform.rotation = part_2_times_inv_part_1_conj_orientation;
					// 	camera_component.animation = None;
					// } else {
					// 	let part_1_orientation: Quat = transformation_library.orientation_data.get_word(part_1_addr).quat;
					// 	let current_orientation: Quat = transform.rotation;
					// 	let new_addr: Addr = CameraPlugin::compute_camera_addr(polyhedra_data_library.icosidodecahedron, &part_2_times_inv_part_1_orientation);
	
					// 	let new_orientation: Quat = transformation_library.orientation_data.get_word(new_addr).quat;
	
					// 	trace_expr!(new_orientation, part_2_times_inv_part_1_orientation, new_orientation.dot(part_2_times_inv_part_1_orientation), new_orientation.conjugate(), part_2_times_inv_part_1_orientation.conjugate());
					// 	let new_to_part_1_rotation: Quat = part_1_orientation * new_orientation.conjugate();
					// 	let rotation_2_b: Quat = new_to_part_1_rotation.conjugate() * part_1_orientation * (new_to_part_1_rotation.conjugate() * current_orientation).conjugate();
	
					// 	// new_orientation == part_2_times_inv_part_1_orientation
	
					// 	transform.rotation = rotation_2_b.conjugate() * part_2_times_inv_part_1_orientation;
					// }

					// let icosidodecahedron_data: &Data = polyhedra_data_library.icosidodecahedron;

					// let part_2_times_part_1_addr:				Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &part_2_times_part_1_orientation);
					// let part_2_times_part_1_conj_addr:			Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &part_2_times_part_1_conj_orientation);
					// let part_2_times_inv_part_1_addr:			Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &part_2_times_inv_part_1_orientation);
					// let part_2_times_inv_part_1_conj_addr:		Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &part_2_times_inv_part_1_conj_orientation);
					// let inv_part_2_times_part_1_addr:			Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &inv_part_2_times_part_1_orientation);
					// let inv_part_2_times_part_1_conj_addr:		Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &inv_part_2_times_part_1_conj_orientation);
					// let inv_part_2_times_inv_part_1_addr:		Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &inv_part_2_times_inv_part_1_orientation);
					// let inv_part_2_times_inv_part_1_conj_addr:	Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &inv_part_2_times_inv_part_1_conj_orientation);

					// trace_expr!(0,
					// 	part_2_times_part_1_addr,
					// 	part_2_times_part_1_conj_addr,
					// 	part_2_times_inv_part_1_addr,
					// 	part_2_times_inv_part_1_conj_addr,
					// 	inv_part_2_times_part_1_addr,
					// 	inv_part_2_times_part_1_conj_addr,
					// 	inv_part_2_times_inv_part_1_addr,
					// 	inv_part_2_times_inv_part_1_conj_addr
					// );



					// if camera_component.animation.is_some() {
					// 	// let final_transformation_quat: Quat = match preferences.debug.inv_camera_addr {
					// 	// 	0 => part_2_times_part_1_rotation,
					// 	// 	1 => part_2_times_part_1_conj_rotation,
					// 	// 	2 => part_2_times_inv_part_1_rotation,
					// 	// 	3 => part_2_times_inv_part_1_conj_rotation,
					// 	// 	4 => inv_part_2_times_part_1_rotation,
					// 	// 	5 => inv_part_2_times_part_1_conj_rotation,
					// 	// 	6 => inv_part_2_times_inv_part_1_rotation,
					// 	// 	7 => inv_part_2_times_inv_part_1_conj_rotation,
					// 	// 	8 => part_1_rotation,
					// 	// 	9 => inv_part_1_rotation,
					// 	// 	10 => part_2_rotation,
					// 	// 	11 => inv_part_2_rotation,
					// 	// 	_ => inv_reorientation_quat_full
					// 	// };

					// 	// let corresponding_reorientation_orientation: Quat = part_2_rotation.conjugate() * (part_1_rotation * home_base_orientation);
					// 	// let corresponding_reorientation_addr: Addr = CameraPlugin::compute_camera_addr(icosidodecahedron_data, &corresponding_reorientation_orientation)
					// 	// let dirty_camera_quat: Quat = final_transformation_quat * transformation_library.orientation_data[PENTAGON_INDEX_OFFSET][PENTAGON_INDEX_OFFSET].quat;
					// 	// let dirty_camera_quat: Quat = home_base_orientation * (part_2_rotation.conjugate() * (part_1_rotation * home_base_orientation)).conjugate() * home_base_orientation;
					// 	// transform.rotation = dirty_camera_quat;
					// 	transform.rotation = part_2_times_inv_part_1_orientation;
					// 	// transform.rotation = part_2_times_inv_part_1_conj_orientation;
					// 	camera_component.animation = None;
					// } else {
					// 	// transform.rotate(inv_reorientation_quat_full);
					// 	// let part_1_orientation: Quat = transformation_library.orientation_data.get_word(part_1_addr).quat;
					// 	// let current_orientation: Quat = transform.rotation;
					// 	// let current_to_part_1_rotation: Quat = part_1_orientation * current_orientation.conjugate();

					// 	// transform.rotation = part_2_times_inv_part_1_conj_rotation * (current_orientation * part_1_orientation.conjugate()) * home_base_orientation;
					// 	// transform.rotation = (part_1_orientation * current_orientation.conjugate()) * part_2_times_inv_part_1_orientation;
					// 	// transform.rotation = (current_orientation * part_1_orientation.conjugate()) * part_2_times_inv_part_1_orientation;

					// 	// transform.rotate(current_to_part_1_rotation.conjugate() * part_2_times_inv_part_1_rotation * current_to_part_1_rotation);
					// 	// transform.rotate(current_to_part_1_rotation.conjugate() * part_2_times_inv_part_1_rotation.conjugate() * current_to_part_1_rotation);
					// 	// transform.rotate(part_2_times_inv_part_1_rotation.conjugate());
					// 	// transform.rotate(current_to_part_1_rotation);
					// 	// let new_addr: Addr = CameraPlugin::compute_camera_addr(polyhedra_data_library.icosidodecahedron, &part_2_times_inv_part_1_orientation);
					// 	// let rotation_1: Quat = polyhedra_data_library.icosidodecahedron.faces[new_addr.line_index()].get_rotation_quat(((PENTAGON_SIDE_COUNT + part_1_addr.word_index() - new_addr.word_index()) % PENTAGON_SIDE_COUNT) as u32);

					// 	// let rotation_1_a: Quat = rotation_1 * part_1_orientation * (rotation_1 * current_orientation).conjugate();
					// 	// let rotation_1_b: Quat = rotation_1.conjugate() * part_1_orientation * (rotation_1.conjugate() * current_orientation).conjugate();

					// 	// let new_orientation: Quat = transformation_library.orientation_data.get_word(new_addr).quat;

					// 	// trace_expr!(new_orientation, part_2_times_inv_part_1_orientation, new_orientation.dot(part_2_times_inv_part_1_orientation), new_orientation.conjugate(), part_2_times_inv_part_1_orientation.conjugate());
					// 	// let new_to_part_1_rotation: Quat = part_1_orientation * new_orientation.conjugate();

					// 	// let rotation_2_a: Quat = new_to_part_1_rotation * part_1_orientation * (new_to_part_1_rotation * current_orientation).conjugate();
					// 	// let rotation_2_b: Quat = new_to_part_1_rotation.conjugate() * part_1_orientation * (new_to_part_1_rotation.conjugate() * current_orientation).conjugate();

					// 	// transform.rotation = match camera_component.debug_camera_logic {
					// 	// 	1 => { (part_1_orientation * part_2_times_inv_part_1_orientation.conjugate()) * current_to_part_1_rotation.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// 	2 => { current_to_part_1_rotation.conjugate() * (part_1_orientation * part_2_times_inv_part_1_orientation.conjugate()) * part_2_times_inv_part_1_orientation },
					// 	// 	3 => { (part_2_times_inv_part_1_orientation * part_1_orientation.conjugate()) * current_to_part_1_rotation.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// 	4 => { current_to_part_1_rotation.conjugate() * (part_2_times_inv_part_1_orientation * part_1_orientation.conjugate()) * part_2_times_inv_part_1_orientation },
					// 	// 	5 => { (part_1_orientation * part_2_times_inv_part_1_orientation.conjugate()) * current_orientation.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// 	6 => { (part_1_orientation * part_2_times_inv_part_1_orientation.conjugate()) * (part_1_orientation * part_2_times_inv_part_1_orientation.conjugate()) * current_to_part_1_rotation.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// 	7 => { current_to_part_1_rotation * rotation_1 * part_2_times_inv_part_1_orientation },
					// 	// 	8 => { current_to_part_1_rotation * part_2_times_inv_part_1_orientation },
					// 	// 	9 => { current_to_part_1_rotation.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// 	10 => { rotation_1_a * part_2_times_inv_part_1_orientation },
					// 	// 	11 => { rotation_1_a.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// 	12 => { rotation_1_b * part_2_times_inv_part_1_orientation },
					// 	// 	13 => { rotation_1_b.conjugate() * part_2_times_inv_part_1_orientation }, // promising, only works when rotating face 0 when looking at face 0 (only one that can do that, though)
					// 	// 	14 => { rotation_2_a * part_2_times_inv_part_1_orientation },
					// 	// 	15 => { rotation_2_a.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// 	16 => { rotation_2_b * part_2_times_inv_part_1_orientation },
					// 	// 	17 => { rotation_2_b.conjugate() * part_2_times_inv_part_1_orientation }, // WE HAVE A WINNER, BABEEEEE
					// 	// 	_ => { part_2_times_inv_part_1_orientation },
					// 	// };
					// 	// transform.rotate(current_to_part_1_rotation);

					// 	// transform.rotation = match camera_component.debug_camera_logic {
					// 	// 	1 => { rotation_2_b.conjugate() * new_orientation },
					// 	// 	2 => { rotation_2_b.conjugate() * new_orientation.conjugate() },
					// 	// 	_ => { rotation_2_b.conjugate() * part_2_times_inv_part_1_orientation },
					// 	// };
					// }

					// let rotation_quat: Quat = transform.rotation * transformation_library.orientation_data[PENTAGON_INDEX_OFFSET][PENTAGON_INDEX_OFFSET].quat.conjugate();

					// transform.rotation = transformation_library.orientation_data.get_word(*transformation_library.book_pack_data.addr.get_word(reorientation_addr)).quat;
					// transform.rotation = transformation_library.orientation
					// transform.rotate(*transformation_library.book_pack_data.quat.get_word(*transformation_library.book_pack_data.addr.get_word(reorientation_addr)));
				}
			} else {
				let rotation: Quat = Quat::IDENTITY.short_slerp(*word_pack.quat, animation.s_at_time(&now));
				let puzzle_state: &InflatedPuzzleState = &extended_puzzle_state.puzzle_state;

				for (piece_component, mut transform) in queries.q1_mut().iter_mut() {
					let piece_index: usize = piece_component.index;

					if word_pack.mask.affects_piece(extended_puzzle_state.puzzle_state.pos[piece_index] as usize) {
						*transform = Transform::from_rotation(rotation * transformation_library.orientation_data[puzzle_state.pos[piece_index] as usize][puzzle_state.rot[piece_index] as usize].quat);
					}
				}

				// A rotation is in progress, so don't process further input
				return;
			}
		}

		for input_index in 0_usize .. HALF_PENTAGON_PIECE_COUNT {
			if keyboard_input.just_pressed(input_data.rotation_keys[input_index]) {
				let camera_addr: Addr = queries
					.q0_mut()
					.iter_mut()
					.next()
					.map_or(
						Addr::default(),
						|(_camera_component, transform): (Mut<CameraComponent>, Mut<Transform>)| -> Addr {
							CameraPlugin::compute_camera_addr(&polyhedra_data_library.icosidodecahedron, &transform.rotation)
						}
					);

				if !warn_expect!(camera_addr.is_valid_with_mask(Addr::from((None, Some(0), Some(0))))) {
					return;
				}

				let reoriented_positions: &PuzzleStateComponent = {
					&transformation_library
						.book_pack_data
						.trfm
						.get_word(
							*transformation_library
								.book_pack_data
								.addr
								.get_word(
									camera_addr | Addr::from_page(TransformationType::Reorientation as usize)
								)
						)
						.as_ref()
						.pos
				};
				let mut cycles: u32 = 1_u32;
				let addr: Addr = {
					let mut rotations: i32 = 1_i32;

					if keyboard_input.pressed(input_data.rotate_twice) {
						rotations += 1_i32;
						cycles += 1_u32;
					}

					if keyboard_input.pressed(input_data.counter_clockwise) {
						rotations *= -1_i32;
					}

					Addr::from((
						TransformationType::StandardRotation as usize,
						reoriented_positions[
							if keyboard_input.pressed(input_data.alt_hemi) {
								InflatedPuzzleState::invert_position(input_data.default_positions[input_index])
							} else {
								input_data.default_positions[input_index]
							}
						] as usize,
						(rotations + PENTAGON_SIDE_COUNT as i32) as usize % PENTAGON_SIDE_COUNT
					))
				};

				let mut animation: Animation = Animation {
					addr,
					start: Instant::now(),
					duration: Duration::from_millis((
						if preferences.speed.uniform_transformation_duration {
							preferences.speed.rotation_millis
						} else { 
							cycles * preferences.speed.rotation_millis
						}
					) as u64)
				};

				extended_puzzle_state.animation = Some(animation.clone());

				let (mut camera_component, transform): (Mut<CameraComponent>, Mut<Transform>) = log_option_none!(queries.q0_mut().iter_mut().next());

				camera_component.prev_addr = camera_addr | Addr::from_page(TransformationType::Reorientation as usize);

				if !keyboard_input.pressed(preferences.input.disable_recentering) {
					animation.addr = camera_addr;
					camera_component.animation = Some(CameraAnimation {
						puzzle_animation: animation,
						start_quat: transform.rotation
					});
				}

				// Don't accept multiple input actions on the same frame
				return;
			}
		}
	}
}

impl Plugin for PuzzlePlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app
			.insert_resource(ExtendedPuzzleState::default())
			.add_startup_system(Self::startup_app
				.system()
				.label(STRING_DATA.labels.puzzle_startup.as_ref())
				.after(STRING_DATA.labels.piece_library_startup.as_ref())
				.after(STRING_DATA.labels.color_data_typed_startup.as_ref())
			)
			.add_system(Self::run_app
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
				PuzzleState as InflatedPuzzleState,
				PuzzleStateConsts
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

	#[test]
	fn test_puzzle_state_properties() -> () {
		const ITERATION_COUNT: usize = 10000000_usize;

		let standard_rotations:									&Page<Transformation>		= &TransformationLibrary::get().book_pack_data.trfm[TransformationType::StandardRotation as usize];
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
			puzzle_state += &standard_rotations[thread_rng.gen_range(PENTAGON_PIECE_RANGE)][thread_rng.gen_range(1_usize .. PENTAGON_SIDE_COUNT)];

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