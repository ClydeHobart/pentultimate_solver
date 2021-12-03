pub mod packs;

use {
	crate::{
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		ui::camera::CameraPlugin
	},
	super::{
		consts::*,
		inflated::{
			PieceStateComponent,
			PuzzleState,
			PuzzleStateComponent,
			PuzzleStateConsts
		}
	},
	bevy::prelude::*,
	bit_field::BitField,
	std::{
		fmt::{
			Debug,
			Error,
			Formatter
		},
		intrinsics::unlikely,
		ops::{
			Neg,
			Range
		},
		mem::{
			MaybeUninit,
			transmute
		}
	}
};

pub use packs::*;

const_assert!(PIECE_COUNT <= u32::BITS as usize);

#[derive(Clone, Copy)]
pub struct Mask(u32);

impl Mask {
	#[inline]
	pub fn affects_piece(&self, piece_index: usize) -> bool {
		self.0 & (1_u32 << piece_index) != 0_u32
	}

	fn from_pentagon_index(pentagon_index: usize) -> Self {
		Self({
			if pentagon_index >= PENTAGON_PIECE_COUNT {
				log::warn!("HemisphereMask::new({}) was called, but {} is an invalid pentagon index", pentagon_index, pentagon_index);

				0_u32
			} else {
				let data: &Data = Data::get(Polyhedron::Icosidodecahedron);
				let faces: &Vec<FaceData> = &data.faces;
				let face_normal: Vec3 = faces[pentagon_index].norm;

				let mut mask: u32 = 0_u32;

				for pent_index in 0_usize .. PIECE_COUNT {
					mask |= ((faces[pent_index].norm.dot(face_normal) > 0.0_f32) as u32) << pent_index;
				}

				mask
			}
		})
	}

	fn from_puzzle_states(prev_puzzle_state: &PuzzleState, curr_puzzle_state: &PuzzleState) -> Self {
		Self({
			let mut mask: u32 = 0_u32;

			for piece_index in PIECE_RANGE {
				mask |= ((curr_puzzle_state.pos[piece_index] != prev_puzzle_state.pos[piece_index] || curr_puzzle_state.rot[piece_index] != prev_puzzle_state.rot[piece_index]) as u32) << piece_index;
			}

			mask
		})
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
	Reorientation,		// This transformation standardizes the puzzle state (reorients piece 0 to be at position 0 with rotation 0)
	StandardRotation,	// This transformation rotates one of the hemispherical halves about one of the 12 pentagonal pieces n fifth-rotations, where n is in [0, 5)
	Count				// Not an actual transformation, this allows us to statically allocate memory for a type that has a variant for each "valid" Type variant
}

impl Type {
	fn invert(self) -> Self {
		match self {
			// Eventually, compound transformations will be added that map to a different (also compound) type
			_ => { self }
		}
	}
}

pub trait Addr {
	fn get_page_index(&self)		-> usize { unimplemented!(); }
	fn get_line_index(&self)		-> usize { unimplemented!(); }
	fn get_long_line_index(&self)	-> usize { unimplemented!(); }
	fn get_word_index(&self)		-> usize { unimplemented!(); }

	fn set_page_index(&mut self, _page_index: usize)			-> &mut Self { unimplemented!(); }
	fn set_line_index(&mut self, _line_index: usize)			-> &mut Self { unimplemented!(); }
	fn set_long_line_index(&mut self, _long_line_index: usize)	-> &mut Self { unimplemented!(); }
	fn set_word_index(&mut self, _word_index: usize)			-> &mut Self { unimplemented!(); }
}

pub trait HalfAddrConsts {
	const INVALID: u8;
	const LINE_INDEX_BITS: Range<usize>;
	const WORD_INDEX_BITS: Range<usize>;
}

#[derive(Clone, Copy)]
pub struct HalfAddr(u8);

impl HalfAddr {
	#[inline(always)]
	pub fn is_valid(&self) -> bool { self.0 != Self::INVALID && self.word_index_is_valid()}

	#[inline(always)]
	pub fn line_index_is_valid(&self) -> bool { Self::is_valid_line_index(unsafe { self.get_line_index_unchecked() }) }

	#[inline(always)]
	pub fn is_valid_line_index(line_index: usize) -> bool { line_index < Library::LINE_COUNT }

	#[inline(always)]
	pub fn is_valid_long_line_index(long_line_index: usize) -> bool { long_line_index < Library::LONG_LINE_COUNT }

	#[inline(always)]
	pub fn word_index_is_valid(&self) -> bool { Self::is_valid_word_index(unsafe { self.get_word_index_unchecked() }) }

	#[inline(always)]
	pub fn is_valid_word_index(word_index: usize) -> bool { word_index < Library::WORD_COUNT }

	pub fn new(line_index: usize, word_index: usize) -> Self {
		*Self::default()
			.set_long_line_index(line_index) // Accomodate long line indices
			.set_word_index(word_index)
	}

	pub fn invalidate(&mut self) -> () {
		self.0 = Self::INVALID;
	}

	#[inline(always)]
	unsafe fn get_line_index_unchecked(&self) -> usize {
		self.0.get_bits(Self::LINE_INDEX_BITS) as usize
	}

	#[inline(always)]
	unsafe fn get_word_index_unchecked(&self) -> usize {
		self.0.get_bits(Self::WORD_INDEX_BITS) as usize
	}
}

impl HalfAddrConsts for HalfAddr {
	const INVALID: u8 = u8::MAX;
	const LINE_INDEX_BITS: Range<usize> = 3_usize .. u8::BIT_LENGTH;
	const WORD_INDEX_BITS: Range<usize> = 0_usize .. 3_usize;
}

impl Addr for HalfAddr {
	fn get_line_index(&self) -> usize {
		assert!(self.is_valid() && self.line_index_is_valid());

		// Safe: checked above
		unsafe { self.get_line_index_unchecked() }
	}

	fn get_long_line_index(&self) -> usize {
		const_assert!(Library::LONG_LINE_COUNT <= 1_usize << (HalfAddr::LINE_INDEX_BITS.end - HalfAddr::LINE_INDEX_BITS.start));
		assert!(self.is_valid());

		// Safe: checked above (const_assert! shows the long line index is always valid)
		unsafe { self.get_line_index_unchecked() }
	}

	fn get_word_index(&self) -> usize {
		assert!(self.is_valid());

		// Safe: checked above
		unsafe { self.get_word_index_unchecked() }
	}

	fn set_line_index(&mut self, line_index: usize) -> &mut Self {
		assert!(Self::is_valid_line_index(line_index));
		self.0.set_bits(Self::LINE_INDEX_BITS, line_index as u8);

		self
	}

	fn set_long_line_index(&mut self, long_line_index: usize) -> &mut Self {
		// This assert is unnecessary, since the call to set_bits() would panic if this weren't the case, but it makes
		// the issue more clear
		assert!(Self::is_valid_long_line_index(long_line_index));
		self.0.set_bits(Self::LINE_INDEX_BITS, long_line_index as u8);

		self
	}

	fn set_word_index(&mut self, word_index: usize) -> &mut Self {
		assert!(Self::is_valid_word_index(word_index));
		self.0.set_bits(Self::WORD_INDEX_BITS, word_index as u8);

		self
	}
}

impl From<(usize, usize)> for HalfAddr {
	fn from((line_index, word_index): (usize, usize)) -> Self { Self::new(line_index, word_index) }
}

impl Debug for HalfAddr {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
		formatter
			.debug_struct("HalfAddr")
			.field("line_index", unsafe { &self.get_line_index_unchecked() })
			.field("word_index", unsafe { &self.get_word_index_unchecked() })
			.finish()
	}
}

impl Default for HalfAddr {
	fn default() -> Self {
		Self(Self::INVALID)
	}
}

pub trait FullAddrConsts {
	const INVALID_INDEX: u8;
}

#[derive(Clone, Copy)]
pub struct FullAddr {
	page_index:	u8,
	half_addr:	HalfAddr
}

impl FullAddrConsts for FullAddr {
	const INVALID_INDEX: u8 = u8::MAX;
}

impl FullAddr {
	#[inline(always)]
	pub fn is_valid(&self) -> bool { self.page_index_is_valid() && self.half_addr_is_valid() }

	#[inline(always)]
	pub fn page_index_is_valid(&self) -> bool { Self::is_valid_page_index(unsafe { self.get_page_index_unchecked() }) }

	#[inline(always)]
	pub fn is_valid_page_index(page_index: usize) -> bool { page_index < Library::PAGE_COUNT }

	#[inline(always)]
	pub fn half_addr_is_valid(&self) -> bool { Self::is_valid_half_addr(self.half_addr) }

	#[inline(always)]
	pub fn is_valid_half_addr(half_addr: HalfAddr) -> bool {
		half_addr.line_index_is_valid() && half_addr.word_index_is_valid()
	}

	#[inline(always)]
	pub fn line_index_is_valid(&self) -> bool { self.half_addr.line_index_is_valid() }

	#[inline(always)]
	pub fn word_index_is_valid(&self) -> bool { self.half_addr.word_index_is_valid() }

	pub fn get_page_index_type(&self) -> Option<Type> {
		if self.page_index_is_valid() {
			Some(unsafe { transmute(self.page_index) })
		} else {
			None
		}
	}

	pub fn get_cycles(&self) -> u32 {
		match self.get_page_index_type() {
			Some(page_index_type) => {
				match page_index_type {
					Type::Reorientation => {
						1_u32
					},
					Type::StandardRotation => {
						if self.word_index_is_valid() {
							(((self.get_word_index() as i32 + 2_i32) % PENTAGON_SIDE_COUNT as i32) - 2_i32).abs() as u32
						} else {
							0_u32
						}
					},
					_ => { unreachable!() }
				}
			},
			None => {
				0_u32
			}
		}
	}

	pub fn get_half_addr(&self) -> HalfAddr { self.half_addr }

	pub fn set_half_addr(&mut self, half_addr: HalfAddr) -> &mut FullAddr {
		assert!(self.half_addr_is_valid());
		self.half_addr = half_addr;

		self
	}

	pub fn invalidate(&mut self) -> () {
		*self = Self::default()
	}

	pub fn invalidate_page_index(&mut self) -> () {
		self.page_index = Self::INVALID_INDEX;
	}

	pub fn invalidate_half_addr(&mut self) -> () {
		self.half_addr.invalidate();
	}

	pub fn invert(&self) -> Self {
		Self {
			page_index: self.get_page_index_type().map_or(
				Self::INVALID_INDEX,
				|page_index_type: Type| -> u8 { page_index_type.invert() as u8 }
			),
			half_addr: if self.half_addr_is_valid() {
				HalfAddr::from((
					self.get_line_index(),
					{
						(
							(
								self.get_word_index() as i32
									* -1_i32
									+ PENTAGON_SIDE_COUNT as i32
							) % PENTAGON_SIDE_COUNT as i32
						) as usize
					}
				))
			} else {
				HalfAddr::default()
			}
		}
	}

	#[inline(always)]
	unsafe fn get_page_index_unchecked(&self) -> usize {
		self.page_index as usize
	}
}

impl Addr for FullAddr {
	fn get_page_index(&self) -> usize {
		assert!(self.page_index_is_valid());

		self.page_index as usize
	}

	fn get_line_index(&self) -> usize {
		// asserts in HalfAddr::get_line_index()
		self.half_addr.get_line_index()
	}

	fn get_word_index(&self) -> usize {
		// asserts in HalfAddr::get_word_index()
		self.half_addr.get_word_index()
	}

	fn set_page_index(&mut self, page_index: usize) -> &mut FullAddr {
		assert!(Self::is_valid_page_index(page_index));
		self.page_index = page_index as u8;

		self
	}

	fn set_line_index(&mut self, line_index: usize) -> &mut FullAddr {
		// asserts in HalfAddr::set_line_index()
		self.half_addr.set_line_index(line_index);

		self
	}

	fn set_word_index(&mut self, word_index: usize) -> &mut FullAddr {
		// asserts in HalfAddr::set_word_index()
		self.half_addr.set_word_index(word_index);

		self
	}
}

impl Debug for FullAddr {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
		let mut debug_struct: std::fmt::DebugStruct = formatter.debug_struct("FullAddr");
		let mut page_index_type_local: Type = Type::Count;

		debug_struct.field("page_index", &self.get_page_index_type()
			.map_or(
				&"[invalid]" as &dyn std::fmt::Debug,
				|page_index_type: Type| -> &dyn std::fmt::Debug {
					page_index_type_local = page_index_type; &page_index_type_local
				}
			)
		);

		if self.half_addr.is_valid() && self.half_addr.line_index_is_valid() {
			debug_struct.field("line_index", &self.half_addr.get_line_index());
			debug_struct.field("word_index", &self.half_addr.get_word_index());
		} else {
			debug_struct.field("half_addr", &"[invalid]");
		}

		debug_struct.finish()
	}
}

impl Default for FullAddr {
	fn default() -> Self {
		Self {
			page_index: Self::INVALID_INDEX,
			half_addr: HalfAddr::default()
		}
	}
}

impl From<(usize, usize, usize)> for FullAddr {
	fn from((page_index, line_index, word_index): (usize, usize, usize)) -> Self {
		Self {
			page_index: if Self::is_valid_page_index(page_index) { page_index as u8 } else { Self::INVALID_INDEX },
			half_addr: if HalfAddr::is_valid_line_index(line_index) && HalfAddr::is_valid_word_index(word_index) {
				HalfAddr::new(line_index, word_index)
			} else {
				HalfAddr::default()
			}
		}
	}
}

impl From<Type> for FullAddr {
	fn from(page_index_type: Type) -> Self {
		Self {
			page_index: page_index_type as u8,
			.. Self::default()
		}
	}
}

impl From<HalfAddr> for FullAddr {
	fn from(half_addr: HalfAddr) -> Self {
		Self {
			half_addr: if Self::is_valid_half_addr(half_addr) { half_addr } else { HalfAddr::default() },
			.. Self::default()
		}
	}
}

impl From<(Type, HalfAddr)> for FullAddr {
	fn from((page_index_type, half_addr): (Type, HalfAddr)) -> Self {
		Self {
			page_index: page_index_type as u8,
			half_addr
		}
	}
}

#[derive(Clone, Copy, Default)]
pub struct Action {
	transformation:		FullAddr,
	camera_start:		HalfAddr,
	standardization:	HalfAddr
}

impl Action {
	pub fn new(transformation: FullAddr, camera_start: HalfAddr, standardization: HalfAddr) -> Self {
		Self {
			transformation,
			camera_start,
			standardization
		}
	}
	#[inline(always)]
	pub fn transformation(&self) -> &FullAddr { &self.transformation }
	#[inline(always)]
	pub fn camera_start(&self) -> &HalfAddr { &self.camera_start }
	#[inline(always)]
	pub fn standardization(&self) -> FullAddr { (Type::Reorientation, self.standardization).into() }

	pub fn is_valid(&self) -> bool {
		self.transformation.is_valid() && self.camera_start.is_valid() && self.standardization.is_valid()
	}

	pub fn invert(&self) -> Self {
		if self.is_valid() {
			let standardization_word_pack: WordPack = Library::get()
				.book_pack_data
				.get_word_pack(self.standardization());
			let mut inflated_puzzle_state: PuzzleState = PuzzleState::SOLVED_STATE;
			let mut transformation: FullAddr = self.transformation.invert();
			let line_index: usize = transformation.get_line_index();

			transformation.set_line_index(standardization_word_pack.trfm.as_ref().pos[line_index] as usize);
			inflated_puzzle_state += Library::get().book_pack_data.trfm.get_word(transformation);

			Self::new(
				transformation,
				CameraPlugin::compute_camera_addr(
					&(*standardization_word_pack.quat
						* Library::get().orientation_data.get_word(self.camera_start).quat)
				),
				inflated_puzzle_state.standardization_half_addr()
			)
		} else {
			Self::default()
		}
	}
}

pub struct OrientationData {
	pub quat: Quat
}

#[derive(Default, PartialEq)]
pub struct Transformation(PuzzleState);

impl Transformation {
	pub fn arrays(&self) -> (&PuzzleStateComponent, &PuzzleStateComponent) {
		self.0.arrays()
	}

	pub fn arrays_mut(&mut self) -> (&mut PuzzleStateComponent, &mut PuzzleStateComponent) {
		self.0.arrays_mut()
	}

	pub fn as_ref(&self) -> &PuzzleState {
		&self.0
	}

	pub fn as_ref_mut(&mut self) -> &mut PuzzleState {
		&mut self.0
	}

	pub fn is_valid(&self) -> bool {
		self.0.is_valid()
	}
}

impl Debug for Transformation {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), Error> {
		let (pos_string, rot_string): (String, String) = self.0.debug_strings();

		formatter
			.debug_struct("Transformation")
			.field("pos", &pos_string)
			.field("rot", &rot_string)
			.finish()
	}
}

impl<'a> Neg for &'a Transformation {
	type Output = Transformation;

	fn neg(self: &'a Transformation) -> Self::Output {
		let prev_state: PuzzleState = PuzzleState::SOLVED_STATE;
		let curr_state: PuzzleState = &prev_state + self;

		&prev_state - &curr_state
	}
}

type Trfm = Transformation;

/*
The TrfmPage means different things depending on its corresponding Type
* Reorientation:
	* The first index is the current position of piece 0
	* The second index is the current rotation of piece 0
* StandardRotation:
	* The first index is which piece will be the basis of rotation
	* The second index is how many times it will be turned
*/

pub trait LibraryConsts {
	const WORD_COUNT:		usize;
	const LINE_COUNT:		usize;
	const LONG_LINE_COUNT:	usize;
	const PAGE_COUNT:		usize;
}

pub type Word<T>		= T;
pub type Line<T>		= [Word<T>; Library::WORD_COUNT];
pub type Page<T>		= [Line<T>; Library::LINE_COUNT];
pub type LongPage<T>	= [Line<T>; Library::LONG_LINE_COUNT];
pub type Book<T>		= [Page<T>; Library::PAGE_COUNT];

pub trait FindWord<T>
	where
		T: PartialEq
{
	fn find_word(&self, word: &Word<T>) -> Option<FullAddr>;
}

impl<T> FindWord<T> for Word<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<FullAddr> {
		if self == target_word {
			Some(FullAddr::default())
		} else {
			None
		}
	}
}

impl<T> FindWord<T> for Line<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<FullAddr> {
		for (word_index, word) in self.iter().enumerate() {
			if let Some(mut address) = word.find_word(target_word) {
				return Some(*address.set_word_index(word_index));
			}
		}

		None
	}
}

impl<T> FindWord<T> for Page<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<FullAddr> {
		for (line_index, line) in self.iter().enumerate() {
			if let Some(mut address) = line.find_word(target_word) {
				return Some(*address.set_line_index(line_index));
			}
		}

		None
	}
}

impl<T> FindWord<T> for Book<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<FullAddr> {
		for (page_index, page) in self.iter().enumerate() {
			if let Some(mut address) = page.find_word(target_word) {
				return Some(*address.set_page_index(page_index));
			}
		}

		None
	}
}

pub trait GetWord<A : Addr, T> {
	fn get_word(&self, addr: A) -> &Word<T>;
	fn get_word_mut(&mut self, addr: A) -> &mut Word<T>;
}

impl<A : Addr + Sized, T> GetWord<A, T> for Page<T> {
	fn get_word(&self, addr: A) -> &Word<T> {
		&self[addr.get_line_index()][addr.get_word_index()]
	}

	fn get_word_mut(&mut self, addr: A) -> &mut Word<T> {
		&mut self[addr.get_line_index()][addr.get_word_index()]
	}
}

impl<A : Addr + Sized, T> GetWord<A, T> for LongPage<T> {
	fn get_word(&self, addr: A) -> &Word<T> {
		&self[addr.get_long_line_index()][addr.get_word_index()]
	}

	fn get_word_mut(&mut self, addr: A) -> &mut Word<T> {
		&mut self[addr.get_long_line_index()][addr.get_word_index()]
	}
}

impl<A : Addr + Sized, T> GetWord<A, T> for Book<T> {
	fn get_word(&self, addr: A) -> &Word<T> {
		&self[addr.get_page_index()][addr.get_line_index()][addr.get_word_index()]
	}

	fn get_word_mut(&mut self, addr: A) -> &mut Word<T> {
		&mut self[addr.get_page_index()][addr.get_line_index()][addr.get_word_index()]
	}
}

// A multi-tiered collection of transformations and associated data
// Due to the alignment restrictions on Trfm, it wastes less space to store this as an SoA(oAoA) vs an A(oAoA)oS
pub struct Library {
	pub book_pack_data:		BookPackData,
	pub orientation_data:	LongPage<OrientationData>
}

impl LibraryConsts for Library {
	const WORD_COUNT:		usize = PENTAGON_SIDE_COUNT;
	const LINE_COUNT:		usize = PENTAGON_PIECE_COUNT;
	const LONG_LINE_COUNT:	usize = PIECE_COUNT;
	const PAGE_COUNT:		usize = Type::Count as usize;
}

impl Library {
	fn new() -> Self {
		let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);

		let mut transformation_library: Library = unsafe { std::mem::MaybeUninit::<Library>::zeroed().assume_init() };

		for (long_line_index, orientation_data_long_line) in transformation_library.orientation_data.iter_mut().enumerate() {
			let face_data: &FaceData = &icosidodecahedron_data.faces[long_line_index];

			for (word_index, orientation_data_word) in orientation_data_long_line.iter_mut().enumerate() {
				orientation_data_word.quat = face_data.get_rotated_quat(word_index as u32);
			}
		}

		{
			let (book_pack_data, orientation_data): (&mut BookPackData, &LongPage<OrientationData>) = (&mut transformation_library.book_pack_data, &transformation_library.orientation_data);

			{
				let initial_pent_quat: Quat = icosidodecahedron_data.faces[PENTAGON_INDEX_OFFSET].quat;
				let reorientation_page_index: usize = Type::Reorientation as usize;

				let mut page_pack_mut: PagePackMut = book_pack_data.get_page_pack_mut(Type::Reorientation as usize);

				page_pack_mut.iter_mut(|line_index: usize, mut line_pack_mut: LinePackMut| -> () {
					line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
						let reorientation_quat: Quat = initial_pent_quat * orientation_data[line_index][word_index].quat.conjugate();
						let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) = word_pack_mut.trfm.arrays_mut();

						for piece_index in PIECE_RANGE {
							let (pos, rot): (usize, usize) = icosidodecahedron_data.get_pos_and_rot(
								&(reorientation_quat * icosidodecahedron_data.faces[piece_index].quat),
								None // We could put a filter in here, but it'd be slower, and the quat math is precise enough that it's unnecessary here
							);

							pos_array[piece_index] = pos as PieceStateComponent;
							rot_array[piece_index] = rot as PieceStateComponent;
						}

						*word_pack_mut.quat = reorientation_quat;
						*word_pack_mut.mask = Mask::from_puzzle_states(&PuzzleState::SOLVED_STATE, word_pack_mut.trfm.as_ref());
						*word_pack_mut.addr = FullAddr::default();
					});
				});

				let trfm_page:		&Page<Trfm>		= page_pack_mut.trfm;
				let addr_page_mut:	&mut Page<FullAddr>	= page_pack_mut.addr;

				for (line_index, addr_line_mut) in addr_page_mut.iter_mut().enumerate() {
					for (word_index, addr_word_mut) in addr_line_mut.iter_mut().enumerate() {
						*addr_word_mut = trfm_page
							.find_word(&(-&trfm_page[line_index][word_index]))
							.map(|mut address: FullAddr| -> FullAddr {
								*address.set_page_index(reorientation_page_index)
							})
							.unwrap_or_default();
					}
				}
			}

			{
				let mut page_pack_mut: PagePackMut = book_pack_data.get_page_pack_mut(Type::StandardRotation as usize);

				page_pack_mut.iter_mut(|line_index: usize, mut line_pack_mut: LinePackMut| -> () {
					let face_data: &FaceData = &icosidodecahedron_data.faces[line_index];
					let mask: Mask = Mask::from_pentagon_index(line_index);

					line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
						let rotation_quat: Quat = face_data.get_rotation_quat(word_index as u32);
						let mask: Mask = if word_index != 0 { mask } else { Mask(0_u32) };

						let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) = word_pack_mut.trfm.arrays_mut();

						for piece_index in PIECE_RANGE {
							let (pos, rot): (usize, usize) = if mask.affects_piece(piece_index) {
								icosidodecahedron_data.get_pos_and_rot(
									&(rotation_quat * icosidodecahedron_data.faces[piece_index].quat),
									None // We could put a filter in here, but it'd be slower, and the quat math should be precise enough that it's unnecessary here
								)
							} else {
								(piece_index, 0)
							};

							pos_array[piece_index] = pos as PieceStateComponent;
							rot_array[piece_index] = rot as PieceStateComponent;
						}

						*word_pack_mut.quat = rotation_quat;
						*word_pack_mut.mask = mask;
						*word_pack_mut.addr = FullAddr::from((
							Type::StandardRotation as usize,
							line_index,
							(Library::WORD_COUNT - word_index) % Library::WORD_COUNT
						));
					});
				});
			}
		}

		transformation_library
	}

	pub fn get() -> &'static Self {
		// Self::new() is completely deterministic, so this is safe. Re-running the function whilst another thread is
		// already trying to generate it is not optimal, but it can only happen at the beginning of execution
		unsafe {
			if unlikely(LIBRARY.1) {
				LIBRARY.0 = MaybeUninit::<Self>::new(Self::new());
				LIBRARY.1 = false;
			}

			LIBRARY.0.assume_init_ref()
		}
	}
}

static mut LIBRARY: (MaybeUninit<Library>, bool) = (MaybeUninit::<Library>::uninit(), true);

pub struct TransformationPlugin;

impl Plugin for TransformationPlugin {
	fn build(&self, _app: &mut AppBuilder) -> () {
		// Initialize the Library
		Library::get();
	}
}

#[cfg(test)]
mod tests {
	use {
		crate::prelude::*,
		super::*
	};

	fn test_validity(transformation_library: &Library) -> () {
		let book_pack: BookPack = transformation_library.book_pack_data.get_book_pack(());

		book_pack.iter(|page_index: usize, page_pack: PagePack| -> () {
			page_pack.iter(|line_index: usize, line_pack: LinePack| -> () {
				line_pack.iter(|word_index: usize, word_pack: WordPack| -> () {
					let trfm: &Trfm = word_pack.trfm;

					if !trfm.is_valid() {
						log::error!("Trfm ({}, {}, {}) is invalid", page_index, line_index, word_index);
						error_expr!(word_pack.trfm);

						panic!();
					}

					if !word_pack.addr.is_valid() {
						log::error!("Inverse address for transformation ({}, {}, {}) is invalid", page_index, line_index, word_index);
						error_expr!(word_pack.addr);

						panic!();
					}

					let inv_addr: FullAddr = *word_pack.addr;
					let inv_trfm: &Trfm = book_pack.get_word_pack(inv_addr).trfm;

					if *trfm != -inv_trfm || -trfm != *inv_trfm {
						log::error!("Transformation addressed by ({}, {}, {}), the associated address of transformation ({}, {}, {}) is not the true inverse transformation",
							inv_addr.get_page_index(), inv_addr.get_line_index(), inv_addr.get_word_index(),
							page_index, line_index, word_index
						);
						error_expr!(trfm, -trfm, inv_trfm, -inv_trfm);

						panic!();
					}

					let solved_state: PuzzleState = PuzzleState::SOLVED_STATE;
					let middle_state: PuzzleState = &solved_state + trfm;
					let end_state: PuzzleState = &middle_state + inv_trfm;

					if end_state != solved_state {
						log::error!("solved_state + trfm + inv_trfm != solved_state for transformation ({}, {}, {})",
							page_index, line_index, word_index
						);
						error_expr!(solved_state, trfm, middle_state, inv_trfm, end_state);

						panic!();
					}
				});
			});
		});
	}

	fn test_reorientations(transformation_library: &Library) -> () {
		let page_pack: PagePack = transformation_library.book_pack_data.get_page_pack(Type::Reorientation as usize);
		let standard_rotation_page: &Page<Trfm> = &transformation_library.book_pack_data.trfm[Type::StandardRotation as usize];
		let reorientation_tests: [Vec<(usize, usize)>; PENTAGON_PIECE_COUNT] = from_ron::<[Vec<(usize, usize)>; PENTAGON_PIECE_COUNT]>(STRING_DATA.tests.reorientation_tests.as_ref()).to_option().unwrap();

		page_pack.iter(|line_index: usize, line_pack: LinePack| -> () {
			let pent_puzzle_state: PuzzleState = {
				let mut solved_state: PuzzleState = PuzzleState::SOLVED_STATE;

				for (standard_rotation_pent_index, standard_rotation_rotation_index) in &reorientation_tests[line_index] {
					solved_state += &standard_rotation_page[*standard_rotation_pent_index][*standard_rotation_rotation_index];
				}

				solved_state
			};

			if !pent_puzzle_state.is_valid() {
				log::error!("Puzzle state for pentagon {} isn't valid", line_index);
				error_expr!(pent_puzzle_state);

				panic!();
			}

			line_pack.iter(|word_index: usize, word_pack: WordPack| -> () {
				let prev_puzzle_state: PuzzleState	= {
					let mut pent_puzzle_state_clone: PuzzleState = pent_puzzle_state.clone();

					pent_puzzle_state_clone += &standard_rotation_page[line_index][word_index];

					pent_puzzle_state_clone
				};

				if !prev_puzzle_state.is_valid() {
					log::error!("Puzzle state isn't valid before reorientation with pentagon {} and rotation {}", line_index, word_index);
					error_expr!(pent_puzzle_state, standard_rotation_page[line_index][word_index], prev_puzzle_state);

					panic!();
				}

				if prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET] != line_index as PieceStateComponent || prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET] != word_index as PieceStateComponent {
					log::error!("Reorientation with pentagon {} and rotation {} has position {} and rotation {} for piece {}",
						line_index,
						word_index,
						prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET],
						prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET],
						PENTAGON_INDEX_OFFSET
					);
					error_expr!(prev_puzzle_state);

					panic!();
				}

				let trfm:						&Trfm		= &word_pack.trfm;

				let mut curr_puzzle_state:		PuzzleState	= prev_puzzle_state.clone();
				let mut curr_puzzle_state_alt:	PuzzleState	= prev_puzzle_state.clone();

				curr_puzzle_state += trfm;

				if !curr_puzzle_state.is_standardized() {
					log::error!("Puzzle state isn't standardized after reorientation with pentagon {} and rotation {}", line_index, word_index);
					error_expr!(prev_puzzle_state, trfm, curr_puzzle_state);

					panic!();
				}

				if !curr_puzzle_state.is_valid() {
					log::error!("Puzzle state isn't valid after reorientation with pentagon {} and rotation {}", line_index, word_index);
					error_expr!(prev_puzzle_state, trfm, curr_puzzle_state);

					panic!();
				}

				curr_puzzle_state_alt.naive_add_assign(trfm);

				if curr_puzzle_state_alt != curr_puzzle_state {
					log::error!("Reoriented puzzle state doesn't match the naively reoriented puzzle state with pentagon {} and rotation {}", line_index, word_index);
					error_expr!(prev_puzzle_state, trfm, curr_puzzle_state, curr_puzzle_state_alt);

					panic!();
				}

				let transformation_from_states:	Trfm = &curr_puzzle_state - &prev_puzzle_state;

				if transformation_from_states != *trfm {
					log::error!("Trfm from previous to current state doesn't match applied transformation with pentagon {} and rotation {}", line_index, word_index);
					error_expr!(trfm, prev_puzzle_state, curr_puzzle_state, transformation_from_states);

					panic!();
				}

				for piece_index in PIECE_RANGE {
					if word_pack.mask.affects_piece(piece_index) != (
						curr_puzzle_state.pos[piece_index] != prev_puzzle_state.pos[piece_index] ||
						curr_puzzle_state.rot[piece_index] != prev_puzzle_state.rot[piece_index]
					) {
						log::error!("The mask's affects_piece() result doesn't match reality for piece {} with pentagon {} and rotation {}", piece_index, line_index, word_index);
						error_expr!(word_pack.mask.affects_piece(piece_index), prev_puzzle_state, curr_puzzle_state);

						panic!();
					}
				}
			});
		});
	}

	fn test_standard_rotations(transformation_library: &Library) -> () {
		let page_pack: PagePack = transformation_library.book_pack_data.get_page_pack(Type::StandardRotation as usize);

		page_pack.iter(|line_index: usize, line_pack: LinePack| -> () {
			line_pack.iter(|word_index: usize, word_pack: WordPack| -> () {
				let trfm:						&Trfm				= &word_pack.trfm;
				let inv_trfm:					&Trfm				= &transformation_library.book_pack_data.get_word_pack(*word_pack.addr).trfm;

				let mut curr_puzzle_state:		PuzzleState			= PuzzleState::SOLVED_STATE;
				let mut curr_puzzle_state_alt:	PuzzleState			= PuzzleState::SOLVED_STATE;

				for turn in 1_usize ..= PENTAGON_SIDE_COUNT {
					let prev_puzzle_state:		PuzzleState = curr_puzzle_state.clone();

					curr_puzzle_state += trfm;
					curr_puzzle_state_alt.naive_add_assign(trfm);

					if curr_puzzle_state_alt != curr_puzzle_state {
						log::error!("curr_puzzle_state_alt != curr_puzzle_state after turn {} with pentagon {} and rotation {}", turn, line_index, word_index);
						error_expr!(prev_puzzle_state, trfm, curr_puzzle_state, curr_puzzle_state_alt);

						panic!();
					}

					if !curr_puzzle_state.is_valid() {
						log::error!("Puzzle state isn't valid after turn {} with pentagon {} and rotation {}", turn, line_index, word_index);
						error_expr!(prev_puzzle_state, trfm, curr_puzzle_state);

						panic!();
					}

					let mut prev_puzzle_state_from_inverse_transformation = curr_puzzle_state.clone();

					prev_puzzle_state_from_inverse_transformation += inv_trfm;

					if prev_puzzle_state_from_inverse_transformation != prev_puzzle_state {
						log::error!("prev_puzzle_state_from_inverse_transformation != prev_puzzle_state after turn {} with pentagon {} and rotation {}", turn, line_index, word_index);
						error_expr!(curr_puzzle_state, inv_trfm, prev_puzzle_state_from_inverse_transformation, prev_puzzle_state);

						panic!();
					}
				}

				if curr_puzzle_state != PuzzleState::SOLVED_STATE {
					log::error!("curr_puzzle_state != curr_puzzle_state with pentagon {} and rotation {}", line_index, word_index);
					error_expr!(curr_puzzle_state);

					panic!();
				}
			});
		});
	}

	#[test]
	fn test_transformation_library() -> () {
		init_env_logger();

		let transformation_library: &Library = Library::get();

		test_validity(transformation_library);

		// Though Type::Reorientation is listed before StandardRotation (intentionally: it doesn't actually change the (standardized) state of the puzzle),
		// Type::StandardRotation needs to be tested first, since the former is dependent on the latter
		test_standard_rotations(transformation_library);
		test_reorientations(transformation_library);
	}
}