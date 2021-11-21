pub mod packs;

use {
	crate::{
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		}
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
	std::{
		fmt::{
			Debug,
			Error,
			Formatter
		},
		ops::{
			BitOr,
			Deref,
			Neg
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
			} else if let Some(data) = Data::get(Polyhedron::Icosidodecahedron) {
				let faces: &Vec<FaceData> = &data.faces;
				let face_normal: Vec3 = faces[pentagon_index].norm;

				let mut mask: u32 = 0_u32;

				for pent_index in 0_usize .. PIECE_COUNT {
					mask |= ((faces[pent_index].norm.dot(face_normal) > 0.0_f32) as u32) << pent_index;
				}

				mask
			} else {
				log::warn!("Couldn't get Icosidodecahedron Data");

				0_u32
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

#[derive(Clone, Copy, PartialEq)]
pub enum Type {
	Reorientation,		// This transformation standardizes the puzzle state (reorients piece 0 to be at position 0 with rotation 0)
	StandardRotation,	// This transformation rotates one of the hemispherical halves about one of the 12 pentagonal pieces n fifth-rotations, where n is in [0, 5)
	Count				// Not an actual transformation, this allows us to statically allocate memory for a type that has a variant for each "valid" Type variant
}

impl Type {
	pub fn addr(self) -> Addr {
		Addr::from_page(self as usize)
	}
}

#[derive(Clone, Copy, Debug)]
pub struct Addr {
	page_index: i8,
	line_index: i8,
	word_index: i8
}

pub trait AddrConsts {
	const INVALID_INDEX: i8;
}

impl AddrConsts for Addr {
	const INVALID_INDEX: i8 = -1_i8;
}

impl Addr {
	#[inline(always)]
	pub fn is_valid(&self) -> bool { self.is_valid_with_mask(Self::from((Some(0), Some(0), Some(0)))) }

	pub fn is_valid_with_mask(&self, mask: Self) -> bool {
		(mask.page_index == Self::INVALID_INDEX || self.page_index_is_valid()) &&
		(mask.line_index == Self::INVALID_INDEX || self.line_index_is_valid()) &&
		(mask.word_index == Self::INVALID_INDEX || self.word_index_is_valid())
	}

	pub fn from_page(page_index: usize) -> Self {
		Self {
			page_index: page_index as i8,
			.. Self::default()
		}
	}

	pub fn from_line(line_index: usize) -> Self {
		Self {
			line_index: line_index as i8,
			.. Self::default()
		}
	}

	pub fn from_word(word_index: usize) -> Self {
		Self {
			word_index: word_index as i8,
			.. Self::default()
		}
	}

	#[inline(always)]
	pub fn page_index_is_valid(&self) -> bool { self.page_index > Self::INVALID_INDEX && self.page_index < Library::PAGE_COUNT as i8 }

	#[inline(always)]
	pub fn line_index_is_valid(&self) -> bool { self.line_index > Self::INVALID_INDEX && self.line_index < Library::LINE_COUNT as i8 }

	#[inline(always)]
	pub fn word_index_is_valid(&self) -> bool { self.word_index > Self::INVALID_INDEX && self.word_index < Library::WORD_COUNT as i8 }

	pub fn page_index(&self) -> usize { assert!(self.page_index_is_valid()); self.page_index as usize }
	pub fn line_index(&self) -> usize { assert!(self.line_index_is_valid()); self.line_index as usize }
	pub fn word_index(&self) -> usize { assert!(self.word_index_is_valid()); self.word_index as usize }

	#[inline(always)]
	pub fn long_line_index_is_valid(&self) -> bool { self.line_index > Self::INVALID_INDEX && self.line_index < Library::LONG_LINE_COUNT as i8 }

	pub fn long_line_index(&self) -> usize { assert!(self.long_line_index_is_valid()); self.line_index as usize }
}

impl BitOr for Addr {
	type Output = Self;

	fn bitor(self, rhs: Self) -> Self::Output {
		Self {
			page_index: if self.page_index == Self::INVALID_INDEX { rhs.page_index } else { self.page_index },
			line_index: if self.line_index == Self::INVALID_INDEX { rhs.line_index } else { self.line_index },
			word_index: if self.word_index == Self::INVALID_INDEX { rhs.word_index } else { self.word_index },
		}
	}
}

impl Default for Addr {
	fn default() -> Self {
		Self {
			page_index: Self::INVALID_INDEX,
			line_index: Self::INVALID_INDEX,
			word_index: Self::INVALID_INDEX
		}
	}
}

impl From<(usize, usize, usize)> for Addr {
	fn from((page_index, line_index, word_index): (usize, usize, usize)) -> Self {
		Self {
			page_index: page_index as i8,
			line_index: line_index as i8,
			word_index: word_index as i8
		}
	}
}

impl From<(Option<usize>, Option<usize>, Option<usize>)> for Addr {
	fn from((page_index, line_index, word_index): (Option<usize>, Option<usize>, Option<usize>)) -> Self {
		Self {
			page_index: page_index.map_or(Self::INVALID_INDEX, |page_index: usize| -> i8 { page_index as i8 }),
			line_index: line_index.map_or(Self::INVALID_INDEX, |line_index: usize| -> i8 { line_index as i8 }),
			word_index: word_index.map_or(Self::INVALID_INDEX, |word_index: usize| -> i8 { word_index as i8 })
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
	fn find_word(&self, word: &Word<T>) -> Option<Addr>;
}

impl<T> FindWord<T> for Word<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<Addr> {
		if self == target_word {
			Some(Addr::default())
		} else {
			None
		}
	}
}

impl<T> FindWord<T> for Line<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<Addr> {
		for (word_index, word) in self.iter().enumerate() {
			if let Some(mut address) = word.find_word(target_word) {
				address.word_index = word_index as i8;

				return Some(address)
			}
		}

		None
	}
}

impl<T> FindWord<T> for Page<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<Addr> {
		for (line_index, line) in self.iter().enumerate() {
			if let Some(mut address) = line.find_word(target_word) {
				address.line_index = line_index as i8;

				return Some(address)
			}
		}

		None
	}
}

impl<T> FindWord<T> for Book<T>
	where
		T: PartialEq
{
	fn find_word(&self, target_word: &Word<T>) -> Option<Addr> {
		for (page_index, page) in self.iter().enumerate() {
			if let Some(mut address) = page.find_word(target_word) {
				address.page_index = page_index as i8;

				return Some(address)
			}
		}

		None
	}
}

pub trait GetWord<T> {
	fn get_word(&self, addr: Addr) -> &Word<T>;
	fn get_word_mut(&mut self, addr: Addr) -> &mut Word<T>;
}

impl<T> GetWord<T> for Page<T> {
	fn get_word(&self, addr: Addr) -> &Word<T> {
		&self[addr.line_index()][addr.word_index()]
	}

	fn get_word_mut(&mut self, addr: Addr) -> &mut Word<T> {
		&mut self[addr.line_index()][addr.word_index()]
	}
}

impl<T> GetWord<T> for LongPage<T> {
	fn get_word(&self, addr: Addr) -> &Word<T> {
		&self[addr.long_line_index()][addr.word_index()]
	}

	fn get_word_mut(&mut self, addr: Addr) -> &mut Word<T> {
		&mut self[addr.long_line_index()][addr.word_index()]
	}
}

impl<T> GetWord<T> for Book<T> {
	fn get_word(&self, addr: Addr) -> &Word<T> {
		&self[addr.page_index()][addr.line_index()][addr.word_index()]
	}

	fn get_word_mut(&mut self, addr: Addr) -> &mut Word<T> {
		&mut self[addr.page_index()][addr.line_index()][addr.word_index()]
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
		let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron).unwrap();

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
				let page_address: Addr = Addr::from_page(Type::Reorientation as usize);

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
						*word_pack_mut.addr = Addr::default();
					});
				});

				let trfm_page:		&Page<Trfm>		= page_pack_mut.trfm;
				let addr_page_mut:	&mut Page<Addr>	= page_pack_mut.addr;

				for (line_index, addr_line_mut) in addr_page_mut.iter_mut().enumerate() {
					for (word_index, addr_word_mut) in addr_line_mut.iter_mut().enumerate() {
						*addr_word_mut = trfm_page
							.find_word(&(-&trfm_page[line_index][word_index]))
							.map(|address: Addr| -> Addr {
								address | page_address
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
						*word_pack_mut.addr = Addr::from((
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

	pub fn get() -> &'static Self { &TRANSFOMATION_LIBRARY }
}

pub struct LibraryRef(&'static Library);

impl Default for LibraryRef {
	fn default() -> Self {
		LibraryRef(Library::get())
	}
}

impl Deref for LibraryRef {
	type Target = Library;

	#[must_use]
	fn deref(&self) -> &Self::Target {
		self.0
	}
}

pub struct TransformationPlugin;

impl Plugin for TransformationPlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app.insert_resource::<LibraryRef>(LibraryRef::default());
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

					let inv_addr: Addr = *word_pack.addr;
					let inv_trfm: &Trfm = book_pack.get_word_pack(inv_addr).trfm;

					if *trfm != -inv_trfm || -trfm != *inv_trfm {
						log::error!("Transformation addressed by ({}, {}, {}), the associated address of transformation ({}, {}, {}) is not the true inverse transformation",
							inv_addr.page_index, inv_addr.line_index, inv_addr.word_index,
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

lazy_static!{
	static ref TRANSFOMATION_LIBRARY: Library = Library::new();
}