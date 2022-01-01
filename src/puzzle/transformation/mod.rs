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
		util::StaticDataLibrary,
		max
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
		convert::TryFrom,
		fmt::{
			Debug,
			Error,
			Formatter,
			Write
		},
		iter::{
			DoubleEndedIterator,
			Rev
		},
		mem::{
			MaybeUninit,
			transmute
		},
		ops::{
			Add,
			AddAssign,
			Neg,
			Range
		},
		slice::Iter,
		sync::{
			Mutex,
			MutexGuard,
			PoisonError
		}
	}
};

pub use packs::*;

const_assert!(PIECE_COUNT <= u32::BITS as usize);

#[derive(Clone, Copy, Default)]
pub struct Mask {
	affected_poses:		u32,
	affected_pieces:	u32
}

impl Mask {
	#[inline]
	pub fn affects_pos(&self, piece_index: usize) -> bool {
		self.affected_poses & (1_u32 << piece_index) != 0_u32
	}

	#[inline]
	pub fn affects_piece(&self, piece_index: usize) -> bool {
		self.affected_pieces & (1_u32 << piece_index) != 0_u32
	}

	fn from_pentagon_index(pentagon_index: usize) -> Self {
		if pentagon_index >= PENTAGON_PIECE_COUNT {
			log::warn!(
				"HemisphereMask::new({}) was called, but {} is an invalid pentagon index",
				pentagon_index,
				pentagon_index
			);

			Self {
				affected_poses:		0_u32,
				affected_pieces:	0_u32
			}
		} else {
			let data: &Data = Data::get(Polyhedron::Icosidodecahedron);
			let faces: &Vec<FaceData> = &data.faces;
			let face_normal: Vec3 = faces[pentagon_index].norm;

			let mut mask: u32 = 0_u32;

			for pent_index in 0_usize .. PIECE_COUNT {
				mask |= ((faces[pent_index].norm.dot(face_normal) > 0.0_f32) as u32) << pent_index;
			}

			Self {
				affected_poses:		mask,
				affected_pieces:	mask
			}
		}
	}
}

impl From<&Transformation> for Mask {
	fn from(transformation: &Transformation) -> Self {
		let mut affected_poses:		u32												= 0_u32;
		let mut affected_pieces:	u32												= 0_u32;
		let (pos, rot):				(&PuzzleStateComponent, &PuzzleStateComponent)	= transformation.as_ref().arrays();

		for piece_index in PIECE_RANGE {
			let affects_pos: bool = pos[piece_index] as usize != piece_index;

			affected_poses |= (affects_pos as u32) << piece_index;
			affected_pieces |= ((affects_pos || rot[piece_index] != 0 as PieceStateComponent) as u32) << piece_index;
		}

		Mask {
			affected_poses,
			affected_pieces
		}
	}
}

enum ComplexClass {
	Complex1,
	Complex2,
	Complex3
}

const COMPLEX_CLASS_COUNT: usize = 3_usize;

impl ComplexClass {
	const fn is_invert_not_mirror(self) -> bool {
		match self {
			Self::Complex1 => false,
			Self::Complex2 => false,
			Self::Complex3 => true
		}
	}

	const fn comprising_simples(self) -> &'static [HalfAddr] {
		macro_rules! ha { ($line_index:expr, $word_index:expr) => { HalfAddr::new($line_index, $word_index) } }
		const COMPRISING_SIMPLES_LUT: [&[HalfAddr]; COMPLEX_CLASS_COUNT] = [
			&[
				ha!(10,	4),	ha!(8,	1),	ha!(10,	1),	ha!(8,	4)
			],
			&[
				ha!(10,	4),	ha!(8,	1),	ha!(10,	1),	ha!(8,	4),
				ha!(10,	4),	ha!(8,	1),	ha!(10,	1),	ha!(8,	4)
			],
			&[
				ha!(8,	3),	ha!(7,	2),	ha!(8,	2),	ha!(7,	3),
				ha!(8,	3),	ha!(7,	2),	ha!(8,	2),	ha!(7,	3),
				ha!(8,	3),	ha!(7,	2),	ha!(8,	2),	ha!(7,	3),
				ha!(1,	3),	ha!(3,	3),	ha!(7,	2),	ha!(3,	2),
				ha!(1,	3),	ha!(6,	3),	ha!(1,	2),	ha!(3,	2),
				ha!(6,	3),	ha!(3,	3),	ha!(1,	2),	ha!(7,	2),
			]
		];

		COMPRISING_SIMPLES_LUT[self as usize]
	}

	const fn invert_bit(self) -> u8 { if self.is_invert_not_mirror() { 2_u8 } else { 1_u8 } }

	const fn mirror(self, sub_type: u8) -> u8 { (sub_type - COMPLEX_OFFSET as u8 ^ 1_u8) + COMPLEX_OFFSET as u8 }

	const fn invert(self, sub_type: u8) -> u8 {
		(sub_type - COMPLEX_OFFSET as u8 ^ self.invert_bit()) + COMPLEX_OFFSET as u8
	}
}

macro_rules! list_type {
	($macro:path) => {
		$macro!(
			Reorientation,	// This will spherically rotate the puzzle so that the origin orientation is at the indexed orientation
			Simple,			// This rotates the hemispherical halves about the pentagonal piece `line_index` `word_index` fifth-rotations
			Complex1A,		// This swaps two pairs of pentagons. No guarantees are made for triangles. PuzzleState.pos: [0, 1, _, _, 4, 5, ...] -> [1, 0, _, _, 5, 4, ...]
			Complex1B,		// This is Complex1A mirrored
			Complex2A,		// This is Complex1A performed twice, effectively rotating two pairs of pentagons opposite directions. PuzzleState.rot: [0, 0, _, _, 0, 0, ...] -> [4, 4, _, _, 1, 1, ...]
			Complex2B,		// This is Complex2A mirrored
			Complex3A,		// This cycles the positions of 3 triangles, while changing the rotations of 2 of them. Pentagons are left untouched.
			Complex3B,		// This is Complex3A mirrored
			Complex3C,		// This is Complex3A inverted
			Complex3D,		// This is Complex3B inverted
		);
	}
}

macro_rules! define_type {
	($($variant:ident,)*) => {
		#[derive(Clone, Copy, Debug, PartialEq)]
		pub enum Type {
			$(
				$variant,
			)*
		}

		const fn type_count() -> usize {
			let mut max_type: usize = 0_usize;

			$(
				max_type = max!(max_type, Type::$variant as usize);
			)*

			max_type + 1_usize
		}

		pub const TYPE_COUNT: usize = type_count();
		pub const COMPLEX_OFFSET: usize = Type::Simple as usize + 1_usize;

		const TYPE_MIRROR_LUT: [u8; TYPE_COUNT] = [ $(Type::$variant.mirror_variant(),)* ];

		const TYPE_INVERT_LUT: [u8; TYPE_COUNT] = [ $(Type::$variant.invert_variant(),)* ];

		#[allow(non_snake_case)]
		struct ComprisingSimplesData {
			$($variant: Page<[HalfAddr; Type::$variant.comprising_simples_len()]>,)*
		}

		impl GetWord<FullAddr, [HalfAddr]> for ComprisingSimplesData {
			fn get_word(&self, addr: FullAddr) -> &Word<[HalfAddr]> {
				match addr.get_page_index_type() {
					$(Some(Type::$variant) => { self.$variant.get_word(*addr.get_half_addr()) }),*
					None => { panic!("ComprisingSimplesData::get_word() called with addr {:?}", addr); }
				}
			}

			fn get_word_mut(&mut self, addr: FullAddr) -> &mut Word<[HalfAddr]> {
				match addr.get_page_index_type() {
					$(Some(Type::$variant) => { self.$variant.get_word_mut(*addr.get_half_addr()) }),*
					None => { panic!("ComprisingSimplesData::get_word_mut() called with addr {:?}", addr); }
				}
			}
		}
	}
}

list_type!(define_type);

impl Type {
	#[inline(always)]
	pub const fn is_reorientation(self) -> bool { self as u8 == Self::Reorientation as u8 }

	#[inline(always)]
	pub const fn is_simple(self) -> bool { self as u8 == Self::Simple as u8 }

	#[inline(always)]
	pub const fn is_complex(self) -> bool { self as u8 >= COMPLEX_OFFSET as u8 }

	#[inline]
	pub const fn mirror(self) -> Self { unsafe { transmute::<u8, Self>(TYPE_MIRROR_LUT[self as usize]) } }

	#[inline]
	pub const fn invert(self) -> Self { unsafe { transmute::<u8, Self>(TYPE_INVERT_LUT[self as usize]) } }

	#[inline]
	pub const fn is_mirrored(self) -> bool {
		self.is_complex() && (self as u8 - COMPLEX_OFFSET as u8) & 1_u8 != 0_u8
	}

	pub const fn is_inverted(self) -> bool {
		match self.complex_class() {
			Some(complex_class) =>
				(self as u8 - COMPLEX_OFFSET as u8) & complex_class.invert_bit() != 0_u8,
			None => false
		}
	}

	pub const fn is_invert_not_mirror(self) -> bool {
		match self.complex_class() {
			Some(complex_class) => complex_class.is_invert_not_mirror(),
			None => false
		}
	}

	const fn mirror_variant(self) -> u8 {
		if let Some(complex_class) = self.complex_class() {
			complex_class.mirror(self as u8)
		} else {
			self as u8
		}
	}

	const fn invert_variant(self) -> u8 {
		if let Some(complex_class) = self.complex_class() {
			complex_class.invert(self as u8)
		} else {
			self as u8
		}
	}

	const fn complex_class(self) -> Option<ComplexClass> {
		macro_rules! map_to_class {
			($($($type_variant:ident)|* => $class_variant:ident),*) => {
				match self {
					$(
						$(Self::$type_variant)|* => Some(ComplexClass::$class_variant),
					)*
					_ => None
				}
			}
		}

		map_to_class!(
			Complex1A | Complex1B							=> Complex1,
			Complex2A | Complex2B							=> Complex2,
			Complex3A | Complex3B | Complex3C | Complex3D	=> Complex3
		)
	}

	const fn comprising_simples_len(self) -> usize {
		if let Some(complex_class) = self.complex_class() {
			complex_class.comprising_simples().len()
		} else if self.is_simple() {
			1_usize
		} else {
			0_usize
		}
	}
}

impl TryFrom<u8> for Type {
	type Error = ();

	fn try_from(value: u8) -> Result<Self, Self::Error> {
		if value < TYPE_COUNT as u8 {
			Ok(unsafe { transmute::<u8, Self>(value) })
		} else {
			Err(())
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
	const INVALID:			u8;
	const LINE_INDEX_BITS:	Range<usize>;
	const WORD_INDEX_BITS:	Range<usize>;
	const WORD_INDEX_MASK:	u8;
	const ORIGIN:			HalfAddr;
}

#[derive(Clone, Copy, PartialEq)]
pub struct HalfAddr(u8);

impl HalfAddr {
	#[inline(always)]
	pub fn is_valid(&self) -> bool { self.0 != Self::INVALID && self.word_index_is_valid()}

	#[inline(always)]
	pub fn line_index_is_valid(&self) -> bool { Self::is_valid_line_index(unsafe { self.get_line_index_unchecked() }) }

	#[inline(always)]
	pub const fn is_valid_line_index(line_index: usize) -> bool { line_index < Library::LINE_COUNT }

	#[inline(always)]
	pub const fn is_valid_long_line_index(long_line_index: usize) -> bool { long_line_index < Library::LONG_LINE_COUNT }

	#[inline(always)]
	pub fn word_index_is_valid(&self) -> bool { Self::is_valid_word_index(unsafe { self.get_word_index_unchecked() }) }

	#[inline(always)]
	pub const fn is_valid_word_index(word_index: usize) -> bool { word_index < Library::WORD_COUNT }

	pub const fn default() -> Self { Self(Self::INVALID) }

	pub const fn new(line_index: usize, word_index: usize) -> Self {
		if Self::is_valid_long_line_index(line_index) && Self::is_valid_word_index(word_index) {
			Self((line_index as u8) << Self::LINE_INDEX_BITS.start | word_index as u8)
		} else {
			Self::default()
		}
	}

	pub const fn origin() -> Self { Self::new(0_usize, 0_usize) }

	pub fn invalidate(&mut self) -> () {
		self.0 = Self::INVALID;
	}

	#[inline(always)]
	pub fn as_reorientation(self) -> FullAddr { FullAddr::from((Type::Reorientation, self)) }

	#[inline(always)]
	const unsafe fn get_line_index_unchecked(&self) -> usize { (self.0 >> Self::LINE_INDEX_BITS.start) as usize }

	#[inline(always)]
	const unsafe fn get_word_index_unchecked(&self) -> usize { (self.0 & Self::WORD_INDEX_MASK) as usize }
}

impl HalfAddrConsts for HalfAddr {
	const INVALID:			u8				= u8::MAX;
	const LINE_INDEX_BITS:	Range<usize>	= 3_usize .. u8::BIT_LENGTH;
	const WORD_INDEX_BITS:	Range<usize>	= 0_usize .. 3_usize;
	const WORD_INDEX_MASK:	u8				= (1_u8 << HalfAddr::WORD_INDEX_BITS.end) - 1_u8;
	const ORIGIN:			HalfAddr		= HalfAddr::new(0_usize, 0_usize);
}

impl Add<FullAddr> for HalfAddr {
	type Output = Self;

	/// # `add()`
	/// 
	/// Transform an orientation to where a transformation maps it to
	/// 
	/// ## Params
	/// 
	/// * `self`: an orientation `HalfAddr`
	/// * `rhs`: a transformation `FullAddr`
	/// 
	/// ## Returns
	/// 
	/// The resultant orientation `HalfAddr` representing where the `self` orientation is mapped to after performing
	/// the `rhs` transformation.
	fn add(self, rhs: FullAddr) -> Self::Output {
		if self.is_valid() && rhs.is_valid() {
			let mut sum: HalfAddr = Library::get()
				.book_pack_data
				.trfm
				.get_word(rhs)
				.as_ref()
				.half_addr(self.get_line_index());

			*sum.set_word_index((sum.get_word_index() + self.get_word_index()) % Library::WORD_COUNT)
		} else {
			Self::default()
		}
	}
}

impl AddAssign<FullAddr> for HalfAddr { fn add_assign(&mut self, rhs: FullAddr) -> () { *self = *self + rhs; } }

impl Addr for HalfAddr {
	fn get_line_index(&self) -> usize {
		assert!(self.is_valid() && self.line_index_is_valid());

		// Safe: checked above
		unsafe { self.get_line_index_unchecked() }
	}

	fn get_long_line_index(&self) -> usize {
		const_assert!(Library::LONG_LINE_COUNT
			<= 1_usize << (HalfAddr::LINE_INDEX_BITS.end - HalfAddr::LINE_INDEX_BITS.start));
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

impl From<u8> for HalfAddr { fn from(value: u8) -> Self { Self(value) } }

impl From<HalfAddr> for u8 { fn from(value: HalfAddr) -> Self { value.0 } }

impl Debug for HalfAddr {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
		formatter
			.debug_struct("HalfAddr")
			.field("line_index", unsafe { &self.get_line_index_unchecked() })
			.field("word_index", unsafe { &self.get_word_index_unchecked() })
			.finish()
	}
}

impl Default for HalfAddr { fn default() -> Self { HalfAddr::default() } }

pub trait FullAddrConsts {
	const INVALID_INDEX: u8;
}

#[derive(Clone, Copy, PartialEq)]
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

	pub fn get_page_index_type(self) -> Option<Type> {
		if self.page_index_is_valid() {
			Some(unsafe { transmute(self.page_index) })
		} else {
			None
		}
	}

	#[inline(always)]
	pub fn is_page_index_reorientation(&self) -> bool { self.page_index == Type::Reorientation as u8 }

	#[inline(always)]
	pub fn is_page_index_simple(&self) -> bool { self.page_index == Type::Simple as u8 }

	pub fn get_cycles(self) -> u32 {
		if self.is_page_index_reorientation() {
			1_u32
		} else {
			Self::get_cycles_for_comprising_simples(&self.get_comprising_simples())
		}
	}

	pub fn get_cycles_for_comprising_simples(comprising_simples: &[HalfAddr]) -> u32 {
		comprising_simples
			.iter()
			.map(|half_addr: &HalfAddr| -> u32 {
				const CYCLES_LUT: [u8; Library::WORD_COUNT] = [0_u8, 1_u8, 2_u8, 2_u8, 1_u8];

				CYCLES_LUT[half_addr.get_word_index()] as u32
			})
			.sum()
	}

	pub fn get_comprising_simples(self) -> &'static [HalfAddr] {
		if self.is_valid() {
			Library::get().comprising_simples_data.get_word(self)
		} else {
			&[]
		}
	}

	pub fn get_comprising_simples_string(self) -> String {
		let mut comprising_simples_string: String = String::new();

		for (comprising_simple_index, comprising_simple) in self.get_comprising_simples().iter().enumerate() {
			write!(
				comprising_simples_string,
				"{0}{1:\t>2$}ha!({3},\t{4}),",
				if comprising_simple_index & 0b11 == 0_usize {
					"\n"
				} else {
					""
				},
				"",
				if comprising_simple_index & 0b11 == 0_usize {
					4_usize
				} else {
					1_usize
				},
				comprising_simple.get_line_index(),
				comprising_simple.get_word_index()
			).unwrap();
		}

		comprising_simples_string
	}

	#[inline(always)]
	pub fn get_half_addr(&self) -> &HalfAddr { &self.half_addr }

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

	pub fn invert(self) -> Self {
		if self.is_valid() {
			*Library::get()
				.book_pack_data
				.addr
				.get_word(self)
		} else {
			Self::default()
		}
	}

	pub fn mirror(self) -> Self {
		if self.is_valid() {
			let page_index_type: Type = self.get_page_index_type().unwrap();

			(
				page_index_type.mirror(),
				if page_index_type.is_complex() {
					*self.get_half_addr()
				} else {

					HalfAddr::new(
						Self::mirror_line_index(self.get_line_index()),
						Self::invert_word_index(self.get_word_index())
					)
				}
			).into()
		} else {
			Self::default()
		}
	}

	pub fn is_identity_transformation(self) -> bool {
		match self.get_page_index_type() {
			Some(Type::Reorientation)	=> *self.get_half_addr() == HalfAddr::ORIGIN,
			Some(Type::Simple)			=> self.get_word_index() == 0_usize,
			_							=> false
		}
	}

	#[inline]
	pub const fn mirror_line_index(line_index: usize) -> usize {
		const LINE_INDEX_LUT: [u8; Library::LINE_COUNT] = [
			0_u8,	1_u8,	2_u8,	3_u8, // Unchanged
			5_u8,	4_u8,	7_u8,	6_u8, // ABCD -> BADC
			10_u8,	11_u8,	8_u8,	9_u8, // ABCD -> CDAB
		];

		LINE_INDEX_LUT[line_index] as usize
	}

	#[inline]
	pub const fn invert_word_index(word_index: usize) -> usize {
		const WORD_INDEX_LUT: [u8; Library::WORD_COUNT] = [0_u8, 4_u8, 3_u8, 2_u8, 1_u8];

		WORD_INDEX_LUT[word_index] as usize
	}

	#[inline(always)]
	unsafe fn get_page_index_unchecked(&self) -> usize {
		self.page_index as usize
	}
}

impl Add<HalfAddr> for FullAddr {
	type Output = Self;

	/// `add()`
	/// 
	/// Reorient a transformation
	/// 
	/// ## Params
	/// 
	/// * `self`: a transformation `FullAddr`
	/// * `rhs`: a Reorientation `HalfAddr`
	/// 
	/// ## Returns
	/// 
	/// The resultant transformation after applying the `rhs.as_reorientation()` transformation. This differs from `{
	/// self.half_addr += rhs; self }` because Simple transformations don't have their word indices affected by
	/// reorientations.
	fn add(self, rhs: HalfAddr) -> Self::Output {
		let mut sum: Self = self;

		sum.half_addr += rhs.as_reorientation();

		if self.is_page_index_simple() {
			sum.set_word_index(self.get_word_index());
		}

		sum
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
		let mut page_index_type_local: Option<Type> = None;

		debug_struct.field("page_index", &self.get_page_index_type()
			.map_or(
				&"[invalid]" as &dyn std::fmt::Debug,
				|page_index_type: Type| -> &dyn std::fmt::Debug {
					page_index_type_local = Some(page_index_type);

					page_index_type_local.as_ref().unwrap()
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

impl From<u16> for FullAddr { fn from(value: u16) -> Self { unsafe { transmute::<u16, Self>(value) } } }

impl From<FullAddr> for u16 { fn from(value: FullAddr) -> Self { unsafe { transmute::<FullAddr, Self>(value) } } }

#[derive(Clone, Copy, Debug, Default)]
pub struct Action {
	transformation:		FullAddr,
	camera_start:		HalfAddr
}

impl Action {
	pub fn new(transformation: FullAddr, camera_start: HalfAddr) -> Self {
		Self {
			transformation,
			camera_start
		}
	}

	#[inline(always)]
	pub fn transformation(&self) -> &FullAddr { &self.transformation }
	#[inline(always)]
	pub fn camera_start(&self) -> &HalfAddr { &self.camera_start }
	#[inline(always)]
	pub fn standardization(&self) -> FullAddr { (HalfAddr::ORIGIN + self.transformation).as_reorientation().invert() }

	pub fn is_valid(&self) -> bool { self.transformation.is_valid() && self.camera_start.is_valid() }

	pub fn camera_end(&self) -> HalfAddr {
		if self.transformation.is_page_index_reorientation() {
			*self.transformation.get_half_addr()
		} else {
			self.camera_start
		}
	}

	pub fn invert(&self) -> Self {
		if self.is_valid() {
			if self.transformation.is_page_index_reorientation() {
				Self::new(
					self.camera_start.as_reorientation(),
					*self.transformation.get_half_addr()
				)
			} else {
				let self_standardization: FullAddr = self.standardization();

				Self::new(
					self.transformation.invert() + *self_standardization.get_half_addr(),
					self.camera_end() + self_standardization
				)
			}
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
* Simple:
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

pub trait GetWord<A : Addr, T : ?Sized> {
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
	pub book_pack_data:			BookPackData,
	pub orientation_data:		LongPage<OrientationData>,
	comprising_simples_data:	ComprisingSimplesData
}

impl LibraryConsts for Library {
	const WORD_COUNT:		usize = PENTAGON_SIDE_COUNT;
	const LINE_COUNT:		usize = PENTAGON_PIECE_COUNT;
	const LONG_LINE_COUNT:	usize = PIECE_COUNT;
	const PAGE_COUNT:		usize = TYPE_COUNT;
}

impl Library {
	fn initialize(&mut self) -> () {
		Data::initialize();

		let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);

		for (long_line_index, orientation_data_long_line)
			in self.orientation_data.iter_mut().enumerate()
		{
			let face_data: &FaceData = &icosidodecahedron_data.faces[long_line_index];

			for (word_index, orientation_data_word)
				in orientation_data_long_line.iter_mut().enumerate()
			{
				orientation_data_word.quat = face_data.get_rotated_quat(word_index as u32);
			}
		}

		let (
				book_pack_data,
				comprising_simples_data,
				orientation_data
			):
			(
				&mut BookPackData,
				&mut ComprisingSimplesData,
				&LongPage<OrientationData>
			) =
			(
				&mut self.book_pack_data,
				&mut self.comprising_simples_data,
				&self.orientation_data
			);

		for page_index in 0_usize .. Library::PAGE_COUNT {
			let transformation_type: Type = Type::try_from(page_index as u8).unwrap();

			if transformation_type.is_simple() {
				for (line_index, line)
					in comprising_simples_data.Simple.iter_mut().enumerate()
				{
					for (word_index, word) in line.iter_mut().enumerate() {
						word[0] = HalfAddr::new(line_index, word_index);
					}
				}
			} else if transformation_type.is_complex() {
				let class_comprising_simples: &[HalfAddr] = transformation_type
					.complex_class()
					.unwrap()
					.comprising_simples();
				let is_mirrored: bool = transformation_type.is_mirrored();
				let is_inverted: bool = transformation_type.is_inverted() && transformation_type.is_invert_not_mirror();

				let class_comprising_simples_iter =
					|f: &mut dyn FnMut(&mut dyn DoubleEndedIterator<Item = &HalfAddr>) -> ()| -> () {
						let mut fwd_iter: Iter<HalfAddr> = class_comprising_simples.iter();
						let mut rev_iter: Rev<Iter<HalfAddr>> = class_comprising_simples.iter().rev();

						if is_inverted {
							f(&mut rev_iter);
						} else {
							f(&mut fwd_iter);
						}
					};

				for line_index in 0_usize .. Library::LINE_COUNT {
					for word_index in 0_usize .. Library::WORD_COUNT {
						let half_addr: HalfAddr = HalfAddr::new(line_index, word_index);
						let comprising_simples: &mut [HalfAddr] = comprising_simples_data
							.get_word_mut(FullAddr::from((transformation_type, half_addr)));

						class_comprising_simples_iter(
							&mut |class_comprising_simples_iter: &mut dyn DoubleEndedIterator<Item = &HalfAddr>| -> () {
								for (comprising_simple_index, class_comprising_simple)
									in class_comprising_simples_iter.enumerate()
								{
									comprising_simples[comprising_simple_index] = *(
										FullAddr::from((
											Type::Simple as usize,
											if is_mirrored {
												FullAddr::mirror_line_index(class_comprising_simple.get_line_index())
											} else {
												class_comprising_simple.get_line_index()
											},
											if is_mirrored ^ is_inverted {
												FullAddr::invert_word_index(class_comprising_simple.get_word_index())
											} else {
												class_comprising_simple.get_word_index()
											}
										)) + half_addr
									).get_half_addr();
								}
							}
						);
					}
				}
			}

			let mut page_pack_mut: PagePackMut = book_pack_data.get_page_pack_mut(page_index);

			match transformation_type {
				Type::Reorientation => {
					let origin_conj_quat: Quat = icosidodecahedron_data.faces[PENTAGON_INDEX_OFFSET].quat.conjugate();

					page_pack_mut.iter_mut(|line_index: usize, mut line_pack_mut: LinePackMut| -> () {
						line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
							let reorientation_quat: Quat = orientation_data[line_index][word_index].quat * origin_conj_quat;
							let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) =
								word_pack_mut.trfm.arrays_mut();

							for piece_index in PIECE_RANGE {
								let (pos, rot): (usize, usize) = icosidodecahedron_data.get_pos_and_rot(
									&(reorientation_quat * icosidodecahedron_data.faces[piece_index].quat),
									None /* We could put a filter in here, but it'd be slower, and the quat math is
										precise enough that it's unnecessary here */
								);

								pos_array[piece_index] = pos as PieceStateComponent;
								rot_array[piece_index] = rot as PieceStateComponent;
							}

							*word_pack_mut.quat = reorientation_quat;
							*word_pack_mut.mask = Mask::from(&*word_pack_mut.trfm);
							*word_pack_mut.addr = FullAddr::default();
						});
					});

					let trfm_page:		&Page<Trfm>			= page_pack_mut.trfm;
					let addr_page_mut:	&mut Page<FullAddr>	= page_pack_mut.addr;

					for (line_index, addr_line_mut) in addr_page_mut.iter_mut().enumerate() {
						for (word_index, addr_word_mut) in addr_line_mut.iter_mut().enumerate() {
							*addr_word_mut = trfm_page
								.find_word(&(-&trfm_page[line_index][word_index]))
								.map(|mut address: FullAddr| -> FullAddr {
									*address.set_page_index(page_index)
								})
								.unwrap_or_default();
						}
					}
				},
				Type::Simple => {
					page_pack_mut.iter_mut(|line_index: usize, mut line_pack_mut: LinePackMut| -> () {
						let face_data: &FaceData = &icosidodecahedron_data.faces[line_index];
						let mask: Mask = Mask::from_pentagon_index(line_index);

						line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
							let rotation_quat: Quat = face_data.get_rotation_quat(word_index as u32);
							let mask: Mask = if word_index != 0 { mask } else { Mask::default() };

							let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) =
								word_pack_mut.trfm.arrays_mut();

							for piece_index in PIECE_RANGE {
								let (pos, rot): (usize, usize) = if mask.affects_piece(piece_index) {
									icosidodecahedron_data.get_pos_and_rot(
										&(rotation_quat * icosidodecahedron_data.faces[piece_index].quat),
										None /* We could put a filter in here, but it'd be slower, and the quat
											math is precise enough that it's unnecessary here */
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
								transformation_type.invert() as usize,
								line_index,
								FullAddr::invert_word_index(word_index)
							));
						});
					});
				},
				_ => {
					let (non_complex_trfm_pages, complex_trfm_pages):
						(&mut [Page<Trfm>], &mut [Page<Trfm>]) =
						book_pack_data.trfm.split_at_mut(COMPLEX_OFFSET);
					let simple_trfm_page: &Page<Trfm> = &non_complex_trfm_pages[Type::Simple as usize];
					let complex_trfm_page: &mut Page<Trfm> = &mut complex_trfm_pages[page_index - COMPLEX_OFFSET];

					for (line_index, complex_trfm_line)
						in complex_trfm_page.iter_mut().enumerate()
					{
						for (word_index, complex_trfm_word)
							in complex_trfm_line.iter_mut().enumerate()
						{
							let mut puzzle_state: PuzzleState = PuzzleState::SOLVED_STATE;

							for comprising_simple in comprising_simples_data.get_word(FullAddr::from((page_index, line_index, word_index)))
							{
								puzzle_state += simple_trfm_page.get_word(*comprising_simple);
							}

							*complex_trfm_word = Transformation(puzzle_state);
						}
					}

					let mut page_pack_mut: PagePackMut = book_pack_data.get_page_pack_mut(page_index);

					page_pack_mut.iter_mut(|line_index: usize, mut line_pack_mut: LinePackMut| -> () {
						line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
							*word_pack_mut.quat = Quat::IDENTITY;
							*word_pack_mut.mask = Mask::from(&*word_pack_mut.trfm);
							*word_pack_mut.addr = FullAddr::from((
								transformation_type.invert() as usize,
								line_index,
								word_index
							));
						});
					});
				}
			}
		}
	}
}

static mut LIBRARY: MaybeUninit<Library> = MaybeUninit::<Library>::uninit();

lazy_static! {
	static ref LIBRARY_MUTEX: Mutex<bool> = Mutex::<bool>::new(false);
}

impl StaticDataLibrary for Library {
	fn initialize() -> () {
		let mut mutex_guard: MutexGuard<bool> = LIBRARY_MUTEX
			.lock()
			.unwrap_or_else(PoisonError::<MutexGuard<bool>>::into_inner);

		if !*mutex_guard {
			unsafe {
				LIBRARY.assume_init_mut().initialize();
			}

			*mutex_guard = true;
		}
	}

	fn get() -> &'static Self { unsafe { LIBRARY.assume_init_ref() } }
}

pub struct TransformationPlugin;

impl Plugin for TransformationPlugin {
	fn build(&self, _: &mut AppBuilder) -> () { <Library as StaticDataLibrary>::initialize(); }
}

#[cfg(test)]
mod tests {
	use {
		crate::prelude::*,
		super::*
	};

	fn test_validity() -> () {
		let library: &Library = Library::get();
		let book_pack: BookPack = library.book_pack_data.get_book_pack(());

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
						log::error!(
							"Inverse address for transformation ({}, {}, {}) is invalid",
							page_index, line_index, word_index
						);
						error_expr!(word_pack.addr);

						panic!();
					}

					let inv_addr: FullAddr = *word_pack.addr;
					let inv_trfm: &Trfm = book_pack.get_word_pack(inv_addr).trfm;

					if *trfm != -inv_trfm || -trfm != *inv_trfm {
						log::error!(
							"Transformation addressed by ({}, {}, {}), the associated address of transformation \
								({}, {}, {}) is not the true inverse transformation",
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
						log::error!(
							"solved_state + trfm + inv_trfm != solved_state for transformation ({}, {}, {})",
							page_index, line_index, word_index
						);
						error_expr!(solved_state, trfm, middle_state, inv_trfm, end_state);

						panic!();
					}
				});
			});
		});
	}

	fn test_reorientations() -> () {
		let library: &Library = Library::get();
		let page_pack: PagePack =
			library.book_pack_data.get_page_pack(Type::Reorientation as usize);
		let simple_page: &Page<Trfm> =
			&library.book_pack_data.trfm[Type::Simple as usize];
		let reorientation_tests: [Vec<(usize, usize)>; PENTAGON_PIECE_COUNT] =
			from_ron::<[Vec<(usize, usize)>; PENTAGON_PIECE_COUNT]>(
				STRING_DATA.tests.reorientation_tests.as_ref()
			).to_option().unwrap();

		page_pack.iter(|line_index: usize, line_pack: LinePack| -> () {
			let pent_puzzle_state: PuzzleState = {
				let mut solved_state: PuzzleState = PuzzleState::SOLVED_STATE;

				for (simple_pent_index, simple_rotation_index)
					in &reorientation_tests[line_index]
				{
					solved_state += &simple_page
						[*simple_pent_index]
						[*simple_rotation_index];
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

					pent_puzzle_state_clone += &simple_page[line_index][word_index];

					pent_puzzle_state_clone
				};

				if !prev_puzzle_state.is_valid() {
					log::error!(
						"Puzzle state isn't valid before reorientation with pentagon {} and rotation {}",
						line_index,
						word_index
					);
					error_expr!(pent_puzzle_state, simple_page[line_index][word_index], prev_puzzle_state);

					panic!();
				}

				if prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET] != line_index as PieceStateComponent
					|| prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET] != word_index as PieceStateComponent
				{
					log::error!(
						"Reorientation with pentagon {} and rotation {} has position {} and rotation {} for piece {}",
						line_index,
						word_index,
						prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET],
						prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET],
						PENTAGON_INDEX_OFFSET
					);
					error_expr!(prev_puzzle_state);

					panic!();
				}

				let trfm:								&Trfm		= &word_pack.trfm;

				let standardization_full_addr:			FullAddr	= prev_puzzle_state.standardization_full_addr();

				let mut curr_puzzle_state:				PuzzleState	= prev_puzzle_state.clone();
				let mut curr_puzzle_state_alt:			PuzzleState	= prev_puzzle_state.clone();
				let mut reoriented_solved_state:		PuzzleState = PuzzleState::SOLVED_STATE;
				let mut reoriented_solved_state_alt:	PuzzleState = PuzzleState::SOLVED_STATE;

				reoriented_solved_state += trfm;

				if reoriented_solved_state.standardization_full_addr() != standardization_full_addr {
					log::error!(
						"Reoriented solved state's standardization address doesn't match what's expected after \
							reorientation with pentagon {} and rotation {}",
						line_index,
						word_index
					);
					error_expr!(trfm, reoriented_solved_state, prev_puzzle_state);

					panic!();
				}

				if !reoriented_solved_state.is_valid() {
					log::error!(
						"Puzzle state isn't valid after reorientation with pentagon {} and rotation {}",
						line_index,
						word_index
					);
					error_expr!(prev_puzzle_state, trfm, curr_puzzle_state);

					panic!();
				}

				reoriented_solved_state_alt.naive_add_assign(trfm);

				if reoriented_solved_state_alt != reoriented_solved_state {
					log::error!(
						"Reoriented puzzle state doesn't match the naively reoriented puzzle state with pentagon {} \
							and rotation {}",
						line_index,
						word_index
					);
					error_expr!(trfm, reoriented_solved_state, reoriented_solved_state_alt);

					panic!();
				}

				if standardization_full_addr != *word_pack.addr {
					log::error!(
						"Standardization address for current state doesn't match inverse address with pentagon {} \
							and rotation {}",
						line_index,
						word_index
					);
					error_expr!(prev_puzzle_state, standardization_full_addr, word_pack.addr);

					panic!();
				}

				// Reorientation was originally in the other direction, and the standardization check only works for the
				// other direction, so just run the rest how the test was originally conducted

				let word_pack: WordPack = Library::get()
					.book_pack_data
					.get_word_pack(standardization_full_addr);
				let trfm: &Trfm = word_pack.trfm;

				curr_puzzle_state += trfm;

				if !curr_puzzle_state.is_standardized() {
					log::error!(
						"Puzzle state isn't standardized after reorientation with pentagon {} and rotation {}",
						line_index,
						word_index
					);
					error_expr!(prev_puzzle_state, trfm, curr_puzzle_state);

					panic!();
				}

				if !curr_puzzle_state.is_valid() {
					log::error!(
						"Puzzle state isn't valid after reorientation with pentagon {} and rotation {}",
						line_index,
						word_index
					);
					error_expr!(prev_puzzle_state, trfm, curr_puzzle_state);

					panic!();
				}

				curr_puzzle_state_alt.naive_add_assign(trfm);

				if curr_puzzle_state_alt != curr_puzzle_state {
					log::error!(
						"Reoriented puzzle state doesn't match the naively reoriented puzzle state with pentagon {} \
							and rotation {}",
						line_index,
						word_index
					);
					error_expr!(prev_puzzle_state, trfm, curr_puzzle_state, curr_puzzle_state_alt);

					panic!();
				}

				let transformation_from_states:	Trfm = &curr_puzzle_state - &prev_puzzle_state;

				if transformation_from_states != *trfm {
					log::error!(
						"Trfm from previous to current state doesn't match applied transformation with pentagon {} and \
							rotation {}",
						line_index,
						word_index
					);
					error_expr!(trfm, prev_puzzle_state, curr_puzzle_state, transformation_from_states);

					panic!();
				}

				for piece_index in PIECE_RANGE {
					if word_pack.mask.affects_piece(piece_index) != (
						curr_puzzle_state.pos[piece_index] != prev_puzzle_state.pos[piece_index] ||
						curr_puzzle_state.rot[piece_index] != prev_puzzle_state.rot[piece_index]
					) {
						log::error!(
							"The mask's affects_piece() result doesn't match reality for piece {} with pentagon {} and \
								rotation {}",
							piece_index,
							line_index,
							word_index
						);
						error_expr!(word_pack.mask.affects_piece(piece_index), prev_puzzle_state, curr_puzzle_state);

						panic!();
					}
				}
			});
		});
	}

	fn test_simples() -> () {
		let library: &Library = Library::get();
		let page_pack: PagePack = library.book_pack_data.get_page_pack(Type::Simple as usize);

		page_pack.iter(|line_index: usize, line_pack: LinePack| -> () {
			line_pack.iter(|word_index: usize, word_pack: WordPack| -> () {
				let trfm:						&Trfm				= &word_pack.trfm;
				let inv_trfm:					&Trfm				= &library.book_pack_data.get_word_pack(*word_pack.addr).trfm;

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
		<Library as StaticDataLibrary>::initialize();
		test_validity();

		// Though Type::Reorientation is listed before Simple (intentionally: it doesn't actually change the (standardized) state of the puzzle),
		// Type::Simple needs to be tested first, since the former is dependent on the latter
		test_simples();
		test_reorientations();
	}
}