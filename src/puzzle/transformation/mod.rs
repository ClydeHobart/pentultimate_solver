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
		preferences::AnimationSpeedData,
		ui::input::PuzzleActionType,
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
	bevy_inspector_egui::{
		options::NumberAttributes,
		Context,
		Inspectable
	},
	egui::Ui,
	rand::{
		rngs::ThreadRng,
		Rng
	},
	serde::{
		Deserialize,
		Serialize
	},
	std::{
		convert::{
			AsMut,
			AsRef,
			TryFrom
		},
		cmp::max,
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
			Range,
			Sub,
			SubAssign
		},
		slice::Iter,
		sync::{
			Mutex,
			MutexGuard,
			PoisonError
		},
		time::Duration
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
	Complex3,
	Complex4,
	Complex5,
	Complex6
}

impl ComplexClass {
	const fn is_invert_not_mirror(self) -> bool { !matches!(self, Self::Complex1 | Self::Complex2) }

	const fn comprising_simples(self) -> &'static [HalfAddr] {
		macro_rules! slice_of_slices_of_half_addrs { ($([$(($line_index:expr, $word_index:expr)),*]),*) => {
			&[ $( &[ $( HalfAddr::new($line_index, $word_index), )* ], )* ]
		} }

		const COMPRISING_SIMPLES_LUT: &[&[HalfAddr]] = slice_of_slices_of_half_addrs![
			[
				(10, 4), (8, 1), (10, 1), (8, 4)
			],
			[
				(10, 4), (8, 1), (10, 1), (8, 4), (10, 4), (8, 1), (10, 1), (8, 4)
			],
			[
				(8,	3),	(7,	2),	(8,	2),	(7,	3),
				(8,	3),	(7,	2),	(8,	2),	(7,	3),
				(8,	3),	(7,	2),	(8,	2),	(7,	3),
				(1,	3),	(3,	3),	(7,	2),	(3,	2),
				(1,	3),	(6,	3),	(1,	2),	(3,	2),
				(6,	3),	(3,	3),	(1,	2),	(7,	2)
			],
			[
				(2, 1),
				(11, 4), (4, 1), (11, 1), (4, 4),
				(5, 4), (9, 1), (5, 1), (9, 4),
				(7, 1),
				(8, 4),
				(11, 4), (4, 1), (11, 1), (4, 4),
				(8, 1),
				(7, 4),
				(9, 1), (5, 4), (9, 4), (5, 1), (9, 1), (5, 4), (9, 4), (5, 1),
				(8, 1), (5, 4), (8, 4), (5, 1), (8, 1), (5, 4), (8, 4), (5, 1)
			],
			[
				(1, 1), (4, 4), (1, 4), (4, 1), (1, 1), (4, 4), (1, 4), (4, 1),
				(10, 1),
				(4, 4), (1, 1), (4, 1), (1, 4), (4, 4), (1, 1), (4, 1), (1, 4),
				(10, 4)
			],
			[
				(8, 4), (5, 1), (8, 1), (5, 4),
				(2, 1),
				(8, 4), (5, 1), (8, 1), (5, 4),
				(2, 4)
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
			Complex4A,		// This rotates the facing pentagon once clockwise. No guarantees are made for triangles. PuzzleState.rot: [0, ...] -> [1, ...]
			Complex4B,		// This is Complex4A mirrored
			Complex4C,		// This is Complex4A inverted
			Complex4D,		// This is Complex4B inverted
			Complex5A,		// This rotates two pentagons in opposite directions. No guarantees are made for triangles. PuzzleState.rot: [0, 0, ...] -> [1, 4, ...]
			Complex5B,		// This is Copmlex5A mirrored
			Complex5C,		// This is Complex5A inverted
			Complex5D,		// This is Complex5B inverted
			Complex6A,		// This commutes 3 adjacent pentagons. No guarantees are made for triangles.
			Complex6B,		// This is Complex6A mirrored
			Complex6C,		// This is Complex6A inverted
			Complex6D,		// This is Complex6B inverted
		);
	}
}

macro_rules! define_type {
	($($variant:ident,)*) => {
		#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
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
			Complex3A | Complex3B | Complex3C | Complex3D	=> Complex3,
			Complex4A | Complex4B | Complex4C | Complex4D	=> Complex4,
			Complex5A | Complex5B | Complex5C | Complex5D	=> Complex5,
			Complex6A | Complex6B | Complex6C | Complex6D	=> Complex6
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

#[derive(Clone, Copy, Deserialize, PartialEq, Serialize)]
pub struct HalfAddr(u8);

impl HalfAddr {
	#[inline(always)]
	pub fn as_reorientation(self) -> FullAddr { FullAddr::from((Type::Reorientation, self)) }

	#[inline(always)]
	pub fn as_simple(self) -> FullAddr { FullAddr::from((Type::Simple, self)) }

	pub fn invalidate(&mut self) -> () {
		self.0 = Self::INVALID;
	}

	#[inline(always)]
	pub fn is_valid(&self) -> bool { self.0 != Self::INVALID && self.word_index_is_valid()}

	#[inline(always)]
	pub fn line_index_is_valid(&self) -> bool { Self::is_valid_line_index(unsafe { self.get_line_index_unchecked() }) }

	#[inline(always)]
	pub fn word_index_is_valid(&self) -> bool { Self::is_valid_word_index(unsafe { self.get_word_index_unchecked() }) }

	pub fn orientation(self) -> Option<&'static Quat> {
		if self.is_valid() { Some(&Library::get().orientation_long_page.get_word(self)) } else { None }
	}

	pub const fn default() -> Self { Self(Self::INVALID) }

	#[inline(always)]
	pub const fn is_valid_line_index(line_index: usize) -> bool { line_index < Library::LINE_COUNT }

	#[inline(always)]
	pub const fn is_valid_long_line_index(long_line_index: usize) -> bool { long_line_index < Library::LONG_LINE_COUNT }

	#[inline(always)]
	pub const fn is_valid_word_index(word_index: usize) -> bool { word_index < Library::WORD_COUNT }

	pub const fn new(line_index: usize, word_index: usize) -> Self {
		if Self::is_valid_long_line_index(line_index) && Self::is_valid_word_index(word_index) {
			Self((line_index as u8) << Self::LINE_INDEX_BITS.start | word_index as u8)
		} else {
			Self::default()
		}
	}

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

impl Add for HalfAddr {
	type Output = Self;

	fn add(self, rhs: Self) -> Self { self + rhs.as_reorientation() }
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
	fn add(self, rhs: FullAddr) -> Self {
		if self.is_valid() && rhs.is_valid() {
			let mut sum: Self = rhs.transformation().unwrap().as_ref().half_addr(self.get_line_index());

			*sum.set_word_index((sum.get_word_index() + self.get_word_index()) % Library::WORD_COUNT)
		} else {
			Self::default()
		}
	}
}

impl AddAssign for HalfAddr { fn add_assign(&mut self, rhs: Self) -> () { *self = *self + rhs; } }

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

pub struct RandHalfAddrParams<'a> {
	pub thread_rng:					&'a mut ThreadRng,
	pub allow_long_line_indices:	bool
}

impl<'a> From<RandHalfAddrParams<'a>> for HalfAddr {
	fn from(rand_half_addr_params: RandHalfAddrParams<'a>) -> Self {
		const LONG_LINE_COUNT_F32: f32 = Library::LONG_LINE_COUNT as f32;
		const LINE_COUNT_F32: f32 = Library::LINE_COUNT as f32;
		const WORD_COUNT_F32: f32 = Library::WORD_COUNT as f32;

		*Self::default()
			.set_long_line_index(
				(rand_half_addr_params.thread_rng.gen::<f32>()
					* if rand_half_addr_params.allow_long_line_indices {
						LONG_LINE_COUNT_F32
					} else {
						LINE_COUNT_F32
					}
				) as usize)
			.set_word_index((rand_half_addr_params.thread_rng.gen::<f32>() * WORD_COUNT_F32) as usize)
	}
}

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

#[derive(Clone, Copy, Default)]
pub struct HalfAddrAttrs {
	pub allow_long_line_indices: bool
}

impl Inspectable for HalfAddr {
	type Attributes = HalfAddrAttrs;

	fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, context: &mut Context) -> bool {
		let mut changed: bool = false;

		ui.vertical_centered(|ui: &mut Ui| -> () {
			egui::Grid::new(context.id()).show(ui, |ui: &mut Ui| {
				if ui.small_button("Zero").clicked() && *self != Self::ORIGIN {
					*self = Self::ORIGIN;
					changed = true;
				}

				if ui.small_button("Invalid").clicked() && self.is_valid() {
					self.invalidate();
					changed = true;
				}

				ui.end_row();

				if self.is_valid() {
					ui.label("line_index");

					let mut line_index: usize = self.get_line_index();

					if line_index.ui(
						ui,
						NumberAttributes::<usize>::between(
							0_usize,
							if options.allow_long_line_indices {
								Library::LONG_LINE_COUNT
							} else {
								Library::LINE_COUNT
							} - 1_usize
						),
						&mut context.with_id(0_u64)
					) {
						self.set_long_line_index(line_index);
						changed = true;
					}

					ui.end_row();
					ui.label("word_index");

					let mut word_index: usize = self.get_word_index();

					if word_index.ui(
						ui,
						NumberAttributes::<usize>::between(
							0_usize,
							Library::WORD_COUNT - 1_usize
						),
						&mut context.with_id(1_u64)
					) {
						self.set_word_index(word_index);
						changed = true;
					}

					ui.end_row();
				} else {
					let mut filler_index: i32 = -1_i32;

					ui.set_enabled(false);
					ui.label("line_index");
					filler_index.ui(ui, NumberAttributes::<i32>::default(), &mut context.with_id(0_u64));
					ui.end_row();
					ui.label("word_index");
					filler_index.ui(ui, NumberAttributes::<i32>::default(), &mut context.with_id(1_u64));
					ui.end_row();
				}
			});
		});

		changed
	}
}

impl Neg for HalfAddr {
	type Output = Self;

	fn neg(self) -> Self::Output { *self.as_reorientation().inverse().get_half_addr() }
}

impl Sub for HalfAddr {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self { self + (-rhs) }
}

pub trait FullAddrConsts {
	const INVALID_INDEX: u8;
}

#[derive(Clone, Copy, Deserialize, PartialEq, Serialize)]
pub struct FullAddr {
	page_index:	u8,
	half_addr:	HalfAddr
}

impl FullAddrConsts for FullAddr {
	const INVALID_INDEX: u8 = u8::MAX;
}

impl FullAddr {
	pub fn get_comprising_simples(self) -> &'static [HalfAddr] {
		if self.is_valid() {
			Library::get().comprising_simples.get_word(self)
		} else {
			&[]
		}
	}

	pub fn get_comprising_simples_string(self) -> String {
		let mut comprising_simples_string: String = String::new();

		for (comprising_simple_index, comprising_simple)
			in self.get_comprising_simples().iter().enumerate()
		{
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

	pub fn get_cycles(self) -> u32 {
		if self.is_page_index_reorientation() {
			1_u32
		} else {
			Self::get_cycles_for_comprising_simples(&self.get_comprising_simples())
		}
	}

	#[inline(always)]
	pub fn get_half_addr(&self) -> &HalfAddr { &self.half_addr }

	pub fn get_page_index_type(self) -> Option<Type> {
		if self.page_index_is_valid() {
			Some(unsafe { transmute(self.page_index) })
		} else {
			None
		}
	}

	#[inline(always)]
	pub fn half_addr_is_valid(&self) -> bool { Self::is_valid_half_addr(self.half_addr) }

	pub fn invalidate(&mut self) -> () {
		*self = Self::default()
	}

	pub fn invalidate_page_index(&mut self) -> () {
		self.page_index = Self::INVALID_INDEX;
	}

	pub fn invalidate_half_addr(&mut self) -> () {
		self.half_addr.invalidate();
	}

	pub fn inverse(self) -> Self {
		if self.is_valid() { *Library::get().book_pack_data.inverse.get_word(self) } else { Self::default() }
	}

	pub fn is_identity_transformation(self) -> bool {
		match self.get_page_index_type() {
			Some(Type::Reorientation)	=> *self.get_half_addr() == HalfAddr::ORIGIN,
			Some(Type::Simple)			=> self.get_word_index() == 0_usize,
			_							=> false
		}
	}

	#[inline(always)]
	pub fn is_page_index_reorientation(&self) -> bool { self.page_index == Type::Reorientation as u8 }

	#[inline(always)]
	pub fn is_page_index_simple(&self) -> bool { self.page_index == Type::Simple as u8 }

	#[inline(always)]
	pub fn is_valid(&self) -> bool { self.page_index_is_valid() && self.half_addr_is_valid() }

	#[inline(always)]
	pub fn line_index_is_valid(&self) -> bool { self.half_addr.line_index_is_valid() }

	pub fn mask(self) -> Option<&'static Mask> {
		if self.is_valid() { Some(Library::get().book_pack_data.mask.get_word(self)) } else { None }
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

	#[inline(always)]
	pub fn page_index_is_valid(&self) -> bool { Self::is_valid_page_index(unsafe { self.get_page_index_unchecked() }) }

	pub fn rotation(self) -> Option<&'static Quat> {
		if self.is_valid() && !self.get_page_index_type().unwrap().is_complex() {
			Some(Library::get().rotation_shrt_book.get_word(self))
		} else {
			None
		}
	}

	pub fn set_half_addr(&mut self, half_addr: HalfAddr) -> &mut FullAddr {
		assert!(self.half_addr_is_valid());
		self.half_addr = half_addr;

		self
	}

	#[inline(always)]
	pub fn standardization(self) -> HalfAddr { -(HalfAddr::ORIGIN + self) }

	pub fn transformation(self) -> Option<&'static Transformation> {
		if self.is_valid() { Some(Library::get().book_pack_data.transformation.get_word(self)) } else { None }
	}

	#[inline(always)]
	pub fn word_index_is_valid(&self) -> bool { self.half_addr.word_index_is_valid() }

	pub fn word_pack(self) -> Option<WordPack<'static>> {
		if self.is_valid() { Some(Library::get().book_pack_data.get_word_pack(self)) } else { None }
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

	#[inline]
	pub const fn invert_word_index(word_index: usize) -> usize {
		const WORD_INDEX_LUT: [u8; Library::WORD_COUNT] = [0_u8, 4_u8, 3_u8, 2_u8, 1_u8];

		WORD_INDEX_LUT[word_index] as usize
	}

	#[inline(always)]
	pub fn is_valid_half_addr(half_addr: HalfAddr) -> bool {
		half_addr.line_index_is_valid() && half_addr.word_index_is_valid()
	}

	#[inline(always)]
	pub fn is_valid_page_index(page_index: usize) -> bool { page_index < Library::PAGE_COUNT }

	#[inline]
	pub const fn mirror_line_index(line_index: usize) -> usize {
		const LINE_INDEX_LUT: [u8; Library::LINE_COUNT] = [
			0_u8,	1_u8,	2_u8,	3_u8, // Unchanged
			5_u8,	4_u8,	7_u8,	6_u8, // ABCD -> BADC
			10_u8,	11_u8,	8_u8,	9_u8, // ABCD -> CDAB
		];

		LINE_INDEX_LUT[line_index] as usize
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

		sum.half_addr += rhs;

		if self.is_page_index_simple() {
			sum.set_word_index(self.get_word_index());
		}

		sum
	}
}

impl AddAssign<HalfAddr> for FullAddr { fn add_assign(&mut self, rhs: HalfAddr) -> () { *self = *self + rhs; } }

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

impl Sub<HalfAddr> for FullAddr {
	type Output = Self;

	fn sub(self, rhs: HalfAddr) -> Self::Output { self + (-rhs) }
}

impl SubAssign<HalfAddr> for FullAddr { fn sub_assign(&mut self, rhs: HalfAddr) -> () { *self = *self - rhs; } }

#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct Action {
	pub transformation:	FullAddr,
	pub camera_start:	HalfAddr
}

impl Action {
	pub fn new(transformation: FullAddr, camera_start: HalfAddr) -> Self {
		Self {
			transformation,
			camera_start
		}
	}

	pub fn compute_duration(
		&self,
		animation_speed_data: &AnimationSpeedData,
		action_type: PuzzleActionType
	) -> Duration {
		if matches!(action_type, PuzzleActionType::Undo | PuzzleActionType::Redo)
			&& !animation_speed_data.animate_undo_and_redo
		{
			Duration::ZERO
		} else {
			Duration::from_millis(animation_speed_data.rotation_millis as u64)
				* if animation_speed_data.uniform_transformation_duration || !self.transformation.is_valid() {
					1_u32
				} else {
					max(self.transformation.get_cycles(), 1_u32)
				}
		}
	}

	pub fn get_camera_end(&self) -> HalfAddr {
		if self.transformation.is_page_index_reorientation() {
			*self.transformation.get_half_addr()
		} else {
			self.camera_start
		}
	}

	pub fn get_standardized_camera_end(&self) -> HalfAddr { self.get_camera_end() + self.standardization() }

	pub fn invert(&self) -> Self {
		if self.is_valid() {
			if self.transformation.is_page_index_reorientation() {
				Self::new(
					self.camera_start.as_reorientation(),
					*self.transformation.get_half_addr()
				)
			} else {
				let standardization: HalfAddr = self.standardization();

				Self::new(
					self.transformation.inverse() + standardization,
					self.get_camera_end() + standardization
				)
			}
		} else {
			Self::default()
		}
	}

	pub fn is_valid(&self) -> bool { self.transformation.is_valid() && self.camera_start.is_valid() }

	#[inline(always)]
	pub fn standardization(&self) -> HalfAddr { self.transformation.standardization() }
}

impl Add<HalfAddr> for Action {
	type Output = Self;

	fn add(self, rhs: HalfAddr) -> Self {
		Self {
			transformation:	self.transformation + rhs,
			camera_start:	self.camera_start + rhs
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

	pub fn is_valid(&self) -> bool {
		self.0.is_valid()
	}
}

impl AsMut<PuzzleState> for Transformation { fn as_mut(&mut self) -> &mut PuzzleState { &mut self.0 } }

impl AsRef<PuzzleState> for Transformation { fn as_ref(&self) -> &PuzzleState { &self.0 } }

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
	const SHRT_PAGE_COUNT:	usize;
}

pub type Word<T>		= T;
pub type Line<T>		= [Word<T>; Library::WORD_COUNT];
pub type Page<T>		= [Line<T>; Library::LINE_COUNT];
pub type LongPage<T>	= [Line<T>; Library::LONG_LINE_COUNT];
pub type Book<T>		= [Page<T>; Library::PAGE_COUNT];
pub type ShrtBook<T>	= [Page<T>; Library::SHRT_PAGE_COUNT];

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

impl<A : Addr + Sized, T> GetWord<A, T> for ShrtBook<T> {
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
	book_pack_data:			BookPackData,
	rotation_shrt_book:		ShrtBook<Quat>,
	orientation_long_page:	LongPage<Quat>,
	comprising_simples:		ComprisingSimplesData
}

impl LibraryConsts for Library {
	const WORD_COUNT:		usize = PENTAGON_SIDE_COUNT;
	const LINE_COUNT:		usize = PENTAGON_PIECE_COUNT;
	const LONG_LINE_COUNT:	usize = PIECE_COUNT;
	const PAGE_COUNT:		usize = TYPE_COUNT;
	const SHRT_PAGE_COUNT:	usize = COMPLEX_OFFSET;
}

impl Library {
	fn initialize(&mut self) -> () {
		Data::initialize();

		let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);

		for (long_line_index, orientation_long_line)
			in self.orientation_long_page.iter_mut().enumerate()
		{
			let face_data: &FaceData = &icosidodecahedron_data.faces[long_line_index];

			for (word_index, orientation_word)
				in orientation_long_line.iter_mut().enumerate()
			{
				*orientation_word = face_data.get_rotated_quat(word_index as u32);
			}
		}

		let (
				book_pack_data,
				rotation_shrt_book,
				comprising_simples,
				orientation_long_page
			):
			(
				&mut BookPackData,
				&mut ShrtBook<Quat>,
				&mut ComprisingSimplesData,
				&LongPage<Quat>
			) =
			(
				&mut self.book_pack_data,
				&mut self.rotation_shrt_book,
				&mut self.comprising_simples,
				&self.orientation_long_page
			);

		for page_index in 0_usize .. Library::PAGE_COUNT {
			let transformation_type: Type = Type::try_from(page_index as u8).unwrap();

			if transformation_type.is_simple() {
				for (line_index, line)
					in comprising_simples.Simple.iter_mut().enumerate()
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
						let comprising_simples: &mut [HalfAddr] = comprising_simples
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
					let rotation_page: &mut Page<Quat> = &mut rotation_shrt_book[Type::Reorientation as usize];

					page_pack_mut.iter_mut(|line_index: usize, mut line_pack_mut: LinePackMut| -> () {
						let orientation_line: &Line<Quat> = &orientation_long_page[line_index];
						let rotation_line: &mut Line<Quat> = &mut rotation_page[line_index];

						line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
							let reorientation_quat: Quat = orientation_line[word_index] * origin_conj_quat;
							let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) =
								word_pack_mut.transformation.arrays_mut();

							for piece_index in PIECE_RANGE {
								let (pos, rot): (usize, usize) = icosidodecahedron_data.get_pos_and_rot(
									&(reorientation_quat * icosidodecahedron_data.faces[piece_index].quat),
									None /* We could put a filter in here, but it'd be slower, and the quat math
										is precise enough that it's unnecessary here */
								);

								pos_array[piece_index] = pos as PieceStateComponent;
								rot_array[piece_index] = rot as PieceStateComponent;
							}

							*word_pack_mut.mask = Mask::from(&*word_pack_mut.transformation);
							*word_pack_mut.inverse = FullAddr::default();
							rotation_line[word_index] = reorientation_quat;
						});
					});

					let trfm_page:		&Page<Trfm>			= page_pack_mut.transformation;
					let addr_page_mut:	&mut Page<FullAddr>	= page_pack_mut.inverse;

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
						let rotation_page: &mut Page<Quat> = &mut rotation_shrt_book[Type::Simple as usize];

						line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
							let rotation_quat: Quat = face_data.get_rotation_quat(word_index as u32);
							let mask: Mask = if word_index != 0 { mask } else { Mask::default() };
							let rotation_line: &mut Line<Quat> = &mut rotation_page[line_index];

							let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) =
								word_pack_mut.transformation.arrays_mut();

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

							*word_pack_mut.mask = mask;
							*word_pack_mut.inverse = FullAddr::from((
								transformation_type.invert() as usize,
								line_index,
								FullAddr::invert_word_index(word_index)
							));
							rotation_line[word_index] = rotation_quat;
						});
					});
				},
				_ => {
					let (non_complex_trfm_pages, complex_trfm_pages):
						(&mut [Page<Trfm>], &mut [Page<Trfm>]) =
						book_pack_data.transformation.split_at_mut(COMPLEX_OFFSET);
					let simple_trfm_page: &Page<Trfm> = &non_complex_trfm_pages[Type::Simple as usize];
					let complex_trfm_page: &mut Page<Trfm> = &mut complex_trfm_pages[page_index - COMPLEX_OFFSET];

					for (line_index, complex_trfm_line)
						in complex_trfm_page.iter_mut().enumerate()
					{
						for (word_index, complex_trfm_word)
							in complex_trfm_line.iter_mut().enumerate()
						{
							let mut puzzle_state: PuzzleState = PuzzleState::SOLVED_STATE;

							for comprising_simple 
								in comprising_simples
									.get_word(FullAddr::from((page_index, line_index, word_index)))
							{
								puzzle_state += simple_trfm_page.get_word(*comprising_simple);
							}

							*complex_trfm_word = Transformation(puzzle_state);
						}
					}

					let mut page_pack_mut: PagePackMut = book_pack_data.get_page_pack_mut(page_index);

					page_pack_mut.iter_mut(|line_index: usize, mut line_pack_mut: LinePackMut| -> () {
						line_pack_mut.iter_mut(|word_index: usize, word_pack_mut: WordPackMut| -> () {
							*word_pack_mut.mask = Mask::from(&*word_pack_mut.transformation);
							*word_pack_mut.inverse = FullAddr::from((
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
	fn build(&self, _: &mut App) -> () { <Library as StaticDataLibrary>::initialize(); }
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
					let trfm: &Trfm = word_pack.transformation;

					if !trfm.is_valid() {
						log::error!("Trfm ({}, {}, {}) is invalid", page_index, line_index, word_index);
						error_expr!(word_pack.transformation);

						panic!();
					}

					if !word_pack.inverse.is_valid() {
						log::error!(
							"Inverse address for transformation ({}, {}, {}) is invalid",
							page_index, line_index, word_index
						);
						error_expr!(word_pack.inverse);

						panic!();
					}

					let inv_addr: FullAddr = *word_pack.inverse;
					let inv_trfm: &Trfm = book_pack.get_word_pack(inv_addr).transformation;

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
			&library.book_pack_data.transformation[Type::Simple as usize];
		let reorientation_tests: [Vec<(usize, usize)>; PENTAGON_PIECE_COUNT] =
			<[Vec<(usize, usize)>; PENTAGON_PIECE_COUNT]>::from_file(
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

				let trfm:								&Trfm		= &word_pack.transformation;

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

				if standardization_full_addr != *word_pack.inverse {
					log::error!(
						"Standardization address for current state doesn't match inverse address with pentagon {} \
							and rotation {}",
						line_index,
						word_index
					);
					error_expr!(prev_puzzle_state, standardization_full_addr, word_pack.inverse);

					panic!();
				}

				// Reorientation was originally in the other direction, and the standardization check only works for the
				// other direction, so just run the rest how the test was originally conducted

				let word_pack: WordPack = Library::get()
					.book_pack_data
					.get_word_pack(standardization_full_addr);
				let trfm: &Trfm = word_pack.transformation;

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
				let trfm:						&Trfm				= &word_pack.transformation;
				let inv_trfm:					&Trfm				= &library
					.book_pack_data
					.get_word_pack(*word_pack.inverse)
					.transformation;

				let mut curr_puzzle_state:		PuzzleState			= PuzzleState::SOLVED_STATE;
				let mut curr_puzzle_state_alt:	PuzzleState			= PuzzleState::SOLVED_STATE;

				for turn in 1_usize ..= PENTAGON_SIDE_COUNT {
					let prev_puzzle_state:		PuzzleState = curr_puzzle_state.clone();

					curr_puzzle_state += trfm;
					curr_puzzle_state_alt.naive_add_assign(trfm);

					if curr_puzzle_state_alt != curr_puzzle_state {
						log::error!(
							"curr_puzzle_state_alt != curr_puzzle_state after turn {} with pentagon {} and rotation {}",
							turn,
							line_index,
							word_index
						);
						error_expr!(prev_puzzle_state, trfm, curr_puzzle_state, curr_puzzle_state_alt);

						panic!();
					}

					if !curr_puzzle_state.is_valid() {
						log::error!(
							"Puzzle state isn't valid after turn {} with pentagon {} and rotation {}",
							turn,
							line_index,
							word_index
						);
						error_expr!(prev_puzzle_state, trfm, curr_puzzle_state);

						panic!();
					}

					let mut prev_puzzle_state_from_inverse_transformation = curr_puzzle_state.clone();

					prev_puzzle_state_from_inverse_transformation += inv_trfm;

					if prev_puzzle_state_from_inverse_transformation != prev_puzzle_state {
						log::error!(
							"prev_puzzle_state_from_inverse_transformation != prev_puzzle_state after turn {} with \
								pentagon {} and rotation {}",
							turn,
							line_index,
							word_index
						);
						error_expr!(
							curr_puzzle_state,
							inv_trfm,
							prev_puzzle_state_from_inverse_transformation,
							prev_puzzle_state
						);

						panic!();
					}
				}

				if curr_puzzle_state != PuzzleState::SOLVED_STATE {
					log::error!(
						"curr_puzzle_state != curr_puzzle_state with pentagon {} and rotation {}",
						line_index,
						word_index
					);
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

		/* Though Type::Reorientation is listed before Simple (intentionally: it doesn't actually change the
		(standardized) state of the puzzle), Type::Simple needs to be tested first, since the former is dependent on the
		latter */
		test_simples();
		test_reorientations();
	}
}
