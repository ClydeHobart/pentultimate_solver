use {
	std::{
		convert::{
			TryFrom,
			TryInto
		},
		fmt::{
			Debug,
			Formatter
		},
		iter::DoubleEndedIterator,
		mem::{
			MaybeUninit,
			take
		},
		ops::{
			Deref,
			DerefMut,
			Range
		},
		sync::Once
	},
	bevy::prelude::Quat,
	bit_field::BitArray,
	serde::{
		Deserialize,
		Deserializer,
		Serialize,
		Serializer
	},
	crate::{
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		prelude::*,
		puzzle::{
			consts::*,
			inflated::{
				PieceStateComponent,
				PuzzleState,
				PuzzleStateComponent,
				PuzzleStateConsts
			}
		},
		util::StaticDataLibrary
	},
	super::{
		Addr,
		GetWord,
		FindWord,
		FullAddr,
		HalfAddr,
		HalfAddrConsts,
		FullMask,
		Transformation
	}
};

pub type GenusIndexType = u8;

#[derive(Clone, Copy, Deserialize, Eq, PartialEq, Serialize)]
pub struct GenusIndex(GenusIndexType);

impl GenusIndex {
	#[inline(always)]
	pub fn is_complex(self) -> bool { self.is_valid() && self.0 >= Self::COMPLEX_OFFSET as u8 }

	#[inline(always)]
	pub fn is_reorientation(self) -> bool { self.0 == Self::REORIENTATION.0 }

	#[inline(always)]
	pub fn is_simple(self) -> bool { self.0 == Self::SIMPLE.0 }

	#[inline(always)]
	pub fn is_valid(self) -> bool { self.0 as usize <= Library::get_genus_count() }

	pub fn invert(self) -> Self {
		if self.is_valid() {
			Library::get().get_family_info(self).map(|family_info: &FamilyInfo| -> Self {
				family_info.invert_genus_index(self)
			}).unwrap_or(self)
		} else {
			GenusIndex::INVALID
		}
	}

	pub fn mirror(self) -> Self {
		if self.is_valid() {
			Library::get().get_family_info(self).map(|family_info: &FamilyInfo| -> Self {
				family_info.mirror_genus_index(self)
			}).unwrap_or(self)
		} else {
			GenusIndex::INVALID
		}
	}

	#[inline(always)]
	fn from_usize(genus_index: usize) -> Self { Self(genus_index as GenusIndexType) }
}

pub trait GenusIndexConsts {
	const REORIENTATION:	GenusIndex;
	const SIMPLE:			GenusIndex;
	const COMPLEX_OFFSET:	usize;
	const INVALID:			GenusIndex;
}

impl GenusIndexConsts for GenusIndex {
	const REORIENTATION:	GenusIndex	= GenusIndex(0 as GenusIndexType);
	const SIMPLE:			GenusIndex	= GenusIndex(Self::REORIENTATION.0 + 1 as GenusIndexType);
	const COMPLEX_OFFSET:	usize		= Self::SIMPLE.0 as usize + 1_usize;
	const INVALID:			GenusIndex	= GenusIndex(GenusIndexType::MAX);
}

impl Debug for GenusIndex {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
		if self.is_valid() {
			formatter.write_str(&Library::get().genus_infos[usize::from(*self)].name)
		} else {
			formatter.write_str("[invalid]")
		}
	}
}

impl Default for GenusIndex { fn default() -> Self { Self::INVALID } }

impl Deref for GenusIndex {
	type Target = GenusIndexType;

	fn deref(&self) -> &GenusIndexType { &self.0 }
}

impl DerefMut for GenusIndex { fn deref_mut(&mut self) -> &mut GenusIndexType { &mut self.0 } }

impl From<GenusIndex> for usize { fn from(genus_index: GenusIndex) -> Self { genus_index.0 as Self } }

impl<'a> TryFrom<&'a str> for GenusIndex {
	type Error = ();

	fn try_from(genus_index_str: &'a str) -> Result<Self, ()> {
		for (genus_index, genus_info) in Library::get().genus_infos.iter().enumerate() {
			if genus_index_str == genus_info.name {
				return Ok(Self::from_usize(genus_index));
			}
		}

		Err(())
	}
}

impl TryFrom<GenusIndexType> for GenusIndex {
	type Error = ();

	fn try_from(value: GenusIndexType) -> Result<Self, ()> {
		let genus_index: Self = Self(value);

		if genus_index.is_valid() {
			Ok(genus_index)
		} else {
			Err(())
		}
	}
}

pub trait GenusIndexBitArrayConsts {
	type Block;
	const SIZE:	usize;
	const ALL:	GenusIndexBitArray;
	const NONE:	GenusIndexBitArray;
}

pub struct GenusIndexBitArray(pub [<Self as GenusIndexBitArrayConsts>::Block; Self::SIZE]);

impl GenusIndexBitArray {
	#[inline(always)]
	pub fn get_bit<G: Into<usize>>(&self, genus_index: G) -> bool { self.0.get_bit(genus_index.into()) }

	#[inline(always)]
	pub fn set_bit<G: Into<usize> + Sized>(&mut self, genus_index: G, enabled: bool) -> () {
		self.0.set_bit(genus_index.into(), enabled)
	}

	const fn size() -> usize {
		(1_usize << GenusIndexType::BITS) / <Self as GenusIndexBitArrayConsts>::Block::BITS as usize
	}
}

impl GenusIndexBitArrayConsts for GenusIndexBitArray {
	type Block			= u32;
	const SIZE:	usize	= Self::size();
	const ALL:	Self	= GenusIndexBitArray([Self::Block::MAX; Self::SIZE]);
	const NONE:	Self	= GenusIndexBitArray([Self::Block::MIN; Self::SIZE]);
}

impl<G: Copy + TryInto<GenusIndex>> From<&[G]> for GenusIndexBitArray {
	fn from(genus_indices: &[G]) -> Self {
		let mut genus_index_bit_array: Self = Self::NONE;

		for genus_index in genus_indices {
			if let Ok(genus_index) = TryInto::<GenusIndex>::try_into(*genus_index) {
				if genus_index.is_valid() {
					genus_index_bit_array.set_bit(genus_index, true);
				}
			}
		}

		genus_index_bit_array
	}
}

/* With GenusIndexType as a u8, size_of::<GenusIndexBitArray>() == 32_usize. This is acceptable. With GenusIndexType as
a u16, or some other integer type of greater or equal size, size_of::<GenusIndexBitArray>() >= 8192_usize. This is
unacceptable. If there's a need to increase the bit count of GenusIndexType to be greater than 8, GenusIndexBitArray
should be modified to be a dynamically allocated bit vector. */
assert_type_eq_all!(GenusIndexType, u8);

pub struct GenusIndexString(pub GenusIndex);

impl<'de> Deserialize<'de> for GenusIndexString {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		let genus_index_string: String = String::deserialize(deserializer)?;

		for (genus_index, genus_info) in Library::get().genus_infos.iter().enumerate() {
			if genus_info.name == genus_index_string {
				return Ok(GenusIndexString(GenusIndex(genus_index as u8)));
			}
		}

		Err(<D::Error as serde::de::Error>::custom(
			format!("could not find genus index matching string \"{}\"", genus_index_string)
		))
	}
}

impl Serialize for GenusIndexString {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		if self.0.is_valid() {
			format!("{}", Library::get().genus_infos[usize::from(self.0)].name).serialize(serializer)
		} else {
			Err(<S::Error as serde::ser::Error>::custom(format!("genus index being serialized is invalid")))
		}
	}
}

#[derive(Deserialize, Serialize)]
struct FamilyInput {
	name:			String,
	seed_simples:	Vec<(u8, u8)>
}

#[derive(Default, Deserialize, Serialize)]
pub struct OrderInfo(Vec<FamilyInput>);

struct FamilyInfo {
	name:				String,
	base_genus:			GenusIndex,
	inverse_is_mirror:	bool
}

impl FamilyInfo {
	#[inline(always)]
	fn get_genus_count(&self) -> usize { if self.inverse_is_mirror { 2_usize } else { 4_usize } }

	fn get_genus_range(&self) -> Range<usize> {
		let genus_index_start: usize = self.base_genus.into();

		genus_index_start .. genus_index_start + self.get_genus_count()
	}

	fn invert_genus_index(&self, genus_index: GenusIndex) -> GenusIndex {
		let genus_range: Range<usize> = self.get_genus_range();

		if genus_range.contains(&usize::from(genus_index)) {
			GenusIndex(((genus_index.0 - genus_range.start as GenusIndexType)
					^ if self.inverse_is_mirror { 1 as GenusIndexType } else { 2 as GenusIndexType }
				) + genus_range.start as GenusIndexType
			)
		} else {
			genus_index
		}
	}

	fn mirror_genus_index(&self, genus_index: GenusIndex) -> GenusIndex {
		let genus_range: Range<usize> = self.get_genus_range();

		if genus_range.contains(&usize::from(genus_index)) {
			GenusIndex(((genus_index.0 - genus_range.start as GenusIndexType)
					^ 1 as GenusIndexType
				) + genus_range.start as GenusIndexType
			)
		} else {
			genus_index
		}
	}
}

#[derive(Clone)]
pub struct GenusRange(Range<GenusIndexType>);

impl Default for GenusRange { fn default() -> Self { GenusRange(GenusIndex::INVALID.0 .. GenusIndex::INVALID.0) } }

impl TryFrom<&str> for GenusRange {
	type Error = ();

	fn try_from(base_genus_index_str: &str) -> Result<Self, ()> {
		if let Ok(base_genus_index) = GenusIndex::try_from(base_genus_index_str) {
			if let Some(family_info) = Library::get().get_family_info(base_genus_index) {
				let genus_range: Range<usize> = family_info.get_genus_range();

				Ok(Self(genus_range.start as GenusIndexType .. genus_range.end as GenusIndexType))
			} else {
				/* While it's not a full-fledge family, if it's a valid index w/o a family info, it's either
				REORIENTATION or SIMPLE, which are still valid genera in an of themselves */
				Ok(Self(base_genus_index.0 .. base_genus_index.0 + 1 as GenusIndexType))
			}
		} else {
			Err(())
		}
	}
}

impl From<GenusRange> for GenusIndexBitArray {
	fn from(genus_range: GenusRange) -> Self {
		let genus_count: GenusIndexType = Library::get_genus_count() as GenusIndexType;

		let mut genus_index_bit_array: Self = Self::NONE;

		for genus_index_type in genus_range.0.start.min(genus_count) .. genus_range.0.end.min(genus_count) {
			genus_index_bit_array.set_bit(genus_index_type, true);
		}

		genus_index_bit_array
	}
}

type SimpleOffsetType = u32;
type SimpleSliceLenType = u16;
type FamilyIndexType = u8;

#[derive(Clone)]
struct GenusInfo {
	name:				String,
	simple_offset:		SimpleOffsetType,
	simple_slice_len:	SimpleSliceLenType,
	family_index:		Option<FamilyIndexType>
}

impl GenusInfo {
	fn new(name: String, simple_slice: &[HalfAddr], family_index: Option<FamilyIndexType>) -> Self {
		Self {
			name,
			simple_offset:		0 as SimpleOffsetType,
			simple_slice_len:	simple_slice.len() as SimpleSliceLenType,
			family_index
		}
	}
}

pub trait LibraryConsts {
	const ORGANISMS_PER_SPECIES:	usize;
	const SPECIES_PER_GENUS:		usize;
	const ORGANISMS_PER_GENUS:		usize;
	const SPECIES_PER_LARGE_GENUS:	usize;
	const GENERA_PER_SMALL_CLASS:	usize;
}

pub type Organism<T>	= T;
pub type Species<T>		= [Organism<T>;	Library::ORGANISMS_PER_SPECIES];
pub type Genus<T>		= [Species<T>;	Library::SPECIES_PER_GENUS];
pub type LargeGenus<T>	= [Species<T>;	Library::SPECIES_PER_LARGE_GENUS];
// Family is reserved for grouping complex genera together; order is reserved for grouping families together
pub type Class<T>		= Vec<Genus<T>>;
pub type SmallClass<T>	= [Genus<T>;	Library::GENERA_PER_SMALL_CLASS];

#[derive(Default)]
pub struct Library {
	family_infos:		Vec<FamilyInfo>,
	genus_infos:		Vec<GenusInfo>,
	simples:			Vec<HalfAddr>,
	transformations:	Class<Transformation>,
	full_masks:				Class<FullMask>,
	inverse_addrs:		Class<FullAddr>,
	rotations:			SmallClass<Quat>,
	orientations:		LargeGenus<Quat>,
}

#[derive(Debug)]
pub enum PushFamilyErr {
	InvalidSeedSimpleSlice,
	FamilyIndexTooLarge,
	GenusIndexTooLarge,
	SimpleOffsetTooLarge,
	SeedSimpleSliceTooShort,
	SeedSimpleSliceTooLong,
	DoesNotTransformPuzzleState,
	HomomorphismExists(FullAddr)
}

pub type SimpleSlice<'a> = Option<&'a [HalfAddr]>;

impl Library {
	#[inline(always)]
	pub fn get_transformation(full_addr: FullAddr) -> &'static Transformation {
		debug_assert!(full_addr.is_valid());

		Library::get().transformations.get_word(full_addr)
	}

	#[inline(always)]
	pub fn get_full_mask(full_addr: FullAddr) -> &'static FullMask {
		debug_assert!(full_addr.is_valid());

		Library::get().full_masks.get_word(full_addr)
	}

	#[inline(always)]
	pub fn get_inverse_addr(full_addr: FullAddr) -> &'static FullAddr {
		debug_assert!(full_addr.is_valid());

		Library::get().inverse_addrs.get_word(full_addr)
	}

	#[inline(always)]
	pub fn get_rotation(full_addr: FullAddr) -> &'static Quat {
		debug_assert!(full_addr.is_valid() && full_addr.get_genus_index() < Self::GENERA_PER_SMALL_CLASS);

		Library::get().rotations.get_word(full_addr)
	}

	#[inline(always)]
	pub fn get_orientation(half_addr: HalfAddr) -> &'static Quat {
		debug_assert!(half_addr.is_valid());

		Library::get().orientations.get_word(half_addr)
	}

	#[inline(always)]
	pub fn get_simple_slice(full_addr: FullAddr) -> &'static [HalfAddr] {
		debug_assert!(full_addr.is_valid());

		get_simple_slice(&Library::get(), full_addr)
	}

	#[inline(always)]
	pub fn get_genus_count() -> usize { return Library::get().genus_infos.len() }

	fn get_simple_slice_genus(&self, genus_index: GenusIndex) -> Option<Box<Genus<SimpleSlice>>> {
		let simples_range: Range<usize> = {
			if let Some(simples_range) = self.get_simples_range(genus_index) {
				simples_range
			} else {
				return None;
			}
		};
		let simple_slice_len: usize = self.genus_infos[usize::from(genus_index)].simple_slice_len as usize;
		let mut simple_slice_genus: Genus<SimpleSlice> = Genus::<SimpleSlice>::default();

		for (species_index, simples_species) in self
			.simples
			[simples_range]
			.chunks(Self::ORGANISMS_PER_SPECIES * simple_slice_len)
			.enumerate()
		{
			let simple_slice_species: &mut Species<SimpleSlice> = &mut simple_slice_genus[species_index];

			for (organism_index, simple_slice) in simples_species
				.chunks(simple_slice_len)
				.enumerate()
			{
				simple_slice_species[organism_index] = Some(simple_slice);
			}
		}

		Some(Box::<Genus<SimpleSlice>>::new(simple_slice_genus))
	}

	fn get_simples_range(&self, genus_index: GenusIndex) -> Option<Range<usize>> {
		let genus_index: usize = genus_index.into();

		if genus_index >= self.genus_infos.len() {
			return None;
		}

		let genus_info: &GenusInfo = &self.genus_infos[genus_index];
		let simple_slice_start: usize = genus_info.simple_offset as usize;
		let simple_slice_end: usize = simple_slice_start
			+ Self::ORGANISMS_PER_GENUS * genus_info.simple_slice_len as usize;

		if simple_slice_end > self.simples.len() {
			return None;
		}

		return Some(simple_slice_start .. simple_slice_end);
	}

	fn get_family_info(&self, genus_index: GenusIndex) -> Option<&FamilyInfo> {
		if let Some(family_index) = self.genus_infos[usize::from(genus_index)].family_index {
			Some(&self.family_infos[family_index as usize])
		} else {
			None
		}
	}

	fn is_seed_simple_slice_valid(&self, seed_simple_slice: &[HalfAddr]) -> Result<(), PushFamilyErr> {
		for simple in seed_simple_slice {
			if !simple.is_valid() {
				return Err(PushFamilyErr::InvalidSeedSimpleSlice);
			}
		}

		if self.family_infos.len() > FamilyIndexType::MAX as usize {
			return Err(PushFamilyErr::FamilyIndexTooLarge);
		}

		if self.genus_infos.len() > GenusIndexType::MAX as usize - 1_usize {
			return Err(PushFamilyErr::GenusIndexTooLarge);
		}

		if self.simples.len() > SimpleOffsetType::MAX as usize {
			return Err(PushFamilyErr::SimpleOffsetTooLarge);
		}

		if seed_simple_slice.len() <= 1_usize {
			return Err(PushFamilyErr::SeedSimpleSliceTooShort);
		}

		if seed_simple_slice.len() > SimpleSliceLenType::MAX as usize {
			return Err(PushFamilyErr::SeedSimpleSliceTooLong);
		}

		let mut puzzle_state: PuzzleState = PuzzleState::default();

		for simple in seed_simple_slice {
			puzzle_state += simple.as_simple();
		}

		if puzzle_state.is_solved() {
			return Err(PushFamilyErr::DoesNotTransformPuzzleState);
		}

		// Check for a homomorphic genus
		for family_info in &self.family_infos {
			let base_genus_index: usize = family_info.base_genus.into();
			let base_genus_info: &GenusInfo = &self.genus_infos[base_genus_index];

			// If the base genus for the family has a different simple slice length, it won't be a match
			if seed_simple_slice.len() != base_genus_info.simple_slice_len as usize {
				continue;
			}

			let mut full_addr: FullAddr = HalfAddr::ORIGIN.as_reorientation();

			for genus_index in family_info.get_genus_range() {
				if !Self::simple_slices_have_identical_organism_indicies(
					seed_simple_slice,
					get_simple_slice(self, *full_addr.set_genus_index(genus_index))
				) {
					continue;
				}

				
				if let Some(mut homomorphism) = self
					.get_simple_slice_genus(GenusIndex(genus_index as GenusIndexType))
					.and_then(|simple_slice_genus: Box<Genus<SimpleSlice>>| -> Option<FullAddr> {
						(*simple_slice_genus).find_word(&Some(seed_simple_slice))
					})
				{
					return Err(PushFamilyErr::HomomorphismExists(*homomorphism.set_genus_index(genus_index)));
				}
			}
		}

		Ok(())
	}

	fn simple_slices_have_identical_organism_indicies(
		simple_slice_a: &[HalfAddr],
		simple_slice_b: &[HalfAddr]
	) -> bool {
		simple_slice_a.iter().enumerate().all(|(simple_slice_index, simple): (usize, &HalfAddr)| -> bool {
			simple.get_organism_index() == simple_slice_b[simple_slice_index].get_organism_index()
		})
	}

	fn initialize(&mut self) -> () {
		self.family_infos		= Vec::<FamilyInfo>::new();
		self.genus_infos		= Vec::<GenusInfo>::with_capacity(Self::GENERA_PER_SMALL_CLASS);
		self.simples			= Vec::<HalfAddr>::with_capacity(Self::ORGANISMS_PER_GENUS);
		self.transformations	= Class::<Transformation>::with_capacity(Self::ORGANISMS_PER_GENUS);
		self.full_masks				= Class::<FullMask>::with_capacity(Self::ORGANISMS_PER_GENUS);
		self.inverse_addrs		= Class::<FullAddr>::with_capacity(Self::ORGANISMS_PER_GENUS);

		let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);

		self.initialize_orientations(icosidodecahedron_data);
		self.initialize_reorientation_genus(icosidodecahedron_data);
		self.initialize_simple_genus(icosidodecahedron_data);
		self.push_order(OrderInfo::from_file_or_default(&STRING_DATA.files.library));
	}

	fn initialize_orientations(&mut self, icosidodecahedron_data: &Data) -> () {
		for (species_index, orientation_species) in self.orientations.iter_mut().enumerate() {
			let face_data: &FaceData = &icosidodecahedron_data.faces[species_index];

			for (organism_index, orientation) in orientation_species.iter_mut().enumerate() {
				*orientation = face_data.get_rotated_quat(organism_index as u32);
			}
		}
	}

	fn initialize_reorientation_genus(&mut self, icosidodecahedron_data: &Data) -> () {
		self.push_genus(GenusInfo::new(
			"Reorientation".into(),
			&[],
			None
		));

		let reorientation_genus_index: usize = GenusIndex::REORIENTATION.into();
		let (
			orientation_genus,
			transformation_genus,
			mask_genus,
			inverse_addr_genus,
			rotation_genus
		): (
			&    [Species<Quat>],
			&mut Genus<Transformation>,
			&mut Genus<FullMask>,
			&mut Genus<FullAddr>,
			&mut Genus<Quat>
		) = (
			&    self.orientations		[0_usize .. Self::SPECIES_PER_GENUS],
			&mut self.transformations	[reorientation_genus_index],
			&mut self.full_masks		[reorientation_genus_index],
			&mut self.inverse_addrs		[reorientation_genus_index],
			&mut self.rotations			[reorientation_genus_index]
		);
		let origin_conj_quat: Quat = icosidodecahedron_data.faces[PENTAGON_INDEX_OFFSET].quat.conjugate();

		for species_index in 0_usize .. Self::SPECIES_PER_GENUS {
			let orientation_species:	&    Species<Quat>				= &    orientation_genus	[species_index];
			let transformation_species:	&mut Species<Transformation>	= &mut transformation_genus	[species_index];
			let mask_species:			&mut Species<FullMask>			= &mut mask_genus			[species_index];
			let rotation_species:		&mut Species<Quat>				= &mut rotation_genus		[species_index];

			for organism_index in 0_usize .. Self::ORGANISMS_PER_SPECIES {
				let orientation:	&    Quat				= &    orientation_species		[organism_index];
				let transformation:	&mut Transformation		= &mut transformation_species	[organism_index];
				let mask:			&mut FullMask			= &mut mask_species				[organism_index];
				let rotation:		&mut Quat				= &mut rotation_species			[organism_index];

				let reorientation_quat: Quat = *orientation * origin_conj_quat;
				let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) =
					transformation.arrays_mut();

				for piece_index in PIECE_RANGE {
					let (pos, rot): (usize, usize) = icosidodecahedron_data.get_pos_and_rot(
						&(reorientation_quat * icosidodecahedron_data.faces[piece_index].quat),
						None /* We could put a filter in here, but it'd be slower, and the quat math
							is precise enough that it's unnecessary here */
					);

					pos_array[piece_index] = pos as PieceStateComponent;
					rot_array[piece_index] = rot as PieceStateComponent;
				}

				*mask = FullMask::from(transformation.arrays());
				*rotation = reorientation_quat;
			}
		}

		for (species_index, inverse_addr_species)
			in inverse_addr_genus.iter_mut().enumerate()
		{
			for (organism_index, inverse_addr) in inverse_addr_species.iter_mut().enumerate() {
				*inverse_addr = (&*transformation_genus)
					.find_word(&(-&transformation_genus[species_index][organism_index]))
					.map(|mut inverse_addr: FullAddr| -> FullAddr {
						*inverse_addr.set_genus_index(GenusIndex::REORIENTATION.into())
					})
					.unwrap_or_default();
			}
		}
	}

	fn initialize_simple_genus(&mut self, icosidodecahedron_data: &Data) -> () {
		self.push_genus(GenusInfo::new(
			"Simple".into(),
			&[HalfAddr::default()],
			None
		));

		let simple_genus_index: usize = GenusIndex::SIMPLE.into();
		let (
			simple_genus,
			transformation_genus,
			mask_genus,
			inverse_addr_genus,
			rotation_genus
		): (
			&mut [HalfAddr],
			&mut Genus<Transformation>,
			&mut Genus<FullMask>,
			&mut Genus<FullAddr>,
			&mut Genus<Quat>
		) = (
			&mut self.simples			[0_usize .. Self::ORGANISMS_PER_GENUS],
			&mut self.transformations	[simple_genus_index],
			&mut self.full_masks		[simple_genus_index],
			&mut self.inverse_addrs		[simple_genus_index],
			&mut self.rotations			[simple_genus_index]
		);

		for (species_index, simple_species)
			in simple_genus.chunks_mut(Self::ORGANISMS_PER_SPECIES).enumerate()
		{
			let transformation_species:	&mut Species<Transformation>	= &mut transformation_genus	[species_index];
			let mask_species:			&mut Species<FullMask>			= &mut mask_genus			[species_index];
			let inverse_addr_species:	&mut Species<FullAddr>			= &mut inverse_addr_genus	[species_index];
			let rotation_species:		&mut Species<Quat>				= &mut rotation_genus		[species_index];
			let face_data:				&FaceData						= &icosidodecahedron_data.faces[species_index];
			let face_mask:				FullMask							= FullMask::from_pentagon_index(species_index);

			for organism_index in 0_usize .. Self::ORGANISMS_PER_SPECIES {
				let transformation:	&mut Transformation		= &mut transformation_species	[organism_index];
				let mask:			&mut FullMask			= &mut mask_species				[organism_index];
				let inverse_addr:	&mut FullAddr			= &mut inverse_addr_species		[organism_index];
				let simple:			&mut HalfAddr			= &mut simple_species			[organism_index];
				let rotation:		&mut Quat				= &mut rotation_species			[organism_index];

				*mask = if organism_index != 0_usize { face_mask } else { FullMask::default() };
				*inverse_addr = FullAddr::from((
					usize::from(GenusIndex::SIMPLE),
					species_index,
					FullAddr::invert_organism_index(organism_index)
				));
				*simple = HalfAddr::new(species_index, organism_index);
				*rotation = face_data.get_rotation_quat(organism_index as u32);

				let mask: FullMask = *mask;
				let rotation: Quat = *rotation;
				let (pos_array, rot_array): (&mut PuzzleStateComponent, &mut PuzzleStateComponent) =
					transformation.arrays_mut();

				for piece_index in PIECE_RANGE {
					let (pos, rot): (usize, usize) = if mask.affects_piece(piece_index) {
						icosidodecahedron_data.get_pos_and_rot(
							&(rotation * icosidodecahedron_data.faces[piece_index].quat),
							None /* We could put a filter in here, but it'd be slower, and the quat
								math is precise enough that it's unnecessary here */
						)
					} else {
						(piece_index, 0_usize)
					};

					pos_array[piece_index] = pos as PieceStateComponent;
					rot_array[piece_index] = rot as PieceStateComponent;
				}
			}
		}
	}

	fn initialize_simple_slice(
		simple_slice:		&mut [HalfAddr],
		seed_simple_slice:	&[HalfAddr],
		mirror:				bool,
		invert:				bool,
		reorientation:		HalfAddr
	) -> () {
		if simple_slice.len() == seed_simple_slice.len() {
			let get_species_index = if mirror {
				|seed_simple: HalfAddr| -> usize { FullAddr::mirror_species_index(seed_simple.get_species_index()) }
			} else {
				|seed_simple: HalfAddr| -> usize { seed_simple.get_species_index() }
			};
			let get_organism_index = if mirror ^ invert {
				|seed_simple: HalfAddr| -> usize { FullAddr::invert_organism_index(seed_simple.get_organism_index()) }
			} else {
				|seed_simple: HalfAddr| -> usize { seed_simple.get_organism_index() }
			};
			let mut seed_simple_slice_iter =
				|seed_simple_slice_iter: &mut dyn DoubleEndedIterator<Item = &HalfAddr>| -> () {
					for (simple_slice_index, seed_simple) in seed_simple_slice_iter.enumerate() {
						simple_slice[simple_slice_index] = *(FullAddr::from((
							usize::from(GenusIndex::SIMPLE),
							get_species_index(*seed_simple),
							get_organism_index(*seed_simple)
						)) + reorientation).get_half_addr()
					}
				};

			if invert {
				seed_simple_slice_iter(&mut seed_simple_slice.iter().rev());
			} else {
				seed_simple_slice_iter(&mut seed_simple_slice.iter());
			}
		}
	}

	fn initialize_simple_slice_genus(
		&mut self,
		genus_index:		GenusIndex,
		seed_simple_slice:	&[HalfAddr],
		mirror:				bool,
		invert:				bool
	) -> () {
		let simples_range: Range<usize> = {
			if let Some(simples_range) = self.get_simples_range(genus_index) {
				simples_range
			} else {
				return;
			}
		};

		if simples_range.end - simples_range.start != Self::ORGANISMS_PER_GENUS * seed_simple_slice.len() {
			return;
		}

		for (species_index, simple_slice_species) in &mut self
			.simples
			[simples_range]
			.chunks_mut(Self::ORGANISMS_PER_SPECIES * seed_simple_slice.len())
			.enumerate()
		{
			for (organism_index, simple_slice) in simple_slice_species
				.chunks_mut(seed_simple_slice.len())
				.enumerate()
			{
				Self::initialize_simple_slice(
					simple_slice,
					seed_simple_slice,
					mirror,
					invert,
					HalfAddr::new(species_index, organism_index)
				)
			}
		}
	}

	fn push_genus(&mut self, mut genus_info: GenusInfo) -> () {
		let simple_offset:		usize = self.simples.len();
		let simple_slice_len:	usize = genus_info.simple_slice_len as usize;

		genus_info.simple_offset = simple_offset as SimpleOffsetType;
		self.genus_infos.push(genus_info);
		self.simples.resize(
			simple_offset + Self::ORGANISMS_PER_GENUS * simple_slice_len,
			HalfAddr::default()
		);
		self.transformations.push(Genus::<Transformation>::default());
		self.full_masks.push(Genus::<FullMask>::default());
		self.inverse_addrs.push(Genus::<FullAddr>::default());
	}

	fn push_family(&mut self, mut family_input: FamilyInput) -> Result<FamilyIndexType, PushFamilyErr> {
		let seed_simples: Vec<HalfAddr> = take(&mut family_input.seed_simples)
			.iter()
			.map(|(species_index, organism_index): &(u8, u8)| -> HalfAddr {
				HalfAddr::new(*species_index as usize, *organism_index as usize)
			})
			.collect();
		let seed_simple_slice: &[HalfAddr] = &*seed_simples;

		self.is_seed_simple_slice_valid(seed_simple_slice)?;

		let simple_slice_len:		usize			= seed_simple_slice.len();
		let family_index:			FamilyIndexType	= self.family_infos.len() as FamilyIndexType;
		let mut family_info:		FamilyInfo		= FamilyInfo {
			name:				take(&mut family_input.name),
			base_genus:			GenusIndex::from_usize(self.genus_infos.len()),
			inverse_is_mirror:	false // To be determined
		};

		self.push_genus(GenusInfo::new(
			family_info.name.clone(),
			seed_simple_slice,
			Some(family_index)
		));
		self.initialize_simple_slice_genus(
			family_info.base_genus,
			seed_simple_slice,
			false,
			false
		);
		self.push_genus(GenusInfo::new(
			format!("{}'", family_info.name),
			seed_simple_slice,
			Some(family_index)
		));
		self.initialize_simple_slice_genus(
			GenusIndex(family_info.base_genus.0 + 1 as GenusIndexType),
			seed_simple_slice,
			true,
			false
		);

		family_info.inverse_is_mirror = {
			let mut temp_inverse_simples: Vec<HalfAddr> = vec![HalfAddr::default(); simple_slice_len];

			Self::initialize_simple_slice(
				&mut *temp_inverse_simples,
				seed_simple_slice,
				false,
				true,
				HalfAddr::ORIGIN
			);

			let mirror_genus_index: GenusIndex = GenusIndex(family_info.base_genus.0 + 1 as GenusIndexType);
			let mirror_simple_slice: &[HalfAddr] =
				get_simple_slice(&*self, FullAddr::from((mirror_genus_index, HalfAddr::ORIGIN)));

			Self::simple_slices_have_identical_organism_indicies(
				&*temp_inverse_simples,
				mirror_simple_slice
			) && self
				.get_simple_slice_genus(mirror_genus_index)
				.and_then(|simple_slice_genus: Box<Genus<SimpleSlice>>| -> Option<FullAddr> {
					(*simple_slice_genus).find_word(&Some(&*temp_inverse_simples))
				})
				.is_some()
		};

		if !family_info.inverse_is_mirror {
			self.push_genus(GenusInfo::new(
				format!("{}\"", family_info.name),
				seed_simple_slice,
				Some(family_index)
			));
			self.initialize_simple_slice_genus(
				GenusIndex(family_info.base_genus.0 + 2 as GenusIndexType),
				seed_simple_slice,
				false,
				true
			);
			self.push_genus(GenusInfo::new(
				format!("{}\"'", family_info.name),
				seed_simple_slice,
				Some(family_index)
			));
			self.initialize_simple_slice_genus(
				GenusIndex(family_info.base_genus.0 + 3 as GenusIndexType),
				seed_simple_slice,
				true,
				true
			);
		}

		for genus_index_usize in family_info.get_genus_range() {
			let simple_slice_len:		usize		= self.genus_infos[genus_index_usize].simple_slice_len as usize;
			let genus_index:			GenusIndex	= GenusIndex::from_usize(genus_index_usize);
			let inverse_genus_index:	usize		= family_info.invert_genus_index(genus_index).into();
			let (
				simples_genus,
				transformation_genus,
				mask_genus,
				inverse_addr_genus
			): (
				&    [HalfAddr],
				&mut Genus<Transformation>,
				&mut Genus<FullMask>,
				&mut Genus<FullAddr>,
			) = (
				&    self.simples			[self.get_simples_range(GenusIndex::from_usize(genus_index_usize)).unwrap()],
				&mut self.transformations	[genus_index_usize],
				&mut self.full_masks		[genus_index_usize],
				&mut self.inverse_addrs		[genus_index_usize],
			);

			for (species_index, simples_species) in simples_genus
				.chunks(Self::ORGANISMS_PER_SPECIES * simple_slice_len)
				.enumerate()
			{
				let transformation_species:	&mut Species<Transformation>	= &mut transformation_genus	[species_index];
				let mask_species:			&mut Species<FullMask>			= &mut mask_genus			[species_index];
				let inverse_addr_species:	&mut Species<FullAddr>			= &mut inverse_addr_genus	[species_index];

				for (organism_index, simple_slice) in simples_species
					.chunks(simple_slice_len)
					.enumerate()
				{
					let transformation:	&mut Transformation	= &mut transformation_species	[organism_index];
					let mask:			&mut FullMask		= &mut mask_species				[organism_index];
					let inverse_addr:	&mut FullAddr		= &mut inverse_addr_species		[organism_index];

					let mut puzzle_state: PuzzleState = PuzzleState::SOLVED_STATE;

					for simple in simple_slice {
						puzzle_state += simple.as_simple();
					}

					*transformation	= Transformation(puzzle_state);
					*mask			= FullMask::from(transformation.arrays());
					*inverse_addr	= FullAddr::from((inverse_genus_index, species_index, organism_index));
				}
			}
		}

		self.family_infos.push(family_info);

		Ok(family_index)
	}

	fn push_order(&mut self, order_info: OrderInfo) -> () {
		for family_input in order_info.0 {
			if let Err(push_family_err) = self.push_family(family_input) {
				log::warn!("PushFamilyErr: {:?}", push_family_err);
			}
		}
	}
}

fn get_simple_slice(library: &Library, full_addr: FullAddr) -> &[HalfAddr] {
	let genus_info: &GenusInfo = &library.genus_infos[full_addr.get_genus_index()];
	let simple_slice_len: usize = genus_info.simple_slice_len as usize;
	let simple_slice_start: usize = genus_info.simple_offset as usize
		+ simple_slice_len
		* (Library::ORGANISMS_PER_SPECIES * full_addr.get_species_index() + full_addr.get_organism_index());

	&library.simples[simple_slice_start .. simple_slice_start + simple_slice_len]
}

impl LibraryConsts for Library {
	const ORGANISMS_PER_SPECIES:	usize = PENTAGON_SIDE_COUNT;
	const SPECIES_PER_GENUS:		usize = PENTAGON_PIECE_COUNT;
	const ORGANISMS_PER_GENUS:		usize = Self::ORGANISMS_PER_SPECIES * Self::SPECIES_PER_GENUS;
	const SPECIES_PER_LARGE_GENUS:	usize = PIECE_COUNT;
	const GENERA_PER_SMALL_CLASS:	usize = GenusIndex::COMPLEX_OFFSET;
}

/* Library needs a more complicated setup because Library::initialize() relies on Library::get(), so a lazy_static won't
suffice */
impl StaticDataLibrary for Library {
	fn pre_init() -> Option<Box<dyn FnOnce() -> ()>> { Some(Box::new(|| -> () { Data::initialize(); }))}

	fn init() -> Option<Box<dyn FnOnce() -> ()>> { Some(Box::new(|| -> () {
		unsafe {
			LIBRARY = MaybeUninit::<Library>::new(Library::default());

			LIBRARY.assume_init_mut().initialize();
		}
	}))}

	fn get() -> &'static Self { unsafe { LIBRARY.assume_init_ref() } }

	fn get_once() -> Option<&'static Once> { Some(&LIBRARY_ONCE) }
}

static mut LIBRARY: MaybeUninit<Library> = MaybeUninit::<Library>::uninit();
static LIBRARY_ONCE: Once = Once::new();

#[cfg(test)]
mod tests {
	use {
		super::*,
		crate::util::StaticDataLibrary
	};

	fn test_validity() -> () {
		let library: &Library = Library::get();

		for genus_index in 0_usize .. library.genus_infos.len() {
			let transformation_genus:	&Genus<Transformation>	= &library.transformations	[genus_index];
			let inverse_addr_genus:		&Genus<FullAddr>		= &library.inverse_addrs	[genus_index];

			for species_index in 0_usize .. Library::SPECIES_PER_GENUS {
				let transformation_species:	&Species<Transformation>	= &transformation_genus		[species_index];
				let inverse_addr_species:	&Species<FullAddr>			= &inverse_addr_genus		[species_index];

				for organism_index in 0_usize .. Library::ORGANISMS_PER_SPECIES {
					let transformation:	&Transformation	= &transformation_species	[organism_index];
					let inverse_addr:	&FullAddr		= &inverse_addr_species		[organism_index];

					if !transformation.is_valid() {
						log::error!(
							"Transformation ({}, {}, {}) is invalid",
							genus_index, species_index, organism_index
						);
						error_expr!(transformation);

						panic!();
					}

					if !inverse_addr.is_valid() {
						log::error!(
							"Inverse address for transformation ({}, {}, {}) is invalid",
							genus_index, species_index, organism_index
						);
						error_expr!(inverse_addr);

						panic!();
					}

					let inverse_transformation: &Transformation = library.transformations.get_word(*inverse_addr);

					if *transformation != -inverse_transformation || -transformation != *inverse_transformation {
						log::error!(
							"Transformation addressed by ({}, {}, {}), the associated address of transformation \
								({}, {}, {}) is not the true inverse transformation",
								inverse_addr.get_genus_index(),
								inverse_addr.get_species_index(),
								inverse_addr.get_organism_index(),
								genus_index,
								species_index,
								organism_index
						);
						error_expr!(transformation, -transformation, inverse_transformation, -inverse_transformation);

						panic!();
					}

					let solved_state: PuzzleState = PuzzleState::SOLVED_STATE;
					let middle_state: PuzzleState = &solved_state + transformation;
					let end_state: PuzzleState = &middle_state + inverse_transformation;

					if end_state != solved_state {
						log::error!(
							"solved_state + transformation + inverse_transformation != solved_state for transformation \
								({}, {}, {})",
							genus_index, species_index, organism_index
						);
						error_expr!(solved_state, transformation, middle_state, inverse_transformation, end_state);

						panic!();
					}
				}
			}
		}
	}

	fn test_reorientations() -> () {
		let library: &Library = Library::get();
		let reorientation_genus_index: usize = GenusIndex::REORIENTATION.into();
		let simple_transformation_genus: &Genus<Transformation> = &library.transformations[usize::from(GenusIndex::SIMPLE)];
		let reorientation_tests: [Vec<(usize, usize)>; PENTAGON_PIECE_COUNT] =
			<[Vec<(usize, usize)>; PENTAGON_PIECE_COUNT]>::from_file(
				STRING_DATA.tests.reorientation_tests.as_ref()
			).to_option().unwrap();

		for species_index in 0_usize .. Library::SPECIES_PER_GENUS {
			let transformation_species:	&Species<Transformation>	= &library.transformations[reorientation_genus_index][species_index];
			let inverse_addr_species:	&Species<FullAddr>			= &library.inverse_addrs[reorientation_genus_index][species_index];
			let pent_puzzle_state: PuzzleState = {
				let mut solved_state: PuzzleState = PuzzleState::SOLVED_STATE;

				for (simple_pent_index, simple_rotation_index)
					in &reorientation_tests[species_index]
				{
					solved_state += &simple_transformation_genus
						[*simple_pent_index]
						[*simple_rotation_index];
				}

				solved_state
			};

			if !pent_puzzle_state.is_valid() {
				log::error!("Puzzle state for pentagon {} isn't valid", species_index);
				error_expr!(pent_puzzle_state);

				panic!();
			}

			for organism_index in 0_usize .. Library::ORGANISMS_PER_SPECIES {
				let transformation:	&Transformation	= &transformation_species	[organism_index];
				let inverse_addr:	&FullAddr		= &inverse_addr_species		[organism_index];

				let prev_puzzle_state: PuzzleState	= {
					let mut pent_puzzle_state_clone: PuzzleState = pent_puzzle_state.clone();

					pent_puzzle_state_clone += &simple_transformation_genus[species_index][organism_index];

					pent_puzzle_state_clone
				};

				if !prev_puzzle_state.is_valid() {
					log::error!(
						"Puzzle state isn't valid before reorientation with pentagon {} and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(pent_puzzle_state, simple_transformation_genus[species_index][organism_index], prev_puzzle_state);

					panic!();
				}

				if prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET] != species_index as PieceStateComponent
					|| prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET] != organism_index as PieceStateComponent
				{
					log::error!(
						"Reorientation with pentagon {} and rotation {} has position {} and rotation {} for piece {}",
						species_index,
						organism_index,
						prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET],
						prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET],
						PENTAGON_INDEX_OFFSET
					);
					error_expr!(prev_puzzle_state);

					panic!();
				}

				let standardization_full_addr:			FullAddr	= prev_puzzle_state.standardization_full_addr();

				let mut curr_puzzle_state:				PuzzleState	= prev_puzzle_state.clone();
				let mut curr_puzzle_state_alt:			PuzzleState	= prev_puzzle_state.clone();
				let mut reoriented_solved_state:		PuzzleState = PuzzleState::SOLVED_STATE;
				let mut reoriented_solved_state_alt:	PuzzleState = PuzzleState::SOLVED_STATE;

				reoriented_solved_state += transformation;

				if reoriented_solved_state.standardization_full_addr() != standardization_full_addr {
					log::error!(
						"Reoriented solved state's standardization address doesn't match what's expected after \
							reorientation with pentagon {} and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(transformation, reoriented_solved_state, prev_puzzle_state);

					panic!();
				}

				if !reoriented_solved_state.is_valid() {
					log::error!(
						"Puzzle state isn't valid after reorientation with pentagon {} and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(prev_puzzle_state, transformation, curr_puzzle_state);

					panic!();
				}

				reoriented_solved_state_alt.naive_add_assign(transformation);

				if reoriented_solved_state_alt != reoriented_solved_state {
					log::error!(
						"Reoriented puzzle state doesn't match the naively reoriented puzzle state with pentagon {} \
							and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(transformation, reoriented_solved_state, reoriented_solved_state_alt);

					panic!();
				}

				if standardization_full_addr != *inverse_addr {
					log::error!(
						"Standardization address for current state doesn't match inverse address with pentagon {} \
							and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(prev_puzzle_state, standardization_full_addr, inverse_addr);

					panic!();
				}

				// Reorientation was originally in the other direction, and the standardization check only works for the
				// other direction, so just run the rest how the test was originally conducted

				let word_pack_transformation: &Transformation = library.transformations.get_word(standardization_full_addr);
				let word_pack_mask: &FullMask = library.full_masks.get_word(standardization_full_addr);

				curr_puzzle_state += word_pack_transformation;

				if !curr_puzzle_state.is_standardized() {
					log::error!(
						"Puzzle state isn't standardized after reorientation with pentagon {} and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(prev_puzzle_state, word_pack_transformation, curr_puzzle_state);

					panic!();
				}

				if !curr_puzzle_state.is_valid() {
					log::error!(
						"Puzzle state isn't valid after reorientation with pentagon {} and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(prev_puzzle_state, word_pack_transformation, curr_puzzle_state);

					panic!();
				}

				curr_puzzle_state_alt.naive_add_assign(word_pack_transformation);

				if curr_puzzle_state_alt != curr_puzzle_state {
					log::error!(
						"Reoriented puzzle state doesn't match the naively reoriented puzzle state with pentagon {} \
							and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(prev_puzzle_state, word_pack_transformation, curr_puzzle_state, curr_puzzle_state_alt);

					panic!();
				}

				let transformation_from_states:	Transformation = &curr_puzzle_state - &prev_puzzle_state;

				if transformation_from_states != *word_pack_transformation {
					log::error!(
						"Transformation from previous to current state doesn't match applied transformation with pentagon {} and \
							rotation {}",
						species_index,
						organism_index
					);
					error_expr!(word_pack_transformation, prev_puzzle_state, curr_puzzle_state, transformation_from_states);

					panic!();
				}

				for piece_index in PIECE_RANGE {
					if word_pack_mask.affects_piece(piece_index) != (
						curr_puzzle_state.pos[piece_index] != prev_puzzle_state.pos[piece_index] ||
						curr_puzzle_state.rot[piece_index] != prev_puzzle_state.rot[piece_index]
					) {
						log::error!(
							"The mask's affects_piece() result doesn't match reality for piece {} with pentagon {} and \
								rotation {}",
							piece_index,
							species_index,
							organism_index
						);
						error_expr!(word_pack_mask.affects_piece(piece_index), prev_puzzle_state, curr_puzzle_state);

						panic!();
					}
				}
			}
		}
	}

	fn test_simples() -> () {
		let library:				&Library				= Library::get();
		let simple_genus_index:		usize					= GenusIndex::SIMPLE.into();
		let transformation_genus:	&Genus<Transformation>	= &library.transformations[simple_genus_index];
		let inverse_addr_genus:		&Genus<FullAddr>		= &library.inverse_addrs[simple_genus_index];

		for species_index in 0_usize .. Library::SPECIES_PER_GENUS {
			let transformation_species:	&Species<Transformation>	= &transformation_genus		[species_index];
			let inverse_addr_species:	&Species<FullAddr>			= &inverse_addr_genus		[species_index];

			for organism_index in 0_usize .. Library::ORGANISMS_PER_SPECIES {
				let transformation:				&Transformation		= &transformation_species	[organism_index];
				let inverse_addr:				&FullAddr			= &inverse_addr_species		[organism_index];
				let inverse_transformation:		&Transformation		= transformation_genus.get_word(*inverse_addr);
				let mut curr_puzzle_state:		PuzzleState			= PuzzleState::SOLVED_STATE;
				let mut curr_puzzle_state_alt:	PuzzleState			= PuzzleState::SOLVED_STATE;

				for turn in 1_usize ..= PENTAGON_SIDE_COUNT {
					let prev_puzzle_state:		PuzzleState = curr_puzzle_state.clone();

					curr_puzzle_state += transformation;
					curr_puzzle_state_alt.naive_add_assign(transformation);

					if curr_puzzle_state_alt != curr_puzzle_state {
						log::error!(
							"curr_puzzle_state_alt != curr_puzzle_state after turn {} with pentagon {} and rotation {}",
							turn,
							species_index,
							organism_index
						);
						error_expr!(prev_puzzle_state, transformation, curr_puzzle_state, curr_puzzle_state_alt);

						panic!();
					}

					if !curr_puzzle_state.is_valid() {
						log::error!(
							"Puzzle state isn't valid after turn {} with pentagon {} and rotation {}",
							turn,
							species_index,
							organism_index
						);
						error_expr!(prev_puzzle_state, transformation, curr_puzzle_state);

						panic!();
					}

					let mut prev_puzzle_state_from_inverse_transformation = curr_puzzle_state.clone();

					prev_puzzle_state_from_inverse_transformation += inverse_transformation;

					if prev_puzzle_state_from_inverse_transformation != prev_puzzle_state {
						log::error!(
							"prev_puzzle_state_from_inverse_transformation != prev_puzzle_state after turn {} with \
								pentagon {} and rotation {}",
							turn,
							species_index,
							organism_index
						);
						error_expr!(
							curr_puzzle_state,
							inverse_transformation,
							prev_puzzle_state_from_inverse_transformation,
							prev_puzzle_state
						);

						panic!();
					}
				}

				if curr_puzzle_state != PuzzleState::SOLVED_STATE {
					log::error!(
						"curr_puzzle_state != curr_puzzle_state with pentagon {} and rotation {}",
						species_index,
						organism_index
					);
					error_expr!(curr_puzzle_state);

					panic!();
				}
			}
		}
	}

	#[test]
	fn test_transformation_library() -> () {
		init_env_logger();
		<Library as StaticDataLibrary>::build();
		test_validity();

		/* Though Type::Reorientation is listed before Simple (intentionally: it doesn't actually change the
		(standardized) state of the puzzle), Type::Simple needs to be tested first, since test_reorientations() is
		dependent on test_simples() */
		test_simples();
		test_reorientations();
	}
}