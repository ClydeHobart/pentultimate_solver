use {
    super::{Addr, FindWord, FullAddr, FullMask, GetWord, HalfAddr, Transformation},
    crate::{
        math::polyhedra::{
            data::{Data, FaceData},
            Polyhedron,
        },
        piece::consts::*,
        prelude::*,
        puzzle::inflated::{PieceStateComponent, PuzzleState, PuzzleStateComponent},
        util::{
            inspectable_bit_array::{InspectableBitSlice, InspectableBitSliceAttrs},
            StaticDataLibrary,
        },
    },
    bevy::prelude::Quat,
    bevy_inspector_egui::{Context, Inspectable},
    bitvec::prelude::*,
    egui::Ui,
    itertools::izip,
    memoffset::raw_field,
    serde::{Deserialize, Deserializer, Serialize, Serializer},
    simple_error::SimpleError,
    std::{
        convert::{TryFrom, TryInto},
        error::Error,
        fmt::{Debug, Formatter},
        iter::{DoubleEndedIterator, IntoIterator},
        marker::PhantomData,
        mem::{take, transmute, MaybeUninit},
        ops::{Deref, DerefMut, Range},
        ptr::{write, write_bytes},
        rc::Rc,
        sync::{
            atomic::{AtomicBool, Ordering},
            Mutex, MutexGuard, Once, RwLock, RwLockReadGuard, RwLockWriteGuard,
        },
    },
};

pub mod tools;

pub type GenusIndexType = u8;

#[derive(Clone, Copy, Deserialize, Eq, PartialEq, Serialize)]
pub struct GenusIndex(GenusIndexType);

impl GenusIndex {
    pub const INVALID: GenusIndex = GenusIndex(GenusIndexType::MAX);
    pub const REORIENTATION: GenusIndex = GenusIndex(0 as GenusIndexType);
    pub const SIMPLE: GenusIndex = GenusIndex(Self::REORIENTATION.0 + 1 as GenusIndexType);

    #[inline(always)]
    pub fn is_complex(self) -> bool {
        self.is_valid() && self.0 >= Self::COMPLEX_OFFSET as u8
    }

    #[inline(always)]
    pub fn is_reorientation(self) -> bool {
        self.0 == Self::REORIENTATION.0
    }

    #[inline(always)]
    pub fn is_simple(self) -> bool {
        self.0 == Self::SIMPLE.0
    }

    #[inline(always)]
    pub fn is_valid(self) -> bool {
        self.0 as usize <= Library::get_genus_count()
    }

    pub fn invert(self) -> Self {
        if self.is_valid() {
            Library::get()
                .get_family_info_from_genus_index(self)
                .invert_genus_index(self)
        } else {
            GenusIndex::INVALID
        }
    }

    pub fn mirror(self) -> Self {
        if self.is_valid() {
            Library::get()
                .get_family_info_from_genus_index(self)
                .mirror_genus_index(self)
        } else {
            GenusIndex::INVALID
        }
    }

    const COMPLEX_OFFSET: usize = Self::SIMPLE.0 as usize + 1_usize;

    #[inline(always)]
    fn from_usize(genus_index: usize) -> Self {
        Self(genus_index as GenusIndexType)
    }
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

impl Default for GenusIndex {
    fn default() -> Self {
        Self::INVALID
    }
}

impl Deref for GenusIndex {
    type Target = GenusIndexType;

    fn deref(&self) -> &GenusIndexType {
        &self.0
    }
}

impl DerefMut for GenusIndex {
    fn deref_mut(&mut self) -> &mut GenusIndexType {
        &mut self.0
    }
}

impl From<GenusIndex> for GenusIndexType {
    fn from(genus_index: GenusIndex) -> Self {
        genus_index.0
    }
}
impl From<GenusIndex> for usize {
    fn from(genus_index: GenusIndex) -> Self {
        genus_index.0 as Self
    }
}

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

#[derive(Clone, PartialEq)]
pub struct GenusIndexBitArray<T: BitStore = u32>(BitVec<T>);

impl<T: BitStore> GenusIndexBitArray<T> {
    pub fn all() -> Self {
        let mut genus_index_bit_array: Self = Self::default();

        genus_index_bit_array.0.fill(true);

        genus_index_bit_array
    }

    pub fn none() -> Self {
        let mut genus_index_bit_array: Self = Self(BitVec::<T>::default());

        genus_index_bit_array.refresh();

        genus_index_bit_array
    }

    #[inline(always)]
    pub fn get_bit<G: Into<usize>>(&self, genus_index: G) -> bool {
        self.0[genus_index.into()]
    }

    #[inline(always)]
    pub fn set_bit<G: Into<usize> + Sized>(&mut self, genus_index: G, enabled: bool) {
        self.0.set(genus_index.into(), enabled);
    }

    pub fn iter(&self) -> GenusIndexBitArrayIter<T> {
        GenusIndexBitArrayIter::<T> {
            genus_index_bit_array: self,
            current_index: 0_usize,
        }
    }

    pub fn refresh(&mut self) {
        self.0.resize(Library::get_genus_count(), false);
    }
}

impl<T: BitStore> Debug for GenusIndexBitArray<T> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.debug_list().entries(self.iter()).finish()
    }
}

impl<T: BitStore> Default for GenusIndexBitArray<T> {
    fn default() -> Self {
        Self::none()
    }
}

impl<T: BitStore, G: Copy + TryInto<GenusIndex>> From<&[G]> for GenusIndexBitArray<T> {
    fn from(genus_indices: &[G]) -> Self {
        let mut genus_index_bit_array: Self = Self::none();

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

impl<'de, T: BitStore> Deserialize<'de> for GenusIndexBitArray<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(Vec::<GenusIndexString>::deserialize(deserializer)?
            .into_iter()
            .map(|genus_index_string: GenusIndexString| -> GenusIndex { genus_index_string.0 })
            .collect::<Vec<GenusIndex>>()
            .as_slice()
            .into())
    }
}

impl<T: BitStore> Serialize for GenusIndexBitArray<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.iter()
            .map(|genus_index: GenusIndex| -> String { format!("{:?}", genus_index) })
            .collect::<Vec<String>>()
            .serialize(serializer)
    }
}

impl<T: BitStore> Inspectable for GenusIndexBitArray<T> {
    type Attributes = ();

    fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
        self.refresh();

        let mut inspectable_bit_array_wrapper: InspectableBitSlice<T, Lsb0> =
            InspectableBitSlice::<T, Lsb0>(&mut self.0);

        inspectable_bit_array_wrapper.ui(
            ui,
            InspectableBitSliceAttrs {
                length: None,
                fetch_label: Some(Rc::new(|genus_index: usize| -> String {
                    if let Ok(genus_index) = if let Ok(bit_index_genus_index_type) =
                        GenusIndexType::try_from(genus_index)
                    {
                        GenusIndex::try_from(bit_index_genus_index_type)
                    } else {
                        Err(())
                    } {
                        format!("{:?}", genus_index)
                    } else {
                        "[OUT OF RANGE]".into()
                    }
                })),
            },
            context,
        )
    }
}

pub struct GenusIndexBitArrayIter<'a, T: BitStore> {
    genus_index_bit_array: &'a GenusIndexBitArray<T>,
    current_index: usize,
}

impl<'a, T: BitStore> Iterator for GenusIndexBitArrayIter<'a, T> {
    type Item = GenusIndex;

    fn next(&mut self) -> Option<GenusIndex> {
        self.current_index += self.genus_index_bit_array.0[self.current_index..].leading_zeros();

        if self.current_index < self.genus_index_bit_array.0.len() {
            let next: Option<GenusIndex> = Some(GenusIndex::from_usize(self.current_index));

            self.current_index += 1_usize;

            next
        } else {
            None
        }
    }
}

pub struct GenusIndexString(pub GenusIndex);

impl<'de> Deserialize<'de> for GenusIndexString {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let genus_index_string: String = String::deserialize(deserializer)?;

        for (genus_index, genus_info) in Library::get().genus_infos.iter().enumerate() {
            if genus_info.name == genus_index_string {
                return Ok(GenusIndexString(GenusIndex(genus_index as u8)));
            }
        }

        Err(<D::Error as serde::de::Error>::custom(format!(
            "could not find genus index matching string \"{}\"",
            genus_index_string
        )))
    }
}

impl Serialize for GenusIndexString {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if self.0.is_valid() {
            Library::get().genus_infos[usize::from(self.0)]
                .name
                .serialize(serializer)
        } else {
            Err(<S::Error as serde::ser::Error>::custom(
                "genus index being serialized is invalid",
            ))
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct FamilyInput {
    pub name: String,
    pub seed_simples: Vec<(u8, u8)>,
}

#[derive(Default, Deserialize, Serialize)]
pub struct OrderInput(Vec<FamilyInput>);

#[derive(Debug)]
struct FamilyInfo {
    name: String,
    base_genus: GenusIndex,
    inverse_is_mirror: bool,
}

impl FamilyInfo {
    fn new(name: String, base_genus: GenusIndex, inverse_is_mirror: bool) -> Self {
        Self {
            name,
            base_genus,
            inverse_is_mirror,
        }
    }

    #[inline(always)]
    fn get_genus_count(&self) -> usize {
        if !self.base_genus.is_complex() {
            1_usize
        } else if self.inverse_is_mirror {
            2_usize
        } else {
            4_usize
        }
    }

    fn get_genus_range(&self) -> Range<usize> {
        let genus_index_start: usize = self.base_genus.into();

        genus_index_start..genus_index_start + self.get_genus_count()
    }

    fn invert_genus_index(&self, genus_index: GenusIndex) -> GenusIndex {
        let genus_range: Range<usize> = self.get_genus_range();

        if genus_range.contains(&usize::from(genus_index)) && self.base_genus.is_complex() {
            GenusIndex(
                ((genus_index.0 - genus_range.start as GenusIndexType)
                    ^ if self.inverse_is_mirror {
                        1 as GenusIndexType
                    } else {
                        2 as GenusIndexType
                    })
                    + genus_range.start as GenusIndexType,
            )
        } else {
            genus_index
        }
    }

    fn mirror_genus_index(&self, genus_index: GenusIndex) -> GenusIndex {
        let genus_range: Range<usize> = self.get_genus_range();

        if genus_range.contains(&usize::from(genus_index)) && self.base_genus.is_complex() {
            GenusIndex(
                ((genus_index.0 - genus_range.start as GenusIndexType) ^ 1 as GenusIndexType)
                    + genus_range.start as GenusIndexType,
            )
        } else {
            genus_index
        }
    }
}

#[derive(Clone, Debug)]
pub struct GenusRange(Range<GenusIndexType>);

impl Default for GenusRange {
    fn default() -> Self {
        GenusRange(GenusIndex::INVALID.0..GenusIndex::INVALID.0)
    }
}

impl TryFrom<&str> for GenusRange {
    type Error = ();

    fn try_from(base_genus_index_str: &str) -> Result<Self, ()> {
        if let Ok(base_genus_index) = GenusIndex::try_from(base_genus_index_str) {
            let genus_range: Range<usize> = Library::get()
                .get_family_info_from_genus_index(base_genus_index)
                .get_genus_range();

            Ok(Self(
                genus_range.start as GenusIndexType..genus_range.end as GenusIndexType,
            ))
        } else {
            Err(())
        }
    }
}

impl From<GenusRange> for GenusIndexBitArray {
    fn from(genus_range: GenusRange) -> Self {
        let genus_count: usize = Library::get_genus_count();

        let mut genus_index_bit_array: Self = Self::none();

        genus_index_bit_array.0[(genus_range.0.start as usize).min(genus_count)
            ..(genus_range.0.end as usize).min(genus_count)]
            .fill(true);

        genus_index_bit_array
    }
}

type SimpleOffset = u32;
type SimpleSliceLen = u16;
type FamilyIndex = u8;

#[derive(Debug, Clone)]
struct GenusInfo {
    name: String,
    simple_offset: SimpleOffset,
    simple_slice_len: SimpleSliceLen,
    family_index: FamilyIndex,
}

impl GenusInfo {
    fn new(name: String, simple_slice: &[HalfAddr], family_index: FamilyIndex) -> Self {
        Self {
            name,
            simple_offset: 0 as SimpleOffset,
            simple_slice_len: simple_slice.len() as SimpleSliceLen,
            family_index,
        }
    }
}

pub type Organism<T> = T;
pub type Species<T> = [Organism<T>; Library::ORGANISMS_PER_SPECIES];
pub type Genus<T> = [Species<T>; Library::SPECIES_PER_GENUS];
pub type LargeGenus<T> = [Species<T>; Library::SPECIES_PER_LARGE_GENUS];
/* Family is reserved for grouping complex genera together; order is reserved for grouping families
together */
pub type Class<T> = Vec<Genus<T>>;
pub type SmallClass<T> = [Genus<T>; Library::GENERA_PER_SMALL_CLASS];

#[derive(Debug, Default)]
pub struct Library {
    family_infos: Vec<FamilyInfo>,
    genus_infos: Vec<GenusInfo>,
    simples: Vec<HalfAddr>,
    transformations: Class<Transformation>,
    full_masks: Class<FullMask>,
    inverse_addrs: Class<FullAddr>,
    rotations: SmallClass<Quat>,
    orientations: LargeGenus<Quat>,
}

#[derive(Debug)]
pub enum InitializeFamilyErr {
    InvalidSeedSimpleSlice,
    FamilyIndexTooLarge,
    GenusIndexTooLarge,
    SimpleOffsetTooLarge,
    SeedSimpleSliceTooShort,
    SeedSimpleSliceTooLong,
    DoesNotTransformPuzzleState,
    HomomorphismExists(FullAddr),
}

pub type SimpleSlice<'a> = Option<&'a [HalfAddr]>;

enum _InitializationInstruction {
    CheckOutput,
    InitializeOrientations,
    InitializeReorientationGenus,
    InitializeSimpleGenus,
    InitializeOrder(OrderInput),
    InitializeFamily(FamilyInput),
    PushGenus(GenusInfo),
}

pub struct LibraryRef(RwLockReadGuard<'static, Library>);

impl Deref for LibraryRef {
    type Target = Library;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

pub enum LibraryOrganismType {
    SimpleSlice,
    Transformation,
    FullMask,
    InverseAddr,
    Rotation,
    Orientation,
}

#[derive(Debug)]
pub enum LibraryOrganismTryGetErr {
    InvalidLibraryOrganismType,
    InvalidAddr,
}

pub trait LibraryOrganism {
    fn try_get(lo_ref: &LibraryOrganismRef<Self>) -> Result<&Self, LibraryOrganismTryGetErr>;
}

impl LibraryOrganism for [HalfAddr] {
    fn try_get(lo_ref: &LibraryOrganismRef<Self>) -> Result<&Self, LibraryOrganismTryGetErr> {
        match lo_ref.lo_type {
            LibraryOrganismType::SimpleSlice => {
                let full_addr: FullAddr = lo_ref.addr;

                if full_addr.is_valid() {
                    let genus_info: &GenusInfo =
                        &lo_ref.library_ref.genus_infos[full_addr.get_genus_index()];
                    let simple_slice_len: usize = genus_info.simple_slice_len as usize;
                    let simple_slice_start: usize = genus_info.simple_offset as usize
                        + simple_slice_len
                            * (Library::ORGANISMS_PER_SPECIES * full_addr.get_species_index()
                                + full_addr.get_organism_index());

                    Ok(&lo_ref.library_ref.simples
                        [simple_slice_start..simple_slice_start + simple_slice_len])
                } else {
                    Err(LibraryOrganismTryGetErr::InvalidAddr)
                }
            }
            _ => Err(LibraryOrganismTryGetErr::InvalidLibraryOrganismType),
        }
    }
}

impl LibraryOrganism for Transformation {
    fn try_get(lo_ref: &LibraryOrganismRef<Self>) -> Result<&Self, LibraryOrganismTryGetErr> {
        match lo_ref.lo_type {
            LibraryOrganismType::Transformation => {
                if lo_ref.addr.is_valid() {
                    Ok(lo_ref.library_ref.transformations.get_word(lo_ref.addr))
                } else {
                    Err(LibraryOrganismTryGetErr::InvalidAddr)
                }
            }
            _ => Err(LibraryOrganismTryGetErr::InvalidLibraryOrganismType),
        }
    }
}

impl LibraryOrganism for FullMask {
    fn try_get(lo_ref: &LibraryOrganismRef<Self>) -> Result<&Self, LibraryOrganismTryGetErr> {
        match lo_ref.lo_type {
            LibraryOrganismType::FullMask => {
                if lo_ref.addr.is_valid() {
                    Ok(lo_ref.library_ref.full_masks.get_word(lo_ref.addr))
                } else {
                    Err(LibraryOrganismTryGetErr::InvalidAddr)
                }
            }
            _ => Err(LibraryOrganismTryGetErr::InvalidLibraryOrganismType),
        }
    }
}

impl LibraryOrganism for FullAddr {
    fn try_get(lo_ref: &LibraryOrganismRef<Self>) -> Result<&Self, LibraryOrganismTryGetErr> {
        match lo_ref.lo_type {
            LibraryOrganismType::InverseAddr => {
                if lo_ref.addr.is_valid() {
                    Ok(lo_ref.library_ref.inverse_addrs.get_word(lo_ref.addr))
                } else {
                    Err(LibraryOrganismTryGetErr::InvalidAddr)
                }
            }
            _ => Err(LibraryOrganismTryGetErr::InvalidLibraryOrganismType),
        }
    }
}

impl LibraryOrganism for Quat {
    fn try_get(lo_ref: &LibraryOrganismRef<Self>) -> Result<&Self, LibraryOrganismTryGetErr> {
        match lo_ref.lo_type {
            LibraryOrganismType::Rotation => {
                let full_addr: FullAddr = lo_ref.addr;

                if full_addr.is_valid()
                    && full_addr.get_genus_index() < Library::GENERA_PER_SMALL_CLASS
                {
                    Ok(lo_ref.library_ref.rotations.get_word(full_addr))
                } else {
                    Err(LibraryOrganismTryGetErr::InvalidAddr)
                }
            }
            LibraryOrganismType::Orientation => {
                let half_addr: HalfAddr = *lo_ref.addr.get_half_addr();

                if half_addr.is_valid() {
                    Ok(lo_ref.library_ref.orientations.get_word(half_addr))
                } else {
                    Err(LibraryOrganismTryGetErr::InvalidAddr)
                }
            }
            _ => Err(LibraryOrganismTryGetErr::InvalidLibraryOrganismType),
        }
    }
}

pub struct LibraryOrganismRef<T: LibraryOrganism + ?Sized> {
    library_ref: LibraryRef,
    addr: FullAddr,
    lo_type: LibraryOrganismType,
    _pd: PhantomData<T>,
}

impl<T: LibraryOrganism + ?Sized> LibraryOrganismRef<T> {
    pub fn try_get(&self) -> Result<&T, LibraryOrganismTryGetErr> {
        T::try_get(self)
    }
}

impl<T: LibraryOrganism + ?Sized> From<(FullAddr, LibraryOrganismType)> for LibraryOrganismRef<T> {
    fn from((addr, lo_type): (FullAddr, LibraryOrganismType)) -> Self {
        Self {
            library_ref: Library::get(),
            addr,
            lo_type,
            _pd: Default::default(),
        }
    }
}

struct LibraryRefMut(RwLockWriteGuard<'static, Library>);

impl Deref for LibraryRefMut {
    type Target = Library;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl DerefMut for LibraryRefMut {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

struct LibraryWriteGuard(Option<MutexGuard<'static, ()>>);

impl LibraryWriteGuard {
    fn new() -> Self {
        Self(Some(Self::get_mutex_guard()))
    }

    fn lock(&mut self) {
        self.0.get_or_insert_with(Self::get_mutex_guard);
    }

    fn get_mutex_guard() -> MutexGuard<'static, ()> {
        LIBRARY_MUTEX.lock().unwrap()
    }
}

impl Library {
    pub const GENERA_PER_SMALL_CLASS: usize = GenusIndex::COMPLEX_OFFSET;
    pub const ORGANISMS_PER_GENUS: usize = Self::ORGANISMS_PER_SPECIES * Self::SPECIES_PER_GENUS;
    pub const ORGANISMS_PER_SPECIES: usize = usize::PENTAGON_VERTEX_COUNT;
    pub const SPECIES_PER_GENUS: usize = usize::PENTAGON_PIECE_COUNT;
    pub const SPECIES_PER_LARGE_GENUS: usize = usize::PIECE_COUNT;

    #[inline(always)]
    pub fn get_simple_slice(full_addr: FullAddr) -> LibraryOrganismRef<[HalfAddr]> {
        (full_addr, LibraryOrganismType::SimpleSlice).into()
    }

    #[inline(always)]
    pub fn get_transformation(full_addr: FullAddr) -> LibraryOrganismRef<Transformation> {
        (full_addr, LibraryOrganismType::Transformation).into()
    }

    #[inline(always)]
    pub fn get_full_mask(full_addr: FullAddr) -> LibraryOrganismRef<FullMask> {
        (full_addr, LibraryOrganismType::FullMask).into()
    }

    #[inline(always)]
    pub fn get_inverse_addr(full_addr: FullAddr) -> LibraryOrganismRef<FullAddr> {
        (full_addr, LibraryOrganismType::InverseAddr).into()
    }

    #[inline(always)]
    pub fn get_rotation(full_addr: FullAddr) -> LibraryOrganismRef<Quat> {
        (full_addr, LibraryOrganismType::Rotation).into()
    }

    #[inline(always)]
    pub fn get_orientation(half_addr: HalfAddr) -> LibraryOrganismRef<Quat> {
        (
            half_addr.as_reorientation(),
            LibraryOrganismType::Orientation,
        )
            .into()
    }

    #[inline(always)]
    pub fn get_genus_count() -> usize {
        Self::get().genus_infos.len()
    }

    #[inline(always)]
    pub fn get_family_count() -> usize {
        Self::get().family_infos.len()
    }

    pub fn get_family_index(genus_index: GenusIndex) -> usize {
        break_assert!(genus_index.is_valid());

        Self::get().get_genus_info(genus_index).family_index as usize
    }

    pub fn get_base_genus_index(family_index: usize) -> GenusIndex {
        break_assert!(family_index < Self::get_family_count());

        Self::get().family_infos[family_index].base_genus
    }

    pub fn get_family_genus_count(family_index: usize) -> GenusIndexType {
        break_assert!(family_index < Self::get_family_count());

        Self::get().family_infos[family_index].get_genus_count() as GenusIndexType
    }

    pub fn get_family_genus_range(family_index: usize) -> Range<GenusIndexType> {
        break_assert!(family_index < Self::get_family_count());

        let genus_range: Range<usize> = Self::get().family_infos[family_index].get_genus_range();

        genus_range.start as GenusIndexType..genus_range.end as GenusIndexType
    }

    pub fn push_family_and_update_file(family_input: FamilyInput) -> Result<(), Box<dyn Error>> {
        Self::build();

        let mut lwg: LibraryWriteGuard = LibraryWriteGuard::new();

        Self::initialize_family(&mut lwg, family_input).map_err(
            |push_family_err: InitializeFamilyErr| -> Box<dyn Error> {
                Box::new(SimpleError::new(format!("{:?}", push_family_err)))
            },
        )?;

        OrderInput::from(&*Self::get()).to_file(&STRING_DATA.files.library)
    }

    fn get_mut() -> LibraryRefMut {
        assert!(LIBRARY_ASSUME_INIT.load(Ordering::Acquire));

        LibraryRefMut(
            // Safe: see assert above
            unsafe { transmute::<&RwLock<MaybeUninit<Self>>, &RwLock<Self>>(&LIBRARY) }
                .write()
                .unwrap(),
        )
    }

    fn get_simple_slice_genus(&self, genus_index: GenusIndex) -> Option<Box<Genus<SimpleSlice>>> {
        let simples_range: Range<usize> = Self::get_simples_range(genus_index)?;
        let simple_slice_len: usize = self.get_genus_info(genus_index).simple_slice_len as usize;
        let mut simple_slice_genus: Genus<SimpleSlice> = Genus::<SimpleSlice>::default();

        for (species_index, simples_species) in self.simples[simples_range]
            .chunks(Self::ORGANISMS_PER_SPECIES * simple_slice_len)
            .enumerate()
        {
            let simple_slice_species: &mut Species<SimpleSlice> =
                &mut simple_slice_genus[species_index];

            for (organism_index, simple_slice) in
                simples_species.chunks(simple_slice_len).enumerate()
            {
                simple_slice_species[organism_index] = Some(simple_slice);
            }
        }

        Some(Box::<Genus<SimpleSlice>>::new(simple_slice_genus))
    }

    fn get_simples_range(genus_index: GenusIndex) -> Option<Range<usize>> {
        if usize::from(genus_index) >= Self::get_genus_count() {
            return None;
        }

        let library: &Self = &*Self::get();
        let genus_info: &GenusInfo = library.get_genus_info(genus_index);
        let simple_slice_start: usize = genus_info.simple_offset as usize;
        let simple_slice_end: usize =
            simple_slice_start + Self::ORGANISMS_PER_GENUS * genus_info.simple_slice_len as usize;

        if simple_slice_end > Self::get_simple_count() {
            return None;
        }

        Some(simple_slice_start..simple_slice_end)
    }

    fn get_genus_info(&self, genus_index: GenusIndex) -> &GenusInfo {
        &self.genus_infos[usize::from(genus_index)]
    }

    fn get_family_info(&self, family_index: FamilyIndex) -> &FamilyInfo {
        &self.family_infos[family_index as usize]
    }

    fn get_family_info_from_genus_index(&self, genus_index: GenusIndex) -> &FamilyInfo {
        self.get_family_info(self.get_genus_info(genus_index).family_index)
    }

    fn simple_slices_have_identical_organism_indicies(
        simple_slice_a: &[HalfAddr],
        simple_slice_b: &[HalfAddr],
    ) -> bool {
        simple_slice_a.iter().enumerate().all(
            |(simple_slice_index, simple): (usize, &HalfAddr)| -> bool {
                simple.get_organism_index()
                    == simple_slice_b[simple_slice_index].get_organism_index()
            },
        )
    }

    fn initialize() {
        miri_echo!();

        let mut lwg: LibraryWriteGuard = LibraryWriteGuard::new();

        assert!(!LIBRARY_ASSUME_INIT.load(Ordering::Acquire));

        {
            let mut library_write_guard: RwLockWriteGuard<MaybeUninit<Library>> =
                LIBRARY.write().unwrap();
            let library_mut_ptr: *mut Library = library_write_guard.as_mut_ptr();

            macro_rules! init_field {
                ($($field:tt: $field_ty:ty = $capacity:ident,)*) => {
                    $(
                        /* Safe: We are writing into fields at their offset location from
                        `library_mut_ptr` */
                        write(
                            /* Safe: `library_mut_ptr` is a mutable pointer that is rendered const
                            from the `raw_field!()` invocation, so we can safely make it mutable
                            again */
                            raw_field!(library_mut_ptr, Library, $field) as *mut $field_ty,
                            Vec::with_capacity(Self::$capacity)
                        );
                    )*
                }
            }

            macro_rules! zero_field {
                ($($field:tt: $field_ty:ty,)*) => {
                    $(
                        /* Safe: We are writing into fields at their offset location from
                        `library_mut_ptr` */
                        write_bytes(
                            /* Safe: `library_mut_ptr` is a mutable pointer that is rendered const
                            from the `raw_field!()` invocation, so we can safely make it mutable
                            again */
                            raw_field!(library_mut_ptr, Library, $field) as *mut $field_ty,
                            0_u8,
                            1_usize
                        );
                    )*
                }
            }

            // Safe: We are writing into fields at their offset location from library_mut_ptr
            unsafe {
                init_field!(
                    family_infos: Vec<FamilyInfo> = GENERA_PER_SMALL_CLASS,
                    genus_infos: Vec<GenusInfo> = GENERA_PER_SMALL_CLASS,
                    simples: Vec<HalfAddr> = ORGANISMS_PER_GENUS,
                    transformations: Class<Transformation> = ORGANISMS_PER_GENUS,
                    full_masks: Class<FullMask> = ORGANISMS_PER_GENUS,
                    inverse_addrs: Class<FullAddr> = ORGANISMS_PER_GENUS,
                );
            }

            unsafe {
                zero_field!(rotations: SmallClass<Quat>, orientations: LargeGenus<Quat>,);
            }

            /* Correct: All the `Vec` members now have been explicitly initialized, and the
            remaining members are arrays of `Quat`s, which are fine to leave zeroed (Quat::ZERO) */
            const_assert_eq!(0_u32, unsafe { transmute::<f32, u32>(0.0_f32) });
            LIBRARY_ASSUME_INIT.store(true, Ordering::Release);
        }

        let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);

        Self::initialize_orientations(&mut lwg, icosidodecahedron_data);
        Self::initialize_reorientation_genus(&mut lwg, icosidodecahedron_data);
        Self::initialize_simple_genus(&mut lwg, icosidodecahedron_data);
        Self::initialize_order(
            &mut lwg,
            OrderInput::from_file_or_default(&STRING_DATA.files.library),
        );
    }

    fn initialize_orientations(lwg: &mut LibraryWriteGuard, icosidodecahedron_data: &Data) {
        miri_echo!();
        lwg.lock();

        let mut library_ref_mut: LibraryRefMut = Self::get_mut();

        for (species_index, orientation_species) in
            library_ref_mut.orientations.iter_mut().enumerate()
        {
            let face_data: &FaceData = &icosidodecahedron_data.faces[species_index];

            for (organism_index, orientation) in orientation_species.iter_mut().enumerate() {
                *orientation = face_data.get_rotated_quat(organism_index as u32);
            }
        }
    }

    fn initialize_reorientation_genus(lwg: &mut LibraryWriteGuard, icosidodecahedron_data: &Data) {
        miri_echo!();
        lwg.lock();

        const REORIENTATION_STR: &str = "Reorientation";

        let reorientation_genus_index: usize = GenusIndex::REORIENTATION.into();

        {
            let origin_conj_quat: Quat = icosidodecahedron_data.faces[PENTAGON_INDEX_OFFSET]
                .quat
                .conjugate();

            Self::push_genus(
                lwg,
                GenusInfo::new(
                    REORIENTATION_STR.into(),
                    &[],
                    Self::get_family_count() as u8,
                ),
            );

            let library: &mut Library = &mut Self::get_mut();

            library.family_infos.push(FamilyInfo::new(
                REORIENTATION_STR.into(),
                GenusIndex::REORIENTATION,
                false,
            ));

            for (orientation_species, transformation_species, mask_species, rotation_species) in {
                // This type is just used to mutably borrow multiple genus data simultaneously
                #[allow(clippy::type_complexity)]
                let (
                    orientation_genus,
                    transformation_genus,
                    mask_genus,
                    rotation_genus,
                ): (
                    &[Species<Quat>],
                    &mut Genus<Transformation>,
                    &mut Genus<FullMask>,
                    &mut Genus<Quat>,
                ) = (
                    &library.orientations[0_usize..Self::SPECIES_PER_GENUS],
                    &mut library.transformations[reorientation_genus_index],
                    &mut library.full_masks[reorientation_genus_index],
                    &mut library.rotations[reorientation_genus_index],
                );

                izip!(
                    orientation_genus.iter(),
                    transformation_genus.iter_mut(),
                    mask_genus.iter_mut(),
                    rotation_genus.iter_mut()
                )
            } {
                for (orientation, transformation, mask, rotation) in izip!(
                    orientation_species.iter(),
                    transformation_species.iter_mut(),
                    mask_species.iter_mut(),
                    rotation_species.iter_mut()
                ) {
                    let reorientation_quat: Quat = *orientation * origin_conj_quat;
                    let (pos_array, rot_array): (
                        &mut PuzzleStateComponent,
                        &mut PuzzleStateComponent,
                    ) = transformation.arrays_mut();

                    for piece_index in PIECE_RANGE {
                        let (pos, rot): (usize, usize) = icosidodecahedron_data.get_pos_and_rot(
                            &(reorientation_quat * icosidodecahedron_data.faces[piece_index].quat),
                            /* We could put a filter in here, but it'd be slower, and the quat math
                            is precise enough that it's unnecessary here */
                            None,
                        );

                        pos_array[piece_index] = pos as PieceStateComponent;
                        rot_array[piece_index] = rot as PieceStateComponent;
                    }

                    *mask = FullMask::from(transformation.arrays());
                    *rotation = reorientation_quat;
                }
            }
        }

        /* `FullAddr::set_genus_index()` will implicitly call Self::get(), so we need to initialize
        the `inverse_addr_genus` on the stack, then copy it over */
        let mut inverse_addr_genus: Genus<FullAddr> = Genus::<FullAddr>::default();

        {
            let transformation_genus: &Genus<Transformation> =
                &Self::get().transformations[reorientation_genus_index];

            for (transformation_species, inverse_addr_species) in transformation_genus
                .iter()
                .zip(inverse_addr_genus.iter_mut())
            {
                for (transformation, inverse_addr) in transformation_species
                    .iter()
                    .zip(inverse_addr_species.iter_mut())
                {
                    *inverse_addr = transformation_genus
                        .find_word(&(-transformation))
                        .map(|mut inverse_addr: FullAddr| -> FullAddr {
                            *inverse_addr.set_genus_index(reorientation_genus_index)
                        })
                        .unwrap_or_default();
                }
            }
        }

        {
            Self::get_mut().inverse_addrs[reorientation_genus_index] = inverse_addr_genus;
        }
    }

    fn initialize_simple_genus(lwg: &mut LibraryWriteGuard, icosidodecahedron_data: &Data) {
        miri_echo!();
        lwg.lock();

        const SIMPLE_STR: &str = "Simple";

        let simple_genus_index: usize = GenusIndex::SIMPLE.into();

        {
            let mut local_inverse_addr: FullAddr =
                FullAddr::from((GenusIndex::SIMPLE, HalfAddr::ORIGIN));

            Self::push_genus(
                lwg,
                GenusInfo::new(
                    SIMPLE_STR.into(),
                    &[HalfAddr::default()],
                    Self::get_family_count() as u8,
                ),
            );

            let library: &mut Library = &mut Self::get_mut();

            library.family_infos.push(FamilyInfo::new(
                SIMPLE_STR.into(),
                GenusIndex::SIMPLE,
                false,
            ));

            for (
                species_index,
                (
                    simple_species,
                    transformation_species,
                    mask_species,
                    inverse_addr_species,
                    rotation_species,
                ),
            ) in {
                // This type is just used to mutably borrow multiple genus data simultaneously
                #[allow(clippy::type_complexity)]
                let (
                    simple_genus,
                    transformation_genus,
                    mask_genus,
                    inverse_addr_genus,
                    rotation_genus,
                ): (
                    &mut [HalfAddr],
                    &mut Genus<Transformation>,
                    &mut Genus<FullMask>,
                    &mut Genus<FullAddr>,
                    &mut Genus<Quat>,
                ) = (
                    &mut library.simples[0_usize..Self::ORGANISMS_PER_GENUS],
                    &mut library.transformations[simple_genus_index],
                    &mut library.full_masks[simple_genus_index],
                    &mut library.inverse_addrs[simple_genus_index],
                    &mut library.rotations[simple_genus_index],
                );

                izip!(
                    simple_genus.chunks_mut(Self::ORGANISMS_PER_SPECIES),
                    transformation_genus.iter_mut(),
                    mask_genus.iter_mut(),
                    inverse_addr_genus.iter_mut(),
                    rotation_genus.iter_mut()
                )
                .enumerate()
            } {
                let face_data: &FaceData = &icosidodecahedron_data.faces[species_index];
                let face_mask: FullMask = FullMask::from_pentagon_index(species_index);

                local_inverse_addr.set_species_index(species_index);

                for (organism_index, (simple, transformation, mask, inverse_addr, rotation)) in
                    izip!(
                        simple_species.iter_mut(),
                        transformation_species.iter_mut(),
                        mask_species.iter_mut(),
                        inverse_addr_species.iter_mut(),
                        rotation_species.iter_mut()
                    )
                    .enumerate()
                {
                    *simple = *local_inverse_addr
                        .set_organism_index(organism_index)
                        .get_half_addr();

                    let local_rotation: Quat = face_data.get_rotation_quat(organism_index as u32);
                    let local_mask: FullMask = if organism_index != 0_usize {
                        face_mask
                    } else {
                        FullMask::default()
                    };
                    let (pos_array, rot_array): (
                        &mut PuzzleStateComponent,
                        &mut PuzzleStateComponent,
                    ) = transformation.arrays_mut();

                    for piece_index in PIECE_RANGE {
                        let (pos, rot): (usize, usize) = if local_mask.affects_piece(piece_index) {
                            icosidodecahedron_data.get_pos_and_rot(
                                &(local_rotation * icosidodecahedron_data.faces[piece_index].quat),
                                /* We could put a filter in here, but it'd be slower, and the quat
                                math is precise enough that it's unnecessary here */
                                None,
                            )
                        } else {
                            (piece_index, 0_usize)
                        };

                        pos_array[piece_index] = pos as PieceStateComponent;
                        rot_array[piece_index] = rot as PieceStateComponent;
                    }

                    *mask = local_mask;
                    *inverse_addr = *local_inverse_addr
                        .set_organism_index(FullAddr::invert_organism_index(organism_index));
                    *rotation = local_rotation;
                }
            }
        }
    }

    fn initialize_order(lwg: &mut LibraryWriteGuard, order_info: OrderInput) {
        miri_echo!();
        lwg.lock();

        for family_input in order_info.0 {
            if let Err(initialize_family_err) = Self::initialize_family(lwg, family_input) {
                log::warn!("InitializeFamilyErr: {:?}", initialize_family_err);
            }
        }
    }

    fn initialize_family(
        lwg: &mut LibraryWriteGuard,
        mut family_input: FamilyInput,
    ) -> Result<FamilyIndex, InitializeFamilyErr> {
        miri_echo!();
        miri_trace!(target: "miri", "  (\"{}\" @ {})", family_input.name, Self::get_family_count());
        lwg.lock();

        let seed_simples: Vec<HalfAddr> = take(&mut family_input.seed_simples)
            .into_iter()
            .map(|(species_index, organism_index): (u8, u8)| -> HalfAddr {
                HalfAddr::new(species_index as usize, organism_index as usize)
            })
            .collect();
        let seed_simple_slice: &[HalfAddr] = &seed_simples;

        Self::is_seed_simple_slice_valid(seed_simple_slice)?;

        let simple_slice_len: usize = seed_simple_slice.len();
        let family_index: FamilyIndex = Self::get_family_count() as FamilyIndex;
        let mut family_info: FamilyInfo = FamilyInfo {
            name: take(&mut family_input.name),
            base_genus: GenusIndex::from_usize(Self::get_genus_count()),
            inverse_is_mirror: false, // To be determined
        };

        Self::push_genus(
            lwg,
            GenusInfo::new(family_info.name.clone(), seed_simple_slice, family_index),
        );
        Self::initialize_simple_slice_genus(
            lwg,
            family_info.base_genus,
            seed_simple_slice,
            false,
            false,
        );
        Self::push_genus(
            lwg,
            GenusInfo::new(
                format!("{}'", family_info.name),
                seed_simple_slice,
                family_index,
            ),
        );
        Self::initialize_simple_slice_genus(
            lwg,
            GenusIndex(family_info.base_genus.0 + 1 as GenusIndexType),
            seed_simple_slice,
            true,
            false,
        );

        family_info.inverse_is_mirror = {
            miri_trace!(target: "miri", "  allocating temp_inverse_simples with capacity {}", simple_slice_len);

            let mut temp_inverse_simples: Vec<HalfAddr> =
                Vec::<HalfAddr>::with_capacity(simple_slice_len);

            Self::initialize_simple_slice(
                seed_simple_slice,
                false,
                true,
                HalfAddr::ORIGIN,
                &mut temp_inverse_simples,
            );

            let mirror_genus_index: GenusIndex =
                GenusIndex(family_info.base_genus.0 + 1 as GenusIndexType);
            let mirror_simple_slice_ref: LibraryOrganismRef<[HalfAddr]> =
                Self::get_simple_slice(FullAddr::from((mirror_genus_index, HalfAddr::ORIGIN)));
            let mirror_simple_slice: &[HalfAddr] = mirror_simple_slice_ref.try_get().unwrap();

            Self::simple_slices_have_identical_organism_indicies(
                &temp_inverse_simples,
                mirror_simple_slice,
            ) && Self::get()
                .get_simple_slice_genus(mirror_genus_index)
                .and_then(
                    |simple_slice_genus: Box<Genus<SimpleSlice>>| -> Option<FullAddr> {
                        (*simple_slice_genus).find_word(&Some(&*temp_inverse_simples))
                    },
                )
                .is_some()
        };

        if !family_info.inverse_is_mirror {
            Self::push_genus(
                lwg,
                GenusInfo::new(
                    format!("{}\"", family_info.name),
                    seed_simple_slice,
                    family_index,
                ),
            );
            Self::initialize_simple_slice_genus(
                lwg,
                GenusIndex(family_info.base_genus.0 + 2 as GenusIndexType),
                seed_simple_slice,
                false,
                true,
            );
            Self::push_genus(
                lwg,
                GenusInfo::new(
                    format!("{}\"'", family_info.name),
                    seed_simple_slice,
                    family_index,
                ),
            );
            Self::initialize_simple_slice_genus(
                lwg,
                GenusIndex(family_info.base_genus.0 + 3 as GenusIndexType),
                seed_simple_slice,
                true,
                true,
            );
        }

        for genus_index_usize in family_info.get_genus_range() {
            miri_trace!(target: "miri", "  initializing genus index {}", genus_index_usize);
            let genus_index: GenusIndex = GenusIndex::from_usize(genus_index_usize);
            let inverse_genus_index: usize = family_info.invert_genus_index(genus_index).into();
            let mut simple_slice_start: usize = Self::get_simples_range(genus_index).unwrap().start;

            for species_index in 0_usize..Self::SPECIES_PER_GENUS {
                for organism_index in 0_usize..Self::ORGANISMS_PER_SPECIES {
                    let mut puzzle_state: PuzzleState = PuzzleState::SOLVED_STATE;
                    let simple_slice_end = simple_slice_start + simple_slice_len;

                    for simple in Self::get().simples[simple_slice_start..simple_slice_end].iter() {
                        puzzle_state += simple.as_simple();
                    }

                    let inverse_addr: FullAddr =
                        FullAddr::from((inverse_genus_index, species_index, organism_index));
                    let mut library_ref_mut: LibraryRefMut = Self::get_mut();

                    library_ref_mut.transformations[genus_index_usize][species_index]
                        [organism_index] = Transformation(puzzle_state.clone());
                    library_ref_mut.full_masks[genus_index_usize][species_index][organism_index] =
                        FullMask::from(puzzle_state.arrays());
                    library_ref_mut.inverse_addrs[genus_index_usize][species_index]
                        [organism_index] = inverse_addr;

                    simple_slice_start = simple_slice_end;
                }
            }
        }

        Self::get_mut().family_infos.push(family_info);

        Ok(family_index)
    }

    fn is_seed_simple_slice_valid(
        seed_simple_slice: &[HalfAddr],
    ) -> Result<(), InitializeFamilyErr> {
        for simple in seed_simple_slice {
            if !simple.is_valid() {
                return Err(InitializeFamilyErr::InvalidSeedSimpleSlice);
            }
        }

        if Self::get_family_count() > FamilyIndex::MAX as usize {
            return Err(InitializeFamilyErr::FamilyIndexTooLarge);
        }

        if Self::get_genus_count() > GenusIndexType::MAX as usize - 1_usize {
            return Err(InitializeFamilyErr::GenusIndexTooLarge);
        }

        if Self::get_simple_count() > SimpleOffset::MAX as usize {
            return Err(InitializeFamilyErr::SimpleOffsetTooLarge);
        }

        if seed_simple_slice.len() <= 1_usize {
            return Err(InitializeFamilyErr::SeedSimpleSliceTooShort);
        }

        if seed_simple_slice.len() > SimpleSliceLen::MAX as usize {
            return Err(InitializeFamilyErr::SeedSimpleSliceTooLong);
        }

        let mut puzzle_state: PuzzleState = PuzzleState::default();

        for simple in seed_simple_slice {
            puzzle_state += simple.as_simple();
        }

        if puzzle_state.is_solved() {
            return Err(InitializeFamilyErr::DoesNotTransformPuzzleState);
        }

        let library: &Self = &*Self::get();

        // Check for a homomorphic genus
        for family_info in library.family_infos.iter() {
            let base_genus_info: &GenusInfo = library.get_genus_info(family_info.base_genus);

            /* If the base genus for the family has a different simple slice length, it won't be a
            match */
            if seed_simple_slice.len() != base_genus_info.simple_slice_len as usize {
                continue;
            }

            let mut full_addr: FullAddr = HalfAddr::ORIGIN.as_reorientation();

            for genus_index in family_info.get_genus_range() {
                if !Self::simple_slices_have_identical_organism_indicies(
                    seed_simple_slice,
                    Self::get_simple_slice(*full_addr.set_genus_index(genus_index))
                        .try_get()
                        .unwrap(),
                ) {
                    continue;
                }

                if let Some(mut homomorphism) = Self::get()
                    .get_simple_slice_genus(GenusIndex(genus_index as GenusIndexType))
                    .and_then(
                        |simple_slice_genus: Box<Genus<SimpleSlice>>| -> Option<FullAddr> {
                            (*simple_slice_genus).find_word(&Some(seed_simple_slice))
                        },
                    )
                {
                    return Err(InitializeFamilyErr::HomomorphismExists(
                        *homomorphism.set_genus_index(genus_index),
                    ));
                }
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn get_simple_count() -> usize {
        Self::get().simples.len()
    }

    fn initialize_simple_slice(
        seed_simple_slice: &[HalfAddr],
        mirror: bool,
        invert: bool,
        reorientation: HalfAddr,
        simples: &mut Vec<HalfAddr>,
    ) {
        let get_species_index = if mirror {
            |seed_simple: HalfAddr| -> usize {
                FullAddr::mirror_species_index(seed_simple.get_species_index())
            }
        } else {
            |seed_simple: HalfAddr| -> usize { seed_simple.get_species_index() }
        };
        let get_organism_index = if mirror ^ invert {
            |seed_simple: HalfAddr| -> usize {
                FullAddr::invert_organism_index(seed_simple.get_organism_index())
            }
        } else {
            |seed_simple: HalfAddr| -> usize { seed_simple.get_organism_index() }
        };
        let mut seed_simple_slice_iter =
            |seed_simple_slice_iter: &mut dyn DoubleEndedIterator<Item = &HalfAddr>| {
                for seed_simple in seed_simple_slice_iter {
                    simples.push(
                        *(FullAddr::from((
                            usize::from(GenusIndex::SIMPLE),
                            get_species_index(*seed_simple),
                            get_organism_index(*seed_simple),
                        )) + reorientation)
                            .get_half_addr(),
                    );
                }
            };

        if invert {
            seed_simple_slice_iter(&mut seed_simple_slice.iter().rev());
        } else {
            seed_simple_slice_iter(&mut seed_simple_slice.iter());
        }
    }

    fn initialize_simple_slice_genus(
        lwg: &mut LibraryWriteGuard,
        genus_index: GenusIndex,
        seed_simple_slice: &[HalfAddr],
        mirror: bool,
        invert: bool,
    ) {
        miri_echo!();
        lwg.lock();

        let simples_range: Range<usize> = {
            if let Some(simples_range) = Self::get_simples_range(genus_index) {
                simples_range
            } else {
                return;
            }
        };

        if simples_range.end - simples_range.start
            != Self::ORGANISMS_PER_GENUS * seed_simple_slice.len()
        {
            return;
        }

        let mut simples: Vec<HalfAddr> = Vec::<HalfAddr>::with_capacity(simples_range.len());

        for species_index in 0_usize..Self::SPECIES_PER_GENUS {
            for organism_index in 0_usize..Self::ORGANISMS_PER_SPECIES {
                Self::initialize_simple_slice(
                    seed_simple_slice,
                    mirror,
                    invert,
                    HalfAddr::new(species_index, organism_index),
                    &mut simples,
                );
            }
        }

        Self::get_mut().simples[simples_range].copy_from_slice(&simples);
    }

    fn push_genus(lwg: &mut LibraryWriteGuard, mut genus_info: GenusInfo) {
        miri_echo!();
        miri_trace!(target: "miri", "  (\"{}\" @ {})", genus_info.name, Self::get_genus_count());
        lwg.lock();

        let simple_offset: usize = Self::get_simple_count();
        let simple_slice_len: usize = genus_info.simple_slice_len as usize;
        let mut library_ref_mut: LibraryRefMut = Self::get_mut();

        genus_info.simple_offset = simple_offset as SimpleOffset;
        library_ref_mut.genus_infos.push(genus_info);
        library_ref_mut.simples.resize(
            simple_offset + Self::ORGANISMS_PER_GENUS * simple_slice_len,
            HalfAddr::default(),
        );
        library_ref_mut
            .transformations
            .push(Genus::<Transformation>::default());
        library_ref_mut
            .full_masks
            .push(Genus::<FullMask>::default());
        library_ref_mut
            .inverse_addrs
            .push(Genus::<FullAddr>::default());
    }
}

impl From<&Library> for OrderInput {
    fn from(library: &Library) -> OrderInput {
        OrderInput(
            library
                .family_infos
                .iter()
                .map(|family_info: &FamilyInfo| -> FamilyInput {
                    FamilyInput {
                        name: family_info.name.clone(),
                        seed_simples: Library::get_simple_slice(FullAddr {
                            genus_index: family_info.base_genus,
                            half_addr: HalfAddr::ORIGIN,
                        })
                        .try_get()
                        .unwrap()
                        .iter()
                        .map(|half_addr: &HalfAddr| -> (u8, u8) {
                            (
                                half_addr.get_species_index() as u8,
                                half_addr.get_organism_index() as u8,
                            )
                        })
                        .collect(),
                    }
                })
                .collect(),
        )
    }
}

/* Library needs a more complicated setup because Library::initialize() relies on Library::get(), so
a lazy_static won't suffice */
impl StaticDataLibrary for Library {
    type Target = LibraryRef;

    fn pre_init() -> Option<Box<dyn FnOnce()>> {
        Some(Box::new(|| {
            Data::initialize();
        }))
    }

    fn init() -> Option<Box<dyn FnOnce()>> {
        Some(Box::new(|| {
            Library::initialize();
        }))
    }

    fn get() -> LibraryRef {
        assert!(LIBRARY_ASSUME_INIT.load(Ordering::Acquire));

        LibraryRef(
            // Safe: see assert above
            unsafe { transmute::<&RwLock<MaybeUninit<Library>>, &RwLock<Library>>(&LIBRARY) }
                .read()
                .unwrap(),
        )
    }

    fn get_once() -> Option<&'static Once> {
        Some(&LIBRARY_ONCE)
    }
}

/// Primary `Library` storage, statically allocated due to the large arrays it contains
static LIBRARY: RwLock<MaybeUninit<Library>> = RwLock::new(MaybeUninit::uninit());

/// `AtomicBool` indicating that the `MaybeUninit<Library>` in LIBRARY has been initialized
static LIBRARY_ASSUME_INIT: AtomicBool = AtomicBool::new(false);

/// Empty `Mutex` used to lock write access over operations that require more than just
/// `Library::get_mut()`
static LIBRARY_MUTEX: Mutex<()> = Mutex::new(());

/// `Once` for use by `StaticDataLibrary` impl
static LIBRARY_ONCE: Once = Once::new();

#[cfg(test)]
mod tests {
    use {super::*, crate::util::StaticDataLibrary};

    fn test_validity() {
        miri_echo!();

        let library: &Library = &*Library::get();

        for genus_index in 0_usize..library.genus_infos.len() {
            let transformation_genus: &Genus<Transformation> =
                &library.transformations[genus_index];
            let inverse_addr_genus: &Genus<FullAddr> = &library.inverse_addrs[genus_index];

            for species_index in 0_usize..Library::SPECIES_PER_GENUS {
                let transformation_species: &Species<Transformation> =
                    &transformation_genus[species_index];
                let inverse_addr_species: &Species<FullAddr> = &inverse_addr_genus[species_index];

                for organism_index in 0_usize..Library::ORGANISMS_PER_SPECIES {
                    let transformation: &Transformation = &transformation_species[organism_index];
                    let inverse_addr: &FullAddr = &inverse_addr_species[organism_index];

                    if !transformation.is_valid() {
                        log::error!(
                            "Transformation ({}, {}, {}) is invalid",
                            genus_index,
                            species_index,
                            organism_index
                        );
                        error_expr!(transformation);

                        panic!();
                    }

                    if !inverse_addr.is_valid() {
                        log::error!(
                            "Inverse address for transformation ({}, {}, {}) is invalid",
                            genus_index,
                            species_index,
                            organism_index
                        );
                        error_expr!(inverse_addr);

                        panic!();
                    }

                    let inverse_transformation: &Transformation =
                        library.transformations.get_word(*inverse_addr);

                    if *transformation != -inverse_transformation
                        || -transformation != *inverse_transformation
                    {
                        log::error!(
                            "Transformation addressed by ({}, {}, {}), the associated address of \
                                transformation ({}, {}, {}) is not the true inverse transformation",
                            inverse_addr.get_genus_index(),
                            inverse_addr.get_species_index(),
                            inverse_addr.get_organism_index(),
                            genus_index,
                            species_index,
                            organism_index
                        );
                        error_expr!(
                            transformation,
                            -transformation,
                            inverse_transformation,
                            -inverse_transformation
                        );

                        panic!();
                    }

                    let solved_state: PuzzleState = PuzzleState::SOLVED_STATE;
                    let middle_state: PuzzleState = &solved_state + transformation;
                    let end_state: PuzzleState = &middle_state + inverse_transformation;

                    if end_state != solved_state {
                        log::error!(
                            "solved_state + transformation + inverse_transformation != solved_state
                                for transformation ({}, {}, {})",
                            genus_index,
                            species_index,
                            organism_index
                        );
                        error_expr!(
                            solved_state,
                            transformation,
                            middle_state,
                            inverse_transformation,
                            end_state
                        );

                        panic!();
                    }
                }
            }
        }
    }

    fn test_reorientations() {
        miri_echo!();

        let library: &Library = &*Library::get();
        let reorientation_genus_index: usize = GenusIndex::REORIENTATION.into();
        let simple_transformation_genus: &Genus<Transformation> =
            &library.transformations[usize::from(GenusIndex::SIMPLE)];
        let reorientation_tests: [Vec<(usize, usize)>; usize::PENTAGON_PIECE_COUNT] =
            <[Vec<(usize, usize)>; usize::PENTAGON_PIECE_COUNT]>::from_file(
                STRING_DATA.tests.reorientation_tests.as_ref(),
            )
            .ok()
            .unwrap();

        for species_index in 0_usize..Library::SPECIES_PER_GENUS {
            let transformation_species: &Species<Transformation> =
                &library.transformations[reorientation_genus_index][species_index];
            let inverse_addr_species: &Species<FullAddr> =
                &library.inverse_addrs[reorientation_genus_index][species_index];
            let pent_puzzle_state: PuzzleState = {
                let mut solved_state: PuzzleState = PuzzleState::SOLVED_STATE;

                for (simple_pent_index, simple_rotation_index) in
                    &reorientation_tests[species_index]
                {
                    solved_state +=
                        &simple_transformation_genus[*simple_pent_index][*simple_rotation_index];
                }

                solved_state
            };

            if !pent_puzzle_state.is_valid() {
                log::error!("Puzzle state for pentagon {} isn't valid", species_index);
                error_expr!(pent_puzzle_state);

                panic!();
            }

            for organism_index in 0_usize..Library::ORGANISMS_PER_SPECIES {
                let transformation: &Transformation = &transformation_species[organism_index];
                let inverse_addr: &FullAddr = &inverse_addr_species[organism_index];

                let prev_puzzle_state: PuzzleState = {
                    let mut pent_puzzle_state_clone: PuzzleState = pent_puzzle_state.clone();

                    pent_puzzle_state_clone +=
                        &simple_transformation_genus[species_index][organism_index];

                    pent_puzzle_state_clone
                };

                if !prev_puzzle_state.is_valid() {
                    log::error!(
                        "Puzzle state isn't valid before reorientation with pentagon {} and \
                            rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(
                        pent_puzzle_state,
                        simple_transformation_genus[species_index][organism_index],
                        prev_puzzle_state
                    );

                    panic!();
                }

                if prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET]
                    != species_index as PieceStateComponent
                    || prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET]
                        != organism_index as PieceStateComponent
                {
                    log::error!(
                        "Reorientation with pentagon {} and rotation {} has position {} and \
                            rotation {} for piece {}",
                        species_index,
                        organism_index,
                        prev_puzzle_state.pos[PENTAGON_INDEX_OFFSET],
                        prev_puzzle_state.rot[PENTAGON_INDEX_OFFSET],
                        PENTAGON_INDEX_OFFSET
                    );
                    error_expr!(prev_puzzle_state);

                    panic!();
                }

                let standardization_full_addr: FullAddr =
                    prev_puzzle_state.standardization_full_addr();

                let mut curr_puzzle_state: PuzzleState = prev_puzzle_state.clone();
                let mut curr_puzzle_state_alt: PuzzleState = prev_puzzle_state.clone();
                let mut reoriented_solved_state: PuzzleState = PuzzleState::SOLVED_STATE;
                let mut reoriented_solved_state_alt: PuzzleState = PuzzleState::SOLVED_STATE;

                reoriented_solved_state += transformation;

                if reoriented_solved_state.standardization_full_addr() != standardization_full_addr
                {
                    log::error!(
                        "Reoriented solved state's standardization address doesn't match what's \
                            expected after reorientation with pentagon {} and rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(transformation, reoriented_solved_state, prev_puzzle_state);

                    panic!();
                }

                if !reoriented_solved_state.is_valid() {
                    log::error!(
                        "Puzzle state isn't valid after reorientation with pentagon {} and \
                            rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(prev_puzzle_state, transformation, curr_puzzle_state);

                    panic!();
                }

                reoriented_solved_state_alt.naive_add_assign(transformation);

                if reoriented_solved_state_alt != reoriented_solved_state {
                    log::error!(
                        "Reoriented puzzle state doesn't match the naively reoriented puzzle state \
                            with pentagon {} and rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(
                        transformation,
                        reoriented_solved_state,
                        reoriented_solved_state_alt
                    );

                    panic!();
                }

                if standardization_full_addr != *inverse_addr {
                    log::error!(
                        "Standardization address for current state doesn't match inverse address \
                            with pentagon {} and rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(prev_puzzle_state, standardization_full_addr, inverse_addr);

                    panic!();
                }

                // Reorientation was originally in the other direction, and the standardization check only works for the
                // other direction, so just run the rest how the test was originally conducted

                let word_pack_transformation: &Transformation =
                    library.transformations.get_word(standardization_full_addr);
                let word_pack_mask: &FullMask =
                    library.full_masks.get_word(standardization_full_addr);

                curr_puzzle_state += word_pack_transformation;

                if !curr_puzzle_state.is_standardized() {
                    log::error!(
                        "Puzzle state isn't standardized after reorientation with pentagon {} and \
                            rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(
                        prev_puzzle_state,
                        word_pack_transformation,
                        curr_puzzle_state
                    );

                    panic!();
                }

                if !curr_puzzle_state.is_valid() {
                    log::error!(
                        "Puzzle state isn't valid after reorientation with pentagon {} and \
                            rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(
                        prev_puzzle_state,
                        word_pack_transformation,
                        curr_puzzle_state
                    );

                    panic!();
                }

                curr_puzzle_state_alt.naive_add_assign(word_pack_transformation);

                if curr_puzzle_state_alt != curr_puzzle_state {
                    log::error!(
                        "Reoriented puzzle state doesn't match the naively reoriented puzzle state \
                            with pentagon {} and rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(
                        prev_puzzle_state,
                        word_pack_transformation,
                        curr_puzzle_state,
                        curr_puzzle_state_alt
                    );

                    panic!();
                }

                let transformation_from_states: Transformation =
                    &curr_puzzle_state - &prev_puzzle_state;

                if transformation_from_states != *word_pack_transformation {
                    log::error!(
                        "Transformation from previous to current state doesn't match applied \
                            transformation with pentagon {} and rotation {}",
                        species_index,
                        organism_index
                    );
                    error_expr!(
                        word_pack_transformation,
                        prev_puzzle_state,
                        curr_puzzle_state,
                        transformation_from_states
                    );

                    panic!();
                }

                for piece_index in PIECE_RANGE {
                    if word_pack_mask.affects_piece(piece_index)
                        != (curr_puzzle_state.pos[piece_index]
                            != prev_puzzle_state.pos[piece_index]
                            || curr_puzzle_state.rot[piece_index]
                                != prev_puzzle_state.rot[piece_index])
                    {
                        log::error!(
                            "The mask's affects_piece() result doesn't match reality for piece {} \
                                with pentagon {} and rotation {}",
                            piece_index,
                            species_index,
                            organism_index
                        );
                        error_expr!(
                            word_pack_mask.affects_piece(piece_index),
                            prev_puzzle_state,
                            curr_puzzle_state
                        );

                        panic!();
                    }
                }
            }
        }
    }

    fn test_simples() {
        miri_echo!();

        let library: &Library = &*Library::get();
        let simple_genus_index: usize = GenusIndex::SIMPLE.into();
        let transformation_genus: &Genus<Transformation> =
            &library.transformations[simple_genus_index];
        let inverse_addr_genus: &Genus<FullAddr> = &library.inverse_addrs[simple_genus_index];

        for species_index in 0_usize..Library::SPECIES_PER_GENUS {
            let transformation_species: &Species<Transformation> =
                &transformation_genus[species_index];
            let inverse_addr_species: &Species<FullAddr> = &inverse_addr_genus[species_index];

            for organism_index in 0_usize..Library::ORGANISMS_PER_SPECIES {
                let transformation: &Transformation = &transformation_species[organism_index];
                let inverse_addr: &FullAddr = &inverse_addr_species[organism_index];
                let inverse_transformation: &Transformation =
                    transformation_genus.get_word(*inverse_addr);
                let mut curr_puzzle_state: PuzzleState = PuzzleState::SOLVED_STATE;
                let mut curr_puzzle_state_alt: PuzzleState = PuzzleState::SOLVED_STATE;

                for turn in 1_usize..=usize::PENTAGON_VERTEX_COUNT {
                    let prev_puzzle_state: PuzzleState = curr_puzzle_state.clone();

                    curr_puzzle_state += transformation;
                    curr_puzzle_state_alt.naive_add_assign(transformation);

                    if curr_puzzle_state_alt != curr_puzzle_state {
                        log::error!(
                            "curr_puzzle_state_alt != curr_puzzle_state after turn {} with \
                                pentagon {} and rotation {}",
                            turn,
                            species_index,
                            organism_index
                        );
                        error_expr!(
                            prev_puzzle_state,
                            transformation,
                            curr_puzzle_state,
                            curr_puzzle_state_alt
                        );

                        panic!();
                    }

                    if !curr_puzzle_state.is_valid() {
                        log::error!(
                            "Puzzle state isn't valid after turn {} with pentagon {} and rotation \
                                {}",
                            turn,
                            species_index,
                            organism_index
                        );
                        error_expr!(prev_puzzle_state, transformation, curr_puzzle_state);

                        panic!();
                    }

                    let mut prev_puzzle_state_from_inverse_transformation =
                        curr_puzzle_state.clone();

                    prev_puzzle_state_from_inverse_transformation += inverse_transformation;

                    if prev_puzzle_state_from_inverse_transformation != prev_puzzle_state {
                        log::error!(
                            "prev_puzzle_state_from_inverse_transformation != prev_puzzle_state \
                                after turn {} with pentagon {} and rotation {}",
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
    fn test_transformation_library() {
        miri_echo!();
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
