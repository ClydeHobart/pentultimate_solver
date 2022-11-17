pub use self::library::{
    Class, Genus, GenusIndex, GenusIndexBitArray, GenusIndexString, GenusIndexType, GenusRange,
    LargeGenus, Library, LibraryOrganismRef, LibraryRef, Organism, SmallClass, Species,
};

use {
    super::inflated::{MutPosAndRot, PieceStateComponent, PosAndRot, PuzzleState},
    crate::{
        app::prelude::*,
        math::polyhedra::{
            data::{Data, FaceData},
            Polyhedron,
        },
        piece::consts::*,
        preferences::AnimationSpeedData,
        prelude::*,
        ui::input::PuzzleActionType,
        util::StaticDataLibrary,
    },
    bevy::prelude::*,
    bevy_inspector_egui::{options::NumberAttributes, Context, Inspectable},
    bitvec::prelude::*,
    egui::{Grid, Ui},
    rand::{rngs::ThreadRng, Rng},
    serde::{Deserialize, Serialize},
    std::{
        cmp::max,
        convert::{AsMut, AsRef, TryFrom},
        fmt::{Debug, Error, Formatter, Write},
        mem::transmute,
        ops::{Add, AddAssign, Neg, Range, Sub, SubAssign},
        time::Duration,
    },
};

pub mod library;

pub type HalfMask = u32;

const_assert!(usize::PIECE_COUNT <= HalfMask::BITS as usize);

#[derive(Clone, Copy, Default)]
pub struct FullMask {
    pub affected_poses: HalfMask,
    pub affected_pieces: HalfMask,
}

impl FullMask {
    #[inline]
    pub fn affects_pos(&self, piece_index: usize) -> bool {
        self.affected_poses & ((1 as HalfMask) << piece_index) != 0 as HalfMask
    }

    #[inline]
    pub fn affects_piece(&self, piece_index: usize) -> bool {
        self.affected_pieces & ((1 as HalfMask) << piece_index) != 0 as HalfMask
    }

    pub fn affected_poses_string(&self) -> String {
        self.affected_poses.as_bit_string_with_chars('·', '#')
    }

    pub fn affected_pieces_string(&self) -> String {
        self.affected_pieces.as_bit_string_with_chars('·', '#')
    }

    fn from_pentagon_index(pentagon_index: usize) -> Self {
        if pentagon_index >= usize::PENTAGON_PIECE_COUNT {
            log::warn!(
                "HemisphereMask::new({}) was called, but {} is an invalid pentagon index",
                pentagon_index,
                pentagon_index
            );

            Self::default()
        } else {
            let data: &Data = Data::get(Polyhedron::Icosidodecahedron);
            let faces: &Vec<FaceData> = &data.faces;
            let face_normal: Vec3 = faces[pentagon_index].norm;

            let mut half_mask: HalfMask = HalfMask::default();

            for (pent_index, face) in faces.iter().enumerate() {
                half_mask |= ((face.norm.dot(face_normal) > 0.0_f32) as HalfMask) << pent_index;
            }

            Self {
                affected_poses: half_mask,
                affected_pieces: half_mask,
            }
        }
    }
}

impl Debug for FullMask {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("FullMask")
            .field("affected_poses ", &self.affected_poses_string())
            .field("affected_pieces", &self.affected_pieces_string())
            .finish()
    }
}

impl<'p, 'r> From<PosAndRot<'p, 'r>> for FullMask {
    fn from((pos, rot): PosAndRot) -> Self {
        let mut full_mask: Self = Self::default();

        for piece_index in PIECE_RANGE {
            let affects_pos: bool = pos[piece_index] as usize != piece_index;

            full_mask.affected_poses |= (affects_pos as HalfMask) << piece_index;
            full_mask.affected_pieces |=
                ((affects_pos || rot[piece_index] != 0 as PieceStateComponent) as HalfMask)
                    << piece_index;
        }

        full_mask
    }
}

pub trait Addr {
    fn get_genus_index(&self) -> usize {
        unimplemented!();
    }
    fn get_species_index(&self) -> usize {
        unimplemented!();
    }
    fn get_species_large_index(&self) -> usize {
        unimplemented!();
    }
    fn get_organism_index(&self) -> usize {
        unimplemented!();
    }

    fn set_genus_index(&mut self, genus_index: usize) -> &mut Self {
        #![allow(unused_variables)]
        unimplemented!();
    }

    fn set_species_index(&mut self, species_index: usize) -> &mut Self {
        #![allow(unused_variables)]
        unimplemented!();
    }

    fn set_species_large_index(&mut self, species_large_index: usize) -> &mut Self {
        #![allow(unused_variables)]
        unimplemented!();
    }

    fn set_organism_index(&mut self, organism_index: usize) -> &mut Self {
        #![allow(unused_variables)]
        unimplemented!();
    }
}

#[derive(Clone, Copy, Deserialize, PartialEq, Serialize)]
pub struct HalfAddr(u8);

impl HalfAddr {
    pub const ORIGIN: HalfAddr = HalfAddr::new(0_usize, 0_usize);

    #[inline(always)]
    pub fn as_reorientation(self) -> FullAddr {
        FullAddr::from((GenusIndex::REORIENTATION, self))
    }

    #[inline(always)]
    pub fn as_simple(self) -> FullAddr {
        FullAddr::from((GenusIndex::SIMPLE, self))
    }

    pub fn invalidate(&mut self) {
        self.0 = Self::INVALID;
    }

    #[inline(always)]
    pub fn is_valid(&self) -> bool {
        self.0 != Self::INVALID && self.organism_index_is_valid()
    }

    #[inline(always)]
    pub fn species_index_is_valid(&self) -> bool {
        Self::is_valid_species_index(unsafe { self.get_species_index_unchecked() })
    }

    #[inline(always)]
    pub fn organism_index_is_valid(&self) -> bool {
        Self::is_valid_organism_index(unsafe { self.get_organism_index_unchecked() })
    }

    #[inline(always)]
    pub fn get_orientation(self) -> LibraryOrganismRef<Quat> {
        Library::get_orientation(self)
    }

    pub const fn default() -> Self {
        Self(Self::INVALID)
    }

    #[inline(always)]
    pub const fn is_valid_species_index(species_index: usize) -> bool {
        species_index < Library::SPECIES_PER_GENUS
    }

    #[inline(always)]
    pub const fn is_valid_species_large_index(species_large_index: usize) -> bool {
        species_large_index < Library::SPECIES_PER_LARGE_GENUS
    }

    #[inline(always)]
    pub const fn is_valid_organism_index(organism_index: usize) -> bool {
        organism_index < Library::ORGANISMS_PER_SPECIES
    }

    pub const fn new(species_index: usize, organism_index: usize) -> Self {
        if Self::is_valid_species_large_index(species_index)
            && Self::is_valid_organism_index(organism_index)
        {
            Self((species_index as u8) << Self::SPECIES_INDEX_BITS.start | organism_index as u8)
        } else {
            Self::default()
        }
    }

    const INVALID: u8 = u8::MAX;
    const SPECIES_INDEX_BITS: Range<usize> = 3_usize..u8::BITS as usize;
    const ORGANISM_INDEX_BITS: Range<usize> = 0_usize..3_usize;
    const ORGANISM_INDEX_MASK: u8 = (1_u8 << HalfAddr::ORGANISM_INDEX_BITS.end) - 1_u8;

    #[inline(always)]
    const unsafe fn get_species_index_unchecked(&self) -> usize {
        (self.0 >> Self::SPECIES_INDEX_BITS.start) as usize
    }

    #[inline(always)]
    const unsafe fn get_organism_index_unchecked(&self) -> usize {
        (self.0 & Self::ORGANISM_INDEX_MASK) as usize
    }
}

impl Add for HalfAddr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        self + rhs.as_reorientation()
    }
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
    /// The resultant orientation `HalfAddr` representing where the `self` orientation is mapped to
    /// after performing the `rhs` transformation.
    fn add(self, rhs: FullAddr) -> Self {
        if self.is_valid() && rhs.is_valid() {
            let mut sum: Self = rhs
                .get_transformation()
                .try_get()
                .unwrap()
                .as_ref()
                .half_addr(self.get_species_index());

            *sum.set_organism_index(
                (sum.get_organism_index() + self.get_organism_index())
                    % Library::ORGANISMS_PER_SPECIES,
            )
        } else {
            Self::default()
        }
    }
}

impl AddAssign for HalfAddr {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl AddAssign<FullAddr> for HalfAddr {
    fn add_assign(&mut self, rhs: FullAddr) {
        *self = *self + rhs;
    }
}

impl Addr for HalfAddr {
    fn get_species_index(&self) -> usize {
        break_assert!(self.is_valid() && self.species_index_is_valid());

        // Safe: checked above
        unsafe { self.get_species_index_unchecked() }
    }

    fn get_species_large_index(&self) -> usize {
        const_assert!(
            Library::SPECIES_PER_LARGE_GENUS
                <= 1_usize
                    << (HalfAddr::SPECIES_INDEX_BITS.end - HalfAddr::SPECIES_INDEX_BITS.start)
        );
        break_assert!(self.is_valid());

        // Safe: checked above (const_assert! shows the species large index is always valid)
        unsafe { self.get_species_index_unchecked() }
    }

    fn get_organism_index(&self) -> usize {
        break_assert!(self.is_valid());

        // Safe: checked above
        unsafe { self.get_organism_index_unchecked() }
    }

    fn set_species_index(&mut self, species_index: usize) -> &mut Self {
        break_assert!(Self::is_valid_species_index(species_index));
        self.0.view_bits_mut::<Lsb0>()[Self::SPECIES_INDEX_BITS].store(species_index as u8);

        self
    }

    fn set_species_large_index(&mut self, species_large_index: usize) -> &mut Self {
        /* This assert is unnecessary, since the call to set_bits() would panic if this weren't the
        case, but it makes the issue more clear */
        break_assert!(Self::is_valid_species_large_index(species_large_index));
        self.0.view_bits_mut::<Lsb0>()[Self::SPECIES_INDEX_BITS].store(species_large_index as u8);

        self
    }

    fn set_organism_index(&mut self, organism_index: usize) -> &mut Self {
        break_assert!(Self::is_valid_organism_index(organism_index));
        self.0.view_bits_mut::<Lsb0>()[Self::ORGANISM_INDEX_BITS].store(organism_index as u8);

        self
    }
}

impl From<(usize, usize)> for HalfAddr {
    fn from((species_index, organism_index): (usize, usize)) -> Self {
        Self::new(species_index, organism_index)
    }
}

impl From<u8> for HalfAddr {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

pub struct RandHalfAddrParams<'a> {
    pub thread_rng: &'a mut ThreadRng,
    pub allow_species_large_indices: bool,
}

impl<'a> From<RandHalfAddrParams<'a>> for HalfAddr {
    fn from(rand_half_addr_params: RandHalfAddrParams<'a>) -> Self {
        const SPECIES_PER_LARGE_GENUS_F32: f32 = Library::SPECIES_PER_LARGE_GENUS as f32;
        const SPECIES_PER_GENUS_F32: f32 = Library::SPECIES_PER_GENUS as f32;
        const ORGANISMS_PER_SPECIES_F32: f32 = Library::ORGANISMS_PER_SPECIES as f32;

        *Self::default()
            .set_species_large_index(
                (rand_half_addr_params.thread_rng.gen::<f32>()
                    * if rand_half_addr_params.allow_species_large_indices {
                        SPECIES_PER_LARGE_GENUS_F32
                    } else {
                        SPECIES_PER_GENUS_F32
                    }) as usize,
            )
            .set_organism_index(
                (rand_half_addr_params.thread_rng.gen::<f32>() * ORGANISMS_PER_SPECIES_F32)
                    as usize,
            )
    }
}

impl From<HalfAddr> for u8 {
    fn from(value: HalfAddr) -> Self {
        value.0
    }
}

impl Debug for HalfAddr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("HalfAddr")
            .field("species_index", unsafe {
                &self.get_species_index_unchecked()
            })
            .field("organism_index", unsafe {
                &self.get_organism_index_unchecked()
            })
            .finish()
    }
}

impl Default for HalfAddr {
    fn default() -> Self {
        HalfAddr::default()
    }
}

#[derive(Clone, Copy, Default)]
pub struct HalfAddrAttrs {
    pub allow_species_large_indices: bool,
}

impl Inspectable for HalfAddr {
    type Attributes = HalfAddrAttrs;

    fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, context: &mut Context) -> bool {
        let mut changed: bool = false;

        ui.vertical_centered(|ui: &mut Ui| {
            Grid::new(context.id()).show(ui, |ui: &mut Ui| {
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
                    ui.label("species_index");

                    let mut species_index: usize = self.get_species_index();

                    if species_index.ui(
                        ui,
                        NumberAttributes::<usize>::between(
                            0_usize,
                            if options.allow_species_large_indices {
                                Library::SPECIES_PER_LARGE_GENUS
                            } else {
                                Library::SPECIES_PER_GENUS
                            } - 1_usize,
                        ),
                        &mut context.with_id(0_u64),
                    ) {
                        self.set_species_large_index(species_index);
                        changed = true;
                    }

                    ui.end_row();
                    ui.label("organism_index");

                    let mut organism_index: usize = self.get_organism_index();

                    if organism_index.ui(
                        ui,
                        NumberAttributes::<usize>::between(
                            0_usize,
                            Library::ORGANISMS_PER_SPECIES - 1_usize,
                        ),
                        &mut context.with_id(1_u64),
                    ) {
                        self.set_organism_index(organism_index);
                        changed = true;
                    }

                    ui.end_row();
                } else {
                    let mut filler_index: i32 = -1_i32;

                    ui.set_enabled(false);
                    ui.label("species_index");
                    filler_index.ui(
                        ui,
                        NumberAttributes::<i32>::default(),
                        &mut context.with_id(0_u64),
                    );
                    ui.end_row();
                    ui.label("organism_index");
                    filler_index.ui(
                        ui,
                        NumberAttributes::<i32>::default(),
                        &mut context.with_id(1_u64),
                    );
                    ui.end_row();
                }
            });
        });

        changed
    }
}

impl Neg for HalfAddr {
    type Output = Self;

    fn neg(self) -> Self::Output {
        *self.as_reorientation().get_inverse_addr().get_half_addr()
    }
}

impl Sub for HalfAddr {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        self + (-rhs)
    }
}

#[derive(Clone, Copy, Deserialize, PartialEq, Serialize)]
pub struct FullAddr {
    genus_index: GenusIndex,
    half_addr: HalfAddr,
}

impl FullAddr {
    #[inline(always)]
    pub fn get_simple_slice(self) -> LibraryOrganismRef<[HalfAddr]> {
        Library::get_simple_slice(self)
    }

    pub fn get_simple_slice_string(self) -> String {
        let mut simple_slice_string: String = String::new();

        for (simple_index, simple) in self
            .get_simple_slice()
            .try_get()
            .unwrap()
            .iter()
            .enumerate()
        {
            write!(
                simple_slice_string,
                "{0}{1:\t>2$}ha!({3},\t{4}),",
                if simple_index & 0b11 == 0_usize {
                    "\n"
                } else {
                    ""
                },
                "",
                if simple_index & 0b11 == 0_usize {
                    4_usize
                } else {
                    1_usize
                },
                simple.get_species_index(),
                simple.get_organism_index()
            )
            .unwrap();
        }

        simple_slice_string
    }

    pub fn get_cycles(self) -> u32 {
        if self.is_genus_index_reorientation() {
            1_u32
        } else {
            Self::get_simple_slice_cycles(self.get_simple_slice().try_get().unwrap())
        }
    }

    #[inline(always)]
    pub fn get_half_addr(&self) -> &HalfAddr {
        &self.half_addr
    }

    pub fn try_get_genus_index(self) -> Option<GenusIndex> {
        if self.genus_index_is_valid() {
            Some(self.genus_index)
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn half_addr_is_valid(&self) -> bool {
        Self::is_valid_half_addr(self.half_addr)
    }

    pub fn invalidate(&mut self) {
        *self = Self::default()
    }

    pub fn invalidate_genus_index(&mut self) {
        self.genus_index = GenusIndex::INVALID;
    }

    pub fn invalidate_half_addr(&mut self) {
        self.half_addr.invalidate();
    }

    pub fn get_inverse_addr(self) -> Self {
        if self.is_valid() {
            *Library::get_inverse_addr(self).try_get().unwrap()
        } else {
            Self::default()
        }
    }

    pub fn is_identity_transformation(self) -> bool {
        match self.try_get_genus_index() {
            Some(GenusIndex::REORIENTATION) => *self.get_half_addr() == HalfAddr::ORIGIN,
            Some(GenusIndex::SIMPLE) => self.get_organism_index() == 0_usize,
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_genus_index_reorientation(&self) -> bool {
        self.genus_index.is_reorientation()
    }

    #[inline(always)]
    pub fn is_genus_index_simple(&self) -> bool {
        self.genus_index.is_simple()
    }

    #[inline(always)]
    pub fn is_valid(&self) -> bool {
        self.genus_index_is_valid() && self.half_addr_is_valid()
    }

    #[inline(always)]
    pub fn species_index_is_valid(&self) -> bool {
        self.half_addr.species_index_is_valid()
    }

    #[inline(always)]
    pub fn get_full_mask(self) -> LibraryOrganismRef<FullMask> {
        Library::get_full_mask(self)
    }

    pub fn mirror(self) -> Self {
        if self.is_valid() {
            let genus_index: GenusIndex = self.try_get_genus_index().unwrap();

            (
                genus_index.mirror(),
                if genus_index.is_complex() {
                    *self.get_half_addr()
                } else {
                    HalfAddr::new(
                        Self::mirror_species_index(self.get_species_index()),
                        Self::invert_organism_index(self.get_organism_index()),
                    )
                },
            )
                .into()
        } else {
            Self::default()
        }
    }

    #[inline(always)]
    pub fn genus_index_is_valid(&self) -> bool {
        self.genus_index.is_valid()
    }

    #[inline(always)]
    pub fn get_rotation(self) -> LibraryOrganismRef<Quat> {
        Library::get_rotation(self)
    }

    pub fn set_half_addr(&mut self, half_addr: HalfAddr) -> &mut FullAddr {
        self.half_addr = half_addr;

        self
    }

    #[inline(always)]
    pub fn standardization(self) -> HalfAddr {
        -(HalfAddr::ORIGIN + self)
    }

    pub fn get_transformation(self) -> LibraryOrganismRef<Transformation> {
        Library::get_transformation(self)
    }

    #[inline(always)]
    pub fn organism_index_is_valid(&self) -> bool {
        self.half_addr.organism_index_is_valid()
    }

    pub fn get_simple_slice_cycles(simple_slice: &[HalfAddr]) -> u32 {
        simple_slice
            .iter()
            .map(|half_addr: &HalfAddr| -> u32 {
                const CYCLES_LUT: [u8; Library::ORGANISMS_PER_SPECIES] =
                    [0_u8, 1_u8, 2_u8, 2_u8, 1_u8];

                CYCLES_LUT[half_addr.get_organism_index()] as u32
            })
            .sum()
    }

    #[inline]
    pub const fn invert_organism_index(organism_index: usize) -> usize {
        const ORGANISM_INDEX_LUT: [u8; Library::ORGANISMS_PER_SPECIES] =
            [0_u8, 4_u8, 3_u8, 2_u8, 1_u8];

        ORGANISM_INDEX_LUT[organism_index] as usize
    }

    #[inline(always)]
    pub fn is_valid_half_addr(half_addr: HalfAddr) -> bool {
        half_addr.species_index_is_valid() && half_addr.organism_index_is_valid()
    }

    #[inline]
    pub const fn mirror_species_index(species_index: usize) -> usize {
        const SPECIES_INDEX_LUT: [u8; Library::SPECIES_PER_GENUS] = [
            0_u8, 1_u8, 2_u8, 3_u8, // Unchanged
            5_u8, 4_u8, 7_u8, 6_u8, // ABCD -> BADC
            10_u8, 11_u8, 8_u8, 9_u8, // ABCD -> CDAB
        ];

        SPECIES_INDEX_LUT[species_index] as usize
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
    /// The resultant transformation after applying the `rhs.as_reorientation()` transformation.
    /// This differs from `{ self.half_addr += rhs; self }` because Simple transformations don't
    /// have their organism indices affected by reorientations.
    fn add(self, rhs: HalfAddr) -> Self::Output {
        let mut sum: Self = self;

        sum.half_addr += rhs;

        if self.is_genus_index_simple() {
            sum.set_organism_index(self.get_organism_index());
        }

        sum
    }
}

impl AddAssign<HalfAddr> for FullAddr {
    fn add_assign(&mut self, rhs: HalfAddr) {
        *self = *self + rhs;
    }
}

impl Addr for FullAddr {
    fn get_genus_index(&self) -> usize {
        break_assert!(self.genus_index_is_valid());

        self.genus_index.into()
    }

    fn get_species_index(&self) -> usize {
        // Asserts in HalfAddr::get_line_index()
        self.half_addr.get_species_index()
    }

    fn get_organism_index(&self) -> usize {
        // Asserts in HalfAddr::get_word_index()
        self.half_addr.get_organism_index()
    }

    fn set_genus_index(&mut self, genus_index: usize) -> &mut FullAddr {
        // Asserts on unwrap()
        self.genus_index = GenusIndex::try_from(genus_index as GenusIndexType).unwrap();

        self
    }

    fn set_species_index(&mut self, species_index: usize) -> &mut FullAddr {
        // Asserts in HalfAddr::set_line_index()
        self.half_addr.set_species_index(species_index);

        self
    }

    fn set_organism_index(&mut self, organism_index: usize) -> &mut FullAddr {
        // Asserts in HalfAddr::set_word_index()
        self.half_addr.set_organism_index(organism_index);

        self
    }
}

impl Debug for FullAddr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct: std::fmt::DebugStruct = formatter.debug_struct("FullAddr");

        debug_struct.field(
            "genus_index",
            &self.try_get_genus_index().unwrap_or_default(),
        );

        if self.half_addr.is_valid() && self.half_addr.species_index_is_valid() {
            debug_struct.field("species_index", &self.half_addr.get_species_index());
            debug_struct.field("organism_index", &self.half_addr.get_organism_index());
        } else {
            debug_struct.field("half_addr", &"[invalid]");
        }

        debug_struct.finish()
    }
}

impl Default for FullAddr {
    fn default() -> Self {
        Self {
            genus_index: GenusIndex::INVALID,
            half_addr: HalfAddr::default(),
        }
    }
}

impl From<(usize, usize, usize)> for FullAddr {
    fn from((genus_index, species_index, organism_index): (usize, usize, usize)) -> Self {
        Self {
            genus_index: GenusIndex::try_from(
                GenusIndexType::try_from(genus_index).unwrap_or(GenusIndexType::MAX),
            )
            .unwrap_or_default(),
            half_addr: if HalfAddr::is_valid_species_index(species_index)
                && HalfAddr::is_valid_organism_index(organism_index)
            {
                HalfAddr::new(species_index, organism_index)
            } else {
                HalfAddr::default()
            },
        }
    }
}

impl From<GenusIndex> for FullAddr {
    fn from(genus_index: GenusIndex) -> Self {
        Self {
            genus_index: if genus_index.is_valid() {
                genus_index
            } else {
                GenusIndex::INVALID
            },
            ..Self::default()
        }
    }
}

impl From<HalfAddr> for FullAddr {
    fn from(half_addr: HalfAddr) -> Self {
        Self {
            half_addr: if Self::is_valid_half_addr(half_addr) {
                half_addr
            } else {
                HalfAddr::default()
            },
            ..Self::default()
        }
    }
}

impl From<(GenusIndex, HalfAddr)> for FullAddr {
    fn from((genus_index, half_addr): (GenusIndex, HalfAddr)) -> Self {
        Self {
            genus_index: if genus_index.is_valid() {
                genus_index
            } else {
                GenusIndex::INVALID
            },
            half_addr: if Self::is_valid_half_addr(half_addr) {
                half_addr
            } else {
                HalfAddr::default()
            },
        }
    }
}

impl From<u16> for FullAddr {
    fn from(value: u16) -> Self {
        unsafe { transmute::<u16, Self>(value) }
    }
}

impl From<FullAddr> for u16 {
    fn from(value: FullAddr) -> Self {
        unsafe { transmute::<FullAddr, Self>(value) }
    }
}

impl Sub<HalfAddr> for FullAddr {
    type Output = Self;

    fn sub(self, rhs: HalfAddr) -> Self::Output {
        self + (-rhs)
    }
}

impl SubAssign<HalfAddr> for FullAddr {
    fn sub_assign(&mut self, rhs: HalfAddr) {
        *self = *self - rhs;
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct Action {
    pub transformation: FullAddr,
    pub camera_start: HalfAddr,
}

impl Action {
    pub fn new(transformation: FullAddr, camera_start: HalfAddr) -> Self {
        Self {
            transformation,
            camera_start,
        }
    }

    pub fn compute_duration(
        &self,
        animation_speed_data: &AnimationSpeedData,
        action_type: PuzzleActionType,
    ) -> Duration {
        if matches!(action_type, PuzzleActionType::Undo | PuzzleActionType::Redo)
            && !animation_speed_data.animate_undo_and_redo
        {
            Duration::ZERO
        } else {
            Duration::from_millis(animation_speed_data.rotation_millis as u64)
                * if animation_speed_data.uniform_transformation_duration
                    || !self.transformation.is_valid()
                {
                    1_u32
                } else {
                    max(self.transformation.get_cycles(), 1_u32)
                }
        }
    }

    pub fn get_camera_end(&self) -> HalfAddr {
        if self.transformation.is_genus_index_reorientation() {
            *self.transformation.get_half_addr()
        } else {
            self.camera_start
        }
    }

    pub fn get_standardized_camera_end(&self) -> HalfAddr {
        self.get_camera_end() + self.standardization()
    }

    pub fn invert(&self) -> Self {
        if self.is_valid() {
            if self.transformation.is_genus_index_reorientation() {
                Self::new(
                    self.camera_start.as_reorientation(),
                    *self.transformation.get_half_addr(),
                )
            } else {
                let standardization: HalfAddr = self.standardization();

                Self::new(
                    self.transformation.get_inverse_addr() + standardization,
                    self.get_camera_end() + standardization,
                )
            }
        } else {
            Self::default()
        }
    }

    pub fn is_valid(&self) -> bool {
        self.transformation.is_valid() && self.camera_start.is_valid()
    }

    #[inline(always)]
    pub fn standardization(&self) -> HalfAddr {
        self.transformation.standardization()
    }
}

impl Add<HalfAddr> for Action {
    type Output = Self;

    fn add(self, rhs: HalfAddr) -> Self {
        Self {
            transformation: self.transformation + rhs,
            camera_start: self.camera_start + rhs,
        }
    }
}

pub struct OrientationData {
    pub quat: Quat,
}

#[derive(Default, PartialEq)]
pub struct Transformation(PuzzleState);

impl Transformation {
    pub fn arrays(&self) -> PosAndRot {
        self.0.arrays()
    }

    pub fn arrays_mut(&mut self) -> MutPosAndRot {
        self.0.arrays_mut()
    }

    pub fn is_valid(&self) -> bool {
        self.0.is_valid()
    }
}

impl AsMut<PuzzleState> for Transformation {
    fn as_mut(&mut self) -> &mut PuzzleState {
        &mut self.0
    }
}

impl AsRef<PuzzleState> for Transformation {
    fn as_ref(&self) -> &PuzzleState {
        &self.0
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

pub trait FindWord<T>
where
    T: PartialEq,
{
    fn find_word(&self, word: &Organism<T>) -> Option<FullAddr>;
}

impl<T> FindWord<T> for Organism<T>
where
    T: PartialEq,
{
    fn find_word(&self, target_word: &Organism<T>) -> Option<FullAddr> {
        if self == target_word {
            Some(FullAddr::default())
        } else {
            None
        }
    }
}

impl<T> FindWord<T> for Species<T>
where
    T: PartialEq,
{
    fn find_word(&self, target_word: &Organism<T>) -> Option<FullAddr> {
        for (word_index, word) in self.iter().enumerate() {
            if let Some(mut address) = word.find_word(target_word) {
                return Some(*address.set_organism_index(word_index));
            }
        }

        None
    }
}

impl<T> FindWord<T> for Genus<T>
where
    T: PartialEq,
{
    fn find_word(&self, target_word: &Organism<T>) -> Option<FullAddr> {
        for (line_index, line) in self.iter().enumerate() {
            if let Some(mut address) = line.find_word(target_word) {
                return Some(*address.set_species_index(line_index));
            }
        }

        None
    }
}

pub trait GetWord<A: Addr, T: ?Sized> {
    fn get_word(&self, addr: A) -> &Organism<T>;
    fn get_word_mut(&mut self, addr: A) -> &mut Organism<T>;
}

impl<A: Addr + Sized, T> GetWord<A, T> for Genus<T> {
    fn get_word(&self, addr: A) -> &Organism<T> {
        &self[addr.get_species_index()][addr.get_organism_index()]
    }

    fn get_word_mut(&mut self, addr: A) -> &mut Organism<T> {
        &mut self[addr.get_species_index()][addr.get_organism_index()]
    }
}

impl<A: Addr + Sized, T> GetWord<A, T> for LargeGenus<T> {
    fn get_word(&self, addr: A) -> &Organism<T> {
        &self[addr.get_species_large_index()][addr.get_organism_index()]
    }

    fn get_word_mut(&mut self, addr: A) -> &mut Organism<T> {
        &mut self[addr.get_species_large_index()][addr.get_organism_index()]
    }
}

impl<A: Addr + Sized, T> GetWord<A, T> for SmallClass<T> {
    fn get_word(&self, addr: A) -> &Organism<T> {
        &self[addr.get_genus_index()][addr.get_species_index()][addr.get_organism_index()]
    }

    fn get_word_mut(&mut self, addr: A) -> &mut Organism<T> {
        &mut self[addr.get_genus_index()][addr.get_species_index()][addr.get_organism_index()]
    }
}

impl<A: Addr + Sized, T> GetWord<A, T> for Class<T> {
    fn get_word(&self, addr: A) -> &Organism<T> {
        &self[addr.get_genus_index()][addr.get_species_index()][addr.get_organism_index()]
    }

    fn get_word_mut(&mut self, addr: A) -> &mut Organism<T> {
        &mut self[addr.get_genus_index()][addr.get_species_index()][addr.get_organism_index()]
    }
}

pub struct TransformationPlugin;

impl TransformationPlugin {
    pub fn startup() {
        Library::build();
    }
}

impl Plugin for TransformationPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(Self::startup.after(PolyhedraDataPlugin::startup));
    }
}
