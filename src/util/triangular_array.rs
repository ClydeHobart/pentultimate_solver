use {
    crate::prelude::DefaultArray,
    std::{
        convert::{AsMut, TryFrom},
        marker::PhantomData,
        mem::{transmute, MaybeUninit},
        ops::{Index as IndexTrait, IndexMut},
    },
    strum_macros::EnumIter,
};

pub const fn triangle_number(n: usize) -> usize {
    n * (n + 1_usize) / 2_usize
}
pub struct Helpers<const SL: usize>;

impl<const SL: usize> Helpers<SL> {
    pub const SIDE_LEN: usize = SL;

    pub const SIDE_LEN_MINUS_1: usize = Self::SIDE_LEN - 1_usize;
    // Invariant: for any cell in the array, i + j + k == 2_usize * (Self::SIDE_LEN - 1_usize)
    pub const INDEX_SUM: usize = 2_usize * Self::SIDE_LEN_MINUS_1;

    pub const FULL_SIZE: usize = triangle_number(Self::SIDE_LEN);

    pub const TRI_COUNT: usize =
        2_usize * triangle_number(Self::SIDE_LEN_MINUS_1) - Self::SIDE_LEN_MINUS_1;

    pub const fn other_index(index_1: usize, index_2: usize) -> usize {
        Self::INDEX_SUM - index_1 - index_2
    }
}

pub trait UseHelpers {
    type Helpers;
}

#[macro_export]
macro_rules! Helpers {
    ($t:ty) => {
        <$t as UseHelpers>::Helpers
    };
}

#[derive(Clone)]
pub struct TriangularArray<T: Clone + Default, const SL: usize, const FS: usize>([T; FS]);

impl<T: Clone + Default, const SL: usize, const FS: usize> TriangularArray<T, SL, FS> {
    #[cfg(debug_assertions)]
    pub const SIDE_LEN_IS_AT_LEAST_1: u8 = 1_u8 / if SL >= 1_usize { 1_u8 } else { 0_u8 };

    pub const fn side_len() -> usize {
        <Helpers!(Self)>::SIDE_LEN
    }

    pub const fn side_len_minus_1() -> usize {
        <Helpers!(Self)>::SIDE_LEN_MINUS_1
    }

    pub fn init_border<I: Iterator<Item = T>>(
        &mut self,
        index_map: &IndexMap<SL>,
        border: IndexType2D,
        iter: I,
    ) -> () {
        for (index, t) in Index::<SL>::corner(border).iter_row_indices().zip(iter) {
            self.0[index_map[&index]] = t;
        }
    }
}

impl<T: Clone + Default, const SL: usize, const FS: usize> AsMut<TriangularArray<T, SL, FS>>
    for [T; FS]
{
    fn as_mut(&mut self) -> &mut TriangularArray<T, SL, FS> {
        unsafe { transmute::<&mut Self, &mut TriangularArray<T, SL, FS>>(self) }
    }
}

impl<T: Clone + Default, const SL: usize, const FS: usize> Default for TriangularArray<T, SL, FS> {
    fn default() -> Self {
        #[cfg(debug_assertions)]
        let _: u8 = Self::SIDE_LEN_IS_AT_LEAST_1;

        Self(<[T; FS]>::default_array())
    }
}

impl<'a, T: Clone + Default, const SL: usize, const FS: usize>
    IndexMut<(&'a IndexMap<SL>, &'a Index<SL>)> for TriangularArray<T, SL, FS>
{
    fn index_mut(
        &mut self,
        (index_map, index): (&'a IndexMap<SL>, &'a Index<SL>),
    ) -> &mut Self::Output {
        &mut self.0[index_map[index]]
    }
}

impl<T: Clone + Default, const SL: usize, const FS: usize> IndexMut<usize>
    for TriangularArray<T, SL, FS>
{
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<'a, T: Clone + Default, const SL: usize, const FS: usize>
    IndexTrait<(&'a IndexMap<SL>, &'a Index<SL>)> for TriangularArray<T, SL, FS>
{
    type Output = T;

    fn index(&self, (index_map, index): (&'a IndexMap<SL>, &'a Index<SL>)) -> &Self::Output {
        &self.0[index_map[index]]
    }
}

impl<T: Clone + Default, const SL: usize, const FS: usize> IndexTrait<usize>
    for TriangularArray<T, SL, FS>
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<T: Clone + Default, const SL: usize, const FS: usize> UseHelpers
    for TriangularArray<T, SL, FS>
{
    type Helpers = Helpers<SL>;
}

#[macro_export]
macro_rules! TriangularArray {
    ($t:ty, $side_len:expr) => {
        TriangularArray<$t, { $side_len }, { triangle_number($side_len) }> };
}

#[derive(Clone, Copy, Debug, EnumIter, PartialEq)]
#[repr(u8)]
pub enum IndexType2D {
    IJ,
    JK,
    KI,
}

impl IndexType2D {
    pub const COUNT: u8 = 3_u8;

    fn next(self) -> Self {
        unsafe { transmute::<u8, Self>((self as u8 + 1_u8) % Self::COUNT) }
    }

    fn prev(self) -> Self {
        unsafe { transmute::<u8, Self>((self as u8 + Self::COUNT - 1_u8) % Self::COUNT) }
    }
}

impl TryFrom<u8> for IndexType2D {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, ()> {
        if value < Self::COUNT {
            Ok(unsafe { transmute::<u8, Self>(value) })
        } else {
            Err(())
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Index<const SL: usize> {
    pub index_type: IndexType2D,
    pub index_1: usize,
    pub index_2: usize,
    _pd: PhantomData<IndexMap<SL>>,
}

impl<const SL: usize> Index<SL> {
    pub fn corner(index_type: IndexType2D) -> Self {
        Self {
            index_type,
            index_1: <Helpers!(Self)>::SIDE_LEN_MINUS_1,
            index_2: 0_usize,
            _pd: Default::default(),
        }
    }

    pub fn new(index_type: IndexType2D, index_1: usize, index_2: usize) -> Self {
        Self {
            index_type,
            index_1,
            index_2,
            _pd: Default::default(),
        }
    }

    pub fn iter_row_indices(self) -> RowIndicesIterator<SL> {
        RowIndicesIterator::<SL> {
            index: self,
            end: <Helpers!(Self)>::SIDE_LEN,
        }
    }

    pub fn iter_mapped_row_indices(self, index_map: &IndexMap<SL>) -> MappedRowIndicesIterator<SL> {
        MappedRowIndicesIterator::<SL> {
            index_map,
            iter: self.iter_row_indices(),
        }
    }

    pub fn iter_all_indices() -> AllIndicesIterator<SL> {
        AllIndicesIterator::<SL>::default()
    }

    pub fn iter_mapped_all_indices(index_map: &IndexMap<SL>) -> MappedAllIndicesIterator<SL> {
        MappedAllIndicesIterator::<SL> {
            index_map,
            iter: Self::iter_all_indices(),
        }
    }

    pub fn iter_border_indices() -> BorderIndicesIterator<SL> {
        BorderIndicesIterator::<SL>::default()
    }

    pub fn iter_mapped_border_indices(index_map: &IndexMap<SL>) -> MappedBorderIndicesIterator<SL> {
        MappedBorderIndicesIterator::<SL> {
            index_map,
            iter: Self::iter_border_indices(),
        }
    }

    pub fn iter_mapped_trio_indices(index_map: &IndexMap<SL>) -> MappedTrioIndicesIterator<SL> {
        MappedTrioIndicesIterator::<SL>::new(index_map)
    }

    #[inline(always)]
    pub fn index_1_is_border(&self) -> bool {
        self.index_1 == <Helpers!(Self)>::SIDE_LEN_MINUS_1
    }

    #[inline(always)]
    pub fn index_2_is_border(&self) -> bool {
        self.index_2 == <Helpers!(Self)>::SIDE_LEN_MINUS_1
    }

    #[inline(always)]
    pub fn other_index_is_border(&self) -> bool {
        self.index_1 + self.index_2 == <Helpers!(Self)>::SIDE_LEN_MINUS_1
    }

    pub fn is_border(&self) -> bool {
        self.index_1_is_border() || self.index_2_is_border() || self.other_index_is_border()
    }

    pub fn is_first_corner(&self) -> bool {
        self.index_1_is_border() && self.other_index_is_border()
    }

    pub fn is_last_corner(&self) -> bool {
        self.index_1_is_border() && self.index_2_is_border()
    }

    pub fn next_type(&self) -> Self {
        Self {
            index_type: self.index_type.next(),
            index_1: self.index_2,
            index_2: <Helpers!(Self)>::other_index(self.index_1, self.index_2),
            _pd: self._pd,
        }
    }

    pub fn prev_type(&self) -> Self {
        Self {
            index_type: self.index_type.prev(),
            index_1: <Helpers!(Self)>::other_index(self.index_1, self.index_2),
            index_2: self.index_1,
            _pd: self._pd,
        }
    }

    pub fn first_in_row(&self) -> Self {
        Self {
            index_type: self.index_type,
            index_1: self.index_1,
            index_2: <Helpers!(Self)>::SIDE_LEN_MINUS_1 - self.index_1,
            _pd: self._pd,
        }
    }

    pub fn last_in_row(&self) -> Self {
        Self {
            index_type: self.index_type,
            index_1: self.index_1,
            index_2: <Helpers!(Self)>::SIDE_LEN_MINUS_1,
            _pd: self._pd,
        }
    }

    pub fn prev_in_row(mut self) -> Option<Self> {
        if (<Helpers!(Self)>::SIDE_LEN - self.index_1..<Helpers!(Self)>::SIDE_LEN)
            .contains(&self.index_2)
        {
            self.index_2 -= 1_usize;

            Some(self)
        } else {
            None
        }
    }

    pub fn next_in_row(mut self) -> Option<Self> {
        if self.index_2 < <Helpers!(Self)>::SIDE_LEN_MINUS_1 {
            self.index_2 += 1_usize;

            Some(self)
        } else {
            None
        }
    }
}

impl<const SL: usize> Default for Index<SL> {
    fn default() -> Self {
        Self::corner(IndexType2D::IJ)
    }
}

impl<const SL: usize> UseHelpers for Index<SL> {
    type Helpers = Helpers<SL>;
}

pub struct MappedIndexIterator<'im, I: Iterator<Item = Index<SL>>, const SL: usize> {
    index_map: &'im IndexMap<SL>,
    iter: I,
}

impl<'im, I: Iterator<Item = Index<SL>>, const SL: usize> Iterator
    for MappedIndexIterator<'im, I, SL>
{
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        self.iter
            .next()
            .map(|index: Index<SL>| -> usize { self.index_map[&index] })
    }
}

impl<'im, I: Iterator<Item = Index<SL>> + DoubleEndedIterator, const SL: usize> DoubleEndedIterator
    for MappedIndexIterator<'im, I, SL>
{
    fn next_back(&mut self) -> Option<usize> {
        self.iter
            .next_back()
            .map(|index: Index<SL>| -> usize { self.index_map[&index] })
    }
}

pub struct RowIndicesIterator<const SL: usize> {
    index: Index<SL>,
    end: usize,
}

impl<const SL: usize> DoubleEndedIterator for RowIndicesIterator<SL> {
    fn next_back(&mut self) -> Option<Index<SL>> {
        if self.index.index_2 < self.end {
            self.end -= 1_usize;

            Some(Index::<SL> {
                index_2: self.end,
                ..self.index
            })
        } else {
            None
        }
    }
}

impl<const SL: usize> Iterator for RowIndicesIterator<SL> {
    type Item = Index<SL>;

    fn next(&mut self) -> Option<Index<SL>> {
        if self.index.index_2 < self.end {
            let old_index: Index<SL> = self.index;

            self.index = self.index.next_in_row().unwrap_or_else(|| -> Index<SL> {
                Index::<SL> {
                    index_2: self.end,
                    ..self.index
                }
            });

            Some(old_index)
        } else {
            None
        }
    }
}

pub type MappedRowIndicesIterator<'im, const SL: usize> =
    MappedIndexIterator<'im, RowIndicesIterator<SL>, SL>;

pub struct AllIndicesIterator<const SL: usize> {
    index: Index<SL>,
}

impl<const SL: usize> Default for AllIndicesIterator<SL> {
    fn default() -> Self {
        Self {
            index: Index::<SL>::corner(IndexType2D::KI).next_type(),
        }
    }
}

impl<const SL: usize> Iterator for AllIndicesIterator<SL> {
    type Item = Index<SL>;

    fn next(&mut self) -> Option<Index<SL>> {
        if self.index.index_1 < <Helpers!(Index<SL>)>::SIDE_LEN {
            let old_index: Index<SL> = self.index;

            if !self.index.other_index_is_border() {
                self.index.index_2 -= 1_usize;
            } else {
                self.index.index_1 += 1_usize;
                self.index = self.index.last_in_row();
            }

            Some(old_index)
        } else {
            None
        }
    }
}

pub type MappedAllIndicesIterator<'im, const SL: usize> =
    MappedIndexIterator<'im, AllIndicesIterator<SL>, SL>;

#[derive(Default)]
pub struct BorderIndicesIterator<const SL: usize> {
    index_type: u8,
    index_2: usize,
}

impl<const SL: usize> Iterator for BorderIndicesIterator<SL> {
    type Item = Index<SL>;

    fn next(&mut self) -> Option<Index<SL>> {
        if self.index_type < IndexType2D::COUNT {
            let index: Index<SL> = Index::<SL>::new(
                self.index_type.try_into().unwrap(),
                <Helpers!(Index<SL>)>::SIDE_LEN_MINUS_1,
                self.index_2,
            );

            self.index_2 += 1_usize;

            if self.index_2 >= <Helpers!(Index<SL>)>::SIDE_LEN_MINUS_1 {
                self.index_type += 1_u8;
                self.index_2 = 0_usize;
            }

            Some(index)
        } else {
            None
        }
    }
}

pub type MappedBorderIndicesIterator<'im, const SL: usize> =
    MappedIndexIterator<'im, BorderIndicesIterator<SL>, SL>;

pub struct MappedTrioIndicesIterator<'im, const SL: usize> {
    index_map: &'im IndexMap<SL>,
    iter_all: AllIndicesIterator<SL>,
    index_a: Index<SL>,
    checked_1: bool,
    checked_2: bool,
}

type IndexToIndexOption<const SL: usize> = fn(Index<SL>) -> Option<Index<SL>>;

impl<'im, const SL: usize> MappedTrioIndicesIterator<'im, SL> {
    pub fn new(index_map: &'im IndexMap<SL>) -> Self {
        Self {
            index_map,
            iter_all: AllIndicesIterator::<SL>::default(),
            index_a: Index::<SL>::default(),
            checked_1: true,
            checked_2: true,
        }
    }

    fn reset(&mut self, index_a: Index<SL>) -> () {
        self.index_a = index_a;
        self.checked_1 = false;
        self.checked_2 = false;
    }

    fn try_get_other_indices(
        &self,
        try_get_index_b: IndexToIndexOption<SL>,
        try_get_index_c: IndexToIndexOption<SL>,
    ) -> Option<(Index<SL>, Index<SL>)> {
        try_get_index_b(self.index_a.next_type()).zip(try_get_index_c(self.index_a.prev_type()))
    }

    fn try_get_other_indices_1(&self) -> Option<(Index<SL>, Index<SL>)> {
        self.try_get_other_indices(Index::<SL>::prev_in_row, Index::<SL>::next_in_row)
    }

    fn try_get_other_indices_2(&self) -> Option<(Index<SL>, Index<SL>)> {
        self.try_get_other_indices(Index::<SL>::next_in_row, Index::<SL>::prev_in_row)
    }
}

impl<'im, const SL: usize> Iterator for MappedTrioIndicesIterator<'im, SL> {
    type Item = (usize, usize, usize);

    fn next(&mut self) -> Option<(usize, usize, usize)> {
        loop {
            match (self.checked_1, self.checked_2) {
                (false, _) => {
                    self.checked_1 = true;

                    if let Some((index_b, index_c)) = self.try_get_other_indices_1() {
                        return Some((
                            self.index_map[&self.index_a],
                            self.index_map[&index_b],
                            self.index_map[&index_c],
                        ));
                    }
                }
                (true, false) => {
                    self.checked_2 = true;

                    if let Some((index_b, index_c)) = self.try_get_other_indices_2() {
                        return Some((
                            self.index_map[&self.index_a],
                            self.index_map[&index_b],
                            self.index_map[&index_c],
                        ));
                    }
                }
                (true, true) => {
                    if let Some(index_a) = self.iter_all.next() {
                        self.reset(index_a);
                    } else {
                        return None;
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct IndexMap<const SL: usize>([[[usize; SL]; SL]; 3_usize]);

impl<const SL: usize> IndexMap<SL> {
    fn default() -> Self {
        let mut index_map: Self = unsafe { MaybeUninit::<Self>::uninit().assume_init() };

        // Initialize ij 2D array
        {
            let ij_2d_array: &mut [[usize; SL]; SL] = &mut index_map.0[0_usize];
            let mut index: usize = 0_usize;

            for i in 0_usize..<Helpers!(Self)>::SIDE_LEN {
                let j_array: &mut [usize; SL] = &mut ij_2d_array[i];

                for j in (0_usize..<Helpers!(Self)>::SIDE_LEN).rev() {
                    if i + j >= <Helpers!(Self)>::SIDE_LEN_MINUS_1 {
                        j_array[j] = index;

                        index += 1_usize;
                    } else {
                        j_array[j] = usize::MAX;
                    }
                }
            }
        }

        // Initialize jk and ki 2D arrays
        {
            let [ij_2d_array, jk_2d_array, ki_2d_array] = &mut index_map.0;

            // For jk_2d_array, jk represents j; for ki_2d_array, jk represents k
            for jk in 0_usize..<Helpers!(Self)>::SIDE_LEN {
                let index_sum_minus_jk: usize = <Helpers!(Self)>::INDEX_SUM - jk;
                let k_array: &mut [usize; SL] = &mut jk_2d_array[jk];
                let i_array: &mut [usize; SL] = &mut ki_2d_array[jk];

                // For k_array, ki represents k; for i_array, ki represents i
                for ki in 0_usize..<Helpers!(Self)>::SIDE_LEN {
                    // Application of the invariant
                    let ij: usize = index_sum_minus_jk - ki;

                    /* ij could be invalid here, pointing to cells outside the actual triangle.
                    Check for that, and use usize::MAX in that case*/
                    if ij < <Helpers!(Self)>::SIDE_LEN {
                        /* In both the cases where we iterate on j then k or k then i, we already
                        have one of the variables used in the ij 2D array. Variable ij represents
                        the missing index of the two for the corresponding cell. Fill these two
                        tables then (jk and ki) using the already established indices from the
                        corresponding cell of the ij table */
                        k_array[ki] = ij_2d_array[ij][jk];
                        i_array[ki] = ij_2d_array[ki][ij];
                    } else {
                        k_array[ki] = usize::MAX;
                        i_array[ki] = usize::MAX;
                    }
                }
            }
        }

        index_map
    }
}

impl<const SL: usize> Default for IndexMap<SL> {
    fn default() -> Self {
        Self::default()
    }
}

impl<const SL: usize> IndexTrait<&Index<SL>> for IndexMap<SL> {
    type Output = usize;

    fn index(&self, index: &Index<SL>) -> &Self::Output {
        &self.0[index.index_type as usize][index.index_1][index.index_2]
    }
}

impl<const SL: usize> UseHelpers for IndexMap<SL> {
    type Helpers = Helpers<SL>;
}

#[cfg(test)]
mod tests {
    use super::*;

    const SIDE_LEN: usize = 5_usize;
    const SIDE_LEN_MINUS_1: usize = SIDE_LEN - 1_usize;
    const X: usize = usize::MAX;
    const A: usize = 0xA_usize;
    const B: usize = 0xB_usize;
    const C: usize = 0xC_usize;
    const D: usize = 0xD_usize;
    const E: usize = 0xE_usize;

    // type TestTriangularArray = TriangularArray!(u8, SIDE_LEN);
    type TestIndex = Index<SIDE_LEN>;
    type TestIndexMap = IndexMap<SIDE_LEN>;

    lazy_static! {
        static ref INDEX_MAP: TestIndexMap = TestIndexMap::default();
    }

    macro_rules! test_index {
        ($index_type:ident, $index_1:expr, $index_2:expr) => {
            TestIndex::new(IndexType2D::$index_type, $index_1, $index_2)
        };
        ($index_type:ident, $index_1:expr) => {
            test_index!($index_type, $index_1, SIDE_LEN_MINUS_1 - $index_1)
        };
    }

    macro_rules! test_index_slice { ($(($index_type:ident, $index_1:expr, $index_2:expr)),*) => { &[$(
        test_index!($index_type, $index_1, $index_2),
    )*] } }

    #[test]
    fn test_index_map() -> () {
        assert_eq!(
            INDEX_MAP.0,
            [
                [
                    // IJ
                    [X, X, X, X, 0],
                    [X, X, X, 2, 1],
                    [X, X, 5, 4, 3],
                    [X, 9, 8, 7, 6],
                    [E, D, C, B, A]
                ],
                [
                    // JK
                    [X, X, X, X, E],
                    [X, X, X, D, 9],
                    [X, X, C, 8, 5],
                    [X, B, 7, 4, 2],
                    [A, 6, 3, 1, 0]
                ],
                [
                    // KI
                    [X, X, X, X, A],
                    [X, X, X, 6, B],
                    [X, X, 3, 7, C],
                    [X, 1, 4, 8, D],
                    [0, 2, 5, 9, E]
                ]
            ]
        );
    }

    fn test_iter<II: Iterator<Item = TestIndex>, MII: Iterator<Item = usize>>(
        indices_iter: II,
        expected_indices: &[TestIndex],
        mapped_indices_iter: MII,
        expected_mapped_indices: &[usize],
    ) -> () {
        let indices: Vec<TestIndex> = indices_iter.collect();

        assert_eq!(indices, expected_indices);

        let mapped_indices_a: Vec<usize> = indices
            .iter()
            .map(|test_index: &TestIndex| -> usize { INDEX_MAP[test_index] })
            .collect();
        let mapped_indices_b: Vec<usize> = mapped_indices_iter.collect();

        assert_eq!(mapped_indices_a, mapped_indices_b);
        assert_eq!(mapped_indices_b, expected_mapped_indices);
    }

    #[test]
    fn test_iter_row_indices() -> () {
        test_iter(
            test_index!(IJ, 0).iter_row_indices(),
            test_index_slice![(IJ, 0, 4)],
            test_index!(IJ, 0).iter_mapped_row_indices(&*INDEX_MAP),
            &[0],
        );
        test_iter(
            test_index!(IJ, 1).iter_row_indices(),
            test_index_slice![(IJ, 1, 3), (IJ, 1, 4)],
            test_index!(IJ, 1).iter_mapped_row_indices(&*INDEX_MAP),
            &[2, 1],
        );
        test_iter(
            test_index!(IJ, 2).iter_row_indices(),
            test_index_slice![(IJ, 2, 2), (IJ, 2, 3), (IJ, 2, 4)],
            test_index!(IJ, 2).iter_mapped_row_indices(&*INDEX_MAP),
            &[5, 4, 3],
        );
        test_iter(
            test_index!(IJ, 3).iter_row_indices(),
            test_index_slice![(IJ, 3, 1), (IJ, 3, 2), (IJ, 3, 3), (IJ, 3, 4)],
            test_index!(IJ, 3).iter_mapped_row_indices(&*INDEX_MAP),
            &[9, 8, 7, 6],
        );
        test_iter(
            test_index!(IJ, 4).iter_row_indices(),
            test_index_slice![(IJ, 4, 0), (IJ, 4, 1), (IJ, 4, 2), (IJ, 4, 3), (IJ, 4, 4)],
            test_index!(IJ, 4).iter_mapped_row_indices(&*INDEX_MAP),
            &[E, D, C, B, A],
        );
    }

    #[test]
    fn test_iter_border_indices() -> () {
        test_iter(
            TestIndex::iter_border_indices(),
            test_index_slice![
                (IJ, 4, 0),
                (IJ, 4, 1),
                (IJ, 4, 2),
                (IJ, 4, 3),
                (JK, 4, 0),
                (JK, 4, 1),
                (JK, 4, 2),
                (JK, 4, 3),
                (KI, 4, 0),
                (KI, 4, 1),
                (KI, 4, 2),
                (KI, 4, 3)
            ],
            TestIndex::iter_mapped_border_indices(&*INDEX_MAP),
            &[E, D, C, B, A, 6, 3, 1, 0, 2, 5, 9],
        );
    }

    #[test]
    fn test_iter_all_indices() -> () {
        test_iter(
            TestIndex::iter_all_indices(),
            test_index_slice![
                (IJ, 0, 4),
                (IJ, 1, 4),
                (IJ, 1, 3),
                (IJ, 2, 4),
                (IJ, 2, 3),
                (IJ, 2, 2),
                (IJ, 3, 4),
                (IJ, 3, 3),
                (IJ, 3, 2),
                (IJ, 3, 1),
                (IJ, 4, 4),
                (IJ, 4, 3),
                (IJ, 4, 2),
                (IJ, 4, 1),
                (IJ, 4, 0)
            ],
            TestIndex::iter_mapped_all_indices(&*INDEX_MAP),
            &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E],
        );
    }

    #[test]
    fn test_iter_trios() -> () {
        assert_eq!(
            TestIndex::iter_mapped_trio_indices(&*INDEX_MAP)
                .collect::<Vec<(usize, usize, usize)>>(),
            &[
                (0, 1, 2),
                (1, 3, 4),
                (2, 4, 5),
                (3, 6, 7),
                (4, 7, 8),
                (4, 2, 1),
                (5, 8, 9),
                (6, A, B),
                (7, B, C),
                (7, 4, 3),
                (8, C, D),
                (8, 5, 4),
                (9, D, E),
                (B, 7, 6),
                (C, 8, 7),
                (D, 9, 8)
            ]
        );
    }
}
