use {
    self::consts::*,
    crate::{
        app::prelude::*,
        math::{
            polyhedra::{
                data::{Data, EdgeData, FaceData, VertexData},
                properties::ICOSIDODECAHEDRON,
                Polyhedron,
            },
            *,
        },
        max,
        preferences::prelude::*,
        prelude::*,
    },
    bevy::{
        ecs::{query::WorldQuery, world::EntityMut},
        prelude::*,
        render::{
            mesh::{Indices, Mesh as BevyMesh},
            render_resource::PrimitiveTopology,
        },
    },
    bevy_inspector_egui::Inspectable,
    itertools::izip,
    num_traits::PrimInt,
    serde::Deserialize,
    std::{
        cmp::Ordering,
        convert::TryFrom,
        f32::consts::{PI, TAU},
        mem::MaybeUninit as MU,
        ops::{Add, Range},
    },
    strum::{EnumCount as EnumCountTrait, IntoEnumIterator},
    strum_macros::{EnumCount, EnumIter},
};

pub mod consts {
    use {
        super::Type,
        crate::{max, puzzle::inflated::PieceStateComponent as IPSC},
        std::ops::Range,
    };

    pub trait TypeConsts: Sized {
        const PENTAGON_PIECE_COUNT: Self;
        const PENTAGON_VERTEX_COUNT: Self;
        const TRIANGLE_PIECE_COUNT: Self;
        const TRIANGLE_VERTEX_COUNT: Self;
        const PIECE_COUNT: Self;
    }

    impl TypeConsts for usize {
        const PENTAGON_PIECE_COUNT: Self = Type::Pentagon.instance_count();
        const PENTAGON_VERTEX_COUNT: Self = Type::Pentagon.vertex_count();
        const TRIANGLE_PIECE_COUNT: Self = Type::Triangle.instance_count();
        const TRIANGLE_VERTEX_COUNT: Self = Type::Triangle.vertex_count();
        const PIECE_COUNT: Self = Self::PENTAGON_PIECE_COUNT + Self::TRIANGLE_PIECE_COUNT;
    }

    impl TypeConsts for f32 {
        const PENTAGON_PIECE_COUNT: Self = Type::Pentagon.instance_count() as f32;
        const PENTAGON_VERTEX_COUNT: Self = Type::Pentagon.vertex_count() as f32;
        const TRIANGLE_PIECE_COUNT: Self = Type::Triangle.instance_count() as f32;
        const TRIANGLE_VERTEX_COUNT: Self = Type::Triangle.vertex_count() as f32;
        const PIECE_COUNT: Self = Self::PENTAGON_PIECE_COUNT + Self::TRIANGLE_PIECE_COUNT;
    }

    impl TypeConsts for IPSC {
        const PENTAGON_PIECE_COUNT: Self = Type::Pentagon.instance_count() as IPSC;
        const PENTAGON_VERTEX_COUNT: Self = Type::Pentagon.vertex_count() as IPSC;
        const TRIANGLE_PIECE_COUNT: Self = Type::Triangle.instance_count() as IPSC;
        const TRIANGLE_VERTEX_COUNT: Self = Type::Triangle.vertex_count() as IPSC;
        const PIECE_COUNT: Self = Self::PENTAGON_PIECE_COUNT + Self::TRIANGLE_PIECE_COUNT;
    }

    pub const HALF_PENTAGON_PIECE_COUNT: usize = usize::PENTAGON_PIECE_COUNT / 2; // 6
    pub const PENTAGON_INDEX_OFFSET: usize = Type::Pentagon.index_offset(); // 0
    pub const TRIANGLE_INDEX_OFFSET: usize = Type::Triangle.index_offset(); // 12
    pub const PENTAGON_PIECE_RANGE: Range<usize> = Type::Pentagon.range();
    pub const TRIANGLE_PIECE_RANGE: Range<usize> = Type::Triangle.range();
    pub const PIECE_RANGE: Range<usize> = 0_usize..usize::PIECE_COUNT;
    pub const ROTATION_BIT_COUNT: u32 = usize::BITS
        - max!(usize::PENTAGON_VERTEX_COUNT, usize::TRIANGLE_VERTEX_COUNT).leading_zeros(); // 3
    pub const ROTATION_BIT_MASK: IPSC = ((1 as IPSC) << ROTATION_BIT_COUNT) - 1; // 0b111
    pub const ZERO_IPSC: IPSC = 0 as IPSC;
}

#[derive(Clone, Copy, Debug, Deserialize, EnumCount, EnumIter, Inspectable, PartialEq)]
pub enum Design {
    OriginalSuperDodecahedron,
    CustomSuperDodecahedron,
    SuperIcosahedron,
    RhombicTriacontahedron,
    RoundedRhombicTriacontahedron,
}

impl Design {
    pub const fn as_polyhedron(self) -> Polyhedron {
        match self {
            Design::OriginalSuperDodecahedron => Polyhedron::Dodecahedron,
            Design::CustomSuperDodecahedron => Polyhedron::Dodecahedron,
            Design::SuperIcosahedron => Polyhedron::Icosahedron,
            Design::RhombicTriacontahedron => Polyhedron::RhombicTriacontahedron,
            Design::RoundedRhombicTriacontahedron => Polyhedron::RhombicTriacontahedron,
        }
    }
}

impl Update for Design {
    fn update(&self, other: &Self, world: &mut World, preferences: &Preferences) {
        if self != other {
            warn_expect!(world.contains_resource::<BevyMeshHandles>(), ?);

            world.resource_scope(
                |world: &mut World, bevy_mesh_handles: Mut<BevyMeshHandles>| {
                    PieceLibrary::get().update_entities(
                        &warn_expect_some!(
                            PieceMats::try_from(&preferences.puzzle.color.colors_with_mat, *self),
                            return
                        ),
                        &bevy_mesh_handles,
                        world,
                        *self,
                    );
                },
            );
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, EnumCount, EnumIter)]
pub enum Type {
    Pentagon,
    Triangle,
}

impl Type {
    pub const fn vertex_count(self) -> usize {
        ICOSIDODECAHEDRON.face_sizes[self as usize].face_size
    }

    pub const fn index_offset(self) -> usize {
        ICOSIDODECAHEDRON.face_sizes[self as usize].initial_face_index
    }

    pub const fn instance_count(self) -> usize {
        ICOSIDODECAHEDRON.face_sizes[self as usize + 1].initial_face_index - self.index_offset()
    }

    pub const fn range(self) -> Range<usize> {
        let index_offset: usize = self.index_offset();

        index_offset..index_offset + self.instance_count()
    }

    pub const fn next_side_index(&self, index: usize) -> usize {
        (index + 1) % self.vertex_count()
    }

    pub const fn prev_side_index(&self, index: usize) -> usize {
        let side_count: usize = self.vertex_count();

        (index + side_count - 1) % side_count
    }

    pub const fn from_index(index: usize) -> Option<Self> {
        const_assert_eq!(Type::Pentagon.index_offset(), 0_usize);
        const_assert_eq!(Type::Pentagon.instance_count(), 12_usize);
        const_assert_eq!(Type::Triangle.index_offset(), 12_usize);
        const_assert_eq!(Type::Triangle.instance_count(), 20_usize);

        match index {
            0..=11 => Some(Type::Pentagon),
            12..=31 => Some(Type::Triangle),
            _ => None,
        }
    }

    pub const fn pyramid_polyhedron(self) -> Polyhedron {
        match self {
            Self::Pentagon => Polyhedron::Icosahedron,
            Self::Triangle => Polyhedron::Dodecahedron,
        }
    }

    pub const fn other(self) -> Self {
        match self {
            Self::Pentagon => Self::Triangle,
            Self::Triangle => Self::Pentagon,
        }
    }
}

#[derive(Clone, Copy, Default)]
struct Tri {
    vertices: [Vec3; 3_usize],
    normal: Vec3,
}

impl Tri {
    fn update_normal(&mut self) -> &mut Self {
        self.normal = Self::compute_normal(&self[0_usize], &self[1_usize], &self[2_usize]);

        self
    }

    fn offset_along_normal(&mut self, distance: f32) -> &mut Self {
        let offset: Vec3 = self.normal * distance;

        for vertex in self.vertices.iter_mut() {
            *vertex += offset;
        }

        self
    }

    fn offset_all_along_plane(&mut self, distance: f32) -> &mut Self {
        self.offset_along_plane(distance, [true; 3_usize])
    }

    fn offset_along_plane(&mut self, distance: f32, mask: [bool; 3]) -> &mut Self {
        if mask.iter().all(|should_offset| !should_offset) || (distance.abs() <= f32::EPSILON) {
            return self;
        }

        let mat: Mat4 = Mat4::from(&*self);
        let transformed: Tri = &mat * *self;
        let mut tri1: Tri = transformed;
        let mut tri2: Tri = transformed;

        for (curr_vert_index, _) in mask
            .iter()
            .filter(|should_offset| **should_offset)
            .enumerate()
        {
            let next_vert_index: usize = (curr_vert_index + 1) % 3;
            let edge_offset: Vec3 = distance
                * (transformed[next_vert_index] - transformed[curr_vert_index])
                    .cross(Vec3::Z)
                    .normalize();

            tri1[curr_vert_index] += edge_offset;
            tri2[next_vert_index] += edge_offset;
        }

        let mut result: Tri = transformed;

        for curr_vert_index in 0_usize..3_usize {
            let prev_vert_index: usize = (curr_vert_index + 2_usize) % 3_usize;
            let next_vert_index: usize = (curr_vert_index + 1_usize) % 3_usize;

            result[curr_vert_index] = Vec3::from((
                warn_expect_some!(
                    Option::<Vec2>::from(two_d::compute_line_intersection(
                        tri1[curr_vert_index].xy(),
                        tri2[next_vert_index].xy(),
                        tri1[prev_vert_index].xy(),
                        tri2[curr_vert_index].xy(),
                        None
                    )),
                    continue
                ),
                0.0_f32,
            ));
        }

        *self = &mat.inverse() * result;
        self.update_normal();

        self
    }

    fn compute_normal(a: &Vec3, b: &Vec3, c: &Vec3) -> Vec3 {
        (*b - *a).cross(*c - *b).normalize()
    }
}

impl From<[Vec3; 3]> for Tri {
    fn from(vertices: [Vec3; 3]) -> Self {
        Tri::from(&vertices[..])
    }
}

impl From<&[Vec3]> for Tri {
    fn from(vertex_slice: &[Vec3]) -> Self {
        let mut tri: Tri = Tri::default();

        tri[..].copy_from_slice(vertex_slice);
        tri.update_normal();

        tri
    }
}

impl From<&Tri> for Mat4 {
    fn from(tri: &Tri) -> Self {
        Self::look_at_rh(
            tri[0_usize],
            tri[0_usize] - tri.normal,
            tri.normal.cross(tri[1_usize] - tri[0_usize]).normalize(),
        )
    }
}

impl<Idx> std::ops::Index<Idx> for Tri
where
    [Vec3; 3]: std::ops::Index<Idx>,
{
    type Output = <[Vec3; 3] as std::ops::Index<Idx>>::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.vertices[index]
    }
}

impl<Idx> std::ops::IndexMut<Idx> for Tri
where
    [Vec3; 3]: std::ops::IndexMut<Idx>,
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        &mut self.vertices[index]
    }
}

impl std::ops::Mul<Tri> for &Mat4 {
    type Output = Tri;

    fn mul(self, mut rhs: Tri) -> Self::Output {
        for vertex in rhs.vertices.iter_mut() {
            *vertex = self.project_point3(*vertex);
        }

        rhs.update_normal();

        rhs
    }
}

#[derive(Clone, Component, Copy)]
pub struct PieceComponent {
    pub index: usize,
    pub piece_type: Type,
}

#[derive(WorldQuery)]
pub struct PieceComponents<'w> {
    pub entity: Entity,
    pub piece_component: &'w PieceComponent,
    pub transform: &'w Transform,
}

pub type PieceQuery<'world, 'state, 'w> = Query<'world, 'state, PieceComponents<'w>>;

#[derive(WorldQuery)]
#[world_query(mutable)]
pub struct PieceComponentsMut<'w> {
    pub entity: Entity,
    pub piece_component: &'w PieceComponent,
    pub transform: &'w mut Transform,
}

pub type PieceQueryMut<'world, 'state, 'w> = Query<'world, 'state, PieceComponentsMut<'w>>;

pub struct PieceMats<'a> {
    pub base_mat: &'a ColAndMat,
    pub color_mats: &'a Vec<ColAndMat>,
}

impl<'a> PieceMats<'a> {
    fn try_from<'b: 'a>(color_data_with_mat: &'b ColorDataWithMat, design: Design) -> Option<Self> {
        let design_polyhedron: Polyhedron = design.as_polyhedron();

        Some(Self {
            base_mat: &color_data_with_mat.base_color,
            color_mats: &color_data_with_mat.polyhedron_to_colors[warn_expect_ok!(
                color_data_with_mat.polyhedron_to_colors.binary_search_by(
                    |(polyhedron, _): &(Polyhedron, Vec<ColAndMat>)| -> Ordering {
                        polyhedron.cmp(&design_polyhedron)
                    }
                ),
                return None
            )]
            .1,
        })
    }
}

struct MeshVertexData {
    position: Vec3,
    normal: Vec3,
    uv: Vec2,
}

trait PushVertex<V: Sized> {
    fn push_vertex(&mut self, vertex: V);
}

struct MeshAttributeData {
    positions: [Vec3; gen::TOTAL_VERTEX_COUNT],
    normals: [Vec3; gen::TOTAL_VERTEX_COUNT],
    uvs: [Vec2; gen::TOTAL_VERTEX_COUNT],
    indices: [u32; gen::TOTAL_INDEX_COUNT],
    face_indices: [u8; gen::TOTAL_FACE_INDEX_COUNT],
}

impl MeshAttributeData {
    #[cfg(debug_assertions)]
    fn zero(&mut self, range: Range<usize>) {
        self.positions[range.clone()].fill(Vec3::ZERO);
        self.normals[range.clone()].fill(Vec3::ZERO);
        self.uvs[range].fill(Vec2::ZERO);
    }
}

impl Default for MeshAttributeData {
    fn default() -> Self {
        unsafe { MU::<Self>::zeroed().assume_init() }
    }
}

struct MeshAttributeDataWithStats<'d> {
    data: &'d mut MeshAttributeData,
    stats: MeshStats,
}

impl<'d> MeshAttributeDataWithStats<'d> {
    fn indices(&mut self) -> TempVec<u32> {
        TempVec::<u32> {
            data: &mut self.data.indices,
            index: &mut self.stats.indices,
        }
    }

    fn face_indices(&mut self) -> TempVec<u8> {
        TempVec::<u8> {
            data: &mut self.data.face_indices,
            index: &mut self.stats.face_indices,
        }
    }
}

impl<'d> PushVertex<MeshVertexData> for MeshAttributeDataWithStats<'d> {
    fn push_vertex(&mut self, mesh_vertex_data: MeshVertexData) {
        self.data.positions[self.stats.vertices] = mesh_vertex_data.position;
        self.data.normals[self.stats.vertices] = mesh_vertex_data.normal;
        self.data.uvs[self.stats.vertices] = mesh_vertex_data.uv;

        self.stats.vertices += 1_usize;
    }
}

impl<'d> PushVertex<(Tri, u32)> for MeshAttributeDataWithStats<'d> {
    fn push_vertex(&mut self, (tri, offset): (Tri, u32)) {
        let offset: u32 = self.stats.vertices as u32 - offset;

        for vertex in tri.vertices {
            self.push_vertex(MeshVertexData {
                position: vertex,
                normal: tri.normal,
                uv: Vec2::ZERO,
            });
        }

        self.indices()
            .extend([offset, offset + 1_u32, offset + 2_u32]);
    }
}

struct TempVec<'a, T> {
    data: &'a mut [T],
    index: &'a mut usize,
}

impl<'a, T> TempVec<'a, T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for t in iter {
            self.data[*self.index] = t;
            *self.index += 1_usize;
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct MeshStats<I: PrimInt + Default = usize> {
    vertices: I,
    indices: I,
    face_indices: I,
}

type MeshStatsRefSlice = &'static [&'static MeshStats];

impl<I: PrimInt + Default> MeshStats<I> {
    fn from<J: PrimInt + Default>(mesh_stats: MeshStats<J>) -> Option<Self> {
        Some(Self {
            vertices: warn_expect_some!(I::from(mesh_stats.vertices), ?),
            indices: warn_expect_some!(I::from(mesh_stats.indices), ?),
            face_indices: warn_expect_some!(I::from(mesh_stats.face_indices), ?),
        })
    }
}

impl MeshStats {
    const fn new(vertices: usize, tris: usize, piece_type: Type) -> Self {
        Self {
            vertices,
            indices: usize::TRIANGLE_VERTEX_COUNT * tris,
            face_indices: piece_type.instance_count(),
        }
    }

    const fn new_base(vertices: usize, tris: usize) -> Self {
        Self {
            vertices,
            indices: usize::TRIANGLE_VERTEX_COUNT * tris,
            face_indices: 0_usize,
        }
    }

    const fn sum_slice(slice: MeshStatsRefSlice) -> Self {
        let mut sum: Self = Self {
            vertices: 0_usize,
            indices: 0_usize,
            face_indices: 0_usize,
        };

        let len: usize = slice.len();
        let mut index: usize = 0_usize;

        while index < len {
            let mesh_stats: &MeshStats = slice[index];

            sum.vertices += mesh_stats.vertices;
            sum.indices += mesh_stats.indices;
            sum.face_indices += mesh_stats.face_indices;
            index += 1_usize;
        }

        sum
    }
}

impl<I: PrimInt + Default> Add for MeshStats<I> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self {
            vertices: self.vertices + rhs.vertices,
            indices: self.indices + rhs.indices,
            face_indices: self.face_indices + rhs.face_indices,
        }
    }
}

#[derive(Clone, Default)]
struct MeshHeader(Range<MeshStats<u32>>);

impl MeshHeader {
    fn new(start: &MeshStats, len: &MeshStats) -> Self {
        || -> Option<Self> {
            Some(Self({
                let start: MeshStats<u32> = warn_expect_some!(MeshStats::<u32>::from(*start), ?);

                start..start + warn_expect_some!(MeshStats::<u32>::from(*len), ?)
            }))
        }()
        .unwrap_or_default()
    }

    fn vertices(&self) -> Range<usize> {
        self.0.start.vertices as usize..self.0.end.vertices as usize
    }
    fn indices(&self) -> Range<usize> {
        self.0.start.indices as usize..self.0.end.indices as usize
    }
    fn face_indices(&self) -> Range<usize> {
        self.0.start.face_indices as usize..self.0.end.face_indices as usize
    }

    fn add_bevy_mesh(
        &self,
        mesh_attribute_data: &MeshAttributeData,
        bevy_meshes: &mut Assets<BevyMesh>,
        bevy_mesh_handle: &mut Handle<BevyMesh>,
    ) {
        let vertices: Range<usize> = self.vertices();
        let indices: Range<usize> = self.indices();

        warn_expect!(vertices.end <= gen::TOTAL_VERTEX_COUNT, ?);
        warn_expect!(indices.end <= gen::TOTAL_INDEX_COUNT, ?);

        macro_rules! assert_eq_align_and_size {
            ($type_a:ty, $type_b:ty) => {
                assert_eq_align!($type_a, $type_b);
                assert_eq_size!($type_a, $type_b);
            };
        }

        assert_eq_align_and_size!(Vec3, [f32; 3_usize]);
        assert_eq_align_and_size!(Vec2, [f32; 2_usize]);
        assert_eq_align_and_size!(Vec<Vec3>, Vec<[f32; 3_usize]>);
        assert_eq_align_and_size!(Vec<Vec2>, Vec<[f32; 2_usize]>);

        let mut bevy_mesh: BevyMesh = BevyMesh::new(PrimitiveTopology::TriangleList);

        bevy_mesh.insert_attribute(
            BevyMesh::ATTRIBUTE_POSITION,
            // Safe: assert_eq_align_and_size! invocations above
            unsafe {
                transmute::<Vec<Vec3>, Vec<[f32; 3_usize]>>(Vec::<Vec3>::from(
                    &mesh_attribute_data.positions[vertices.clone()],
                ))
            },
        );
        bevy_mesh.insert_attribute(
            BevyMesh::ATTRIBUTE_NORMAL,
            // Safe: assert_eq_align_and_size! invocations above
            unsafe {
                transmute::<Vec<Vec3>, Vec<[f32; 3_usize]>>(Vec::<Vec3>::from(
                    &mesh_attribute_data.normals[vertices.clone()],
                ))
            },
        );
        bevy_mesh.insert_attribute(
            BevyMesh::ATTRIBUTE_UV_0,
            // Safe: assert_eq_align_and_size! invocations above
            unsafe {
                transmute::<Vec<Vec2>, Vec<[f32; 2_usize]>>(Vec::<Vec2>::from(
                    &mesh_attribute_data.uvs[vertices],
                ))
            },
        );
        bevy_mesh.set_indices(Some(Indices::U32(Vec::<u32>::from(
            &mesh_attribute_data.indices[indices],
        ))));
        *bevy_mesh_handle = bevy_meshes.add(bevy_mesh);
    }
}

struct MeshHeaders([MeshHeader; gen::TOTAL_MESH_COUNT]);

impl MeshHeaders {
    fn add_bevy_meshes(
        &self,
        mesh_attribute_data: &MeshAttributeData,
        bevy_meshes: &mut Assets<BevyMesh>,
        bevy_mesh_handles: &mut BevyMeshHandles,
    ) {
        warn_expect!(self.0.len() == bevy_mesh_handles.0.len(), ?);

        for (mesh_header, bevy_mesh_handle) in self.0.iter().zip(bevy_mesh_handles.0.iter_mut()) {
            mesh_header.add_bevy_mesh(mesh_attribute_data, bevy_meshes, bevy_mesh_handle);
        }
    }
}

impl Default for MeshHeaders {
    fn default() -> Self {
        Self(<[MeshHeader; gen::TOTAL_MESH_COUNT]>::default_array())
    }
}

struct MeshHeadersWithIndex<'h> {
    headers: &'h mut MeshHeaders,
    index: usize,
}

impl<'h> MeshHeadersWithIndex<'h> {
    fn push(&mut self, mesh_header: MeshHeader) {
        self.headers.0[self.index] = mesh_header;
        self.index += 1_usize;
    }
}

struct PieceHeaderParams<'a> {
    design: Design,
    piece_type: Type,
    mesh_attribute_data_with_stats: &'a mut MeshAttributeDataWithStats<'a>,
    mesh_headers_with_index: &'a mut MeshHeadersWithIndex<'a>,
}

mod gen {
    use super::*;

    pub mod original_super_dodecahedron {
        use super::*;

        pub mod pentagon {
            use super::*;

            pub const PENTAGONAL_ANNULUS_THICKNESS_RATIO: f32 = 0.2_f32;
            pub const PENTAGONAL_ANNULUS_THICKNESS: f32 =
                PENTAGONAL_ANNULUS_THICKNESS_RATIO * ICOSIDODECAHEDRON.edge_length;
            pub const CIRCLE_SUBDIVISION_COUNT: usize = 16_usize;
            pub const TAU_OVER_CIRCLE_SUBDIVISION_COUNT: f32 =
                TAU / CIRCLE_SUBDIVISION_COUNT as f32;
            pub const PENTAGON_COUNT: usize = 4_usize;
            pub const TRIS_PER_PENTAGONAL_ANNULUS: usize = TRIS_PER_QUAD * PENTAGON_VERTEX_COUNT;
            pub const BASE_MESH_TRIS_CAPACITY: usize = TRIS_PER_PENTAGONAL_ANNULUS * PENTAGON_COUNT;
            pub const CURR_TRI_VERT_INDEX: usize = 1_usize;
            pub const OFFSET_MASK: [bool; 3_usize] = [true, true, false];
            pub const PIECE_TYPE: Type = Type::Pentagon;
            pub const BASE_MESH_STATS: MeshStats = MeshStats::new_base(
                2_usize * 2_usize * PENTAGON_VERTEX_COUNT
                    + 2_usize * 4_usize * PENTAGON_VERTEX_COUNT,
                BASE_MESH_TRIS_CAPACITY,
            );
            pub const PRIMARY_PENT_MESH_STATS: MeshStats = MeshStats::new(
                2_usize * PENTAGON_VERTEX_COUNT,
                TRIS_PER_PENTAGONAL_ANNULUS,
                PIECE_TYPE,
            );
            pub const ADJACENT_PENT_MESH_STATS: MeshStats = MeshStats::new(
                CIRCLE_SUBDIVISION_COUNT + 1_usize,
                CIRCLE_SUBDIVISION_COUNT,
                PIECE_TYPE,
            );
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &PRIMARY_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub mod triangle {
            use super::*;

            pub const PIECE_TYPE: Type = Type::Triangle;
            pub const BASE_MESH_STATS: MeshStats = MeshStats::new_base(
                TRIANGLE_VERTEX_COUNT * (TRIANGLE_VERTEX_COUNT + 4_usize + 1_usize) + 1_usize,
                TRIANGLE_VERTEX_COUNT * (1_usize + TRIS_PER_QUAD + 1_usize),
            );
            pub const ADJACENT_PENT_MESH_STATS: MeshStats =
                MeshStats::new(TRIANGLE_VERTEX_COUNT, 1_usize, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub const DESIGN_MESH_COUNT: usize = pentagon::MESH_COUNT + triangle::MESH_COUNT;
        pub const DESIGN_MESH_STATS_SUM: MeshStats =
            MeshStats::sum_slice(&[&pentagon::MESH_STATS_SUM, &triangle::MESH_STATS_SUM]);
        pub const CURR_LOOP_INDEX: usize = 0_usize;
        pub const NEXT_LOOP_INDEX: usize = 1_usize;
    }

    pub mod custom_super_dodecahedron {
        use super::*;

        pub mod pentagon {
            use super::*;

            pub const PENTAGON_COUNT: usize = 3_usize;
            pub const PIECE_TYPE: Type = Type::Pentagon;
            pub const BASE_MESH_STATS: MeshStats = MeshStats::new_base(
                PENTAGON_VERTEX_COUNT + 1_usize + PENTAGON_VERTEX_COUNT * TRIANGLE_VERTEX_COUNT,
                2_usize * PENTAGON_VERTEX_COUNT,
            );
            pub const PRIMARY_PENT_MESH_STATS: MeshStats = MeshStats::new(
                TRIANGLE_VERTEX_COUNT * PENTAGON_VERTEX_COUNT,
                PENTAGON_VERTEX_COUNT,
                PIECE_TYPE,
            );
            pub const ADJACENT_PENT_MESH_STATS: MeshStats =
                MeshStats::new(3_usize * TRIANGLE_VERTEX_COUNT, 3_usize, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &PRIMARY_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub mod triangle {
            use super::*;

            const BASE_MESH_TRIS: usize = 2_usize * TRIANGLE_VERTEX_COUNT;

            pub const PIECE_TYPE: Type = Type::Triangle;
            pub const BASE_MESH_STATS: MeshStats =
                MeshStats::new_base(TRIANGLE_VERTEX_COUNT * BASE_MESH_TRIS, BASE_MESH_TRIS);
            pub const ADJACENT_PENT_MESH_STATS: MeshStats =
                MeshStats::new(TRIANGLE_VERTEX_COUNT, 1_usize, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
                &ADJACENT_PENT_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub const DESIGN_MESH_COUNT: usize = pentagon::MESH_COUNT + triangle::MESH_COUNT;
        pub const DESIGN_MESH_STATS_SUM: MeshStats =
            MeshStats::sum_slice(&[&pentagon::MESH_STATS_SUM, &triangle::MESH_STATS_SUM]);
    }

    pub mod super_icosahedron {
        use super::*;

        pub mod pentagon {
            use super::*;

            const BASE_MESH_TRIS: usize = 2_usize * PENTAGON_VERTEX_COUNT;

            pub const PIECE_TYPE: Type = Type::Pentagon;
            pub const BASE_MESH_STATS: MeshStats =
                MeshStats::new_base(TRIANGLE_VERTEX_COUNT * BASE_MESH_TRIS, BASE_MESH_TRIS);
            pub const ADJACENT_TRI_MESH_STATS: MeshStats =
                MeshStats::new(TRIANGLE_VERTEX_COUNT, 1_usize, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub mod triangle {
            use super::*;

            const BASE_MESH_TRIS: usize = TRIANGLE_VERTEX_COUNT + 1_usize;

            pub const PIECE_TYPE: Type = Type::Triangle;
            pub const BASE_MESH_STATS: MeshStats =
                MeshStats::new_base(TRIANGLE_VERTEX_COUNT * BASE_MESH_TRIS, BASE_MESH_TRIS);
            pub const PRIMARY_TRI_MESH_STATS: MeshStats =
                MeshStats::new(TRIANGLE_VERTEX_COUNT, 1_usize, PIECE_TYPE);
            pub const ADJACENT_TRI_MESH_STATS: MeshStats =
                MeshStats::new(TRIANGLE_VERTEX_COUNT, 1_usize, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &PRIMARY_TRI_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
                &ADJACENT_TRI_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub const DESIGN_MESH_COUNT: usize = pentagon::MESH_COUNT + triangle::MESH_COUNT;
        pub const DESIGN_MESH_STATS_SUM: MeshStats =
            MeshStats::sum_slice(&[&pentagon::MESH_STATS_SUM, &triangle::MESH_STATS_SUM]);
    }

    pub mod rhombic_triacontahedron {
        use super::*;

        pub mod pentagon {
            use super::*;

            pub const PIECE_TYPE: Type = Type::Pentagon;
            pub const QUAD_COUNT: usize = 2_usize * PIECE_TYPE.vertex_count();
            pub const BASE_MESH_STATS: MeshStats =
                MeshStats::new_base(QUAD_COUNT * VERTS_PER_QUAD, QUAD_COUNT * TRIS_PER_QUAD);
            pub const RHOMBUS_MESH_STATS: MeshStats =
                MeshStats::new(VERTS_PER_QUAD, TRIS_PER_QUAD, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &RHOMBUS_MESH_STATS,
                &RHOMBUS_MESH_STATS,
                &RHOMBUS_MESH_STATS,
                &RHOMBUS_MESH_STATS,
                &RHOMBUS_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub mod triangle {
            use super::*;

            pub const PIECE_TYPE: Type = Type::Triangle;
            pub const QUAD_COUNT: usize = 2_usize * PIECE_TYPE.vertex_count();
            pub const BASE_MESH_STATS: MeshStats =
                MeshStats::new_base(QUAD_COUNT * VERTS_PER_QUAD, QUAD_COUNT * TRIS_PER_QUAD);
            pub const RHOMBUS_MESH_STATS: MeshStats =
                MeshStats::new(VERTS_PER_QUAD, TRIS_PER_QUAD, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &RHOMBUS_MESH_STATS,
                &RHOMBUS_MESH_STATS,
                &RHOMBUS_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub const DESIGN_MESH_COUNT: usize = pentagon::MESH_COUNT + triangle::MESH_COUNT;
        pub const DESIGN_MESH_STATS_SUM: MeshStats =
            MeshStats::sum_slice(&[&pentagon::MESH_STATS_SUM, &triangle::MESH_STATS_SUM]);
    }

    pub mod rounded_rhombic_triacontahedron {
        pub use crate::{util::triangular_array::*, Helpers, TriangularArray};

        use super::*;

        pub mod pentagon {
            use super::*;

            #[cfg(debug_assertions)]
            pub mod render {
                pub const RENDER_PIECE_TYPE: bool = true;
                pub const RENDER_BASE_MESH: bool = RENDER_PIECE_TYPE && true;
                pub const RENDER_TRI_ARRAYS: bool = RENDER_BASE_MESH && true;
                pub const RENDER_OUTER_TRI_ARRAYS: bool = RENDER_TRI_ARRAYS && true;
                pub const RENDER_INNER_TRI_ARRAYS: bool = RENDER_TRI_ARRAYS && true;
                pub const RENDER_DISC_SECTIONS: bool = RENDER_BASE_MESH && true;
                pub const RENDER_KITES: bool = RENDER_BASE_MESH && true;
                pub const RENDER_RHOMBUS_MESH: bool = RENDER_PIECE_TYPE && true;
            }

            pub const PIECE_TYPE: Type = Type::Pentagon;
            pub const DISC_SECTION_TRI_COUNT: usize = 2_usize * SUBDIVISION_COUNT;
            pub const ARC_VERT_COUNT: usize = DISC_SECTION_TRI_COUNT + 1_usize;
            pub const DISC_SECTION_VERT_COUNT: usize = ARC_VERT_COUNT + 1_usize;
            pub const BASE_MESH_STATS: MeshStats = MeshStats::new_base(
                PENTAGON_VERTEX_COUNT
                    * (2_usize * TRI_ARRAY_VERT_COUNT + DISC_SECTION_VERT_COUNT + VERTS_PER_QUAD),
                PENTAGON_VERTEX_COUNT
                    * (2_usize * TRI_ARRAY_TRI_COUNT + DISC_SECTION_TRI_COUNT + TRIS_PER_QUAD),
            );
            pub const DISC_SECTION_MESH_STATS: MeshStats =
                MeshStats::new(DISC_SECTION_VERT_COUNT, DISC_SECTION_TRI_COUNT, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub mod triangle {
            use super::*;

            #[cfg(debug_assertions)]
            pub mod render {
                pub const RENDER_PIECE_TYPE: bool = true;
                pub const RENDER_BASE_MESH: bool = RENDER_PIECE_TYPE && true;
                pub const RENDER_TRI_ARRAY: bool = RENDER_BASE_MESH && true;
                pub const RENDER_DISC_SECTIONS: bool = RENDER_BASE_MESH && true;
                pub const RENDER_KITES: bool = RENDER_BASE_MESH && true;
                pub const RENDER_RHOMBUS_MESH: bool = RENDER_PIECE_TYPE && true;
            }

            pub const PIECE_TYPE: Type = Type::Triangle;
            pub const DISC_SECTION_TRI_COUNT: usize = SUBDIVISION_COUNT;
            pub const ARC_VERT_COUNT: usize = DISC_SECTION_TRI_COUNT + 1_usize;
            pub const DISC_SECTION_VERT_COUNT: usize = ARC_VERT_COUNT + 1_usize;
            pub const BASE_MESH_STATS: MeshStats = MeshStats::new_base(
                TRIANGLE_VERTEX_COUNT * (DISC_SECTION_VERT_COUNT + VERTS_PER_QUAD)
                    + TRI_ARRAY_VERT_COUNT,
                TRIANGLE_VERTEX_COUNT * (DISC_SECTION_TRI_COUNT + TRIS_PER_QUAD)
                    + TRI_ARRAY_TRI_COUNT,
            );
            pub const DISC_SECTION_MESH_STATS: MeshStats =
                MeshStats::new(DISC_SECTION_VERT_COUNT, DISC_SECTION_TRI_COUNT, PIECE_TYPE);
            pub const MESH_STATS_SLICE: MeshStatsRefSlice = &[
                &BASE_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
                &DISC_SECTION_MESH_STATS,
            ];
            pub const MESH_COUNT: usize = MESH_STATS_SLICE.len();
            pub const MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(MESH_STATS_SLICE);
        }

        pub const SUBDIVISION_COUNT: usize = 16_usize;
        pub const SUBDIVISION_COUNT_F32: f32 = SUBDIVISION_COUNT as f32;

        pub type TriArray<T> = TriangularArray!(T, SUBDIVISION_COUNT + 1_usize);
        pub type TriArrayIndex = Index<{ <Helpers!(TriArray<()>)>::SIDE_LEN }>;
        pub type TriArrayIndexMap = IndexMap<{ <Helpers!(TriArray<()>)>::SIDE_LEN }>;

        pub const TRI_ARRAY_VERT_COUNT: usize = <Helpers!(TriArray<()>)>::FULL_SIZE;
        pub const TRI_ARRAY_TRI_COUNT: usize = <Helpers!(TriArray<()>)>::TRI_COUNT;
        pub const DESIGN_MESH_COUNT: usize = pentagon::MESH_COUNT + triangle::MESH_COUNT;
        pub const DESIGN_MESH_STATS_SUM: MeshStats =
            MeshStats::sum_slice(&[&pentagon::MESH_STATS_SUM, &triangle::MESH_STATS_SUM]);
    }

    pub const PENTAGON_VERTEX_COUNT: usize = usize::PENTAGON_VERTEX_COUNT;
    pub const TRIANGLE_VERTEX_COUNT: usize = usize::TRIANGLE_VERTEX_COUNT;
    pub const MAX_VERTEX_COUNT: usize = max!(PENTAGON_VERTEX_COUNT, TRIANGLE_VERTEX_COUNT);
    pub const VERTS_PER_QUAD: usize = 4_usize;
    pub const TRIS_PER_QUAD: usize = 2_usize;
    pub const TOTAL_MESH_COUNT: usize = original_super_dodecahedron::DESIGN_MESH_COUNT
        + custom_super_dodecahedron::DESIGN_MESH_COUNT
        + super_icosahedron::DESIGN_MESH_COUNT
        + rhombic_triacontahedron::DESIGN_MESH_COUNT
        + rounded_rhombic_triacontahedron::DESIGN_MESH_COUNT;
    pub const TOTAL_MESH_STATS_SUM: MeshStats = MeshStats::sum_slice(&[
        &original_super_dodecahedron::DESIGN_MESH_STATS_SUM,
        &custom_super_dodecahedron::DESIGN_MESH_STATS_SUM,
        &super_icosahedron::DESIGN_MESH_STATS_SUM,
        &rhombic_triacontahedron::DESIGN_MESH_STATS_SUM,
        &rounded_rhombic_triacontahedron::DESIGN_MESH_STATS_SUM,
    ]);
    pub const TOTAL_VERTEX_COUNT: usize = TOTAL_MESH_STATS_SUM.vertices;
    pub const TOTAL_INDEX_COUNT: usize = TOTAL_MESH_STATS_SUM.indices;
    pub const TOTAL_FACE_INDEX_COUNT: usize = TOTAL_MESH_STATS_SUM.face_indices;
    pub const PLANAR_OFFSET_RATIO: f32 = 1.0_f32 / 64.0_f32;
    pub const NORMAL_OFFSET_RATIO: f32 = 1.0_f32 / 256.0_f32;
    pub const SHELL_DEPTH_RATIO: f32 = 0.25_f32;
    pub const PLANAR_OFFSET: f32 = PLANAR_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;
    pub const NORMAL_OFFSET: f32 = NORMAL_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;
}

#[derive(Default)]
struct PieceHeader(Range<u32>);

impl<'a> TryFrom<&mut PieceHeaderParams<'a>> for PieceHeader {
    type Error = ();

    fn try_from(params: &mut PieceHeaderParams) -> Result<Self, ()> {
        use gen::*;

        type EmptyResult = Result<(), ()>;

        macro_rules! err {
            () => {
                return Err(())
            };
        }
        macro_rules! ok {
            () => {
                return Ok(())
            };
        }

        #[cfg(debug_assertions)]
        macro_rules! render_mesh_begin {
            ($x:ident, $render:expr) => {
                let $x: usize = if $render {
                    usize::MAX
                } else {
                    params.mesh_attribute_data_with_stats.stats.vertices
                };
            };
        }

        #[cfg(debug_assertions)]
        macro_rules! render_mesh_end {
            ($x:ident) => {
                if $x != usize::MAX {
                    params
                        .mesh_attribute_data_with_stats
                        .data
                        .zero($x..params.mesh_attribute_data_with_stats.stats.vertices);
                }
            };
        }

        #[cfg(not(debug_assertions))]
        macro_rules! render_mesh_begin {
            ($render:expr) => {};
        }

        #[cfg(not(debug_assertions))]
        macro_rules! render_mesh_end {
            () => {};
        }

        fn next_vert_index<I: PrimInt>(curr_vert_index: I, vert_count: I) -> I {
            (curr_vert_index + I::one()) % vert_count
        }

        fn prev_vert_index<I: PrimInt>(curr_vert_index: I, vert_count: I) -> I {
            (curr_vert_index + vert_count - I::one()) % vert_count
        }

        let start: u32 = params.mesh_headers_with_index.index as u32;

        let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);
        let icosidodecahedron_verts: &Vec<VertexData> = &icosidodecahedron_data.verts;
        let icosidodecahedron_vert_indices: &Vec<usize> = &icosidodecahedron_data.vert_indices;
        let icosidodecahedron_faces: &Vec<FaceData> = &icosidodecahedron_data.faces;

        let vert_count: usize = params.piece_type.vertex_count();
        let index_offset: usize = params.piece_type.index_offset();
        let instance_count: usize = params.piece_type.instance_count();
        let face_range: Range<usize> = index_offset..index_offset + instance_count;
        let vert_range: Range<usize> = 0_usize..vert_count;
        let transformation: Quat = icosidodecahedron_data.faces[index_offset].quat.inverse();
        let face_iter = |vertices: &mut [MU<Vec3>]| {
            for (vert_index, vert) in icosidodecahedron_data.faces[index_offset]
                .range
                .clone()
                .map(|vert_indices_index: usize| -> &Vec3 {
                    &icosidodecahedron_verts[icosidodecahedron_vert_indices[vert_indices_index]].vec
                })
                .enumerate()
            {
                vertices[vert_index].write(transformation * *vert);
            }
        };
        let pyramid_center = || -> Vec3 {
            transformation * Data::get(params.piece_type.pyramid_polyhedron()).verts[0_usize].vec
        };
        let add_adjacent_face_indices_same_piece =
            |params: &mut PieceHeaderParams, vert_index: usize| {
                let index_offset: u8 = index_offset as u8;

                params.mesh_attribute_data_with_stats.face_indices().extend(
                    face_range.clone().map(|face_index: usize| -> u8 {
                        let face_data: &FaceData = &icosidodecahedron_faces[face_index];

                        icosidodecahedron_data.get_closest_face_index(
                            &(Quat::from_axis_angle(
                                icosidodecahedron_verts[face_data
                                    .get_slice(icosidodecahedron_vert_indices)[vert_index]]
                                    .vec
                                    .normalize(),
                                PI,
                            ) * face_data.norm),
                            None,
                        ) as u8
                            - index_offset
                    }),
                );
            };
        let map_adjacent_face_index_other_piece =
            |face_index: usize, vert_index: usize, next_vert_index: usize| -> usize {
                let edge_index: usize = icosidodecahedron_data
                    .get_edge_index(&{
                        let face_slice: &[usize] = icosidodecahedron_faces[face_index]
                            .get_slice(icosidodecahedron_vert_indices);

                        EdgeData::new(face_slice[vert_index], face_slice[next_vert_index])
                    })
                    .unwrap();

                icosidodecahedron_data.get_closest_face_index(
                    &Vec3::ZERO,
                    Some(&|query_face_index: usize, face_data: &FaceData| -> bool {
                        face_data.edges[edge_index] && query_face_index != face_index
                    }),
                )
            };
        let add_adjacent_face_indices_other_piece =
            |params: &mut PieceHeaderParams, vert_index: usize| {
                let next_vert_index: usize = params.piece_type.next_side_index(vert_index);
                let index_offset: u8 = params.piece_type.other().index_offset() as u8;

                params.mesh_attribute_data_with_stats.face_indices().extend(
                    face_range.clone().map(|face_index: usize| -> u8 {
                        map_adjacent_face_index_other_piece(face_index, vert_index, next_vert_index)
                            as u8
                            - index_offset
                    }),
                );
            };
        let add_piece_range = |params: &mut PieceHeaderParams| {
            params.mesh_attribute_data_with_stats.face_indices().extend(
                params
                    .piece_type
                    .range()
                    .map(|piece_index: usize| -> u8 { (piece_index - index_offset) as u8 }),
            );
        };
        let add_rhombic_triacontahedron_face_indices =
            |params: &mut PieceHeaderParams, vert_index: usize| {
                params.mesh_attribute_data_with_stats.face_indices().extend(
                    params.piece_type.range().map(|face_index: usize| -> u8 {
                        icosidodecahedron_faces[face_index]
                            .get_slice(icosidodecahedron_vert_indices)[vert_index]
                            as u8
                    }),
                );
            };
        let add_regular_polygon = |params: &mut PieceHeaderParams,
                                   vert_loop: &[Vec3],
                                   center: &Vec3,
                                   normal: &Vec3,
                                   offset: u32,
                                   flip_tris: bool| {
            let normal: Vec3 = *normal;
            let uv: Vec2 = Vec2::ZERO;
            let offset: u32 = params.mesh_attribute_data_with_stats.stats.vertices as u32 - offset;

            for vert in vert_loop {
                params
                    .mesh_attribute_data_with_stats
                    .push_vertex(MeshVertexData {
                        position: *vert,
                        normal,
                        uv,
                    });
            }

            params
                .mesh_attribute_data_with_stats
                .push_vertex(MeshVertexData {
                    position: *center,
                    normal,
                    uv,
                });

            let vert_count: u32 = vert_loop.len() as u32;
            let next_vert_index: fn(u32, u32) -> u32 = if flip_tris {
                prev_vert_index::<u32>
            } else {
                next_vert_index::<u32>
            };

            for curr_vert_index in 0_u32..vert_count {
                let next_vert_index: u32 = next_vert_index(curr_vert_index, vert_count);

                params.mesh_attribute_data_with_stats.indices().extend([
                    offset + vert_count,
                    offset + curr_vert_index,
                    offset + next_vert_index,
                ]);
            }
        };
        let add_pyramid = |params: &mut PieceHeaderParams,
                           vert_loop: &[Vec3],
                           curr_vert_index_iter: &mut dyn Iterator<Item = usize>,
                           center: &Vec3,
                           offset: u32,
                           offset_along_plane: bool,
                           offset_along_normal: bool,
                           flip_tris: bool| {
            let vert_count: usize = vert_loop.len();
            let next_vert_index: fn(usize, usize) -> usize = if flip_tris {
                prev_vert_index::<usize>
            } else {
                next_vert_index::<usize>
            };

            for curr_vert_index in curr_vert_index_iter {
                let next_vert_index: usize = next_vert_index(curr_vert_index, vert_count);

                let mut tri: Tri = Tri::from([
                    *center,
                    vert_loop[curr_vert_index],
                    vert_loop[next_vert_index],
                ]);

                if offset_along_plane {
                    tri.offset_all_along_plane(-PLANAR_OFFSET);
                }

                if offset_along_normal {
                    tri.offset_along_normal(NORMAL_OFFSET);
                }

                params
                    .mesh_attribute_data_with_stats
                    .push_vertex((tri, offset));
            }
        };

        let add_kite_group = |params: &mut PieceHeaderParams,
                              far_vertices: &[Vec3],
                              near_vertices: &[Vec3],
                              vert_index_iter: &mut dyn Iterator<Item = usize>,
                              center: &Vec3,
                              offset: u32,
                              offset_along_plane: bool,
                              offset_along_normal: bool,
                              left_is_next: bool| {
            let uv: Vec2 = Vec2::ZERO;
            let left_index: fn(usize, usize) -> usize = if left_is_next {
                next_vert_index::<usize>
            } else {
                prev_vert_index::<usize>
            };

            for vert_index in vert_index_iter {
                let offset: u32 =
                    params.mesh_attribute_data_with_stats.stats.vertices as u32 - offset;
                let mut vert_a: Vec3 = far_vertices[vert_index];
                let mut vert_b: Vec3 = near_vertices[vert_index];
                let mut vert_c: Vec3 = *center;
                let mut vert_d: Vec3 = near_vertices[left_index(vert_index, vert_count)];

                if offset_along_plane || offset_along_normal {
                    let mut left_tri: Tri = Tri::from([vert_c, vert_d, vert_a]);
                    let mut right_tri: Tri = Tri::from([vert_a, vert_b, vert_c]);

                    if offset_along_plane {
                        let mask: [bool; 3_usize] = [true, true, false];

                        left_tri.offset_along_plane(-PLANAR_OFFSET, mask);
                        right_tri.offset_along_plane(-PLANAR_OFFSET, mask);
                    }

                    if offset_along_normal {
                        left_tri.offset_along_normal(NORMAL_OFFSET);
                        right_tri.offset_along_normal(NORMAL_OFFSET);
                    }

                    [vert_a, vert_b, vert_c] = right_tri.vertices;
                    vert_d = left_tri[1_usize];
                }

                let normal: Vec3 = (vert_b - vert_a).cross(vert_d - vert_a);

                params
                    .mesh_attribute_data_with_stats
                    .push_vertex(MeshVertexData {
                        position: vert_a,
                        normal,
                        uv,
                    });
                params
                    .mesh_attribute_data_with_stats
                    .push_vertex(MeshVertexData {
                        position: vert_b,
                        normal,
                        uv,
                    });
                params
                    .mesh_attribute_data_with_stats
                    .push_vertex(MeshVertexData {
                        position: vert_c,
                        normal,
                        uv,
                    });
                params
                    .mesh_attribute_data_with_stats
                    .push_vertex(MeshVertexData {
                        position: vert_d,
                        normal,
                        uv,
                    });
                params.mesh_attribute_data_with_stats.indices().extend([
                    offset,
                    offset + 1_u32,
                    offset + 2_u32,
                ]);
                params.mesh_attribute_data_with_stats.indices().extend([
                    offset + 2_u32,
                    offset + 3_u32,
                    offset,
                ]);
            }
        };

        let add_disc_section = |params: &mut PieceHeaderParams,
                                vert_arc: &[Vec3],
                                center: &Vec3,
                                normal: &Vec3,
                                offset: u32| {
            if vert_arc.len() < 2_usize {
                return;
            }

            let normal: Vec3 = *normal;
            let uv: Vec2 = Vec2::ZERO;
            let offset: u32 = params.mesh_attribute_data_with_stats.stats.vertices as u32 - offset;

            for vert in vert_arc {
                params
                    .mesh_attribute_data_with_stats
                    .push_vertex(MeshVertexData {
                        position: *vert,
                        normal,
                        uv,
                    });
            }

            params
                .mesh_attribute_data_with_stats
                .push_vertex(MeshVertexData {
                    position: *center,
                    normal,
                    uv,
                });

            let vert_count: u32 = vert_arc.len() as u32;

            for curr_vert_index in 0_u32..vert_count - 1_u32 {
                params.mesh_attribute_data_with_stats.indices().extend([
                    offset + vert_count,
                    offset + curr_vert_index,
                    offset + curr_vert_index + 1_u32,
                ]);
            }
        };
        let add_offset_disc_section = |params: &mut PieceHeaderParams,
                                       vert_arc: &mut [Vec3],
                                       center: &Vec3,
                                       normal: &Vec3,
                                       offset: u32| {
            warn_expect!(vert_arc.len() >= 2_usize, ?);

            let arc_len: usize = vert_arc.len();
            let arc_len_minus_1: usize = arc_len - 1_usize;

            let mut center: Vec3 = *center;
            let mut arc_start: Vec3 = vert_arc[0_usize];
            let mut center_start: Vec3 = center;
            let mut arc_end: Vec3 = vert_arc[arc_len_minus_1];
            let mut center_end: Vec3 = center;
            let normal: Vec3 = *normal;

            warn_expect!((arc_start - center_start).cross(arc_end - center_end).dot(normal) > 0.0_f32, ?);

            // Convert vert_arc into 2D space (the normal is the Z axis)
            let mat: Mat4 = Mat4::look_at_rh(
                center,
                center - normal,
                normal.cross(vert_arc[0_usize] - center).normalize(),
            );

            for vert in [
                &mut arc_start,
                &mut center_start,
                &mut arc_end,
                &mut center_end,
            ]
            .into_iter()
            .chain(vert_arc.iter_mut())
            {
                *vert = mat.project_point3(*vert);
            }

            let mut next_segment_start: Vec2;
            let mut segment_offset: Vec2;
            let (mut curr_segment_start_offset, mut curr_segment_end_offset): (Vec2, Vec2) = {
                let curr_segment_start: Vec2 = vert_arc[0_usize].xy();
                let curr_segment_end: Vec2 = vert_arc[1_usize].xy();

                next_segment_start = curr_segment_end;
                segment_offset =
                    PLANAR_OFFSET * (curr_segment_end - curr_segment_start).perp().normalize();

                (
                    curr_segment_start + segment_offset,
                    curr_segment_end + segment_offset,
                )
            };

            vert_arc[0_usize] = curr_segment_start_offset.extend(0.0_f32);

            for start_index in 1_usize..arc_len_minus_1 {
                let next_segment_end: Vec2 = vert_arc[start_index + 1_usize].xy();

                segment_offset =
                    PLANAR_OFFSET * (next_segment_end - next_segment_start).perp().normalize();

                let next_segment_start_offset: Vec2 = next_segment_start + segment_offset;
                let next_segment_end_offset: Vec2 = next_segment_end + segment_offset;

                if let Some(intersection) = warn_expect_some!(Option::<Vec2>::from(
                    two_d::compute_line_intersection(
                        curr_segment_start_offset,
                        curr_segment_end_offset,
                        next_segment_start_offset,
                        next_segment_end_offset,
                        None
                    )
                ), =>)
                {
                    vert_arc[start_index] = intersection.extend(0.0_f32);
                }

                next_segment_start = next_segment_end;
                curr_segment_start_offset = next_segment_start_offset;
                curr_segment_end_offset = next_segment_end_offset;
            }

            vert_arc[arc_len_minus_1] = curr_segment_end_offset.extend(0.0_f32);

            let mut arc_start_xy: Vec2 = arc_start.xy();
            let mut center_start_xy: Vec2 = center_start.xy();
            let offset_start: Vec2 =
                PLANAR_OFFSET * (arc_start_xy - center_start_xy).perp().normalize();

            arc_start_xy += offset_start;
            center_start_xy += offset_start;

            for start_index in 0_usize..arc_len_minus_1 {
                /* Use a ray test for both, since the start edge could've been pushed far enough to
                be past both points */
                if let Some(intersection) = Option::<Vec2>::from(two_d::compute_ray_intersection(
                    center_start_xy,
                    arc_start_xy,
                    vert_arc[start_index].xy(),
                    vert_arc[start_index + 1_usize].xy(),
                    None,
                )) {
                    vert_arc[start_index] = intersection.extend(0.0_f32);
                } else {
                    // No more segments from this direction will intersect. Break the loop
                    break;
                }
            }

            let mut arc_end_xy: Vec2 = arc_end.xy();
            let mut center_end_xy: Vec2 = center_end.xy();
            let offset_end: Vec2 = -PLANAR_OFFSET * (arc_end_xy - center_end_xy).perp().normalize();

            arc_end_xy += offset_end;
            center_end_xy += offset_end;

            for end_index in (1_usize..=arc_len_minus_1).into_iter().rev() {
                /* Use a ray test for both, since the start edge could've been pushed far enough to
                be past both points */
                if let Some(intersection) = Option::<Vec2>::from(two_d::compute_ray_intersection(
                    center_end_xy,
                    arc_end_xy,
                    vert_arc[end_index].xy(),
                    vert_arc[end_index - 1_usize].xy(),
                    None,
                )) {
                    vert_arc[end_index] = intersection.extend(0.0_f32);
                } else {
                    // No more segments from this direction will intersect. Break the loop
                    break;
                }
            }

            center = warn_expect_some!(Option::<Vec2>::from(two_d::compute_line_intersection(
                arc_start_xy,
                center_start_xy,
                arc_end_xy,
                center_end_xy,
                None
            )), =>)
            .unwrap_or_else(|| -> Vec2 { 0.5_f32 * (center_start_xy + center_end_xy) })
            .extend(0.0_f32);

            // Convert vert_arc back into its original 3D space
            let inv: Mat4 = mat.inverse();

            for vert in [&mut center].into_iter().chain(vert_arc.iter_mut()) {
                vert.z = NORMAL_OFFSET;
                *vert = inv.project_point3(*vert);
            }

            add_disc_section(params, vert_arc, &center, &normal, offset);
        };

        let mut mesh_index: usize = 0_usize;
        let mut offset: MeshStats = params.mesh_attribute_data_with_stats.stats;

        let check_mesh_stats = |params: &PieceHeaderParams,
                                mesh_stats_slice: MeshStatsRefSlice,
                                offset: &mut MeshStats,
                                mesh_index: &mut usize|
         -> EmptyResult {
            let new_offset: MeshStats = params.mesh_attribute_data_with_stats.stats;
            let expected_offset: MeshStats = *offset + *mesh_stats_slice[*mesh_index];

            if !warn_expect!(new_offset == expected_offset) {
                warn_expr!(
                    params.design,
                    params.piece_type,
                    mesh_index,
                    offset,
                    mesh_stats_slice[*mesh_index],
                    expected_offset,
                    new_offset
                );

                Err(())
            } else {
                *offset = new_offset;
                *mesh_index += 1_usize;

                Ok(())
            }
        };
        let push_mesh_header = |params: &mut PieceHeaderParams,
                                mesh_stats_slice: MeshStatsRefSlice,
                                offset: &MeshStats,
                                mesh_index: usize|
         -> u32 {
            params
                .mesh_headers_with_index
                .push(MeshHeader::new(offset, mesh_stats_slice[mesh_index]));

            offset.vertices as u32
        };

        match params.design {
            Design::OriginalSuperDodecahedron => {
                use original_super_dodecahedron::*;

                let uv: Vec2 = Vec2::ZERO;
                let add_trapezoidal_band =
                    |params: &mut PieceHeaderParams,
                     curr_vert_loop: &[Vec3],
                     next_vert_loop: &[Vec3],
                     offset: u32| {
                        for curr_vert_index in vert_range.clone() {
                            warn_expect!(curr_vert_loop.len() == next_vert_loop.len(), ?);

                            let vert_count: usize = curr_vert_loop.len();
                            let next_vert_index: usize = (curr_vert_index + 1_usize) % vert_count;
                            let normal: Vec3 = Tri::compute_normal(
                                &next_vert_loop[curr_vert_index],
                                &curr_vert_loop[curr_vert_index],
                                &curr_vert_loop[next_vert_index],
                            );
                            let offset: u32 = params.mesh_attribute_data_with_stats.stats.vertices
                                as u32
                                - offset;

                            params
                                .mesh_attribute_data_with_stats
                                .push_vertex(MeshVertexData {
                                    position: next_vert_loop[curr_vert_index],
                                    normal,
                                    uv,
                                });
                            params
                                .mesh_attribute_data_with_stats
                                .push_vertex(MeshVertexData {
                                    position: curr_vert_loop[curr_vert_index],
                                    normal,
                                    uv,
                                });
                            params
                                .mesh_attribute_data_with_stats
                                .push_vertex(MeshVertexData {
                                    position: curr_vert_loop[next_vert_index],
                                    normal,
                                    uv,
                                });
                            params
                                .mesh_attribute_data_with_stats
                                .push_vertex(MeshVertexData {
                                    position: next_vert_loop[next_vert_index],
                                    normal,
                                    uv,
                                });

                            let vert_index_a: u32 = offset;
                            let vert_index_b: u32 = offset + 1_u32;
                            let vert_index_c: u32 = offset + 2_u32;
                            let vert_index_d: u32 = offset + 3_u32;

                            params.mesh_attribute_data_with_stats.indices().extend([
                                vert_index_a,
                                vert_index_b,
                                vert_index_c,
                                vert_index_c,
                                vert_index_d,
                                vert_index_a,
                            ]);
                        }
                    };

                match params.piece_type {
                    Type::Pentagon => {
                        use pentagon::*;

                        type Vertices<T> = [[T; PENTAGON_VERTEX_COUNT]; PENTAGON_COUNT];
                        type PentagramTris<T> = [[T; PENTAGON_VERTEX_COUNT]; 2_usize];

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let (mut vertices, mut pentagram_tris): (
                            Vertices<MU<Vec3>>,
                            PentagramTris<MU<Tri>>,
                        ) = unsafe { MU::uninit().assume_init() };
                        let (
                            [outer_outer_vertices, outer_inner_vertices, inner_inner_vertices, inner_outer_vertices],
                            [outer_outer_pentagram_tris, outer_inner_pentagram_tris],
                        ) = (&mut vertices, &mut pentagram_tris);

                        face_iter(outer_outer_vertices);

                        // Safe: we just initialized each element of `outer_outer_vertices`
                        let outer_outer_vertices: &[Vec3] =
                            unsafe { MU::slice_assume_init_ref(outer_outer_vertices) };

                        for (
                            curr_vert_index,
                            (
                                outer_outer_vert,
                                outer_inner_vert,
                                inner_inner_vert,
                                inner_outer_vert,
                                outer_outer_pentagram_tri,
                                outer_inner_pentagram_tri,
                            ),
                        ) in izip!(
                            outer_outer_vertices.iter(),
                            outer_inner_vertices.iter_mut(),
                            inner_inner_vertices.iter_mut(),
                            inner_outer_vertices.iter_mut(),
                            outer_outer_pentagram_tris.iter_mut(),
                            outer_inner_pentagram_tris.iter_mut()
                        )
                        .enumerate()
                        {
                            let mut tri: Tri = Tri::from([
                                outer_outer_vertices[PIECE_TYPE.prev_side_index(curr_vert_index)],
                                *outer_outer_vert,
                                outer_outer_vertices[PIECE_TYPE.next_side_index(curr_vert_index)],
                            ]);

                            outer_outer_pentagram_tri.write(tri);
                            tri.offset_along_plane(-PENTAGONAL_ANNULUS_THICKNESS, OFFSET_MASK);
                            outer_inner_pentagram_tri.write(tri);

                            let new_outer_inner_vert: &Vec3 = &tri[CURR_TRI_VERT_INDEX];
                            let depth_offset: Vec3 = -SHELL_DEPTH_RATIO * *new_outer_inner_vert;

                            outer_inner_vert.write(*new_outer_inner_vert);
                            inner_inner_vert.write(*new_outer_inner_vert + depth_offset);
                            inner_outer_vert.write(*outer_outer_vert + depth_offset);
                        }

                        /* Safe: we just initialized each element of `outer_outer_vertices`,
                        `inner_inner_vertices`, `inner_outer_vertices`,
                        `outer_outer_pentagram_tris`, and `outer_inner_pentagram_tris` */
                        #[allow(clippy::type_complexity)]
                        let (
                            outer_inner_vertices,
                            inner_inner_vertices,
                            inner_outer_vertices,
                            outer_outer_pentagram_tris,
                            outer_inner_pentagram_tris,
                        ): (
                            &[Vec3],
                            &[Vec3],
                            &[Vec3],
                            &mut [Tri],
                            &mut [Tri],
                        ) = unsafe {
                            (
                                MU::slice_assume_init_ref(outer_inner_vertices),
                                MU::slice_assume_init_ref(inner_inner_vertices),
                                MU::slice_assume_init_ref(inner_outer_vertices),
                                MU::slice_assume_init_mut(outer_outer_pentagram_tris),
                                MU::slice_assume_init_mut(outer_inner_pentagram_tris),
                            )
                        };

                        let add_pentagonal_annulus =
                            |params: &mut PieceHeaderParams,
                             curr_vert_pentagon: &[Vec3],
                             next_vert_pentagon: &[Vec3],
                             offset: u32| {
                                // Until <[MU<T>; N]>::array_assume_init_ref() is a thing, verify slice
                                // length
                                warn_expect!(curr_vert_pentagon.len() == PENTAGON_VERTEX_COUNT
                                && next_vert_pentagon.len() == PENTAGON_VERTEX_COUNT, ?);

                                let normal: Vec3 = Tri::compute_normal(
                                    &next_vert_pentagon[0_usize],
                                    &curr_vert_pentagon[0_usize],
                                    &curr_vert_pentagon[1_usize],
                                );
                                let offset: u32 =
                                    params.mesh_attribute_data_with_stats.stats.vertices as u32
                                        - offset;
                                let curr_loop_offset: usize = CURR_LOOP_INDEX * vert_count;
                                let next_loop_offset: usize = NEXT_LOOP_INDEX * vert_count;
                                let mut push_vertex = |position: &Vec3| {
                                    params.mesh_attribute_data_with_stats.push_vertex(
                                        MeshVertexData {
                                            position: *position,
                                            normal,
                                            uv,
                                        },
                                    );
                                };

                                curr_vert_pentagon.iter().for_each(&mut push_vertex);
                                next_vert_pentagon.iter().for_each(&mut push_vertex);

                                for curr_vert_index in vert_range.clone() {
                                    let next_vert_index: usize =
                                        PIECE_TYPE.next_side_index(curr_vert_index);
                                    let vert_index_a: u32 =
                                        (next_loop_offset + curr_vert_index) as u32 + offset;
                                    let vert_index_b: u32 =
                                        (curr_loop_offset + curr_vert_index) as u32 + offset;
                                    let vert_index_c: u32 =
                                        (curr_loop_offset + next_vert_index) as u32 + offset;
                                    let vert_index_d: u32 =
                                        (next_loop_offset + next_vert_index) as u32 + offset;

                                    params.mesh_attribute_data_with_stats.indices().extend([
                                        vert_index_a,
                                        vert_index_b,
                                        vert_index_c,
                                        vert_index_c,
                                        vert_index_d,
                                        vert_index_a,
                                    ]);
                                }
                            };

                        // Add the base mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            add_pentagonal_annulus(
                                params,
                                outer_outer_vertices,
                                outer_inner_vertices,
                                vertices_offset,
                            );
                            add_trapezoidal_band(
                                params,
                                outer_inner_vertices,
                                inner_inner_vertices,
                                vertices_offset,
                            );
                            add_pentagonal_annulus(
                                params,
                                inner_inner_vertices,
                                inner_outer_vertices,
                                vertices_offset,
                            );
                            add_trapezoidal_band(
                                params,
                                inner_outer_vertices,
                                outer_outer_vertices,
                                vertices_offset,
                            );
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the primary pent mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            /* Safe: all we are claiming to have initialized are `MU` objects, which
                            do not require initialization */
                            let mut vertices: [[MU<Vec3>; PENTAGON_VERTEX_COUNT]; 2_usize] =
                                unsafe { MU::uninit().assume_init() };

                            for (vertices, pentagram_tris, offset) in izip!(
                                vertices.iter_mut(),
                                [outer_outer_pentagram_tris, outer_inner_pentagram_tris],
                                [-PLANAR_OFFSET, PLANAR_OFFSET].into_iter()
                            ) {
                                for (vert, pentagram_tri) in
                                    vertices.iter_mut().zip(pentagram_tris.iter_mut())
                                {
                                    pentagram_tri.offset_along_plane(offset, OFFSET_MASK);
                                    pentagram_tri.offset_along_normal(NORMAL_OFFSET);
                                    vert.write(pentagram_tri[CURR_TRI_VERT_INDEX]);
                                }
                            }

                            /* Safe: we just initialized each element of `curr_loop_vertices` and
                            `next_loop_vertices` */
                            let (curr_loop_vertices, next_loop_vertices): (&[Vec3], &[Vec3]) = unsafe {
                                (
                                    MU::slice_assume_init_ref(&vertices[CURR_LOOP_INDEX]),
                                    MU::slice_assume_init_ref(&vertices[NEXT_LOOP_INDEX]),
                                )
                            };

                            add_pentagonal_annulus(
                                params,
                                curr_loop_vertices,
                                next_loop_vertices,
                                vertices_offset,
                            );
                            add_piece_range(params);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the adjacent pent meshes
                        for vert_index in vert_range.clone() {
                            /* These meshes are just one circle each, so no need to worry about the
                            offset */
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            let start_edge_index: usize =
                                PIECE_TYPE.next_side_index(vert_index + 1_usize);
                            let end_edge_index: usize =
                                PIECE_TYPE.next_side_index(start_edge_index);
                            let vert_a: Vec3 = inner_inner_vertices[start_edge_index];
                            let vert_b: Vec3 = outer_inner_vertices[start_edge_index];
                            let vert_c: Vec3 = outer_inner_vertices[end_edge_index];
                            let vert_d: Vec3 = inner_inner_vertices[end_edge_index];
                            let edge_bc_midpoint: Vec3 = 0.5_f32 * (vert_b + vert_c);
                            let edge_ad_midpoint: Vec3 = 0.5_f32 * (vert_a + vert_d);
                            let diameter: Vec3 = edge_bc_midpoint - edge_ad_midpoint;
                            let diameter_len: f32 = diameter.length();
                            let radius: Vec3 = ((0.5_f32 * diameter_len - PLANAR_OFFSET)
                                / diameter_len)
                                * diameter;
                            let normal: Vec3 = diameter.cross(vert_c - vert_b).normalize();
                            let center: Vec3 = 0.5_f32 * (edge_bc_midpoint + edge_ad_midpoint)
                                + NORMAL_OFFSET * normal;

                            /* Safe: all we are claiming to have initialized are `MU` objects, which
                            do not require initialization */
                            let mut circle_vertices: [MU<Vec3>; CIRCLE_SUBDIVISION_COUNT] =
                                unsafe { MU::uninit().assume_init() };

                            for (circle_vert_index, circle_vert) in
                                circle_vertices.iter_mut().enumerate()
                            {
                                circle_vert.write(
                                    Quat::from_axis_angle(
                                        normal,
                                        circle_vert_index as f32
                                            * TAU_OVER_CIRCLE_SUBDIVISION_COUNT,
                                    ) * radius
                                        + center,
                                );
                            }

                            add_regular_polygon(
                                params,
                                // Safe: we just initialized each element of `circle_vertices`
                                unsafe { MU::slice_assume_init_ref(&circle_vertices) },
                                &center,
                                &normal,
                                vertices_offset,
                                false,
                            );
                            add_adjacent_face_indices_same_piece(params, vert_index);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }
                    }
                    Type::Triangle => {
                        use triangle::*;

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let mut vertices: [[MU<Vec3>; TRIANGLE_VERTEX_COUNT]; 2_usize] =
                            unsafe { MU::uninit().assume_init() };
                        let [outer_vert_loop, inner_vert_loop] = &mut vertices;

                        face_iter(outer_vert_loop);

                        // Safe: we just initialized each element of `outer_vert_loop`
                        let outer_vert_loop: &[Vec3] =
                            unsafe { MU::slice_assume_init_ref(outer_vert_loop) };

                        let outer_center: Vec3 = pyramid_center();

                        // Add the base mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);
                            let inner_center: Vec3 = {
                                let mut inner_center_sum: Vec3 = Vec3::ZERO;

                                for (inner_vert, outer_vert) in
                                    inner_vert_loop.iter_mut().zip(outer_vert_loop.iter())
                                {
                                    inner_vert.write((1.0_f32 - SHELL_DEPTH_RATIO) * *outer_vert);
                                    inner_center_sum += *outer_vert;
                                }

                                inner_center_sum / TRIANGLE_VERTEX_COUNT as f32
                            };

                            // Safe: we just initialized each element of `inner_vert_loop`
                            let inner_vert_loop: &[Vec3] =
                                unsafe { MU::slice_assume_init_ref(inner_vert_loop) };

                            add_pyramid(
                                params,
                                outer_vert_loop,
                                &mut vert_range.clone(),
                                &outer_center,
                                vertices_offset,
                                false,
                                false,
                                false,
                            );
                            add_trapezoidal_band(
                                params,
                                inner_vert_loop,
                                outer_vert_loop,
                                vertices_offset,
                            );
                            add_regular_polygon(
                                params,
                                inner_vert_loop,
                                &inner_center,
                                &(-inner_center).normalize(),
                                vertices_offset,
                                true,
                            );
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the adjacent pent meshes
                        for vert_index in vert_range.clone() {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            add_pyramid(
                                params,
                                outer_vert_loop,
                                &mut [vert_index].into_iter(),
                                &outer_center,
                                vertices_offset,
                                true,
                                true,
                                false,
                            );
                            add_adjacent_face_indices_other_piece(params, vert_index);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }
                    }
                }
            }
            Design::CustomSuperDodecahedron => {
                use custom_super_dodecahedron::*;

                match params.piece_type {
                    Type::Pentagon => {
                        use pentagon::*;

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let mut vertices: [[MU<Vec3>; PENTAGON_VERTEX_COUNT]; PENTAGON_COUNT] =
                            unsafe { MU::uninit().assume_init() };
                        let [outer_pentagon, midpoint_pentagon, pentagram_pentagon] = &mut vertices;

                        face_iter(outer_pentagon);

                        // Safe: we just initialized each element of `outer_pentagon`
                        let outer_pentagon: &[Vec3] =
                            unsafe { MU::slice_assume_init_ref(outer_pentagon) };

                        let center: Vec3 = {
                            let mut center_sum: Vec3 = Vec3::ZERO;

                            for (vert_index, (midpoint_vert, pentagram_vert, outer_vert)) in izip!(
                                midpoint_pentagon.iter_mut(),
                                pentagram_pentagon.iter_mut(),
                                outer_pentagon.iter()
                            )
                            .enumerate()
                            {
                                let next_side_index: usize = PIECE_TYPE.next_side_index(vert_index);

                                midpoint_vert.write(
                                    (*outer_vert + outer_pentagon[next_side_index]) * 0.5_f32,
                                );
                                pentagram_vert.write(
                                    (ONE_OVER_PHI as f32) * *outer_vert
                                        + (ONE_OVER_PHI_SQUARED as f32)
                                            * outer_pentagon
                                                [PIECE_TYPE.next_side_index(next_side_index)],
                                );
                                center_sum += *outer_vert;
                            }

                            center_sum / PENTAGON_VERTEX_COUNT as f32
                        };

                        /* Safe: we just initialized each element of `midpoint_pentagon` and
                        `pentagram_pentagon` */
                        let (midpoint_pentagon, pentagram_pentagon): (&[Vec3], &[Vec3]) = unsafe {
                            (
                                MU::slice_assume_init_ref(midpoint_pentagon),
                                MU::slice_assume_init_ref(pentagram_pentagon),
                            )
                        };

                        // Add the base mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            add_regular_polygon(
                                params,
                                outer_pentagon,
                                &center,
                                &center.normalize(),
                                vertices_offset,
                                false,
                            );
                            add_pyramid(
                                params,
                                outer_pentagon,
                                &mut vert_range.clone(),
                                &Vec3::ZERO,
                                vertices_offset,
                                false,
                                false,
                                true,
                            );
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the primary pent mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            for vert_index in vert_range.clone() {
                                let mut tri: Tri = Tri::from([
                                    outer_pentagon[vert_index],
                                    pentagram_pentagon[vert_index],
                                    pentagram_pentagon[PIECE_TYPE.prev_side_index(vert_index)],
                                ]);

                                tri.offset_all_along_plane(-PLANAR_OFFSET);
                                tri.offset_along_normal(NORMAL_OFFSET);
                                params
                                    .mesh_attribute_data_with_stats
                                    .push_vertex((tri, vertices_offset));
                            }

                            add_piece_range(params);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the adjacent pent meshes
                        for vert_index in vert_range {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);
                            let prev_vert_index: usize = PIECE_TYPE.prev_side_index(vert_index);

                            for tri_index in 0_usize..3 {
                                let mut tri: Tri = match tri_index {
                                    0 => Tri::from([
                                        outer_pentagon[vert_index],
                                        midpoint_pentagon[vert_index],
                                        pentagram_pentagon[vert_index],
                                    ]),
                                    1 => Tri::from([
                                        center,
                                        pentagram_pentagon[prev_vert_index],
                                        pentagram_pentagon[vert_index],
                                    ]),
                                    _ => Tri::from([
                                        outer_pentagon[vert_index],
                                        pentagram_pentagon[prev_vert_index],
                                        midpoint_pentagon[prev_vert_index],
                                    ]),
                                };

                                tri.offset_all_along_plane(-PLANAR_OFFSET);
                                tri.offset_along_normal(NORMAL_OFFSET);
                                params
                                    .mesh_attribute_data_with_stats
                                    .push_vertex((tri, vertices_offset));
                            }

                            add_adjacent_face_indices_same_piece(params, vert_index);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }
                    }
                    Type::Triangle => {
                        use triangle::*;

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let mut vertices: [MU<Vec3>; TRIANGLE_VERTEX_COUNT] =
                            unsafe { MU::uninit().assume_init() };

                        face_iter(&mut vertices);

                        // Safe: we just initialized each element of `vertices`
                        let vertices: &[Vec3] = unsafe { MU::slice_assume_init_ref(&vertices) };
                        let center: Vec3 = pyramid_center();

                        // Add the base mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            add_pyramid(
                                params,
                                vertices,
                                &mut vert_range.clone(),
                                &center,
                                vertices_offset,
                                false,
                                false,
                                false,
                            );
                            add_pyramid(
                                params,
                                vertices,
                                &mut vert_range.clone(),
                                &Vec3::ZERO,
                                vertices_offset,
                                false,
                                false,
                                true,
                            );
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the adjacent pent meshes
                        for vert_index in vert_range {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            add_pyramid(
                                params,
                                vertices,
                                &mut [vert_index].into_iter(),
                                &center,
                                vertices_offset,
                                true,
                                true,
                                false,
                            );
                            add_adjacent_face_indices_other_piece(params, vert_index);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }
                    }
                }
            }
            Design::SuperIcosahedron => {
                use super_icosahedron::*;

                match params.piece_type {
                    Type::Pentagon => {
                        use pentagon::*;

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let mut vertices: [MU<Vec3>; PENTAGON_VERTEX_COUNT] =
                            unsafe { MU::uninit().assume_init() };

                        face_iter(&mut vertices);

                        // Safe: we just initialized each element of `vertices`
                        let vertices: &[Vec3] = unsafe { MU::slice_assume_init_ref(&vertices) };
                        let center: Vec3 = pyramid_center();

                        // Add the base mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            add_pyramid(
                                params,
                                vertices,
                                &mut vert_range.clone(),
                                &center,
                                vertices_offset,
                                false,
                                false,
                                false,
                            );
                            add_pyramid(
                                params,
                                vertices,
                                &mut vert_range.clone(),
                                &Vec3::ZERO,
                                vertices_offset,
                                false,
                                false,
                                true,
                            );
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the adjacent tri meshes
                        for vert_index in vert_range {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            add_pyramid(
                                params,
                                vertices,
                                &mut [vert_index].into_iter(),
                                &center,
                                vertices_offset,
                                true,
                                true,
                                false,
                            );
                            add_adjacent_face_indices_other_piece(params, vert_index);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }
                    }
                    Type::Triangle => {
                        use triangle::*;

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let mut vertices: [[MU<Vec3>; TRIANGLE_VERTEX_COUNT]; 2_usize] =
                            unsafe { MU::uninit().assume_init() };
                        let [outer_triangle, midpoint_triangle] = &mut vertices;

                        face_iter(outer_triangle);

                        // Safe: we just initialized each element of `outer_triangle`
                        let outer_triangle: &[Vec3] =
                            unsafe { MU::slice_assume_init_ref(outer_triangle) };

                        for (vert_index, (midpoint_vert, outer_vert)) in midpoint_triangle
                            .iter_mut()
                            .zip(outer_triangle.iter())
                            .enumerate()
                        {
                            midpoint_vert.write(
                                (*outer_vert
                                    + outer_triangle[PIECE_TYPE.next_side_index(vert_index)])
                                    * 0.5_f32,
                            );
                        }

                        // Safe: we just initialized each element of `midpoint_triangle`
                        let midpoint_triangle: &[Vec3] =
                            unsafe { MU::slice_assume_init_ref(midpoint_triangle) };

                        // Add the base mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            params
                                .mesh_attribute_data_with_stats
                                .push_vertex((Tri::from(outer_triangle), vertices_offset));
                            add_pyramid(
                                params,
                                outer_triangle,
                                &mut vert_range.clone(),
                                &Vec3::ZERO,
                                vertices_offset,
                                false,
                                false,
                                true,
                            );
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the primary tri mesh
                        {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            let mut tri: Tri = Tri::from(midpoint_triangle);

                            tri.offset_all_along_plane(-PLANAR_OFFSET);
                            tri.offset_along_normal(NORMAL_OFFSET);
                            params
                                .mesh_attribute_data_with_stats
                                .push_vertex((tri, vertices_offset));
                            add_piece_range(params);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }

                        // Add the adjacent tri meshes
                        for vert_index in vert_range {
                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            let mut tri: Tri = Tri::from([
                                outer_triangle[vert_index],
                                midpoint_triangle[vert_index],
                                midpoint_triangle[PIECE_TYPE.prev_side_index(vert_index)],
                            ]);

                            tri.offset_all_along_plane(-PLANAR_OFFSET);
                            tri.offset_along_normal(NORMAL_OFFSET);
                            params
                                .mesh_attribute_data_with_stats
                                .push_vertex((tri, vertices_offset));
                            add_adjacent_face_indices_same_piece(params, vert_index);
                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                        }
                    }
                }
            }
            Design::RhombicTriacontahedron => {
                use rhombic_triacontahedron::*;

                let rhombic_triacontahedron_data: &Data =
                    Data::get(Polyhedron::RhombicTriacontahedron);
                let center: Vec3 = pyramid_center();

                /* Safe: all we are claiming to have initialized are `MU` objects, which do not
                require initialization */
                let mut vertices: [[MU<Vec3>; MAX_VERTEX_COUNT]; 2_usize] =
                    unsafe { MU::uninit().assume_init() };
                let [polygon, plane_intersections] = &mut vertices;

                face_iter(&mut polygon[vert_range.clone()]);

                // Safe: we just initialized the accessed elements of `polygon`
                let polygon: &[Vec3] =
                    unsafe { MU::slice_assume_init_ref(&polygon[vert_range.clone()]) };

                for (vert_index, (plane_intersection, polygon_vert)) in plane_intersections
                    [vert_range.clone()]
                .iter_mut()
                .zip(polygon.iter())
                .enumerate()
                {
                    let next_vert_index: usize = params.piece_type.next_side_index(vert_index);

                    plane_intersection.write(warn_expect_some!(
                        Option::<Vec3>::from(three_d::compute_line_plane_intersection(
                            center,
                            transformation
                                * rhombic_triacontahedron_data.verts
                                    [map_adjacent_face_index_other_piece(
                                        index_offset,
                                        vert_index,
                                        next_vert_index
                                    )]
                                .vec
                                - center,
                            Vec3::ZERO,
                            polygon_vert.cross(polygon[next_vert_index]),
                            None
                        )),
                        return Err(())
                    ));
                }

                // Safe: we just initialized the accessed elements of `plane_intersections`
                let plane_intersections: &[Vec3] =
                    unsafe { MU::slice_assume_init_ref(&plane_intersections[vert_range.clone()]) };

                let mesh_stats_slice: MeshStatsRefSlice = match params.piece_type {
                    Type::Pentagon => pentagon::MESH_STATS_SLICE,
                    Type::Triangle => triangle::MESH_STATS_SLICE,
                };

                // Add the base mesh
                {
                    let vertices_offset: u32 =
                        push_mesh_header(params, mesh_stats_slice, &offset, mesh_index);

                    add_kite_group(
                        params,
                        polygon,
                        plane_intersections,
                        &mut vert_range.clone(),
                        &center,
                        vertices_offset,
                        false,
                        false,
                        false,
                    );
                    add_kite_group(
                        params,
                        plane_intersections,
                        polygon,
                        &mut vert_range.clone(),
                        &Vec3::ZERO,
                        vertices_offset,
                        false,
                        false,
                        true,
                    );
                    check_mesh_stats(params, mesh_stats_slice, &mut offset, &mut mesh_index)?;
                }

                // Add the rhombus meshes
                for vert_index in vert_range {
                    let vertices_offset: u32 =
                        push_mesh_header(params, mesh_stats_slice, &offset, mesh_index);

                    add_kite_group(
                        params,
                        polygon,
                        plane_intersections,
                        &mut [vert_index].into_iter(),
                        &center,
                        vertices_offset,
                        true,
                        true,
                        false,
                    );

                    add_rhombic_triacontahedron_face_indices(params, vert_index);
                    check_mesh_stats(params, mesh_stats_slice, &mut offset, &mut mesh_index)?;
                }
            }
            Design::RoundedRhombicTriacontahedron => {
                use rounded_rhombic_triacontahedron::*;

                const RADIUS: f32 = {
                    const BASE_VECTOR_0: [f32; 3_usize] = unsafe {
                        transmute::<Vec3, [f32; 3_usize]>(ICOSIDODECAHEDRON.base_vectors[0_usize])
                    };
                    const BASE_VECTOR_1: [f32; 3_usize] = unsafe {
                        transmute::<Vec3, [f32; 3_usize]>(ICOSIDODECAHEDRON.base_vectors[1_usize])
                    };

                    /* MIDPOINT is the mean of BASE_VECTOR_0 and BASE_VECTOR_1. Since both base
                    vectors lie on the same sphere, the radius through MIDPOINT is perpendicular to
                    the line segment between the two base vectors */
                    const MIDPOINT: [f32; 3_usize] = [
                        0.5_f32 * (BASE_VECTOR_0[0_usize] + BASE_VECTOR_1[0_usize]),
                        0.5_f32 * (BASE_VECTOR_0[1_usize] + BASE_VECTOR_1[1_usize]),
                        0.5_f32 * (BASE_VECTOR_0[2_usize] + BASE_VECTOR_1[2_usize]),
                    ];
                    const MIDPOINT_RADIUS: f32 = const_sqrt_f64(
                        (MIDPOINT[0_usize] * MIDPOINT[0_usize]
                            + MIDPOINT[1_usize] * MIDPOINT[1_usize]
                            + MIDPOINT[2_usize] * MIDPOINT[2_usize]) as f64,
                    ) as f32;

                    /* The length we want is the radius that goes through MIDPOINT, terminating when
                    the segment between BASE_VECTOR_0 and the endpoint is perpendicular to the
                    radius through BASE_VECTOR_0.
                    BASE_VECTOR_0: B
                    MIDPOINT: M
                    center: C
                    "segment endpoint": X (we want the length of segment XC, which is essentially
                        the magnitude of X, since C is the origin)

                    Triangle BMC and triangle XBC are similar, since C, M, and X are colinear.
                    Thus, |X| == |B| ^ 2 / |M| */
                    BASE_VECTOR_0[0_usize] * BASE_VECTOR_0[0_usize] / MIDPOINT_RADIUS
                };
                const GENEROUS_EPSILON: f32 = 5.0_f32 * f32::EPSILON;

                fn length_is_radius(v: Vec3) -> bool {
                    (v.length() - RADIUS).abs() < GENEROUS_EPSILON
                }

                macro_rules! warn_expect_length_is_radius {
                    ($vert:expr) => {
                        if !warn_expect!(length_is_radius($vert)) {
                            warn_expr!($vert.length(), $vert.length() - RADIUS, GENEROUS_EPSILON);

                            debug_break();

                            err!();
                        }
                    };
                }

                fn resize_to_sphere(v: Vec3) -> Result<Vec3, ()> {
                    if warn_expect!(v.is_finite()) {
                        Ok(RADIUS * v.normalize())
                    } else {
                        warn_expr!(v);

                        err!();
                    }
                }
                fn init_arc_vertices<const AVC: usize>(
                    disc_centers: &[Vec3],
                    disc_kisses: &[Vec3],
                    arc_vertices: &mut [[MU<Vec3>; AVC]],
                    angle: f32,
                    piece_type: Type,
                ) -> EmptyResult {
                    let vertex_count: usize = piece_type.vertex_count();

                    warn_expect!(
                        disc_centers.len() == vertex_count
                            && disc_kisses.len() == vertex_count
                            && arc_vertices.len() == vertex_count,
                        return Err(())
                    );

                    let disc_section_tri_count_inv: f32 = 1.0_f32
                        / if matches!(piece_type, Type::Pentagon) {
                            pentagon::DISC_SECTION_TRI_COUNT
                        } else {
                            triangle::DISC_SECTION_TRI_COUNT
                        } as f32;

                    for (center_index, (arc_vertices, disc_center, initial_vert)) in izip!(
                        arc_vertices.iter_mut(),
                        disc_centers.iter(),
                        disc_kisses.iter()
                    )
                    .enumerate()
                    {
                        warn_expect_length_is_radius!(*initial_vert);

                        let axis: Vec3 = disc_center.normalize();
                        let final_vert: Vec3 =
                            disc_kisses[piece_type.prev_side_index(center_index)];

                        warn_expect_length_is_radius!(final_vert);

                        for (arc_vert_index, arc_vert) in arc_vertices.iter_mut().enumerate() {
                            let arc_vert: &Vec3 = arc_vert.write(
                                Quat::from_axis_angle(
                                    axis,
                                    arc_vert_index as f32 * angle * disc_section_tri_count_inv,
                                ) * *initial_vert,
                            );

                            warn_expect_length_is_radius!(*arc_vert);
                        }
                    }

                    ok!();
                }

                let index_map: &TriArrayIndexMap = &TriArrayIndexMap::default();

                /* Safe: all we are claiming to have initialized are `MU` objects, which do not
                require initialization */
                let mut vertices: [[MU<Vec3>; MAX_VERTEX_COUNT]; 2_usize] =
                    unsafe { MU::uninit().assume_init() };
                let [disc_centers, disc_kisses] = &mut vertices;

                face_iter(&mut disc_centers[vert_range.clone()]);

                // Safe: we just initialized the accessed elements of `disc_centers`
                let disc_centers: &[Vec3] =
                    unsafe { MU::slice_assume_init_ref(&disc_centers[vert_range.clone()]) };

                for (vert_index, (disc_kiss, disc_center)) in disc_kisses[vert_range.clone()]
                    .iter_mut()
                    .zip(disc_centers.iter())
                    .enumerate()
                {
                    disc_kiss.write(resize_to_sphere(
                        *disc_center + disc_centers[params.piece_type.next_side_index(vert_index)],
                    )?);
                }

                // Safe: we just initialized the accessed elements of `disc_kisses`
                let disc_kisses: &[Vec3] =
                    unsafe { MU::slice_assume_init_ref(&disc_kisses[vert_range.clone()]) };

                let angle: f32 = {
                    let center: Vec3 = disc_centers[0_usize];
                    let center_to_first_vert: Vec3 = disc_kisses[0_usize] - center;
                    let center_to_last_vert: Vec3 =
                        disc_kisses[params.piece_type.prev_side_index(0_usize)] - center;
                    let cross_dot_center: f32 = center_to_first_vert
                        .cross(center_to_last_vert)
                        .normalize()
                        .dot(center.normalize());

                    if !warn_expect!((cross_dot_center - 1.0_f32).abs() < f32::EPSILON) {
                        warn_expr!(cross_dot_center);

                        return Err(());
                    }

                    center_to_first_vert.angle_between(center_to_last_vert)
                };
                let init_border = |tri_array: &mut TriArray<Vec3>, border: IndexType2D| {
                    let first_index: TriArrayIndex = TriArrayIndex::corner(border);
                    let first_vert: Vec3 = tri_array[(index_map, &first_index)];
                    let last_vert: Vec3 = tri_array[(index_map, &first_index.last_in_row())];
                    let axis: Vec3 = first_vert.cross(last_vert).normalize();
                    let angle: f32 = first_vert.angle_between(last_vert);

                    tri_array.init_border(
                        index_map,
                        border,
                        (0_usize..=SUBDIVISION_COUNT).map(|index_2: usize| -> Vec3 {
                            Quat::from_axis_angle(
                                axis,
                                index_2 as f32 * angle / SUBDIVISION_COUNT_F32,
                            ) * first_vert
                        }),
                    );
                };
                let bisect_and_connect = |tri_array: &mut TriArray<Vec3>| -> EmptyResult {
                    for big_step_exponent in (1_u32..SUBDIVISION_COUNT.trailing_zeros()).rev() {
                        let big_step_size: usize = 1_usize << big_step_exponent;
                        let small_step_size: usize = big_step_size >> 1_u32;

                        for index_1 in (big_step_size..SUBDIVISION_COUNT).step_by(big_step_size) {
                            for index_2 in (SUBDIVISION_COUNT - index_1..SUBDIVISION_COUNT)
                                .step_by(big_step_size)
                            {
                                for index_type in IndexType2D::iter() {
                                    tri_array[(
                                        index_map,
                                        &TriArrayIndex::new(
                                            index_type,
                                            index_1,
                                            index_2 + small_step_size,
                                        ),
                                    )] = resize_to_sphere(
                                        tri_array[(
                                            index_map,
                                            &TriArrayIndex::new(index_type, index_1, index_2),
                                        )] + tri_array[(
                                            index_map,
                                            &TriArrayIndex::new(
                                                index_type,
                                                index_1,
                                                index_2 + big_step_size,
                                            ),
                                        )],
                                    )?;
                                }
                            }
                        }
                    }

                    ok!();
                };
                let add_tri_array = |params: &mut PieceHeaderParams,
                                     tri_array: &[Vec3; TRI_ARRAY_VERT_COUNT],
                                     offset: u32| {
                    let uv: Vec2 = Vec2::ZERO;
                    let offset: u32 =
                        params.mesh_attribute_data_with_stats.stats.vertices as u32 - offset;

                    for vert in tri_array {
                        params
                            .mesh_attribute_data_with_stats
                            .push_vertex(MeshVertexData {
                                position: *vert,
                                normal: vert.normalize(),
                                uv,
                            });
                    }

                    for (index_a, index_b, index_c) in
                        TriArrayIndex::iter_mapped_trio_indices(index_map)
                    {
                        params.mesh_attribute_data_with_stats.indices().extend([
                            offset + index_a as u32,
                            offset + index_b as u32,
                            offset + index_c as u32,
                        ]);
                    }
                };

                // This const assert guarantees MU::zeroed().assume_init() for arrays of `Vec3`s
                const_assert_eq!(
                    unsafe { std::mem::transmute::<[u8; 4_usize], f32>([0_u8; 4_usize]) },
                    0.0_f32
                );

                match params.piece_type {
                    Type::Pentagon => {
                        use pentagon::*;

                        #[cfg(debug_assertions)]
                        use render::*;

                        render_mesh_begin!(x, RENDER_PIECE_TYPE);

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let mut arc_vertices: [[MU<Vec3>; ARC_VERT_COUNT]; PENTAGON_VERTEX_COUNT] =
                            unsafe { MU::uninit().assume_init() };

                        init_arc_vertices(
                            disc_centers,
                            disc_kisses,
                            &mut arc_vertices,
                            angle,
                            params.piece_type,
                        )?;

                        // Safe: we just initialized all elements of `arc_vertices`
                        let arc_vertices: &mut [[Vec3; ARC_VERT_COUNT]; PENTAGON_VERTEX_COUNT] = unsafe {
                            std::mem::transmute::<
                                &mut [[MU<Vec3>; ARC_VERT_COUNT]; PENTAGON_VERTEX_COUNT],
                                &mut [[Vec3; ARC_VERT_COUNT]; PENTAGON_VERTEX_COUNT],
                            >(&mut arc_vertices)
                        };

                        // Add the base mesh
                        {
                            render_mesh_begin!(x, RENDER_BASE_MESH);

                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);
                            let center: Vec3 = resize_to_sphere(
                                transformation * icosidodecahedron_faces[0_usize].norm,
                            )?;

                            // Safe: see `const_assert_eq!` invocation above the enclosing match
                            let mut tri_arrays: [[[Vec3; TRI_ARRAY_VERT_COUNT];
                                PENTAGON_VERTEX_COUNT];
                                2_usize] = unsafe { MU::zeroed().assume_init() };

                            let [outer_tri_arrays, inner_tri_arrays] = &mut tri_arrays;

                            fn iter_tri_arrays(
                                tri_arrays: &mut [[Vec3; TRI_ARRAY_VERT_COUNT];
                                         PENTAGON_VERTEX_COUNT],
                            ) -> impl Iterator<Item = &mut TriArray<Vec3>>
                            {
                                tri_arrays
                                    .iter_mut()
                                    .map(|tri_array: &mut [Vec3; TRI_ARRAY_VERT_COUNT]| -> &mut TriArray<Vec3> {
                                        AsMut::<TriArray<Vec3>>::as_mut(tri_array)
                                    })
                            }

                            for (center_index, (outer_tri_array, inner_tri_array)) in
                                iter_tri_arrays(outer_tri_arrays)
                                    .zip(iter_tri_arrays(inner_tri_arrays))
                                    .enumerate()
                            {
                                /* Initialize border IJ for the outer tri with the corresponding
                                stretch of arc verts*/
                                outer_tri_array.init_border(
                                    index_map,
                                    IndexType2D::IJ,
                                    arc_vertices[center_index]
                                        [SUBDIVISION_COUNT..=2_usize * SUBDIVISION_COUNT]
                                        .iter()
                                        .cloned(),
                                );

                                /* Initialize border JK for the outer tri with the corresponding
                                stretch of arc verts */
                                outer_tri_array.init_border(
                                    index_map,
                                    IndexType2D::JK,
                                    arc_vertices[PIECE_TYPE.prev_side_index(center_index)]
                                        [0_usize..=SUBDIVISION_COUNT]
                                        .iter()
                                        .cloned(),
                                );

                                /* The K and I corners have both been initialized now, so just fill
                                in border KI from those two points */
                                init_border(outer_tri_array, IndexType2D::KI);

                                for border_vert in
                                    TriArrayIndex::iter_mapped_border_indices(index_map)
                                        .map(|index: usize| -> Vec3 { outer_tri_array[index] })
                                {
                                    warn_expect_length_is_radius!(border_vert);
                                }

                                // Fill in the rest of the outer tri
                                bisect_and_connect(outer_tri_array)?;

                                for vert in TriArrayIndex::iter_mapped_all_indices(index_map)
                                    .map(|index: usize| -> Vec3 { outer_tri_array[index] })
                                {
                                    warn_expect_length_is_radius!(vert);
                                }

                                /* Copy border KI from the outer array to border KI of the inner
                                array, keeping in mind that the directions flip */
                                inner_tri_array.init_border(
                                    index_map,
                                    IndexType2D::KI,
                                    TriArrayIndex::corner(IndexType2D::KI)
                                        .iter_mapped_row_indices(index_map)
                                        .rev()
                                        .map(|index: usize| -> Vec3 { outer_tri_array[index] }),
                                );

                                // Initialize the J corner
                                inner_tri_array
                                    [(index_map, &TriArrayIndex::corner(IndexType2D::JK))] = center;

                                /* The I and J corners have both been initialized now, so just fill
                                in border KI from those two points */
                                init_border(inner_tri_array, IndexType2D::IJ);

                                /* The J and K corners have both been initialized now, so just fill
                                in border KI from those two points */
                                init_border(inner_tri_array, IndexType2D::JK);

                                for border_vert in
                                    TriArrayIndex::iter_mapped_border_indices(index_map)
                                        .map(|index: usize| -> Vec3 { inner_tri_array[index] })
                                {
                                    warn_expect_length_is_radius!(border_vert);
                                }

                                // Fill in the rest of the inner tri
                                bisect_and_connect(inner_tri_array)?;

                                for vert in TriArrayIndex::iter_mapped_all_indices(index_map)
                                    .map(|index: usize| -> Vec3 { inner_tri_array[index] })
                                {
                                    warn_expect_length_is_radius!(vert);
                                }
                            }

                            {
                                render_mesh_begin!(x, RENDER_DISC_SECTIONS);

                                for (center_index, arc_vertices) in arc_vertices.iter().enumerate()
                                {
                                    let center: &Vec3 = &disc_centers[center_index];

                                    add_disc_section(
                                        params,
                                        arc_vertices,
                                        center,
                                        &center.normalize(),
                                        vertices_offset,
                                    );
                                }

                                render_mesh_end!(x);
                            }

                            {
                                render_mesh_begin!(x, RENDER_TRI_ARRAYS);

                                {
                                    render_mesh_begin!(x, RENDER_OUTER_TRI_ARRAYS);

                                    for tri_array in outer_tri_arrays {
                                        add_tri_array(params, tri_array, vertices_offset);
                                    }

                                    render_mesh_end!(x);
                                }

                                {
                                    render_mesh_begin!(x, RENDER_INNER_TRI_ARRAYS);

                                    for tri_array in inner_tri_arrays {
                                        add_tri_array(params, tri_array, vertices_offset);
                                    }

                                    render_mesh_end!(x);
                                }

                                render_mesh_end!(x);
                            }

                            {
                                render_mesh_begin!(x, RENDER_KITES);
                                add_kite_group(
                                    params,
                                    disc_kisses,
                                    disc_centers,
                                    &mut { vert_range },
                                    &Vec3::ZERO,
                                    vertices_offset,
                                    false,
                                    false,
                                    true,
                                );
                                render_mesh_end!(x);
                            }

                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                            render_mesh_end!(x);
                        }

                        {
                            render_mesh_begin!(x, RENDER_RHOMBUS_MESH);

                            // Add the disc section meshes
                            for (center_index, arc_vertices) in arc_vertices.iter_mut().enumerate()
                            {
                                let vertices_offset: u32 =
                                    push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                                let center: Vec3 = disc_centers[center_index];

                                add_offset_disc_section(
                                    params,
                                    arc_vertices,
                                    &center,
                                    &center.normalize(),
                                    vertices_offset,
                                );
                                add_rhombic_triacontahedron_face_indices(params, center_index);
                                check_mesh_stats(
                                    params,
                                    MESH_STATS_SLICE,
                                    &mut offset,
                                    &mut mesh_index,
                                )?;
                            }

                            render_mesh_end!(x);
                        }

                        render_mesh_end!(x);
                    }
                    Type::Triangle => {
                        use triangle::*;

                        #[cfg(debug_assertions)]
                        use render::*;

                        render_mesh_begin!(x, RENDER_PIECE_TYPE);

                        /* Safe: all we are claiming to have initialized are `MU` objects, which do
                        not require initialization */
                        let mut arc_vertices: [[MU<Vec3>; ARC_VERT_COUNT]; TRIANGLE_VERTEX_COUNT] =
                            unsafe { MU::uninit().assume_init() };

                        init_arc_vertices(
                            disc_centers,
                            disc_kisses,
                            &mut arc_vertices,
                            angle,
                            params.piece_type,
                        )?;

                        // Safe: we just initialized all elements of `arc_vertices`
                        let arc_vertices: &mut [[Vec3; ARC_VERT_COUNT]; TRIANGLE_VERTEX_COUNT] = unsafe {
                            std::mem::transmute::<
                                &mut [[MU<Vec3>; ARC_VERT_COUNT]; TRIANGLE_VERTEX_COUNT],
                                &mut [[Vec3; ARC_VERT_COUNT]; TRIANGLE_VERTEX_COUNT],
                            >(&mut arc_vertices)
                        };

                        // Add the base mesh
                        {
                            render_mesh_begin!(x, RENDER_BASE_MESH);

                            let vertices_offset: u32 =
                                push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                            // Safe: see `const_assert_eq!` invocation above the enclosing match
                            let mut tri_array: [Vec3; TRI_ARRAY_VERT_COUNT] =
                                unsafe { MU::zeroed().assume_init() };

                            {
                                let tri_array: &mut TriArray<Vec3> = tri_array.as_mut();

                                // Copy each border from the arc vertices
                                for (border, arc_vertices) in
                                    IndexType2D::iter().zip(arc_vertices.iter().rev())
                                {
                                    tri_array.init_border(
                                        index_map,
                                        border,
                                        arc_vertices.iter().cloned(),
                                    );
                                }

                                // All borders have been initialized, so fill in the rest
                                bisect_and_connect(tri_array)?;
                            }

                            {
                                render_mesh_begin!(x, RENDER_DISC_SECTIONS);

                                for (center_index, arc_vertices) in arc_vertices.iter().enumerate()
                                {
                                    let center: &Vec3 = &disc_centers[center_index];

                                    add_disc_section(
                                        params,
                                        arc_vertices,
                                        center,
                                        &center.normalize(),
                                        vertices_offset,
                                    );
                                }

                                render_mesh_end!(x);
                            }

                            {
                                render_mesh_begin!(x, RENDER_TRI_ARRAY);
                                add_tri_array(params, &tri_array, vertices_offset);
                                render_mesh_end!(x);
                            }

                            {
                                render_mesh_begin!(x, RENDER_KITES);
                                add_kite_group(
                                    params,
                                    disc_kisses,
                                    disc_centers,
                                    &mut { vert_range },
                                    &Vec3::ZERO,
                                    vertices_offset,
                                    false,
                                    false,
                                    true,
                                );
                                render_mesh_end!(x);
                            }

                            check_mesh_stats(
                                params,
                                MESH_STATS_SLICE,
                                &mut offset,
                                &mut mesh_index,
                            )?;
                            render_mesh_end!(x);
                        }

                        {
                            render_mesh_begin!(x, RENDER_RHOMBUS_MESH);

                            // Add the disc section meshes
                            for (center_index, arc_vertices) in arc_vertices.iter_mut().enumerate()
                            {
                                let vertices_offset: u32 =
                                    push_mesh_header(params, MESH_STATS_SLICE, &offset, mesh_index);

                                let center: Vec3 = disc_centers[center_index];

                                add_offset_disc_section(
                                    params,
                                    arc_vertices,
                                    &center,
                                    &center.normalize(),
                                    vertices_offset,
                                );
                                add_rhombic_triacontahedron_face_indices(params, center_index);
                                check_mesh_stats(
                                    params,
                                    MESH_STATS_SLICE,
                                    &mut offset,
                                    &mut mesh_index,
                                )?;
                            }

                            render_mesh_end!(x);
                        }

                        render_mesh_end!(x);
                    }
                }
            }
        }

        Ok(Self(start..params.mesh_headers_with_index.index as u32))
    }
}

#[derive(Default)]
struct DesignHeader([PieceHeader; Type::COUNT]);

#[derive(Default)]
struct DesignHeaders([DesignHeader; Design::COUNT]);

#[derive(Default)]
pub struct PieceLibrary {
    mesh_attribute_data: MeshAttributeData,
    mesh_headers: MeshHeaders,
    design_headers: DesignHeaders,
}

impl PieceLibrary {
    fn try_init(&mut self) -> Result<(), ()> {
        let Self {
            mesh_attribute_data,
            mesh_headers,
            design_headers,
        } = self;

        let mut piece_header_params: PieceHeaderParams = PieceHeaderParams {
            design: Design::iter().next().unwrap(),
            piece_type: Type::iter().next().unwrap(),
            mesh_attribute_data_with_stats: &mut MeshAttributeDataWithStats {
                data: mesh_attribute_data,
                stats: MeshStats::<usize>::default(),
            },
            mesh_headers_with_index: &mut MeshHeadersWithIndex {
                headers: mesh_headers,
                index: 0_usize,
            },
        };

        for design in Design::iter() {
            let design_header: &mut DesignHeader = &mut design_headers.0[design as usize];

            piece_header_params.design = design;

            for piece_type in Type::iter() {
                piece_header_params.piece_type = piece_type;
                design_header.0[piece_type as usize] =
                    PieceHeader::try_from(&mut piece_header_params)?;
            }
        }

        warn_expect!(
            piece_header_params.mesh_attribute_data_with_stats.stats == gen::TOTAL_MESH_STATS_SUM,
            return Err(())
        );
        warn_expect!(
            piece_header_params.mesh_headers_with_index.index == gen::TOTAL_MESH_COUNT,
            return Err(())
        );

        Ok(())
    }

    fn add_bevy_meshes(
        &self,
        bevy_meshes: &mut Assets<BevyMesh>,
        bevy_mesh_handles: &mut BevyMeshHandles,
    ) {
        let PieceLibrary {
            mesh_attribute_data,
            mesh_headers,
            ..
        } = self;

        warn_expect!(
            mesh_attribute_data.positions.len() == mesh_attribute_data.normals.len()
                && mesh_attribute_data.positions.len() == mesh_attribute_data.uvs.len(),
            ?
        );

        mesh_headers.add_bevy_meshes(mesh_attribute_data, bevy_meshes, bevy_mesh_handles);
    }

    pub fn update_entities(
        &self,
        piece_mats: &PieceMats,
        bevy_mesh_handles: &BevyMeshHandles,
        world: &mut World,
        design: Design,
    ) {
        warn_expect!(self.mesh_headers.0.len() == bevy_mesh_handles.0.len(), ?);

        /* Cache the entities we'll be using first, since iterating with the query borrows world.
        Safe: all we are claiming to have initialized are `MU` objects, which do not require
        initialization */
        let mut entities: [MU<Entity>; usize::PIECE_COUNT] = unsafe { MU::uninit().assume_init() };
        let mut entity_count: u32 = 0_u32;

        for piece_components in world.query::<PieceComponents>().iter(world) {
            entities[piece_components.piece_component.index].write(piece_components.entity);
            entity_count += 1_u32;
        }

        warn_expect!(entity_count == entities.len() as u32, ?);

        // Safe: we just guaranteed all cells of entities have been written to
        let entities: &[Entity] = unsafe { MU::slice_assume_init_ref(&entities) };

        let design_header: &DesignHeader = &self.design_headers.0[design as usize];

        for piece_type in Type::iter() {
            let mesh_range: Range<usize> =
                Option::<Range<usize>>::from_alt(design_header.0[piece_type as usize].0.clone())
                    .unwrap();
            let mesh_headers: &[MeshHeader] = &self.mesh_headers.0[mesh_range.clone()];
            let bevy_mesh_handles: &[Handle<BevyMesh>] = &bevy_mesh_handles.0[mesh_range];
            let index_offset: usize = piece_type.index_offset();

            for index in piece_type.range() {
                let mut entity_mut: EntityMut = world.entity_mut(entities[index]);

                entity_mut.despawn_descendants();
                entity_mut.with_children(|world_child_builder: &mut WorldChildBuilder| {
                    for (face_indices, bevy_mesh_handle) in mesh_headers
                        .iter()
                        .map(MeshHeader::face_indices)
                        .zip(bevy_mesh_handles.iter())
                    {
                        world_child_builder.spawn_bundle(PbrBundle {
                            mesh: bevy_mesh_handle.clone(),
                            material: if face_indices.is_empty() {
                                piece_mats.base_mat.mat.clone()
                            } else {
                                piece_mats.color_mats[self.mesh_attribute_data.face_indices
                                    [face_indices][index - index_offset]
                                    as usize]
                                    .mat
                                    .clone()
                            },
                            ..PbrBundle::default()
                        });
                    }
                });
            }
        }
    }

    pub fn add_entities(
        &self,
        piece_mats: &PieceMats,
        bevy_mesh_handles: &BevyMeshHandles,
        world: &mut World,
        design: Design,
    ) {
        let icosidodecahedron_faces: &Vec<FaceData> =
            &Data::get(Polyhedron::Icosidodecahedron).faces;

        for piece_type in Type::iter() {
            for index in piece_type.range() {
                world.spawn().insert_bundle((
                    PieceComponent { index, piece_type },
                    Transform::from_matrix(Mat4::from_quat(icosidodecahedron_faces[index].quat)),
                    GlobalTransform::identity(),
                ));
            }
        }

        self.update_entities(piece_mats, bevy_mesh_handles, world, design);
    }
}

impl StaticDataLibrary for PieceLibrary {
    fn pre_init() -> Option<Box<dyn FnOnce()>> {
        Some(Box::new(Data::initialize))
    }
    fn get() -> &'static Self {
        &PIECE_LIBRARY
    }
}

lazy_static! {
    static ref PIECE_LIBRARY: PieceLibrary = {
        let mut piece_library: PieceLibrary = PieceLibrary::default();

        warn_expect_ok!(piece_library.try_init());

        piece_library
    };
}

pub struct BevyMeshHandles([Handle<BevyMesh>; gen::TOTAL_MESH_COUNT]);

impl Default for BevyMeshHandles {
    fn default() -> Self {
        Self(<[Handle<BevyMesh>; gen::TOTAL_MESH_COUNT]>::default_array())
    }
}

pub struct PiecePlugin;

impl PiecePlugin {
    fn startup_piece_library() {
        PieceLibrary::build();
    }

    fn startup(
        mut bevy_meshes: ResMut<Assets<BevyMesh>>,
        mut bevy_mesh_handles: ResMut<BevyMeshHandles>,
    ) {
        PieceLibrary::get().add_bevy_meshes(&mut bevy_meshes, &mut bevy_mesh_handles);
    }

    fn startup_exclusive(world: &mut World) {
        warn_expect!(world.contains_resource::<Preferences>(), ?);
        warn_expect!(world.contains_resource::<BevyMeshHandles>(), ?);

        world.resource_scope(|world: &mut World, preferences: Mut<Preferences>| {
            world.resource_scope(
                |world: &mut World, bevy_mesh_handles: Mut<BevyMeshHandles>| {
                    let design: Design = preferences.puzzle.design;

                    PieceLibrary::get().add_entities(
                        &warn_expect_some!(
                            PieceMats::try_from(&preferences.puzzle.color.colors_with_mat, design),
                            return
                        ),
                        &bevy_mesh_handles,
                        world,
                        design,
                    );
                },
            );
        });
    }
}

impl Plugin for PiecePlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(BevyMeshHandles::default())
            .add_startup_system(Self::startup_piece_library.after(PolyhedraDataPlugin::startup))
            .add_startup_system(Self::startup.after(Self::startup_piece_library))
            .add_startup_system(Self::startup_exclusive.exclusive_system().at_end());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_piece_library() {
        init_env_logger();

        Data::initialize();
        break_assert!(PieceLibrary::default().try_init().is_ok());
    }
}
