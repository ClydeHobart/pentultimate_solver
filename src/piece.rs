use {
	std::{
		cmp::Ordering,
		convert::TryFrom,
		f32::consts::{
			PI,
			TAU
		},
		mem::MaybeUninit,
		ops::{
			Add,
			Range
		}
	},
	bevy::{
		ecs::{
			query::WorldQuery,
			world::EntityMut
		},
		prelude::*,
		render::{
			mesh::{
				Indices,
				Mesh as BevyMesh
			},
			render_resource::PrimitiveTopology
		}
	},
	bevy_inspector_egui::Inspectable,
	num_traits::{
		PrimInt,
		NumCast
	},
	serde::Deserialize,
	strum::{
		EnumCount as EnumCountTrait,
		IntoEnumIterator
	},
	strum_macros::{
		EnumCount,
		EnumIter
	},
	crate::{
		app::prelude::*,
		math::{
			*,
			polyhedra::{
				data::{
					Data,
					EdgeData,
					FaceData,
					VertexData
				},
				Polyhedron,
				properties::ICOSIDODECAHEDRON
			}
		},
		preferences::prelude::*,
		prelude::*
	},
	self::consts::*
};

pub mod consts {
	use {
		std::ops::Range,
		crate::{
			puzzle::inflated::PieceStateComponent as IPSC,
			max
		},
		super::Type
	};

	pub trait TypeConsts: Sized{
		const PENTAGON_PIECE_COUNT:		Self;
		const PENTAGON_VERTEX_COUNT:	Self;
		const TRIANGLE_PIECE_COUNT:		Self;
		const TRIANGLE_VERTEX_COUNT:	Self;
		const PIECE_COUNT:				Self;
	}

	impl TypeConsts for usize {
		const PENTAGON_PIECE_COUNT:		Self = Type::Pentagon.instance_count();
		const PENTAGON_VERTEX_COUNT:	Self = Type::Pentagon.vertex_count();
		const TRIANGLE_PIECE_COUNT:		Self = Type::Triangle.instance_count();
		const TRIANGLE_VERTEX_COUNT:	Self = Type::Triangle.vertex_count();
		const PIECE_COUNT:				Self = Self::PENTAGON_PIECE_COUNT + Self::TRIANGLE_PIECE_COUNT;
	}

	impl TypeConsts for f32 {
		const PENTAGON_PIECE_COUNT:		Self = Type::Pentagon.instance_count()	as f32;
		const PENTAGON_VERTEX_COUNT:	Self = Type::Pentagon.vertex_count()	as f32;
		const TRIANGLE_PIECE_COUNT:		Self = Type::Triangle.instance_count()	as f32;
		const TRIANGLE_VERTEX_COUNT:	Self = Type::Triangle.vertex_count()	as f32;
		const PIECE_COUNT:				Self = Self::PENTAGON_PIECE_COUNT + Self::TRIANGLE_PIECE_COUNT;
	}

	impl TypeConsts for IPSC {
		const PENTAGON_PIECE_COUNT:		Self = Type::Pentagon.instance_count()	as u32;
		const PENTAGON_VERTEX_COUNT:	Self = Type::Pentagon.vertex_count()	as u32;
		const TRIANGLE_PIECE_COUNT:		Self = Type::Triangle.instance_count()	as u32;
		const TRIANGLE_VERTEX_COUNT:	Self = Type::Triangle.vertex_count()	as u32;
		const PIECE_COUNT:				Self = Self::PENTAGON_PIECE_COUNT + Self::TRIANGLE_PIECE_COUNT;
	}

	pub const HALF_PENTAGON_PIECE_COUNT:	usize			= usize::PENTAGON_PIECE_COUNT / 2;				// 6
	pub const PENTAGON_INDEX_OFFSET:		usize			= Type::Pentagon.index_offset();				// 0
	pub const TRIANGLE_INDEX_OFFSET:		usize			= Type::Triangle.index_offset();				// 12
	pub const PENTAGON_PIECE_RANGE:			Range<usize>	= Type::Pentagon.range();
	pub const TRIANGLE_PIECE_RANGE:			Range<usize>	= Type::Triangle.range();
	pub const PIECE_RANGE:					Range<usize>	= 0_usize .. usize::PIECE_COUNT;
	pub const ROTATION_BIT_COUNT:			u32				=
		usize::BITS - max!(usize::PENTAGON_VERTEX_COUNT, usize::TRIANGLE_VERTEX_COUNT).leading_zeros();		// 3
	pub const ROTATION_BIT_MASK:			IPSC			= ((1 as IPSC) << ROTATION_BIT_COUNT) - 1;		// 0b111
	pub const ZERO_IPSC:					IPSC			= 0 as IPSC;
}

#[derive(Clone, Copy, Debug, Deserialize, EnumCount, EnumIter, Inspectable, PartialEq)]
pub enum Design {
	OriginalSuperDodecahedron,
	CustomSuperDodecahedron
}

impl Design {
	pub const fn as_polyhedron(self) -> Polyhedron {
		match self {
			Design::OriginalSuperDodecahedron => Polyhedron::Dodecahedron,
			Design::CustomSuperDodecahedron => Polyhedron::Dodecahedron
		}
	}
}

impl Update for Design {
	fn update(&self, other: &Self, world: &mut World, preferences: &Preferences) -> () {
		if self != other {
			warn_expect!(world.contains_resource::<BevyMeshHandles>(), ?);

			world.resource_scope(|world: &mut World, bevy_mesh_handles: Mut<BevyMeshHandles>| -> () {
				PieceLibrary::get().update_entities(
					&warn_expect_some!(
						PieceMats::try_from(&preferences.puzzle.color.colors_with_mat, *self),
						return
					),
					&bevy_mesh_handles,
					world,
					*self
				);
			});
		}
	}
}

#[derive(Clone, Copy, Debug, Deserialize, EnumCount, EnumIter)]
pub enum Type {
	Pentagon,
	Triangle
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

		index_offset .. index_offset + self.instance_count()
	}

	pub fn next_side_index(&self, index: usize) -> usize {
		(index + 1) % self.vertex_count()
	}

	pub fn prev_side_index(&self, index: usize) -> usize {
		let side_count: usize = self.vertex_count();

		(index + side_count - 1) % side_count
	}

	pub fn from_index(index: usize) -> Option<Self> {
		const_assert_eq!(Type::Pentagon.index_offset(),		0_usize);
		const_assert_eq!(Type::Pentagon.instance_count(),	12_usize);
		const_assert_eq!(Type::Triangle.index_offset(),		12_usize);
		const_assert_eq!(Type::Triangle.instance_count(),	20_usize);

		match index {
			0..=11	=> Some(Type::Pentagon),
			12..=31	=> Some(Type::Triangle),
			_		=> None
		}
	}

	pub const fn pyramid_polyhedron(self) -> Polyhedron {
		match self {
			Self::Pentagon => Polyhedron::Icosahedron,
			Self::Triangle => Polyhedron::Dodecahedron
		}
	}
}

#[derive(Clone, Copy, Default)]
struct Tri {
	vertices:	[Vec3; 3_usize],
	normal:		Vec3
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
			let edge_offset: Vec3 =
				distance * (transformed[next_vert_index] - transformed[curr_vert_index]).cross(Vec3::Z).normalize();

			tri1[curr_vert_index] += edge_offset;
			tri2[next_vert_index] += edge_offset;
		}

		let mut result: Tri = transformed;

		for curr_vert_index in 0_usize .. 3_usize {
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
				0.0_f32
			));
		}

		*self = &mat.inverse() * result;
		self.update_normal();

		self
	}

	fn compute_normal(a: &Vec3, b: &Vec3, c: &Vec3) -> Vec3 { (*b - *a).cross(*c - *b).normalize() }
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
			tri.normal.cross(tri[1_usize] - tri[0_usize]).normalize()
		)
	}
}

impl<Idx> std::ops::Index<Idx> for Tri
	where
		[Vec3; 3]: std::ops::Index<Idx>
{
	type Output = <[Vec3; 3] as std::ops::Index<Idx>>::Output;

	fn index(&self, index: Idx) -> &Self::Output {
		return &self.vertices[index];
	}
}

impl<Idx> std::ops::IndexMut<Idx> for Tri
	where
		[Vec3; 3]: std::ops::IndexMut<Idx>
{
	fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
		return &mut self.vertices[index];
	}
}

impl std::ops::Mul<Tri> for &Mat4 {
	type Output = Tri;

	fn mul(self, mut rhs: Tri) -> Self::Output {
		for vertex in rhs.vertices.iter_mut() {
			*vertex = self.transform_point3(*vertex);
		}

		rhs.update_normal();

		return rhs;
	}
}

#[derive(Clone, Component, Copy)]
pub struct PieceComponent {
	pub index:		usize,
	pub piece_type:	Type
}

#[derive(WorldQuery)]
pub struct PieceComponents<'w> {
	pub entity:				Entity,
	pub piece_component:	&'w PieceComponent,
	pub transform:			&'w Transform
}

pub type PieceQuery<'world, 'state, 'w> = Query<'world, 'state, PieceComponents<'w>>;

#[derive(WorldQuery)]
#[world_query(mutable)]
pub struct PieceComponentsMut<'w> {
	pub entity:				Entity,
	pub piece_component:	&'w PieceComponent,
	pub transform:			&'w mut Transform
}

pub type PieceQueryMut<'world, 'state, 'w> = Query<'world, 'state, PieceComponentsMut<'w>>;

pub struct PieceMats<'a> {
	pub base_mat:	&'a ColAndMat,
	pub color_mats:	&'a Vec<ColAndMat>
}

impl<'a> PieceMats<'a> {
	fn try_from<'b: 'a>(color_data_with_mat: &'b ColorDataWithMat, design: Design) -> Option<Self> {
		let design_polyhedron: Polyhedron = design.as_polyhedron();

		Some(Self {
			base_mat:	&color_data_with_mat.base_color,
			color_mats:	&color_data_with_mat
				.polyhedron_to_colors
				[warn_expect_ok!(
					color_data_with_mat
						.polyhedron_to_colors
						.binary_search_by(|(polyhedron, _) : &(Polyhedron, Vec<ColAndMat>)| -> Ordering {
							polyhedron.cmp(&design_polyhedron)
						}),
					return None
				)]
				.1
		})
	}
}

struct MeshVertexData<
	P: Copy + Into<[f32; 3_usize]>,
	N: Copy + Into<[f32; 3_usize]>,
	U: Copy + Into<[f32; 2_usize]>
> {
	position:	P,
	normal:		N,
	uv:			U
}

type VecMeshVertexData = MeshVertexData<Vec3, Vec3, Vec2>;

trait PushVertex<V> {
	fn push_vertex(&mut self, vertex: &V) -> ();
}

struct MeshAttributeData {
	positions:		Vec<[f32; 3_usize]>,
	normals:		Vec<[f32; 3_usize]>,
	uvs:			Vec<[f32; 2_usize]>,
	indices:		Vec<u32>,
	face_indices:	Vec<u8>,
}

impl MeshAttributeData {
	fn vertices(&self) -> usize { self.positions.len() }

	fn offset(&self) -> MeshStats<usize> { <MeshStats<usize> as From<&Self>>::from(self) }
}

impl Default for MeshAttributeData {
	fn default() -> Self {
		use generation::TOTAL_MESH_STATS_SUM;

		Self {
			positions:		Vec::<[f32; 3_usize]>::with_capacity(TOTAL_MESH_STATS_SUM.vertices),
			normals:		Vec::<[f32; 3_usize]>::with_capacity(TOTAL_MESH_STATS_SUM.vertices),
			uvs:			Vec::<[f32; 2_usize]>::with_capacity(TOTAL_MESH_STATS_SUM.vertices),
			indices:		Vec::<u32>::with_capacity(TOTAL_MESH_STATS_SUM.indices),
			face_indices:	Vec::<u8>::with_capacity(TOTAL_MESH_STATS_SUM.face_indices),
		}
	}
}

impl<
	P: Copy + Into<[f32; 3_usize]>,
	N: Copy + Into<[f32; 3_usize]>,
	U: Copy + Into<[f32; 2_usize]>
> PushVertex<MeshVertexData<P, N, U>> for MeshAttributeData {
	fn push_vertex(&mut self, mesh_vertex_data: &MeshVertexData<P, N, U>) -> () {
		self.positions.push(mesh_vertex_data.position.into());
		self.normals.push(mesh_vertex_data.normal.into());
		self.uvs.push(mesh_vertex_data.uv.into());
	}
}

impl PushVertex<(Tri, u32)> for MeshAttributeData {
	fn push_vertex(&mut self, (tri, offset): &(Tri, u32)) -> () {
		let offset: u32 = self.vertices() as u32 - offset;

		for vertex in tri.vertices {
			self.push_vertex(&MeshVertexData {
				position:	vertex,
				normal:		tri.normal,
				uv:			Vec2::ZERO
			});
		}

		self.indices.extend([offset, offset + 1_u32, offset + 2_u32]);
	}
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct MeshStats<I: PrimInt + Default> {
	vertices:		I,
	indices:		I,
	face_indices:	I
}

type MeshStatsRefSlice = &'static [&'static MeshStats<usize>];

impl<I: PrimInt + Default> MeshStats<I> {
	fn from<J: PrimInt + Default>(mesh_stats: MeshStats<J>) -> Option<Self> { Some(Self {
		vertices:		warn_expect_some!(I::from(mesh_stats.vertices), ?),
		indices:		warn_expect_some!(I::from(mesh_stats.indices), ?),
		face_indices:	warn_expect_some!(I::from(mesh_stats.face_indices), ?)
	}) }
}

impl MeshStats<usize> {
	const fn new(vertices: usize, tris: usize, piece_type: Type) -> Self {
		Self {
			vertices,
			indices: usize::TRIANGLE_VERTEX_COUNT * tris,
			face_indices: piece_type.instance_count()
		}
	}

	const fn new_base(vertices: usize, tris: usize) -> Self {
		Self {
			vertices,
			indices: usize::TRIANGLE_VERTEX_COUNT * tris,
			face_indices: 0_usize
		}
	}

	const fn sum_slice(slice: MeshStatsRefSlice) -> Self {
		let mut sum: Self = Self {
			vertices:		0_usize,
			indices:		0_usize,
			face_indices:	0_usize
		};

		let len: usize = slice.len();
		let mut index: usize = 0_usize;

		while index < len {
			let mesh_stats: &MeshStats<usize> = slice[index];

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

	fn add(self, rhs: Self) -> Self { Self {
		vertices:		self.vertices		+ rhs.vertices,
		indices:		self.indices		+ rhs.indices,
		face_indices:	self.face_indices	+ rhs.face_indices
	} }
}

impl From<&MeshAttributeData> for MeshStats<usize> {
	fn from(mesh_attribute_data: &MeshAttributeData) -> Self { Self {
		vertices:		mesh_attribute_data.vertices(),
		indices:		mesh_attribute_data.indices.len(),
		face_indices:	mesh_attribute_data.face_indices.len()
	} }
}

#[derive(Clone, Default)]
struct MeshHeader(Range<MeshStats<u32>>);

impl MeshHeader {
	fn new(start: &MeshStats<usize>, len: &MeshStats<usize>) -> Self {
		|| -> Option<Self> {
			Some(Self({
				let start: MeshStats<u32> = warn_expect_some!(MeshStats::<u32>::from(*start), ?);

				start .. start + warn_expect_some!(MeshStats::<u32>::from(*len), ?)
			}))
		}().unwrap_or_default()
	}

	fn vertices		(&self) -> Range<usize> { self.0.start.vertices		as usize .. self.0.end.vertices		as usize }
	fn indices		(&self) -> Range<usize> { self.0.start.indices		as usize .. self.0.end.indices		as usize }
	fn face_indices	(&self) -> Range<usize> { self.0.start.face_indices	as usize .. self.0.end.face_indices	as usize }

	fn add_bevy_mesh(
		&self,
		mesh_attribute_data: &MeshAttributeData,
		bevy_meshes: &mut Assets<BevyMesh>,
		bevy_mesh_handle: &mut Handle<BevyMesh>
	) -> () {
		let vertices: Range<usize> = self.vertices();
		let indices: Range<usize> = self.indices();

		warn_expect!(vertices.end <= mesh_attribute_data.vertices(), ?);
		warn_expect!(indices.end <= mesh_attribute_data.indices.len(), ?);

		let mut bevy_mesh: BevyMesh = BevyMesh::new(PrimitiveTopology::TriangleList);

		bevy_mesh.insert_attribute(
			BevyMesh::ATTRIBUTE_POSITION,
			Vec::<[f32; 3_usize]>::from(&mesh_attribute_data.positions[vertices.clone()])
		);
		bevy_mesh.insert_attribute(
			BevyMesh::ATTRIBUTE_NORMAL,
			Vec::<[f32; 3_usize]>::from(&mesh_attribute_data.normals[vertices.clone()])
		);
		bevy_mesh.insert_attribute(
			BevyMesh::ATTRIBUTE_UV_0,
			Vec::<[f32; 2_usize]>::from(&mesh_attribute_data.uvs[vertices])
		);
		bevy_mesh.set_indices(Some(Indices::U32(Vec::<u32>::from(&mesh_attribute_data.indices[indices]))));
		*bevy_mesh_handle = bevy_meshes.add(bevy_mesh);
	}
}

struct MeshHeaders(Vec<MeshHeader>);

impl MeshHeaders {
	fn offset(&self) -> usize { self.0.len() }

	fn add_bevy_meshes(
		&self,
		mesh_attribute_data: &MeshAttributeData,
		bevy_meshes: &mut Assets<BevyMesh>,
		bevy_mesh_handles: &mut BevyMeshHandles
	) -> () {
		warn_expect!(self.0.len() == bevy_mesh_handles.0.len(), ?);

		for (mesh_header, bevy_mesh_handle)
			in self.0.iter().zip(bevy_mesh_handles.0.iter_mut())
		{
			mesh_header.add_bevy_mesh(mesh_attribute_data, bevy_meshes, bevy_mesh_handle);
		}
	}
}

impl Default for MeshHeaders {
	fn default() -> Self { Self(Vec::<MeshHeader>::with_capacity(generation::TOTAL_MESH_COUNT)) }
}

struct PieceHeaderParams<'a> {
	design:					Design,
	piece_type:				Type,
	mesh_attribute_data:	&'a mut MeshAttributeData,
	mesh_headers:			&'a mut MeshHeaders
}

mod generation {
	use super::*;

	pub mod original_super_dodecahedron {
		use super::*;

		pub mod pentagon {
			use super::*;

			pub const PENTAGONAL_ANNULUS_THICKNESS_RATIO:	f32					= 0.2_f32;
			pub const PENTAGONAL_ANNULUS_THICKNESS:			f32					=
				PENTAGONAL_ANNULUS_THICKNESS_RATIO * ICOSIDODECAHEDRON.edge_length;
			pub const CIRCLE_SUBDIVISION_COUNT:				usize				= 16_usize;
			pub const TAU_OVER_CIRCLE_SUBDIVISION_COUNT:	f32					= TAU / CIRCLE_SUBDIVISION_COUNT as f32;
			pub const PENTAGON_COUNT:						usize				= 4_usize;
			pub const OUTER_OUTER_INDEX:					usize				= 0_usize;
			pub const OUTER_INNER_INDEX:					usize				= 1_usize;
			pub const INNER_INNER_INDEX:					usize				= 2_usize;
			pub const INNER_OUTER_INDEX:					usize				= 3_usize;
			pub const TRIS_PER_PENTAGONAL_ANNULUS:			usize				= TRIS_PER_QUAD * PENTAGON_VERTEX_COUNT;
			pub const BASE_MESH_TRIS_CAPACITY:				usize				=
				TRIS_PER_PENTAGONAL_ANNULUS * PENTAGON_COUNT;
			pub const CURR_TRI_VERT_INDEX:					usize				= 1_usize;
			pub const OFFSET_MASK:							[bool; 3_usize]		= [true, true, false];
			pub const PIECE_TYPE:							Type				= Type::Pentagon;
			pub const BASE_MESH_STATS:						MeshStats<usize>	= MeshStats::new_base(
				2_usize * 2_usize * PENTAGON_VERTEX_COUNT + 2_usize * 4_usize * PENTAGON_VERTEX_COUNT,
				BASE_MESH_TRIS_CAPACITY
			);
			pub const PRIMARY_PENT_MESH_STATS:				MeshStats<usize>	=
				MeshStats::new(2_usize * PENTAGON_VERTEX_COUNT, TRIS_PER_PENTAGONAL_ANNULUS, PIECE_TYPE);
			pub const ADJACENT_PENT_MESH_STATS:				MeshStats<usize>	= MeshStats::new(
				CIRCLE_SUBDIVISION_COUNT + 1_usize,
				CIRCLE_SUBDIVISION_COUNT,
				PIECE_TYPE
			);
			pub const MESH_STATS_SLICE:						MeshStatsRefSlice	= &[
				&BASE_MESH_STATS,
				&PRIMARY_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS
			];
			pub const MESH_COUNT:							usize				= MESH_STATS_SLICE.len();
			pub const MESH_STATS_SUM:						MeshStats<usize>	= MeshStats::sum_slice(MESH_STATS_SLICE);
		}

		pub mod triangle {
			use super::*;

			pub const PIECE_TYPE:				Type				= Type::Triangle;
			pub const BASE_MESH_STATS:			MeshStats<usize>	= MeshStats::new_base(
				TRIANGLE_VERTEX_COUNT * (TRIANGLE_VERTEX_COUNT + 4_usize + 1_usize) + 1_usize,
				TRIANGLE_VERTEX_COUNT * (1_usize + TRIS_PER_QUAD + 1_usize)
			);
			pub const ADJACENT_PENT_MESH_STATS:	MeshStats<usize>	=
				MeshStats::new(TRIANGLE_VERTEX_COUNT, 1_usize, PIECE_TYPE);
			pub const MESH_STATS_SLICE:			MeshStatsRefSlice	=
				&[&BASE_MESH_STATS, &ADJACENT_PENT_MESH_STATS, &ADJACENT_PENT_MESH_STATS, &ADJACENT_PENT_MESH_STATS];
			pub const MESH_COUNT:				usize				= MESH_STATS_SLICE.len();
			pub const MESH_STATS_SUM:			MeshStats<usize>	= MeshStats::sum_slice(MESH_STATS_SLICE);
		}

		pub const DESIGN_MESH_COUNT:		usize				= pentagon::MESH_COUNT + triangle::MESH_COUNT;
		pub const DESIGN_MESH_STATS_SUM:	MeshStats<usize>	= MeshStats::sum_slice(&[
			&pentagon::MESH_STATS_SUM,
			&triangle::MESH_STATS_SUM
		]);
		pub const CURR_LOOP_INDEX:			usize				= 0_usize;
		pub const NEXT_LOOP_INDEX:			usize				= 1_usize;
	}

	pub mod custom_super_dodecahedron {
		use super::*;

		pub mod pentagon {
			use super::*;

			pub const PENTAGON_COUNT:			usize				= 3_usize;
			pub const PIECE_TYPE:				Type				= Type::Pentagon;
			pub const BASE_MESH_STATS:			MeshStats<usize>	= MeshStats::new_base(
				PENTAGON_VERTEX_COUNT + 1_usize + PENTAGON_VERTEX_COUNT * TRIANGLE_VERTEX_COUNT,
				2_usize * PENTAGON_VERTEX_COUNT
			);
			pub const PRIMARY_PENT_MESH_STATS:	MeshStats<usize>	= MeshStats::new(
				TRIANGLE_VERTEX_COUNT * PENTAGON_VERTEX_COUNT,
				PENTAGON_VERTEX_COUNT,
				PIECE_TYPE
			);
			pub const ADJACENT_PENT_MESH_STATS:	MeshStats<usize>	=
				MeshStats::new(3_usize * TRIANGLE_VERTEX_COUNT, 3_usize, PIECE_TYPE);
			pub const MESH_STATS_SLICE:			MeshStatsRefSlice	= &[
				&BASE_MESH_STATS,
				&PRIMARY_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS,
				&ADJACENT_PENT_MESH_STATS
			];
			pub const MESH_COUNT:				usize				= MESH_STATS_SLICE.len();
			pub const MESH_STATS_SUM:			MeshStats<usize>	= MeshStats::sum_slice(MESH_STATS_SLICE);
		}

		pub mod triangle {
			use super::*;

			pub const PIECE_TYPE:				Type				= Type::Triangle;
			pub const BASE_MESH_STATS:			MeshStats<usize>	= MeshStats::new_base(
				2_usize * TRIANGLE_VERTEX_COUNT * TRIANGLE_VERTEX_COUNT,
				2_usize * TRIANGLE_VERTEX_COUNT
			);
			pub const ADJACENT_PENT_MESH_STATS:	MeshStats<usize>	=
				MeshStats::new(TRIANGLE_VERTEX_COUNT, 1_usize, PIECE_TYPE);
			pub const MESH_STATS_SLICE:			MeshStatsRefSlice	=
				&[&BASE_MESH_STATS, &ADJACENT_PENT_MESH_STATS, &ADJACENT_PENT_MESH_STATS, &ADJACENT_PENT_MESH_STATS];
			pub const MESH_COUNT:				usize				= MESH_STATS_SLICE.len();
			pub const MESH_STATS_SUM:			MeshStats<usize>	= MeshStats::sum_slice(MESH_STATS_SLICE);
		}

		pub const DESIGN_MESH_COUNT: usize = pentagon::MESH_COUNT + triangle::MESH_COUNT;
		pub const DESIGN_MESH_STATS_SUM: MeshStats<usize> = MeshStats::sum_slice(&[
			&pentagon::MESH_STATS_SUM,
			&triangle::MESH_STATS_SUM
		]);
	}

	pub const PENTAGON_VERTEX_COUNT:	usize	= usize::PENTAGON_VERTEX_COUNT;
	pub const TRIANGLE_VERTEX_COUNT:	usize	= usize::TRIANGLE_VERTEX_COUNT;
	pub const TRIS_PER_QUAD:			usize	= 2_usize;
	pub const TOTAL_MESH_COUNT:			usize	=
		original_super_dodecahedron::DESIGN_MESH_COUNT +
		custom_super_dodecahedron::DESIGN_MESH_COUNT;
	pub const TOTAL_MESH_STATS_SUM: MeshStats<usize> = MeshStats::sum_slice(&[
		&original_super_dodecahedron::DESIGN_MESH_STATS_SUM,
		&custom_super_dodecahedron::DESIGN_MESH_STATS_SUM
	]);
	pub const PLANAR_OFFSET_RATIO:		f32		= 1.0_f32 / 64.0_f32;
	pub const NORMAL_OFFSET_RATIO:		f32		= 1.0_f32 / 256.0_f32;
	pub const SHELL_DEPTH_RATIO:		f32		= 0.25_f32;
	pub const PLANAR_OFFSET:			f32		= PLANAR_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;
	pub const NORMAL_OFFSET:			f32		= NORMAL_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;
}

#[derive(Default)]
struct PieceHeader(Range<u32>);

impl<'a> TryFrom<&mut PieceHeaderParams<'a>> for PieceHeader {
	type Error = ();

	fn try_from(mut params: &mut PieceHeaderParams) -> Result<Self, ()> {
		use generation::*;

		fn next_vert_index<I: PrimInt>(curr_vert_index: I, vert_count: I) -> I {
			(curr_vert_index + I::one()) % vert_count
		}

		fn prev_vert_index<I: PrimInt>(curr_vert_index: I, vert_count: I) -> I {
			(curr_vert_index + vert_count + I::one()) % vert_count
		}

		let start:							u32					= params.mesh_headers.offset() as u32;

		let pyramid_data:					&Data				= Data::get(params.piece_type.pyramid_polyhedron());
		let icosahedron_data:				&Data				= Data::get(Polyhedron::Icosahedron);
		let _dodecahedron_data:				&Data				= Data::get(Polyhedron::Dodecahedron);
		let icosidodecahedron_data:			&Data				= Data::get(Polyhedron::Icosidodecahedron);
		let _rhombic_triacontahedron_data:	&Data				= Data::get(Polyhedron::RhombicTriacontahedron);
		let icosidodecahedron_verts:		&Vec<VertexData>	= &icosidodecahedron_data.verts;
		let icosidodecahedron_vert_indices:	&Vec<usize>			= &icosidodecahedron_data.vert_indices;
		let icosidodecahedron_faces:		&Vec<FaceData>		= &icosidodecahedron_data.faces;

		let vert_count:						usize				= params.piece_type.vertex_count();
		let index_offset:					usize				= params.piece_type.index_offset();
		let instance_count:					usize				= params.piece_type.instance_count();
		let face_range:						Range<usize>		= index_offset .. index_offset + instance_count;
		let vert_range:						Range<usize>		= 0_usize .. vert_count;
		let transformation:					Quat				= icosidodecahedron_data
			.faces
			[index_offset]
			.quat
			.inverse();
		let face_iter: &dyn Fn(&mut [Vec3]) -> () = &|vertices: &mut [Vec3]| -> () {
			for (vert_index, vert) in icosidodecahedron_data
				.faces
				[index_offset]
				.range
				.clone()
				.map(|vert_indices_index: usize| -> &Vec3 {
					&icosidodecahedron_verts[icosidodecahedron_vert_indices[vert_indices_index]].vec
				})
				.enumerate()
			{
				vertices[vert_index] = transformation * *vert;
			}
		};
		let add_adjacent_face_indices_same_piece: &dyn Fn(&mut PieceHeaderParams, usize) -> () =
			&|params: &mut PieceHeaderParams, vert_index: usize| -> () {
				params.mesh_attribute_data.face_indices.extend(face_range
					.clone()
					.map(|face_index: usize| -> u8 {
						let face_data: &FaceData = &icosidodecahedron_faces[face_index];

						icosahedron_data.get_closest_vert_index(
							&(Quat::from_axis_angle(
								icosidodecahedron_verts[
									face_data.get_slice(icosidodecahedron_vert_indices)[vert_index]
								].vec,
								PI
							) * face_data.norm),
							None
						) as u8
					})
				);
			};
		let add_adjacent_face_indices_other_piece: &dyn Fn(&mut PieceHeaderParams, usize) -> () =
			&|params: &mut PieceHeaderParams, vert_index: usize| -> () {
				let next_vert_index: usize = params.piece_type.next_side_index(vert_index);

				params.mesh_attribute_data.face_indices.extend(face_range
					.clone()
					.map(|face_index: usize| -> u8 {
						let edge_index: usize = icosidodecahedron_data.get_edge_index(&{
							let face_slice: &[usize] = icosidodecahedron_faces
								[face_index]
								.get_slice(icosidodecahedron_vert_indices);

							EdgeData::new(
								face_slice[vert_index],
								face_slice[next_vert_index]
							)
						}).unwrap();

						icosidodecahedron_data.get_closest_face_index(
							&Vec3::ZERO,
							Some(&|query_face_index: usize, face_data: &FaceData| -> bool {
								face_data.edges[edge_index] && query_face_index != face_index
							})
						) as u8
					})
				);
			};
		let add_piece_range: &dyn Fn(&mut PieceHeaderParams) -> () = &|params: &mut PieceHeaderParams| -> () {
			params.mesh_attribute_data.face_indices.extend(
				PENTAGON_PIECE_RANGE.map(<u8 as NumCast>::from).map(Option::unwrap)
			);
		};
		let add_regular_polygon: &dyn Fn(&mut PieceHeaderParams, &[Vec3], &Vec3, &Vec3, &Vec2, u32, bool) = &|
			params:		&mut PieceHeaderParams,
			vert_loop:	&[Vec3],
			center:		&Vec3,
			normal:		&Vec3,
			uv:			&Vec2,
			offset:		u32,
			flip_tris:	bool
		| -> () {
			let normal: Vec3 = *normal;
			let uv: Vec2 = *uv;
			let offset: u32 = params.mesh_attribute_data.vertices() as u32 - offset;

			for vert in vert_loop {
				params.mesh_attribute_data.push_vertex(&VecMeshVertexData {
					position: *vert,
					normal,
					uv
				});
			}

			params.mesh_attribute_data.push_vertex(&VecMeshVertexData {
				position: *center,
				normal,
				uv
			});

			let vert_count: u32 = vert_loop.len() as u32;
			let next_vert_index: fn(u32, u32) -> u32 = if flip_tris {
				prev_vert_index::<u32>
			} else {
				next_vert_index::<u32>
			};

			for curr_vert_index in 0_u32 .. vert_count {
				let next_vert_index: u32 = next_vert_index(curr_vert_index, vert_count);

				params.mesh_attribute_data.indices.extend(
					[offset + vert_count, offset + curr_vert_index, offset + next_vert_index]
				);
			}
		};
		let add_pyramid: &dyn Fn(
			&mut PieceHeaderParams,
			&[Vec3],
			&mut dyn Iterator<Item = usize>,
			&Vec3,
			u32,
			bool,
			bool,
			bool
		) = &|
			params:					&mut PieceHeaderParams,
			vert_loop:				&[Vec3],
			curr_vert_index_iter:	&mut dyn Iterator<Item = usize>,
			center:					&Vec3,
			offset:					u32,
			offset_along_plane:		bool,
			offset_along_normal:	bool,
			flip_tris:				bool,
		| -> () {
			let vert_count: usize = vert_loop.len();
			let next_vert_index: fn(usize, usize) -> usize = if flip_tris {
				prev_vert_index::<usize>
			} else {
				next_vert_index::<usize>
			};

			for curr_vert_index in curr_vert_index_iter {
				let next_vert_index: usize = next_vert_index(curr_vert_index, vert_count);

				let mut tri: Tri = Tri::from([*center, vert_loop[curr_vert_index], vert_loop[next_vert_index]]);

				if offset_along_plane {
					tri.offset_all_along_plane(-PLANAR_OFFSET);
				}

				if offset_along_normal {
					tri.offset_along_normal(NORMAL_OFFSET);
				}

				params.mesh_attribute_data.push_vertex(&(tri, offset));
			}
		};

		let mut mesh_index: usize = 0_usize;
		let mut offset: MeshStats<usize> = params.mesh_attribute_data.offset();

		let check_mesh_stats: &dyn Fn(
			&PieceHeaderParams,
			MeshStatsRefSlice,
			&mut MeshStats<usize>,
			&mut usize
		) -> Result<(), ()> = &|
			params:				&PieceHeaderParams,
			mesh_stats_slice:	MeshStatsRefSlice,
			offset:				&mut MeshStats<usize>,
			mesh_index:			&mut usize
		| -> Result<(), ()> {
			let new_offset: MeshStats<usize> = params.mesh_attribute_data.offset();
			let expected_offset: MeshStats<usize> = *offset + *mesh_stats_slice[*mesh_index];

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
		let push_mesh_header: &dyn Fn(
			&mut PieceHeaderParams,
			MeshStatsRefSlice,
			&MeshStats<usize>,
			usize
		) -> u32 = &|
			params:				&mut PieceHeaderParams,
			mesh_stats_slice:	MeshStatsRefSlice,
			offset:				&MeshStats<usize>,
			mesh_index:			usize
		| -> u32 {
			params.mesh_headers.0.push(MeshHeader::new(offset, mesh_stats_slice[mesh_index]));

			offset.vertices as u32
		};

		match params.design {
			Design::OriginalSuperDodecahedron => {
				use original_super_dodecahedron::*;

				let uv: Vec2 = Vec2::ZERO;
				let add_trapezoidal_band: &dyn Fn(&mut PieceHeaderParams, &[Vec3], &[Vec3], u32) -> () = &|
					params:			&mut PieceHeaderParams,
					curr_vert_loop:	&[Vec3],
					next_vert_loop:	&[Vec3],
					offset:			u32
				| -> () {
					for curr_vert_index in vert_range.clone() {
						warn_expect!(curr_vert_loop.len() == next_vert_loop.len(), ?);

						let vert_count: usize = curr_vert_loop.len();
						let next_vert_index: usize = (curr_vert_index + 1_usize) % vert_count;
						let normal: Vec3 = Tri::compute_normal(
							&next_vert_loop[curr_vert_index],
							&curr_vert_loop[curr_vert_index],
							&curr_vert_loop[next_vert_index]
						);
						let offset: u32 = params.mesh_attribute_data.vertices() as u32 - offset;

						params.mesh_attribute_data.push_vertex(&VecMeshVertexData {
							position: next_vert_loop[curr_vert_index],
							normal,
							uv
						});
						params.mesh_attribute_data.push_vertex(&VecMeshVertexData {
							position: curr_vert_loop[curr_vert_index],
							normal,
							uv
						});
						params.mesh_attribute_data.push_vertex(&VecMeshVertexData {
							position: curr_vert_loop[next_vert_index],
							normal,
							uv
						});
						params.mesh_attribute_data.push_vertex(&VecMeshVertexData {
							position: next_vert_loop[next_vert_index],
							normal,
							uv
						});

						let vert_index_a: u32 = offset + 0_u32;
						let vert_index_b: u32 = offset + 1_u32;
						let vert_index_c: u32 = offset + 2_u32;
						let vert_index_d: u32 = offset + 3_u32;

						params.mesh_attribute_data.indices.extend([
							vert_index_a,
							vert_index_b,
							vert_index_c,
							vert_index_c,
							vert_index_d,
							vert_index_a
						]);
					}
				};

				match params.piece_type {
					Type::Pentagon => {
						use pentagon::*;

						// Safe: we will be writing into these cells, and Vec3 has no destructor
						let mut vertices: [[Vec3; PENTAGON_VERTEX_COUNT]; PENTAGON_COUNT] = unsafe {
							MaybeUninit::<[[Vec3; PENTAGON_VERTEX_COUNT]; PENTAGON_COUNT]>::uninit().assume_init()
						};

						// Safe: we will be writing into these cells, and Tri has no destructor
						let mut pentagram_tris: [[Tri; PENTAGON_VERTEX_COUNT]; 2_usize] = unsafe {
							MaybeUninit::<[[Tri; PENTAGON_VERTEX_COUNT]; 2_usize]>::uninit().assume_init()
						};

						face_iter(&mut vertices[OUTER_OUTER_INDEX]);

						for prev_vert_index in vert_range.clone() {
							let curr_vert_index: usize = PIECE_TYPE.next_side_index(prev_vert_index);
							let next_vert_index: usize = PIECE_TYPE.next_side_index(curr_vert_index);
							let outer_outer_vertices: &[Vec3; PENTAGON_VERTEX_COUNT] = &vertices[OUTER_OUTER_INDEX];
							let mut tri: Tri = Tri::from([
								outer_outer_vertices[prev_vert_index],
								outer_outer_vertices[curr_vert_index],
								outer_outer_vertices[next_vert_index]
							]);

							pentagram_tris[OUTER_OUTER_INDEX][curr_vert_index] = tri;
							tri.offset_along_plane(-PENTAGONAL_ANNULUS_THICKNESS, OFFSET_MASK);
							pentagram_tris[OUTER_INNER_INDEX][curr_vert_index] = tri;

							let outer_outer_vert: &Vec3 =
								&pentagram_tris[OUTER_OUTER_INDEX][curr_vert_index][CURR_TRI_VERT_INDEX];
							let outer_inner_vert: &Vec3 =
								&pentagram_tris[OUTER_INNER_INDEX][curr_vert_index][CURR_TRI_VERT_INDEX];
							let depth_offset: Vec3 = -SHELL_DEPTH_RATIO * *outer_outer_vert;

							vertices[OUTER_OUTER_INDEX][curr_vert_index] = *outer_outer_vert;
							vertices[OUTER_INNER_INDEX][curr_vert_index] = *outer_inner_vert;
							vertices[INNER_INNER_INDEX][curr_vert_index] = *outer_inner_vert + depth_offset;
							vertices[INNER_OUTER_INDEX][curr_vert_index] = *outer_outer_vert + depth_offset;
						}

						let add_pentagonal_annulus: &dyn Fn(
							&mut PieceHeaderParams,
							&[Vec3; PENTAGON_VERTEX_COUNT],
							&[Vec3; PENTAGON_VERTEX_COUNT],
							u32
						) -> () = &|
							params: &mut PieceHeaderParams,
							curr_vert_pentagon: &[Vec3; PENTAGON_VERTEX_COUNT],
							next_vert_pentagon: &[Vec3; PENTAGON_VERTEX_COUNT],
							offset: u32
						| -> () {
							let normal: Vec3 = Tri::compute_normal(
								&next_vert_pentagon[0_usize],
								&curr_vert_pentagon[0_usize],
								&curr_vert_pentagon[1_usize]
							);
							let offset: u32 = params.mesh_attribute_data.vertices() as u32 - offset;
							let curr_loop_offset: usize = CURR_LOOP_INDEX * vert_count;
							let next_loop_offset: usize = NEXT_LOOP_INDEX * vert_count;
							let mut push_vertex = |position: &Vec3| -> () {
								params.mesh_attribute_data.push_vertex(&VecMeshVertexData {
									position: *position,
									normal,
									uv
								});
							};

							curr_vert_pentagon.iter().for_each(&mut push_vertex);
							next_vert_pentagon.iter().for_each(&mut push_vertex);

							for curr_vert_index in vert_range.clone() {
								let next_vert_index: usize = PIECE_TYPE.next_side_index(curr_vert_index);
								let vert_index_a: u32 = (next_loop_offset + curr_vert_index) as u32 + offset;
								let vert_index_b: u32 = (curr_loop_offset + curr_vert_index) as u32 + offset;
								let vert_index_c: u32 = (curr_loop_offset + next_vert_index) as u32 + offset;
								let vert_index_d: u32 = (next_loop_offset + next_vert_index) as u32 + offset;

								params.mesh_attribute_data.indices.extend([
									vert_index_a,
									vert_index_b,
									vert_index_c,
									vert_index_c,
									vert_index_d,
									vert_index_a
								]);
							}
						};

						// Add the base mesh
						{
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							add_pentagonal_annulus(
								&mut params,
								&vertices[OUTER_OUTER_INDEX],
								&vertices[OUTER_INNER_INDEX],
								vertices_offset
							);
							add_trapezoidal_band(
								&mut params,
								&vertices[OUTER_INNER_INDEX],
								&vertices[INNER_INNER_INDEX],
								vertices_offset
							);
							add_pentagonal_annulus(
								&mut params,
								&vertices[INNER_INNER_INDEX],
								&vertices[INNER_OUTER_INDEX],
								vertices_offset
							);
							add_trapezoidal_band(
								&mut params,
								&vertices[INNER_OUTER_INDEX],
								&vertices[OUTER_OUTER_INDEX],
								vertices_offset
							);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}

						// Add the primary pent mesh
						{
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							// Safe: we will be writing into these cells, and Vec3 has no destructor
							let mut vertices: [[Vec3; PENTAGON_VERTEX_COUNT]; 2_usize] = unsafe {
								MaybeUninit::<[[Vec3; PENTAGON_VERTEX_COUNT]; 2_usize]>::uninit().assume_init()
							};

							for vert_index in vert_range.clone() {
								let mut init_verts = |ring_index: usize, offset: f32| -> () {
									let pentagram_tri: &mut Tri = &mut pentagram_tris[ring_index][vert_index];

									pentagram_tri.offset_along_plane(offset, OFFSET_MASK);
									pentagram_tri.offset_along_normal(NORMAL_OFFSET);
									vertices[ring_index][vert_index] = pentagram_tri[CURR_TRI_VERT_INDEX];
								};

								init_verts(CURR_LOOP_INDEX, -PLANAR_OFFSET);
								init_verts(NEXT_LOOP_INDEX, PLANAR_OFFSET);
							}

							add_pentagonal_annulus(
								&mut params,
								&vertices[CURR_LOOP_INDEX],
								&vertices[NEXT_LOOP_INDEX],
								vertices_offset
							);
							add_piece_range(&mut params);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}

						// Add the adjacent pent meshes
						for vert_index in vert_range.clone() {
							// These meshes are just one circle each, so no need to worry about the offset
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							let start_edge_index: usize = PIECE_TYPE.next_side_index(vert_index + 1_usize);
							let end_edge_index: usize = PIECE_TYPE.next_side_index(start_edge_index);
							let vert_a: Vec3 = vertices[INNER_INNER_INDEX][start_edge_index];
							let vert_b: Vec3 = vertices[OUTER_INNER_INDEX][start_edge_index];
							let vert_c: Vec3 = vertices[OUTER_INNER_INDEX][end_edge_index];
							let vert_d: Vec3 = vertices[INNER_INNER_INDEX][end_edge_index];
							let edge_bc_midpoint: Vec3 = 0.5_f32 * (vert_b + vert_c);
							let edge_ad_midpoint: Vec3 = 0.5_f32 * (vert_a + vert_d);
							let diameter: Vec3 = edge_bc_midpoint - edge_ad_midpoint;
							let diameter_len: f32 = diameter.length();
							let radius: Vec3 = ((0.5_f32 * diameter_len - PLANAR_OFFSET) / diameter_len) * diameter;
							let normal: Vec3 = diameter.cross(vert_c - vert_b).normalize();
							let center: Vec3 = 0.5_f32 * (edge_bc_midpoint + edge_ad_midpoint) + NORMAL_OFFSET * normal;

							// Safe: we will be writing into these cells, and Vec3 has no destructor
							let mut circle_vertices: [Vec3; CIRCLE_SUBDIVISION_COUNT] = unsafe {
								MaybeUninit::<[Vec3; CIRCLE_SUBDIVISION_COUNT]>::uninit().assume_init()
							};

							for circle_vert_index in 0_usize .. CIRCLE_SUBDIVISION_COUNT {
								circle_vertices[circle_vert_index] = Quat::from_axis_angle(
									normal,
									circle_vert_index as f32 * TAU_OVER_CIRCLE_SUBDIVISION_COUNT
								) * radius + center;
							}

							add_regular_polygon(
								&mut params,
								&circle_vertices,
								&center,
								&normal,
								&uv,
								vertices_offset,
								false
							);
							add_adjacent_face_indices_same_piece(&mut params, vert_index);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}
					},
					Type::Triangle => {
						use triangle::*;

						// Safe: we will be writing into these cells, and Vec3 has no destructor
						let mut vertices: [[Vec3; TRIANGLE_VERTEX_COUNT]; 2_usize] = unsafe {
							MaybeUninit::<[[Vec3; TRIANGLE_VERTEX_COUNT]; 2_usize]>::uninit().assume_init()
						};
						let [
							outer_vert_loop,
							inner_vert_loop
						] = &mut vertices;

						face_iter(outer_vert_loop);

						let outer_center: Vec3 = transformation * pyramid_data.verts[0].vec;

						// Add the base mesh
						{
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);
							let inner_center: Vec3 = {
								let mut inner_center_sum: Vec3 = Vec3::ZERO;
	
								for (vert_index, outer_vert) in outer_vert_loop.iter().enumerate() {
									inner_vert_loop[vert_index] = (1.0_f32 - SHELL_DEPTH_RATIO) * *outer_vert;
									inner_center_sum += *outer_vert;
								}

								inner_center_sum / TRIANGLE_VERTEX_COUNT as f32
							};

							add_pyramid(
								&mut params,
								outer_vert_loop,
								&mut vert_range.clone(),
								&outer_center,
								vertices_offset,
								false,
								false,
								false
							);
							add_trapezoidal_band(
								&mut params,
								inner_vert_loop,
								outer_vert_loop,
								vertices_offset
							);
							add_regular_polygon(
								&mut params,
								inner_vert_loop,
								&inner_center,
								&(-inner_center).normalize(),
								&uv,
								vertices_offset,
								true
							);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}

						// Add the adjacent pent meshes
						for vert_index in vert_range.clone() {
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							add_pyramid(
								&mut params,
								outer_vert_loop,
								&mut [vert_index].into_iter(),
								&outer_center,
								vertices_offset,
								true,
								true,
								false
							);
							add_adjacent_face_indices_other_piece(&mut params, vert_index);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}
					}
				}
			},
			Design::CustomSuperDodecahedron => {
				use custom_super_dodecahedron::*;

				let uv: Vec2 = Vec2::ZERO;

				match params.piece_type {
					Type::Pentagon => {
						use pentagon::*;

						// Safe: we will be writing into these cells, and Vec3 has no destructor
						let mut vertices: [[Vec3; PENTAGON_VERTEX_COUNT]; PENTAGON_COUNT] = unsafe {
							MaybeUninit::<[[Vec3; PENTAGON_VERTEX_COUNT]; PENTAGON_COUNT]>::uninit().assume_init()
						};
						let [
							outer_pentagon,
							midpoint_pentagon,
							pentagram_pentagon
						] = &mut vertices;

						face_iter(outer_pentagon);

						let center: Vec3 = {
							let mut center_sum: Vec3 = Vec3::ZERO;

							for vert_index in vert_range.clone() {
								let outer_vert: Vec3 = outer_pentagon[vert_index];
								let next_side_index: usize = PIECE_TYPE.next_side_index(vert_index);
	
								midpoint_pentagon[vert_index] = (outer_vert
									+ outer_pentagon[next_side_index]
								) * 0.5_f32;
								pentagram_pentagon[vert_index] = (ONE_OVER_PHI as f32)
									* outer_vert
									+ (ONE_OVER_PHI_SQUARED as f32)
									* outer_pentagon[PIECE_TYPE.next_side_index(next_side_index)];
								center_sum += outer_vert;
							}

							center_sum / PENTAGON_VERTEX_COUNT as f32
						};

						// Add the base mesh
						{
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							add_regular_polygon(
								&mut params,
								outer_pentagon,
								&center,
								&center.normalize(),
								&uv,
								vertices_offset,
								false
							);
							add_pyramid(
								&mut params,
								outer_pentagon,
								&mut vert_range.clone(),
								&Vec3::ZERO,
								vertices_offset,
								false,
								false,
								true
							);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}

						// Add the primary pent mesh
						{
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							for vert_index in vert_range.clone() {
								let mut tri: Tri = Tri::from([
									outer_pentagon[vert_index],
									pentagram_pentagon[vert_index],
									pentagram_pentagon[PIECE_TYPE.prev_side_index(vert_index)]
								]);

								tri.offset_all_along_plane(-PLANAR_OFFSET);
								tri.offset_along_normal(NORMAL_OFFSET);
								params.mesh_attribute_data.push_vertex(&(tri, vertices_offset));
							}

							add_piece_range(&mut params);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}

						// Add the adjacent pent meshes
						for vert_index in vert_range.clone() {
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);
							let prev_vert_index: usize = PIECE_TYPE.prev_side_index(vert_index);

							for tri_index in 0_usize .. 3 {
								let mut tri: Tri = match tri_index {
									0 => Tri::from([
										outer_pentagon[vert_index],
										midpoint_pentagon[vert_index],
										pentagram_pentagon[vert_index]
									]),
									1 => Tri::from([
										center,
										pentagram_pentagon[prev_vert_index],
										pentagram_pentagon[vert_index]
									]),
									_ => Tri::from([
										outer_pentagon[vert_index],
										pentagram_pentagon[prev_vert_index],
										midpoint_pentagon[prev_vert_index]
									])
								};

								tri.offset_all_along_plane(-PLANAR_OFFSET);
								tri.offset_along_normal(NORMAL_OFFSET);
								params.mesh_attribute_data.push_vertex(&(tri, vertices_offset));
							}

							add_adjacent_face_indices_same_piece(&mut params, vert_index);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}
					},
					Type::Triangle => {
						use triangle::*;

						// Safe: we will be writing into these cells, and Vec3 has no destructor
						let mut vertices: [Vec3; TRIANGLE_VERTEX_COUNT] = unsafe {
							MaybeUninit::<[Vec3; TRIANGLE_VERTEX_COUNT]>::uninit().assume_init()
						};

						face_iter(&mut vertices);

						let center: Vec3 = transformation * pyramid_data.verts[0].vec;

						// Add the base mesh
						{
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							add_pyramid(
								&mut params,
								&vertices,
								&mut vert_range.clone(),
								&center,
								vertices_offset,
								false,
								false,
								false
							);
							add_pyramid(
								&mut params,
								&vertices,
								&mut vert_range.clone(),
								&Vec3::ZERO,
								vertices_offset,
								false,
								false,
								true
							);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}

						// Add the adjacent pent meshes
						for vert_index in vert_range.clone() {
							let vertices_offset: u32 =
								push_mesh_header(&mut params, MESH_STATS_SLICE, &offset, mesh_index);

							add_pyramid(
								&mut params,
								&vertices,
								&mut [vert_index].into_iter(),
								&center,
								vertices_offset,
								true,
								true,
								false
							);
							add_adjacent_face_indices_other_piece(&mut params, vert_index);
							check_mesh_stats(&params, MESH_STATS_SLICE, &mut offset, &mut mesh_index)?;
						}
					}
				}
			}
		}

		Ok(Self(start .. params.mesh_headers.offset() as u32))
	}
}


#[derive(Default)]
struct DesignHeader([PieceHeader; Type::COUNT]);

#[derive(Default)]
struct DesignHeaders([DesignHeader; Design::COUNT]);

#[derive(Default)]
pub struct PieceLibrary {
	mesh_attribute_data:	MeshAttributeData,
	mesh_headers:			MeshHeaders,
	design_headers:			DesignHeaders
}

impl PieceLibrary {
	fn try_init(&mut self) -> Result<(), ()> {
		let Self {
			mesh_attribute_data,
			mesh_headers,
			design_headers
		} = self;

		let mut piece_header_params: PieceHeaderParams = PieceHeaderParams {
			design: Design::iter().next().unwrap(),
			piece_type: Type::iter().next().unwrap(),
			mesh_attribute_data,
			mesh_headers
		};

		for design in Design::iter() {
			let design_header: &mut DesignHeader = &mut design_headers.0[design as usize];

			piece_header_params.design = design;

			for piece_type in Type::iter() {
				piece_header_params.piece_type = piece_type;
				design_header.0[piece_type as usize] = PieceHeader::try_from(&mut piece_header_params)?;
			}
		}

		Ok(())
	}

	fn add_bevy_meshes(&self, bevy_meshes: &mut Assets<BevyMesh>, bevy_mesh_handles: &mut BevyMeshHandles) -> () {
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
		piece_mats:			&PieceMats,
		bevy_mesh_handles:	&BevyMeshHandles,
		world:				&mut World,
		design:				Design
	) -> () {
		warn_expect!(self.mesh_headers.0.len() == bevy_mesh_handles.0.len(), ?);

		// Cache the entities we'll be using first, since iterating with the query borrows world
		// Safe: we will be writing into these cells, and Entity has no destructor
		let mut entities: [Entity; usize::PIECE_COUNT] = unsafe {
			MaybeUninit::<[Entity; usize::PIECE_COUNT]>::uninit().assume_init()
		};
		let mut entity_count: u32 = 0_u32;

		for piece_components in world.query::<PieceComponents>().iter(world) {
			entities[piece_components.piece_component.index] = piece_components.entity;
			entity_count += 1_u32;
		}

		warn_expect!(entity_count == usize::PIECE_COUNT as u32, ?);

		let design_header: &DesignHeader = &self.design_headers.0[design as usize];

		for piece_type in Type::iter() {
			let mesh_range: Range<usize> =
				Option::<Range<usize>>::from_alt(design_header.0[piece_type as usize].0.clone()).unwrap();
			let mesh_headers: &[MeshHeader] = &self.mesh_headers.0[mesh_range.clone()];
			let bevy_mesh_handles: &[Handle<BevyMesh>] = &bevy_mesh_handles.0[mesh_range];
			let index_offset: usize = piece_type.index_offset();

			for index in piece_type.range() {
				let mut entity_mut: EntityMut = world.entity_mut(entities[index]);

				entity_mut.despawn_descendants();
				entity_mut.with_children(|world_child_builder: &mut WorldChildBuilder| -> () {
					for (face_indices, bevy_mesh_handle)
						in mesh_headers.iter().map(MeshHeader::face_indices).zip(bevy_mesh_handles.iter())
					{
						world_child_builder.spawn_bundle(PbrBundle {
							mesh: bevy_mesh_handle.clone(),
							material: if face_indices.is_empty() {
								piece_mats.base_mat.mat.clone()
							} else {
								piece_mats
									.color_mats
									[
										self
											.mesh_attribute_data
											.face_indices
											[face_indices]
											[index - index_offset]
											as usize
									]
									.mat
									.clone()
							},
							.. PbrBundle::default()
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
		design: Design
	) -> () {
		let icosidodecahedron_faces: &Vec<FaceData> = &Data::get(Polyhedron::Icosidodecahedron).faces;

		for piece_type in Type::iter() {
			for index in piece_type.range() {
				world.spawn().insert_bundle((
					PieceComponent { index, piece_type },
					Transform::from_matrix(Mat4::from_quat(icosidodecahedron_faces[index].quat)),
					GlobalTransform::identity()
				));
			}
		}

		self.update_entities(piece_mats, bevy_mesh_handles, world, design);
	}
}

impl StaticDataLibrary for PieceLibrary {
	fn pre_init() -> Option<Box<dyn FnOnce() -> ()>> { Some(Box::new(Data::initialize)) }
	fn get() -> &'static Self { &PIECE_LIBRARY }
}

lazy_static! {
	static ref PIECE_LIBRARY: PieceLibrary = {
		let mut piece_library: PieceLibrary = PieceLibrary::default();

		warn_expect_ok!(piece_library.try_init());

		piece_library
	};
}

pub struct BevyMeshHandles(Vec<Handle<BevyMesh>>);

impl Default for BevyMeshHandles {
	fn default() -> Self { Self(Vec::<Handle<BevyMesh>>::with_capacity(generation::TOTAL_MESH_COUNT)) }
}

pub struct PiecePlugin;

impl PiecePlugin {
	fn startup_piece_library() -> () { PieceLibrary::build(); }

	fn startup(
		mut bevy_meshes: ResMut<Assets<BevyMesh>>,
		mut bevy_mesh_handles: ResMut<BevyMeshHandles>
	) -> () {
		PieceLibrary::get().add_bevy_meshes(&mut bevy_meshes, &mut bevy_mesh_handles);
	}

	fn startup_exclusive(
		world: &mut World,
	) -> () {
		warn_expect!(world.contains_resource::<Preferences>(), ?);
		warn_expect!(world.contains_resource::<BevyMeshHandles>(), ?);

		world.resource_scope(|world: &mut World, preferences: Mut<Preferences>| -> () {
			world.resource_scope(|world: &mut World, bevy_mesh_handles: Mut<BevyMeshHandles>| -> () {
				let design: Design = preferences.puzzle.design;

				PieceLibrary::get().add_entities(
					&warn_expect_some!(
						PieceMats::try_from(&preferences.puzzle.color.colors_with_mat, design),
						return
					),
					&bevy_mesh_handles,
					world,
					design
				);
			});
		});
	}
}

impl Plugin for PiecePlugin {
	fn build(&self, app: &mut App) -> () {
		app
			.insert_resource(BevyMeshHandles::default())
			.add_startup_system(Self::startup_piece_library.after(PolyhedraDataPlugin::startup))
			.add_startup_system(Self::startup.after(Self::startup_piece_library))
			.add_startup_system(Self::startup_exclusive.exclusive_system().at_end());
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_piece_library() -> () {
		init_env_logger();

		Data::initialize();
		break_assert!(PieceLibrary::default().try_init().is_ok());
	}
}