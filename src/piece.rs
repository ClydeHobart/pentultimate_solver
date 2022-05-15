use {
	std::{
		f32::consts::{
			PI,
			TAU
		},
		mem::MaybeUninit,
		ops::Range
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
		prelude::*,
		util::inspectable_bin_map::*
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
	pub fn as_polyhedron(self) -> Polyhedron {
		match self {
			Design::OriginalSuperDodecahedron => Polyhedron::Dodecahedron,
			Design::CustomSuperDodecahedron => Polyhedron::Dodecahedron
		}
	}
}

impl Update for Design {
	fn update(&self, other: &Self, world: &mut World, preferences: &Preferences) -> () {
		if self != other {
			warn_expect!(world.contains_resource::<PieceLibrary>(),?);

			world.resource_scope(|world: &mut World, piece_library: Mut<PieceLibrary>| -> () {
				let color_data_with_mat: &ColorDataWithMat = &preferences.puzzle.color.colors_with_mat;

				piece_library.0[*self as usize].update_entities(
					world,
					&PieceMats {
						base_mat: &color_data_with_mat.base_color,
						color_mats: warn_expect_some!(
							color_data_with_mat
								.polyhedron_to_colors
								.as_inspectable_bin_map()
								.get(&self.as_polyhedron()),
							return
						),
					}
				);
			});
		}
	}
}

#[derive(Clone, Copy, Debug, Deserialize)]
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
	vertices: [Vec3; 3],
	normal: Vec3
}

impl Tri {
	pub fn update_normal(&mut self) -> &mut Self {
		self.normal = (self[1] - self[0]).cross(self[2] - self[1]).normalize();

		self
	}

	pub fn offset_along_normal(&mut self, distance: f32) -> &mut Self {
		let offset: Vec3 = self.normal * distance;

		for vertex in self.vertices.iter_mut() {
			*vertex += offset;
		}

		self
	}

	pub fn offset_all_along_plane(&mut self, distance: f32) -> &mut Self {
		self.offset_along_plane(distance, [true; 3])
	}

	pub fn offset_along_plane(&mut self, distance: f32, mask: [bool; 3]) -> &mut Self {
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

#[derive(Default)]
struct Mesh {
	tris:			Vec<Tri>,
	bevy_mesh:		Option<Handle<BevyMesh>>,
	face_indices:	Option<Vec<usize>>
}

impl Mesh {
	fn add_bevy_mesh(&mut self, bevy_meshes: &mut ResMut<Assets<BevyMesh>>) -> () {
		self.bevy_mesh = Some(bevy_meshes.add(BevyMesh::from(self as &Mesh)));
	}

	fn has_bevy_mesh(&self) -> bool {
		self.bevy_mesh.is_some()
	}

	fn has_face_indices(&self) -> bool {
		self.face_indices.is_some()
	}
}

impl From<&Mesh> for BevyMesh {
	fn from(mesh: &Mesh) -> Self {
		let mut bevy_mesh: Self = Self::new(PrimitiveTopology::TriangleList);
		let mut positions: Vec<[f32; 3]> = Vec::<[f32; 3]>::new();
		let mut normals: Vec<[f32; 3]> = Vec::<[f32; 3]>::new();

		for tri in &mesh.tris {
			let normal: &[f32; 3] = tri.normal.as_ref();

			for vertex in &tri.vertices {
				positions.push(vertex.as_ref().clone());
				normals.push(normal.clone());
			}
		}

		let len: usize = positions.len();
		let uv: [f32; 2] = [0.0_f32; 2];
		let mut uvs: Vec<[f32; 2]> = Vec::<[f32; 2]>::with_capacity(len);
		let mut indices: Vec<u32> = Vec::<u32>::with_capacity(len);

		for index in 0_u32 .. len as u32 {
			uvs.push(uv.clone());
			indices.push(index);
		}

		bevy_mesh.insert_attribute(BevyMesh::ATTRIBUTE_POSITION, positions);
		bevy_mesh.insert_attribute(BevyMesh::ATTRIBUTE_NORMAL, normals);
		bevy_mesh.insert_attribute(BevyMesh::ATTRIBUTE_UV_0, uvs);
		bevy_mesh.set_indices(Some(Indices::U32(indices)));

		bevy_mesh
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

struct Piece {
	piece_type:		Type,
	base_mesh:		Mesh,
	color_meshes:	Vec<Mesh>
}

impl Piece {
	fn add_bevy_meshes(&mut self, bevy_meshes: &mut ResMut<Assets<BevyMesh>>) -> () {
		self.base_mesh.add_bevy_mesh(bevy_meshes);

		for color_mesh in &mut self.color_meshes {
			color_mesh.add_bevy_mesh(bevy_meshes);
		}
	}

	fn from_design_and_type(design: Design, piece_type: Type) -> LogErrorResult<Self> {
		const PLANAR_OFFSET_RATIO:			f32					= 1.0_f32 / 64.0_f32;
		const NORMAL_OFFSET_RATIO:			f32					= 1.0_f32 / 256.0_f32;
		const SHELL_DEPTH_RATIO:			f32					= 0.25_f32;
		const PLANAR_OFFSET:				f32					= PLANAR_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;
		const NORMAL_OFFSET:				f32					= NORMAL_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;

		let pyramid_data:					&Data				= Data::get(piece_type.pyramid_polyhedron());
		let icosahedron_data:				&Data				= Data::get(Polyhedron::Icosahedron);
		let _dodecahedron_data:				&Data				= Data::get(Polyhedron::Dodecahedron);
		let icosidodecahedron_data:			&Data				= Data::get(Polyhedron::Icosidodecahedron);
		let _rhombic_triacontahedron_data:	&Data				= Data::get(Polyhedron::RhombicTriacontahedron);
		let icosidodecahedron_verts:		&Vec<VertexData>	= &icosidodecahedron_data.verts;
		let icosidodecahedron_vert_indices:	&Vec<usize>			= &icosidodecahedron_data.vert_indices;
		let icosidodecahedron_faces:		&Vec<FaceData>		= &icosidodecahedron_data.faces;
		let vert_count:						usize				= piece_type.vertex_count();
		let index_offset:					usize				= piece_type.index_offset();
		let instance_count:					usize				= piece_type.instance_count();
		let face_range:						Range<usize>		= index_offset .. index_offset + instance_count;
		let double_vert_count:				usize				= 2_usize * vert_count;
		let vert_range:						Range<usize>		= 0_usize .. vert_count;
		let face_iter = icosidodecahedron_data
			.faces
			[index_offset]
			.range
			.clone()
			.map(|vert_indices_index: usize| -> &Vec3 {
				&icosidodecahedron_verts[icosidodecahedron_vert_indices[vert_indices_index]].vec
			});
		let transformation:					Mat4				= Mat4::from_quat(icosidodecahedron_data.faces[index_offset].quat.inverse());
		let build_base_mesh = |center: &Vec3, vertices: &[Vec3]| -> Mesh {
			let mut tris: Vec<Tri> = Vec::with_capacity(double_vert_count);

			for side_index in vert_range.clone() {
				let vert_1: Vec3 = vertices[side_index];
				let vert_2: Vec3 = vertices[piece_type.next_side_index(side_index)];

				tris.push(&transformation * Tri::from([
					Vec3::ZERO,
					vert_2,
					vert_1
				]));
				tris.push(&transformation * Tri::from([
					*center,
					vert_1,
					vert_2
				]));
			}

			Mesh {
				tris,
				.. Default::default()
			}
		};
		let build_neighboring_face_indices_same_piece =
			|vert_index: usize| -> Option<Vec<usize>> {
				Some(face_range.clone().map(|face_index: usize| -> usize {
						let face_data: &FaceData = &icosidodecahedron_faces[face_index];

						icosahedron_data.get_closest_vert_index(
							&(Quat::from_axis_angle(
								icosidodecahedron_verts[
									face_data.get_slice(icosidodecahedron_vert_indices)[vert_index]
								].vec,
								PI
							) * face_data.norm),
							None
						)
					})
					.collect()
				)
			};
		let build_neighboring_face_indices_other_piece =
			|side_index: usize| -> Option<Vec<usize>> {
				let next_side_index: usize = piece_type.next_side_index(side_index);

				Some(face_range.clone().map(|face_index: usize| -> usize {
						let edge_index: usize = icosidodecahedron_data.get_edge_index(&{
							let face_slice: &[usize] = icosidodecahedron_faces
								[face_index]
								.get_slice(icosidodecahedron_vert_indices);

							EdgeData::new(
								face_slice[side_index],
								face_slice[next_side_index]
							)
						}).unwrap();

						icosidodecahedron_data.get_closest_face_index(
							&Vec3::ZERO,
							Some(&|query_face_index: usize, face_data: &FaceData| -> bool {
								face_data.edges[edge_index] && query_face_index != face_index
							})
						)
					})
					.collect()
				)
			};
		let build_color_meshes= |center: &Vec3, vertices: &[Vec3]| -> Vec<Mesh> {
			let mut color_meshes: Vec<Mesh> = Vec::with_capacity(vert_count);

			for side_index in vert_range.clone() {
				color_meshes.push(Mesh {
					tris: vec![
						&transformation * *Tri::from([
							*center,
							vertices[side_index],
							vertices[piece_type.next_side_index(side_index)]
						]).offset_all_along_plane(-PLANAR_OFFSET).offset_along_normal(NORMAL_OFFSET)
					],
					face_indices: build_neighboring_face_indices_other_piece(side_index),
					.. Default::default()
				});
			}

			color_meshes
		};
		let build_pyramid_piece = || -> LogErrorResult<Piece> {
			let mut vertices: Vec<Vec3> = Vec::with_capacity(vert_count);

			for vert in face_iter.clone() {
				vertices.push(*vert);
			}

			let center: Vec3 = pyramid_data.verts[0].vec;

			Ok(Piece {
				piece_type,
				base_mesh: build_base_mesh(&center, &vertices),
				color_meshes: build_color_meshes(&center, &vertices)
			})
		};

		match (design, piece_type) {
			(Design::OriginalSuperDodecahedron, Type::Pentagon) => {
				const PENTAGONAL_RING_THICKNESS_RATIO: f32 = 0.2_f32;
				const PENTAGONAL_RING_THICKNESS: f32 = PENTAGONAL_RING_THICKNESS_RATIO * ICOSIDODECAHEDRON.edge_length;
				const CIRCLE_SUBDIVISION_COUNT: usize = 16_usize;
				const CIRCLE_SUBDIVISION_COUNT_F32: f32 = CIRCLE_SUBDIVISION_COUNT as f32;
				const VERTEX_RING_COUNT: usize = 4_usize;
				const PENTAGON_VERTEX_COUNT: usize = usize::PENTAGON_VERTEX_COUNT;
				const OUTER_OUTER_INDEX: usize = 0_usize;
				const OUTER_INNER_INDEX: usize = 1_usize;
				const INNER_INNER_INDEX: usize = 2_usize;
				const INNER_OUTER_INDEX: usize = 3_usize;
				const TRIS_PER_QUAD: usize = 2_usize;
				const TRIS_PER_QUAD_RING: usize = TRIS_PER_QUAD * PENTAGON_VERTEX_COUNT;
				const BASE_MESH_TRIS_CAPACITY: usize = TRIS_PER_QUAD_RING * VERTEX_RING_COUNT;
				const CURR_TRI_VERT_INDEX: usize = 1_usize;
				const OFFSET_MASK: [bool; 3_usize] = [true, true, false];

				// Safe: we will be writing into these cells, and Vec3 has no destructor
				let mut vertices: [[Vec3; PENTAGON_VERTEX_COUNT]; VERTEX_RING_COUNT] = unsafe {
					MaybeUninit::<[[Vec3; PENTAGON_VERTEX_COUNT]; VERTEX_RING_COUNT]>::uninit().assume_init()
				};

				for (vert_index, vert) in face_iter.clone().enumerate() {
					vertices[OUTER_OUTER_INDEX][vert_index] = *vert;
				}

				// Safe: we will be writing into these cells, and Tri has no destructor
				let mut pentagram_tris: [[Tri; PENTAGON_VERTEX_COUNT]; 2_usize] = unsafe {
					MaybeUninit::<[[Tri; PENTAGON_VERTEX_COUNT]; 2_usize]>::uninit().assume_init()
				};

				for prev_vert_index in vert_range.clone() {
					let curr_vert_index: usize = Type::Pentagon.next_side_index(prev_vert_index);
					let next_vert_index: usize = Type::Pentagon.next_side_index(curr_vert_index);
					let outer_outer_vertices: &[Vec3; PENTAGON_VERTEX_COUNT] = &vertices[OUTER_OUTER_INDEX];
					let mut tri: Tri = Tri::from([
						outer_outer_vertices[prev_vert_index],
						outer_outer_vertices[curr_vert_index],
						outer_outer_vertices[next_vert_index]
					]);

					pentagram_tris[OUTER_OUTER_INDEX][curr_vert_index] = tri;
					tri.offset_along_plane(-PENTAGONAL_RING_THICKNESS, OFFSET_MASK);
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

				let base_mesh: Mesh = {
					let mut tris: Vec<Tri> = Vec::<Tri>::with_length_and_capacity(
						BASE_MESH_TRIS_CAPACITY,
						BASE_MESH_TRIS_CAPACITY);

					for curr_vert_index in vert_range.clone() {
						let next_vert_index: usize = Type::Pentagon.next_side_index(curr_vert_index);

						for curr_vert_ring_index in 0_usize .. VERTEX_RING_COUNT {
							let next_vert_ring_index: usize = (curr_vert_ring_index + 1_usize) % VERTEX_RING_COUNT;
							let tri_index: usize = TRIS_PER_QUAD_RING * curr_vert_ring_index
								+ TRIS_PER_QUAD * curr_vert_index;
							let vert_a: Vec3 = vertices[next_vert_ring_index][curr_vert_index];
							let vert_b: Vec3 = vertices[curr_vert_ring_index][curr_vert_index];
							let vert_c: Vec3 = vertices[curr_vert_ring_index][next_vert_index];
							let vert_d: Vec3 = vertices[next_vert_ring_index][next_vert_index];

							tris[tri_index]				= &transformation * Tri::from([vert_a, vert_b, vert_c]);
							tris[tri_index + 1_usize]	= &transformation * Tri::from([vert_c, vert_d, vert_a]);
						}
					}

					Mesh {
						tris,
						.. Mesh::default()
					}
				};

				let mut color_meshes: Vec<Mesh> = Vec::<Mesh>::with_capacity(1_usize + PENTAGON_VERTEX_COUNT);

				// Add the color mesh for the primary piece
				color_meshes.push({
					let mut tris: Vec<Tri> = Vec::<Tri>::with_length_and_capacity(
						TRIS_PER_QUAD_RING,
						TRIS_PER_QUAD_RING);

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

						init_verts(OUTER_OUTER_INDEX, -PLANAR_OFFSET);
						init_verts(OUTER_INNER_INDEX, PLANAR_OFFSET);
					}

					for curr_vert_index in vert_range.clone() {
						let next_vert_index: usize = Type::Pentagon.next_side_index(curr_vert_index);
						let tri_index: usize = TRIS_PER_QUAD * curr_vert_index;
						let vert_a: Vec3 = vertices[OUTER_INNER_INDEX][curr_vert_index];
						let vert_b: Vec3 = vertices[OUTER_OUTER_INDEX][curr_vert_index];
						let vert_c: Vec3 = vertices[OUTER_OUTER_INDEX][next_vert_index];
						let vert_d: Vec3 = vertices[OUTER_INNER_INDEX][next_vert_index];

						tris[tri_index]				= &transformation * Tri::from([vert_a, vert_b, vert_c]);
						tris[tri_index + 1_usize]	= &transformation * Tri::from([vert_c, vert_d, vert_a]);
					}

					Mesh {
						tris,
						face_indices: Some(PENTAGON_PIECE_RANGE.collect()),
						.. Mesh::default()
					}
				});

				for vert_index in vert_range.clone() {
					color_meshes.push({
						let mut tris: Vec<Tri> = Vec::<Tri>::with_capacity(CIRCLE_SUBDIVISION_COUNT);

						let start_edge_index: usize = Type::Pentagon.next_side_index(vert_index + 1_usize);
						let end_edge_index: usize = Type::Pentagon.next_side_index(start_edge_index);
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
								circle_vert_index as f32 * TAU / CIRCLE_SUBDIVISION_COUNT_F32
							) * radius + center;
						}

						for circle_vert_index in 0_usize .. CIRCLE_SUBDIVISION_COUNT {
							let next_circle_vert_index: usize =
								(circle_vert_index + 1_usize) % CIRCLE_SUBDIVISION_COUNT;
							
							tris.push(&transformation * Tri::from([
								center,
								circle_vertices[circle_vert_index],
								circle_vertices[next_circle_vert_index]
							]));
						}

						Mesh {
							tris,
							face_indices: build_neighboring_face_indices_same_piece(vert_index),
							.. Mesh::default()
						}
					});
				}

				Ok(Piece {
					piece_type,
					base_mesh,
					color_meshes
				})
			},
			(Design::OriginalSuperDodecahedron, Type::Triangle) => {
				const OUTER_INDEX: usize = 0_usize;
				const INNER_INDEX: usize = 1_usize;
				const TRIANGLE_VERTEX_COUNT: usize = usize::TRIANGLE_VERTEX_COUNT;

				// Safe: we will be writing into these cells, and Vec3 has no destructor
				let mut vertices: [[Vec3; TRIANGLE_VERTEX_COUNT]; 2_usize] = unsafe {
					MaybeUninit::<[[Vec3; TRIANGLE_VERTEX_COUNT]; 2_usize]>::uninit().assume_init()
				};

				for (vert_index, vert) in face_iter.clone().enumerate() {
					vertices[OUTER_INDEX][vert_index] = *vert;
					vertices[INNER_INDEX][vert_index] = (1.0_f32 - SHELL_DEPTH_RATIO) * *vert;
				}

				let outer_center: Vec3 = pyramid_data.verts[0].vec;
				let inner_center: Vec3 = vertices
					[INNER_INDEX]
					.iter()
					.sum::<Vec3>()
					/ TRIANGLE_VERTEX_COUNT as f32;

				Ok(Piece {
					piece_type,
					base_mesh: {
						let mut tris: Vec<Tri> = Vec::<Tri>::with_capacity(4_usize * TRIANGLE_VERTEX_COUNT);

						for curr_vert_index in vert_range.clone() {
							let next_vert_index: usize = Type::Triangle.next_side_index(curr_vert_index);
							let vert_a: Vec3 = vertices[INNER_INDEX][curr_vert_index];
							let vert_b: Vec3 = vertices[OUTER_INDEX][curr_vert_index];
							let vert_c: Vec3 = vertices[OUTER_INDEX][next_vert_index];
							let vert_d: Vec3 = vertices[INNER_INDEX][next_vert_index];

							tris.push(&transformation * Tri::from([vert_c, outer_center, vert_b]));
							tris.push(&transformation * Tri::from([vert_c, vert_b, vert_a]));
							tris.push(&transformation * Tri::from([vert_a, vert_d, vert_c]));
							tris.push(&transformation * Tri::from([vert_a, inner_center, vert_d]));
						}

						Mesh {
							tris,
							.. Mesh::default()
						}
					},
					color_meshes: build_color_meshes(&outer_center, &vertices[OUTER_INDEX])
				})
			},
			(Design::CustomSuperDodecahedron, Type::Pentagon) => {
				let mut vertices: Vec<Vec3> = Vec::with_capacity(3 * vert_count);
				let mut center_sum: Vec3 = Vec3::ZERO;

				for vert in face_iter.clone() {
					center_sum += *vert;
					vertices.push(*vert);
				}

				for vert_index in vert_range.clone() {
					vertices.push(
						(vertices[vert_index] + vertices[piece_type.next_side_index(vert_index)]) * 0.5_f32
					);
				}

				for vert_index in vert_range.clone() {
					vertices.push(
						(ONE_OVER_PHI as f32)
							* vertices[vert_index]
							+ (ONE_OVER_PHI_SQUARED as f32)
							* vertices[(vert_index + 2) % vert_count]
					);
				}

				let center: Vec3 = center_sum / vert_count as f32;
				let base_mesh: Mesh = build_base_mesh(&center, &vertices);

				let mut color_meshes: Vec<Mesh> = Vec::with_capacity(vert_count);

				// Add the color mesh for the primary piece
				{
					let mut tris: Vec<Tri> = Vec::with_capacity(vert_count);

					for vert_index in vert_range.clone() {
						let mut tri: Tri = Tri::from([
							vertices[vert_index],
							vertices[vert_index + double_vert_count],
							vertices[piece_type.prev_side_index(vert_index) + double_vert_count]
						]);

						tri.offset_all_along_plane(-PLANAR_OFFSET);
						tri.offset_along_normal(NORMAL_OFFSET);
						tris.push(&transformation * tri);
					}

					color_meshes.push(Mesh {
						tris,
						face_indices: Some(PENTAGON_PIECE_RANGE.collect()),
						.. Mesh::default()
					});
				}

				for vert_index in vert_range.clone() {
					let prev_vert_index: usize = piece_type.prev_side_index(vert_index);
					let mut tris: Vec<Tri> = Vec::with_capacity(3);

					for tri_index in 0_usize .. 3 {
						let mut tri: Tri = match tri_index {
							0 => Tri::from([
								vertices[vert_index],
								vertices[vert_index + vert_count],
								vertices[vert_index + double_vert_count]
							]),
							1 => Tri::from([
								center,
								vertices[prev_vert_index + double_vert_count],
								vertices[vert_index + double_vert_count]
							]),
							_ => Tri::from([
								vertices[vert_index],
								vertices[prev_vert_index + double_vert_count],
								vertices[prev_vert_index + vert_count]
							])
						};

						tri.offset_all_along_plane(-PLANAR_OFFSET);
						tri.offset_along_normal(NORMAL_OFFSET);
						tris.push(&transformation * tri);
					}

					color_meshes.push(Mesh {
						tris,
						face_indices: build_neighboring_face_indices_same_piece(vert_index),
						.. Default::default()
					});
				}

				Ok(Piece {
					piece_type,
					base_mesh,
					color_meshes
				})
			},
			(Design::CustomSuperDodecahedron, Type::Triangle) => {
				build_pyramid_piece()
			}
		}
	}

	fn add_children(&self, entity_mut: &mut EntityMut, piece_mats: &PieceMats, local_index: usize) -> () {
		entity_mut.with_children(|world_child_builder: &mut WorldChildBuilder| -> () {
			// Spawn the bundle for the base mesh
			world_child_builder
				.spawn_bundle(PbrBundle {
					mesh: self.base_mesh.bevy_mesh.as_ref().unwrap().clone(),
					material: piece_mats.base_mat.mat.clone(),
					.. PbrBundle::default()
				});

			for color_mesh in &self.color_meshes {
				world_child_builder
					.spawn_bundle(PbrBundle {
						mesh: color_mesh.bevy_mesh.as_ref().unwrap().clone(),
						material: piece_mats.color_mats[
							color_mesh
								.face_indices
								.as_ref()
								.unwrap()
								[local_index]
						].mat.clone(),
						.. PbrBundle::default()
					});
			}
		});
	}

	pub fn add_entities(
		&self,
		world: &mut World,
		piece_mats: &PieceMats,
		icosidodecahedron_faces: &Vec<FaceData>
	) -> () {
		let piece_type: Type = self.piece_type;
		let index_offset: usize = piece_type.index_offset();

		warn_expect_ok!(self.check_meshes(), return);

		for index in piece_type.range() {
			self.add_children(
				world.spawn()
					.insert_bundle((
						PieceComponent {
							index,
							piece_type
						},
						Transform::from_matrix(Mat4::from_quat(icosidodecahedron_faces[index].quat)),
						GlobalTransform::identity()
					)),
				piece_mats,
				index - index_offset
			);
		}
	}

	fn check_meshes(&self) -> LogErrorResult {
		if !self.base_mesh.has_bevy_mesh() {
			return Err(log_error!(
				Level::Error,
				format!("Piece of Type {:?} doesn't have a BevyMesh for its base_mesh", self.piece_type)
			));
		}

		for (color_mesh_index, color_mesh) in self.color_meshes.iter().enumerate() {
			if !color_mesh.has_bevy_mesh() {
				return Err(log_error!(
					Level::Error,
					format!(
						"Piece of Type {:?} doesn't have a BevyMesh for its color_meshes[{}]",
						self.piece_type,
						color_mesh_index
					)
				));
			}

			if !color_mesh.has_face_indices() {
				return Err(log_error!(
					Level::Error,
					format!(
						"Piece of Type {:?} doesn't have face indices for its color_meshes[{}]",
						self.piece_type,
						color_mesh_index
					)
				));
			}
		}

		Ok(())
	}
}

pub struct PiecePair {
	pentagon: Piece,
	triangle: Piece
}

impl PiecePair {
	fn add_bevy_meshes(&mut self, bevy_meshes: &mut ResMut<Assets<BevyMesh>>) -> () {
		self.pentagon.add_bevy_meshes(bevy_meshes);
		self.triangle.add_bevy_meshes(bevy_meshes);
	}

	pub fn add_entities(
		&self,
		world: &mut World,
		piece_mats: &PieceMats,
		icosidodecahedron_faces: &Vec<FaceData>
	) -> () {
		self.pentagon.add_entities(world, piece_mats, icosidodecahedron_faces);
		self.triangle.add_entities(world, piece_mats, icosidodecahedron_faces);
	}

	pub fn update_entities(&self, world: &mut World, piece_mats: &PieceMats) -> () {
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

		if warn_expect!(entity_count == usize::PIECE_COUNT as u32) {
			let mut update_entities_for_type = |piece: &Piece, piece_type: Type| -> () {
				let index_offset: usize = piece_type.index_offset();

				for index in piece_type.range() {
					let mut entity_mut: EntityMut = world.entity_mut(entities[index]);

					entity_mut.despawn_descendants();
					piece.add_children(&mut entity_mut, piece_mats, index - index_offset);
				}
			};

			update_entities_for_type(&self.pentagon, Type::Pentagon);
			update_entities_for_type(&self.triangle, Type::Triangle);
		}
	}
}

impl TryFrom<Design> for PiecePair {
	type Error = LogError;

	fn try_from(design: Design) -> Result<Self, Self::Error> {
		Ok(PiecePair {
			pentagon: Piece::from_design_and_type(design, Type::Pentagon)?,
			triangle: Piece::from_design_and_type(design, Type::Triangle)?
		})
	}
}

pub struct PieceLibrary(pub Vec<PiecePair>);

impl PieceLibrary {
	fn add_bevy_meshes(&mut self, bevy_meshes: &mut ResMut<Assets<BevyMesh>>) -> () {
		for piece_pair in &mut self.0 {
			piece_pair.add_bevy_meshes(bevy_meshes);
		}
	}

	fn try_default() -> Result<Self, Box<dyn std::error::Error>> {
		let mut piece_library: Self = Self(Vec::<PiecePair>::with_capacity(Design::COUNT));

		for design in Design::iter() {
			piece_library.0.push(PiecePair::try_from(design)?);
		}

		Ok(piece_library)
	}
}

pub struct PiecePlugin;

impl PiecePlugin {
	pub fn startup_app(
		mut piece_library: ResMut<PieceLibrary>,
		mut bevy_meshes: ResMut<Assets<BevyMesh>>
	) -> () {
		piece_library.add_bevy_meshes(&mut bevy_meshes);
	}
}

impl Plugin for PiecePlugin {
	fn build(&self, app: &mut App) -> () {
		app
			.insert_resource(warn_expect_ok!(
				PieceLibrary::try_default(),
				return
			))
			.add_startup_system(Self::startup_app);
	}
}