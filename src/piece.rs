use {
	crate::{
		prelude::*,
		get_data,
		colors::MatHdl,
		math::{
			*,
			polyhedra::{
				data::{
					Data,
					FaceData,
					VertexData
				},
				Polyhedron,
				properties::ICOSIDODECAHEDRON
			}
		},
		strings::STRING_DATA
	},
	std::{
		collections::HashMap,
		ops::Range
	},
	bevy::{
		prelude::*,
		render::{
			mesh::{
				Indices,
				Mesh as BevyMesh
			},
			pipeline::PrimitiveTopology
		}
	},
	serde::Deserialize
};

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq)]
pub enum Design {
	Dodecahedron1
}

impl Design {
	pub fn as_polyhedron(self) -> Polyhedron {
		match self {
			Design::Dodecahedron1 => Polyhedron::Dodecahedron
		}
	}
}

#[derive(Clone, Copy, Debug, Deserialize)]
pub enum Type {
	Pentagon,
	Triangle
}

impl Type {
	pub fn side_count(self) -> usize {
		ICOSIDODECAHEDRON.face_sizes[self as usize].face_size
	}

	pub fn index_offset(self) -> usize {
		ICOSIDODECAHEDRON.face_sizes[self as usize].initial_face_index
	}

	pub fn instance_count(self) -> usize {
		ICOSIDODECAHEDRON.face_sizes[self as usize + 1].initial_face_index - self.index_offset()
	}

	pub fn next_side_index(&self, index: usize) -> usize {
		return (index + 1) % self.side_count();
	}

	pub fn prev_side_index(&self, index: usize) -> usize {
		let side_count: usize = self.side_count();

		return (index + side_count - 1) % side_count;
	}
}

#[derive(Clone, Copy, Default)]
struct Tri {
	vertices: [Vec3; 3],
	normal: Vec3
}

impl Tri {
	pub fn update_normal(&mut self) -> () {
		self.normal = (self[1] - self[0]).cross(self[2] - self[1]).normalize();
	}

	pub fn offset_along_normal(&mut self, distance: f32) -> () {
		let offset: Vec3 = self.normal * distance;

		for vertex in self.vertices.iter_mut() {
			*vertex += offset;
		}
	}

	pub fn offset_all_along_plane(&mut self, distance: f32) -> () {
		self.offset_along_plane(distance, [true; 3]);
	}

	pub fn offset_along_plane(&mut self, distance: f32, mask: [bool; 3]) -> () {
		if mask.iter().all(|should_offset| !should_offset) || (distance.abs() <= f32::EPSILON) {
			return;
		}

		let mat: Mat4 = self.to_mat();
		let transformed: Tri = &mat * *self;
		let mut tri1: Tri = transformed;
		let mut tri2: Tri = transformed;

		for (curr_vert_index, _) in mask
			.iter()
			.filter(|should_offset| **should_offset)
			.enumerate()
		{
			let next_vert_index: usize = (curr_vert_index + 1) % 3;
			let edge_offset: Vec3 = distance * (transformed[next_vert_index] - transformed[curr_vert_index]).cross(Vec3::Z).normalize();

			tri1[curr_vert_index] += edge_offset;
			tri2[next_vert_index] += edge_offset;
		}

		let mut result: Tri = transformed;

		for curr_vert_index in 0_usize .. 3 {
			let prev_vert_index: usize = (curr_vert_index + 2) % 3;
			let next_vert_index: usize = (curr_vert_index + 1) % 3;

			match Option::<Vec2>::from(
				two_d::compute_line_intersection(
				tri1[curr_vert_index].xy(),
				tri2[next_vert_index].xy(),
				tri1[prev_vert_index].xy(),
				tri2[curr_vert_index].xy(),
				None
				)
			) {
				Some(intersection) => {
					result[curr_vert_index] = Vec3::from((intersection, 0.0_f32));
				},
				_ => {}
			}
		}

		*self = &mat.inverse() * result;
		self.update_normal();
	}

	pub fn to_mat(&self) -> Mat4 {
		return Mat4::look_at_rh(
			self[0],
			self[0] - self.normal,
			self.normal.cross(self[1] - self[0]).normalize()
		);
	}
}

impl From<[Vec3; 3]> for Tri {
	fn from(vertices: [Vec3; 3]) -> Self {
		return Tri::from(&vertices[..]);
	}
}

impl From<&[Vec3]> for Tri {
	fn from(vertex_slice: &[Vec3]) -> Self {
		let mut tri: Tri = Tri::default();

		tri[..].copy_from_slice(vertex_slice);
		tri.update_normal();

		return tri;
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

		bevy_mesh.set_attribute(BevyMesh::ATTRIBUTE_POSITION, positions);
		bevy_mesh.set_attribute(BevyMesh::ATTRIBUTE_NORMAL, normals);
		bevy_mesh.set_attribute(BevyMesh::ATTRIBUTE_UV_0, uvs);
		bevy_mesh.set_indices(Some(Indices::U32(indices)));

		bevy_mesh
	}
}

pub struct PieceComponent {
	_index:			usize,
	_piece_type:	Type
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
		const PLANAR_OFFSET_RATIO: f32 = 1.0_f32 / 64.0_f32;
		const NORMAL_OFFSET_RATIO: f32 = 1.0_f32 / 256.0_f32;
		const PLANAR_OFFSET: f32 = PLANAR_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;
		const NORMAL_OFFSET: f32 = NORMAL_OFFSET_RATIO * ICOSIDODECAHEDRON.edge_length;

		let icosahedron_data:				&Data				= get_data!(Icosahedron);
		let dodecahedron_data:				&Data				= get_data!(Dodecahedron);
		let icosidodecahedron_data:			&Data				= get_data!(Icosidodecahedron);
		let _rhombic_triacontahedron_data:	&Data				= get_data!(RhombicTriacontahedron);
		let icosidodecahedron_verts:		&Vec<VertexData>	= &icosidodecahedron_data.verts;
		let icosidodecahedron_vert_indices:	&Vec<usize>			= &icosidodecahedron_data.vert_indices;
		let side_count:						usize				= piece_type.side_count();
		let double_side_count:				usize				= 2_usize * side_count;
		let range:							Range<usize>		= 0_usize .. side_count;
		let transformation:					Mat4				= Mat4::from_quat(icosidodecahedron_data.faces[piece_type.index_offset()].quat);

		let mut vertices: Vec<Vec3>;
		let center: Vec3;

		match design {
			Design::Dodecahedron1 => {
				macro_rules! build_base_mesh {
					() => {
						{
							let mut tris: Vec<Tri> = Vec::with_capacity(double_side_count);
	
							for side_index in range.clone() {
								let vert_1: Vec3 = vertices[side_index];
								let vert_2: Vec3 = vertices[piece_type.next_side_index(side_index)];
	
								tris.push(&transformation * Tri::from([
									Vec3::ZERO,
									vert_2,
									vert_1
								]));
								tris.push(&transformation * Tri::from([
									center,
									vert_1,
									vert_2
								]));
							}
	
							Mesh {
								tris,
								.. Default::default()
							}
						}
					};
				}

				match piece_type {
					Type::Pentagon => {
						vertices = Vec::with_capacity(3 * side_count);

						let mut center_sum: Vec3 = Vec3::ZERO;

						for vert_indices_index in icosidodecahedron_data.faces[piece_type.index_offset()].range.clone() {
							let vert: Vec3 = icosidodecahedron_verts[icosidodecahedron_vert_indices[vert_indices_index]].vec;

							center_sum += vert;
							vertices.push(vert);
						}

						for vert_index in range.clone() {
							vertices.push((vertices[vert_index] + vertices[piece_type.next_side_index(vert_index)]) * 0.5_f32);
						}

						for vert_index in range.clone() {
							vertices.push((ONE_OVER_PHI as f32) * vertices[vert_index] + (ONE_OVER_PHI_SQUARED as f32) * vertices[(vert_index + 2) % side_count]);
						}

						center = center_sum / side_count as f32;

						let base_mesh: Mesh = build_base_mesh!();

						let mut color_meshes: Vec<Mesh> = Vec::with_capacity(side_count);

						// Add the color mesh for the primary piece
						{
							let mut tris: Vec<Tri> = Vec::with_capacity(side_count);

							for vert_index in range.clone() {
								let mut tri: Tri = Tri::from([
									vertices[vert_index],
									vertices[vert_index + double_side_count],
									vertices[piece_type.prev_side_index(vert_index) + double_side_count]
								]);

								tri.offset_all_along_plane(-PLANAR_OFFSET);
								tri.offset_along_normal(NORMAL_OFFSET);
								tris.push(&transformation * tri);
							}

							color_meshes.push(Mesh {
								tris,
								face_indices: Some((0_usize .. piece_type.instance_count()).collect()),
								.. Default::default()
							});
						}

						for vert_index in range.clone() {
							let prev_vert_index: usize = piece_type.prev_side_index(vert_index);
							let mut tris: Vec<Tri> = Vec::with_capacity(3);

							for tri_index in 0_usize .. 3 {
								let mut tri: Tri = match tri_index {
									0 => Tri::from([
										vertices[vert_index],
										vertices[vert_index + side_count],
										vertices[vert_index + double_side_count]
									]),
									1 => Tri::from([
										center,
										vertices[prev_vert_index + double_side_count],
										vertices[vert_index + double_side_count]
									]),
									_ => Tri::from([
										vertices[vert_index],
										vertices[prev_vert_index + double_side_count],
										vertices[prev_vert_index + side_count]
									])
								};

								tri.offset_all_along_plane(-PLANAR_OFFSET);
								tri.offset_along_normal(NORMAL_OFFSET);
								tris.push(&transformation * tri);
							}

							color_meshes.push(Mesh {
								tris,
								face_indices: Some(
									(0_usize .. piece_type.instance_count()).map(|face_index: usize| -> usize {
										let face_vec: Vec3 = icosahedron_data.verts[face_index].vec;

										icosahedron_data.get_closest_vert_index(
											&(face_vec + 2.0_f32 * (
												icosidodecahedron_data.verts[
													icosidodecahedron_data.vert_indices[
														icosidodecahedron_data.faces[
															face_index
														].range.start + vert_index
													]
												].vec - face_vec
											))
										)
									})
									.collect()
								),
								.. Default::default()
							});
						}

						Ok(Piece {
							piece_type,
							base_mesh,
							color_meshes
						})
					},
					Type::Triangle => {
						vertices = Vec::with_capacity(3);

						for vert_indices_index in icosidodecahedron_data.faces[piece_type.index_offset()].range.clone() {
							vertices.push(icosidodecahedron_verts[icosidodecahedron_vert_indices[vert_indices_index]].vec);
						}

						center = dodecahedron_data.verts[0].vec;

						let base_mesh: Mesh = build_base_mesh!();

						let mut color_meshes: Vec<Mesh> = Vec::with_capacity(side_count);

						for side_index in range.clone() {
							let mut tri: Tri = Tri::from([
								center,
								vertices[side_index],
								vertices[piece_type.next_side_index(side_index)]
							]);

							tri.offset_all_along_plane(-PLANAR_OFFSET);
							tri.offset_along_normal(NORMAL_OFFSET);
							color_meshes.push(Mesh {
								tris: vec![&transformation * tri],
								face_indices: Some(
									(0_usize .. piece_type.instance_count()).map(|face_index: usize| -> usize {
										let mut verts: Vec<Vec3> = icosidodecahedron_data
											.faces[
												face_index + piece_type.index_offset()
											]
											.get_slice(icosidodecahedron_vert_indices)
											.iter()
											.map(|vert_index| -> Vec3 {
												icosidodecahedron_verts[*vert_index].vec
											})
											.collect();

										verts.rotate_left(side_index);

										icosahedron_data.get_closest_vert_index(&(verts[0] + verts[1] - verts[2]))
									})
									.collect()
								),
								.. Default::default()
							});
						}

						Ok(Piece {
							piece_type,
							base_mesh,
							color_meshes
						})
					}
				}
			}
		}
	}

	pub fn add_entities(
		&self,
		commands: &mut Commands,
		(base_mat, color_mats, faces): &(&MatHdl, &Vec<MatHdl>, &Vec<FaceData>)
	) -> () {
		let piece_type: Type = self.piece_type;
		let index_offset: usize = piece_type.index_offset();

		log_result_err!(self.check_meshes());

		for index in index_offset .. index_offset + piece_type.instance_count() {
			commands
				.spawn_bundle((
					PieceComponent {
						_index: index,
						_piece_type: piece_type
					},
					// Transform::identity(),
					Transform::from_matrix(
						Mat4::from_quat(faces[index].quat)
						.inverse()
					),
					GlobalTransform::identity()
				))
				.with_children(
					|child_builder: &mut ChildBuilder| -> () {
						// Spawn the bundle for the base mesh
						child_builder
							.spawn_bundle(PbrBundle {
								mesh: self.base_mesh.bevy_mesh.as_ref().unwrap().clone(),
								material: (*base_mat).clone(),
								.. Default::default()
							});

						for color_mesh in &self.color_meshes {
							child_builder
								.spawn_bundle(PbrBundle {
									mesh: color_mesh.bevy_mesh.as_ref().unwrap().clone(),
									material: color_mats[color_mesh.face_indices.as_ref().unwrap()[index - index_offset]].clone(),
									.. Default::default()
								});
						}
					}
				);
		}
	}

	fn check_meshes(&self) -> LogErrorResult {
		if !self.base_mesh.has_bevy_mesh() {
			return Err(log_error!(Level::Error, format!("Piece of Type {:?} doesn't have a BevyMesh for its base_mesh", self.piece_type)));
		}

		for (color_mesh_index, color_mesh) in self.color_meshes.iter().enumerate() {
			if !color_mesh.has_bevy_mesh() {
				return Err(log_error!(Level::Error, format!("Piece of Type {:?} doesn't have a BevyMesh for its color_meshes[{}]", self.piece_type, color_mesh_index)));
			}

			if !color_mesh.has_face_indices() {
				return Err(log_error!(Level::Error, format!("Piece of Type {:?} doesn't have face indices for its color_meshes[{}]", self.piece_type, color_mesh_index)));
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
		commands: &mut Commands,
		param_bundle: &(&MatHdl, &Vec<MatHdl>, &Vec<FaceData>)
	) -> () {
		self.pentagon.add_entities(commands, param_bundle);
		self.triangle.add_entities(commands, param_bundle);
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

#[derive(Deserialize)]
pub struct PieceLibraryData {
	pub default_design: Design,
	designs: Vec<Design>
}

impl TryFrom<&str> for PieceLibraryData {
	type Error = Box<dyn std::error::Error>;

	fn try_from(file_name: &str) -> Result<Self, Self::Error> {
		from_ron(file_name)
	}
}

pub struct PieceLibrary {
	pub data: PieceLibraryData,
	pub pieces: HashMap<Design, PiecePair>
}

impl PieceLibrary {
	fn add_bevy_meshes(&mut self, bevy_meshes: &mut ResMut<Assets<BevyMesh>>) -> () {
		for (_design, piece_pair) in &mut self.pieces {
			piece_pair.add_bevy_meshes(bevy_meshes);
		}
	}
}

impl TryFrom<&str> for PieceLibrary {
	type Error = Box<dyn std::error::Error>;

	fn try_from(file_name: &str) -> Result<Self, Self::Error> {
		let data: PieceLibraryData = PieceLibraryData::try_from(file_name)?;

		if !data.designs.contains(&data.default_design) {
			return Err(Box::new(log_error!(Level::Warn, format!("PieceLibraryData designs didn't contain {:?}", data.default_design))));
		}

		let mut pieces: HashMap<Design, PiecePair> = HashMap::<Design, PiecePair>::with_capacity(data.designs.len());

		for design in &data.designs {
			if pieces.contains_key(design) {
				return Err(Box::new(log_error!(Level::Warn, format!("PieceLibrary pieces already contain {:?}", design))));
			}

			pieces.insert(
				*design, 
				match PiecePair::try_from(*design) {
					Ok(piece_pair) => piece_pair,
					Err(log_error) => {
						return Err(Box::new(log_error));
					}
				}
			);
		}

		Ok(PieceLibrary {
			data,
			pieces
		})
	}
}

pub struct PiecePlugin;

impl PiecePlugin {
	fn startup_app(
		mut piece_library: ResMut<PieceLibrary>,
		mut bevy_meshes: ResMut<Assets<BevyMesh>>
	) -> () {
		piece_library.add_bevy_meshes(&mut bevy_meshes);
	}
}

impl Plugin for PiecePlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app
			.insert_resource(log_result_err!(PieceLibrary::try_from(STRING_DATA.files.piece_library_data.as_ref())))
			.add_startup_system(Self::startup_app.system().label(STRING_DATA.labels.piece_library.as_ref()));
	}
}