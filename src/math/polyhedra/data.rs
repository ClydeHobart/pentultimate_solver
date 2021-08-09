use {
	crate::{
			math::{
			*,
			polyhedra::{
				Polyhedron,
				properties::Properties
			}
		},
		prelude::*
	},
	bevy::render::{
		mesh::{
			Indices,
			Mesh
		},
		pipeline::PrimitiveTopology
	},
	log::Level
};

#[derive(Debug, Default)]
pub struct VertexData {
	pub vec:	Vec3,
	pub norm:	Vec3
}

impl From<Vec3> for VertexData {
	fn from(vec: Vec3) -> Self {
		Self {
			vec,
			norm: vec.normalize_or_zero()
		}
	}
}

#[derive(Clone, Copy, Eq, Debug, Default, Hash, PartialEq)]
pub struct EdgeData(usize, usize);

impl EdgeData {
	pub fn new(vert_index_1: usize, vert_index_2: usize) -> Self {
		EdgeData(std::cmp::min(vert_index_1, vert_index_2), std::cmp::max(vert_index_1, vert_index_2))
	}

	pub fn contains_vert(&self, vert_index: usize) -> bool {
		self.0 == vert_index || self.1 == vert_index
	}
}

type Range = std::ops::Range<usize>;

#[derive(Debug, Default)]
pub struct FaceData {
	pub range:	Range,
	pub norm:	Vec3,
	pub quat:	Quat
}

impl FaceData {
	pub fn new(verts: &Vec<VertexData>, vert_indices: &Vec<usize>, start: usize, end: usize) -> Self {
		let range: Range = start .. end;
		let norm: Vec3 = vert_indices[range.clone()]
			.iter()
			.map(|vert_index: &usize| -> &Vec3 {
				&verts[*vert_index].vec
			})
			.sum::<Vec3>()
			.normalize_or_zero();

		FaceData {
			range,
			norm,
			quat: Quat::IDENTITY
		}
	}

	pub fn get_range(&self) -> Range { self.range.start .. self.range.end }

	pub fn get_slice<'a>(&self, vert_indices: &'a Vec<usize>) -> &'a [usize] { &vert_indices[self.get_range()] }

	pub fn get_slice_mut<'a>(&self, vert_indices: &'a mut Vec<usize>) -> &'a mut [usize] { &mut vert_indices[self.get_range()] }

	pub fn contains_vert(&self, vert_indices: &Vec<usize>, vert_index: usize) -> bool {
		self.get_slice(vert_indices).iter().any(|index: &usize| *index == vert_index)
	}

	pub fn get_size(&self) -> usize { self.range.end - self.range.start }
}

#[derive(Debug)]
pub struct Data {
	pub polyhedron:		Polyhedron,
	pub verts:			Vec<VertexData>,
	pub edges:			Vec<EdgeData>,
	pub vert_indices:	Vec<usize>,
	pub faces:			Vec<FaceData>
}

pub type DataRefOption = Option<&'static Data>;

impl Data {
	pub fn get(polyhedron: Polyhedron) -> DataRefOption {
		let option: &'static Option<Data> = match polyhedron {
				Polyhedron::Icosahedron				=> &ICOSAHEDRON,
				Polyhedron::Dodecahedron			=> &DODECAHEDRON,
				Polyhedron::Icosidodecahedron		=> &ICOSIDODECAHEDRON,
				Polyhedron::RhombicTriacontahedron	=> &RHOMBIC_TRIACONTAHEDRON,
				Polyhedron::Invalid					=> return None,
			};

		if option.is_none() || option.as_ref().unwrap().polyhedron == Polyhedron::Invalid {
			None
		} else {
			option.as_ref()
		}
	}

	pub fn get_closest_vert_index(&self, vec: &Vec3) -> usize {
		Data::get_closest_vert_index_for_verts(&self.verts, vec)
	}

	pub fn get_closest_vert_index_for_verts(verts: &Vec<VertexData>, vec: &Vec3) -> usize {
		use std::cmp::Ordering;

		verts
			.iter()
			.map(|vert: &VertexData| -> f32 {
				vert.norm.dot(*vec)
			})
			.enumerate()
			.max_by(|(_vert_index_a, norm_dot_vec_a): &(usize, f32), (_vert_index_b, norm_dot_vec_b): &(usize, f32)| -> Ordering {
				norm_dot_vec_a.partial_cmp(&norm_dot_vec_b).unwrap_or(Ordering::Equal)
			})
			.map(|(vert_index, _norm_dot_vec): (usize, f32)| -> usize {
				vert_index
			})
			.unwrap()
	}

	pub fn get_closest_face_index(&self, quat: &Quat) -> usize {
		Data::get_closest_face_index_for_faces(&self.faces, quat)
	}

	pub fn get_closest_face_index_for_faces(faces: &Vec<FaceData>, quat: &Quat) -> usize {
		use std::cmp::Ordering;

		faces
			.iter()
			.map(|face: &FaceData| -> f32 {
				face.quat.dot(*quat)
			})
			.enumerate()
			.max_by(|(_face_index_a, quat_dot_vec_a): &(usize, f32), (_face_index_b, quat_dot_vec_b): &(usize, f32)| -> Ordering {
				quat_dot_vec_a.partial_cmp(&quat_dot_vec_b).unwrap_or(Ordering::Equal)
			})
			.map(|(face_index, _quat_dot_vec): (usize, f32)| -> usize {
				face_index
			})
			.unwrap()
	}

	pub fn validate_polyhedra() -> LogErrorResult {
		let log_target: String = log_path!("validate_polyhedra").to_string();

		let validate_polyhedron = |polyhedron: Polyhedron| -> LogErrorResult {
			if Data::get(polyhedron).is_some() {
				Ok(())
			} else {
				Err(log_error!(
					target: log_target,
					Level::Warn, format!("Failed to validate polyhedron {:?}", polyhedron)
				))
			}
		};

		validate_polyhedron(Polyhedron::Icosahedron)?;
		validate_polyhedron(Polyhedron::Dodecahedron)?;
		validate_polyhedron(Polyhedron::Icosidodecahedron)?;
		validate_polyhedron(Polyhedron::RhombicTriacontahedron)?;

		let validate_dual_polyhedra = |polyhedron_a: Polyhedron| -> LogErrorResult {
			let polyhedron_b: Polyhedron = polyhedron_a.dual();

			if polyhedron_b.is_invalid() {
				return Err(log_error!(
					target: log_target,
					Level::Warn, format!("Polyhedron {:?}'s dual is Invalid", polyhedron_a)
				));
			}

			let properties_a: &Properties = Properties::get(polyhedron_a).unwrap();
			let properties_b: &Properties = Properties::get(polyhedron_b).unwrap();
			let data_a: &Data = Data::get(polyhedron_a).unwrap();
			let data_b: &Data = Data::get(polyhedron_b).unwrap();

			if properties_a.vert_count != properties_b.face_count {
				return Err(log_error!(
					target: log_target,
					Level::Warn, format!("Polyhedron {:?}'s vertex count doesn't equal polyhedron {:?}'s face count", polyhedron_a, polyhedron_b)
				));
			}

			for index in 0 .. properties_a.vert_count {
				if !data_a.verts[index].norm.abs_diff_eq(data_b.faces[index].norm, f32::EPSILON) {
					return Err(log_error!(
						target: log_target,
						Level::Warn, format!("Vert {}'s normal vector ({:?}) of polyhedron {:?} isn't close enough to face {}'s normal vector ({:?}) of polyhedron {:?}",
							index, data_a.verts[index].norm, polyhedron_a,
							index, data_b.faces[index].norm, polyhedron_b
						)
					))
				}
			}

			Ok(())
		};

		validate_dual_polyhedra(Polyhedron::Icosahedron)?;
		validate_dual_polyhedra(Polyhedron::Dodecahedron)?;
		validate_dual_polyhedra(Polyhedron::Icosidodecahedron)?;
		validate_dual_polyhedra(Polyhedron::RhombicTriacontahedron)?;

		Ok(())
	}

	pub fn as_mesh(&self, primitive_topology: PrimitiveTopology) -> Option<Mesh> {
		if primitive_topology == PrimitiveTopology::LineStrip || primitive_topology == PrimitiveTopology::TriangleStrip {
			return None;
		}

		let mut mesh = Mesh::new(primitive_topology);
		let mut positions: Vec<[f32; 3]> = Vec::<[f32; 3]>::new();
		let mut normals: Vec<[f32; 3]> = Vec::<[f32; 3]>::new();
		let mut uvs: Vec<[f32; 2]> = Vec::<[f32; 2]>::new();
		let mut append_vert = |vert_index: usize| -> () {
			let vert_data: &VertexData = &self.verts[vert_index];
			positions.push(vert_data.vec.as_ref().clone());
			normals.push(vert_data.norm.as_ref().clone());
			uvs.push([0.0, 0.0]);
		};
		let mut append_all_verts = || -> () {
			for vert_index in 0 .. self.verts.len() {
				append_vert(vert_index);
			}
		};

		match primitive_topology {
			PrimitiveTopology::PointList => {
				append_all_verts();
			},
			PrimitiveTopology::LineList => {
				self.edges.iter().for_each(|edge_data: &EdgeData| -> () {
					append_vert(edge_data.0);
					append_vert(edge_data.1);
				});
			},
			PrimitiveTopology::TriangleList => {
				let vert_indices: &Vec<usize> = &self.vert_indices;
				let mut indices: Vec<u32> = Vec::<u32>::new();

				for face_data in self.faces.iter()
				{
					let initial_index: u32 = positions.len() as u32;
					let range: &Range = &face_data.range;

					for vert_index in vert_indices[range.clone()].iter() {
						positions.push(self.verts[*vert_index].vec.as_ref().clone());
						normals.push(face_data.norm.as_ref().clone());
						uvs.push([0.0, 0.0]);
					}

					for slice_index in 1 .. (face_data.get_size() - 1) as u32 {
						indices.push(initial_index);
						indices.push(initial_index + slice_index);
						indices.push(initial_index + slice_index + 1);
					}
				}

				mesh.set_indices(Some(Indices::U32(indices)));
			},
			_ => { return None; }
		}

		mesh.set_attribute(Mesh::ATTRIBUTE_POSITION, positions);
		mesh.set_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
		mesh.set_attribute(Mesh::ATTRIBUTE_UV_0, uvs);

		Some(mesh)
	}

	fn new_option(polyhedron: Polyhedron) -> Option<Data> {
		let mut data: Data = Data::default();

		data.polyhedron = polyhedron;

		match data.generate() {
			Ok(_) => {
				Some(data)
			},
			Err(log_error) => {
				log_error.log();
				data.reset();

				None
			}
		}
	}

	fn default() -> Self {
		Self {
			polyhedron:		Polyhedron::Invalid,
			verts:			Vec::<VertexData>::new(),
			edges:			Vec::<EdgeData>::new(),
			vert_indices:	Vec::<usize>::new(),
			faces:			Vec::<FaceData>::new()
		}
	}

	fn generate(&mut self) -> LogErrorResult {
		fn try_generate_elements<'a, T: std::fmt::Debug>(
			func:				fn(&mut Data) -> LogErrorResult,
			data_struct:		&'a mut Data,
			elements:			&str,
			get_relevant_data:	fn(&'a Data) -> T
		) -> LogErrorResult {
			if let Err(log_error) = func(data_struct) {
				log_error.log();

				Err(log_error!(
					target: log_path!("generate"),
					Level::Warn, format!("Failed to generate {} for polyhedron {:?}", elements, data_struct.polyhedron),
					Level::Info, format!("Relevant data: {:#?}", get_relevant_data(data_struct))
				))
			} else {
				Ok(())
			}
		}

		try_generate_elements(
			Data::generate_verts,
			self,
			"vertices",
			|data: &Data| -> (&Vec<VertexData>, ) { (&data.verts, ) }
		)?;
		try_generate_elements(
			Data::generate_edges,
			self,
			"edges",
			|data: &Data| -> (&Vec<VertexData>, &Vec<EdgeData>) { (&data.verts, &data.edges) }
		)?;
		try_generate_elements(
			Data::generate_faces,
			self,
			"faces",
			|data: &Data| -> (&Vec<EdgeData>, &Vec<usize>, &Vec<FaceData>) { (&data.edges, &data.vert_indices, &data.faces) }
		)?;

		Ok(())
	}

	fn generate_verts(&mut self) -> LogErrorResult {
		let properties: &Properties = Properties::get(self.polyhedron).unwrap();
		let verts: &mut Vec<VertexData> = &mut self.verts;

		verts.clear();
		verts.reserve_exact(properties.vert_count);
		Data::generate_verts_for_properties(properties, verts);
		trace!(target: log_path!("generate_verts"), "{:#?}", verts);

		if verts.len() != properties.vert_count {
			return Err(log_error!(
				target: log_path!("generate_verts"),
				Level::Warn, format!("Found vertex count ({}) did not match expected vertex count ({})", verts.len(), properties.vert_count)
			));
		}

		Ok(())
	}

	fn generate_verts_for_properties(properties: &Properties, verts: &mut Vec<VertexData>) -> () {
		match properties.polyhedron {
			Polyhedron::Icosahedron => {
				let base_vector: Vec3 = properties.base_vectors[0];
				let mut perm_mat: Mat3 = Mat3::IDENTITY;

				for _perm in 0 .. 3 {
					for vert in 0 .. 4 {
						verts.push(
							VertexData::from(
								perm_mat *
								Mat3::from(
									ReflectionMat3::new(
										vert & 0b10 != 0,
										vert & 0b01 != 0,
										false
									)
								) *
								base_vector
							)
						);
					}

					perm_mat = PERMUTE_AXES * perm_mat;
				}
			},
			Polyhedron::Dodecahedron => {
				let base_vector: Vec3 = properties.base_vectors[0];
				let mut perm_mat: Mat3 = Mat3::IDENTITY;

				for _perm in 0 .. 3 {
					for vert in 0 .. 4 {
						verts.push(
							VertexData::from(
								perm_mat *
								Mat3::from(
									ReflectionMat3::new(
										vert & 0b10 != 0,
										false,
										vert & 0b01 != 0
									)
								) *
								base_vector
							)
						);
					}

					perm_mat = PERMUTE_AXES * perm_mat;
				}

				let base_vector: Vec3 = properties.base_vectors[1];

				for vert in 0 .. 8 {
					verts.push(
						VertexData::from(
							Mat3::from(
								ReflectionMat3::new(
									vert & 0b100 != 0,
									vert & 0b010 != 0,
									vert & 0b001 != 0
								)
							) *
							base_vector
						)
					);
				}
			},
			Polyhedron::Icosidodecahedron => {
				let base_vector_1: Vec3 = properties.base_vectors[0];
				let base_vector_2: Vec3 = properties.base_vectors[1];
				let mut perm_mat: Mat3 = Mat3::IDENTITY;

				for _perm in 0 .. 3 {
					for vert in 0 .. 8 {
						if vert & 0b11 == 0 {
							verts.push(
								VertexData::from(
									perm_mat *
									Mat3::from(
										ReflectionMat3::new(
											vert & 0b100 != 0,
											false,
											false
										)
									) *
									base_vector_1
								)
							);
						}

						verts.push(
							VertexData::from(
								perm_mat *
								Mat3::from(
									ReflectionMat3::new(
										vert & 0b100 != 0,
										vert & 0b010 != 0,
										vert & 0b001 != 0
									)
								) *
								base_vector_2
							)
						);
					}

					perm_mat = PERMUTE_AXES * perm_mat;
				}
			},
			Polyhedron::RhombicTriacontahedron => {
				Data::generate_verts_for_properties(&polyhedra::properties::ICOSAHEDRON, verts);
				Data::generate_verts_for_properties(&polyhedra::properties::DODECAHEDRON, verts);
			},
			_ => {}
		}

		trace!(target: log_path!("generate_verts"), "{:?} verts: {:#?}", properties.polyhedron, verts);
	}

	fn generate_edges(&mut self) -> LogErrorResult {
		let properties: &Properties =  Properties::get(self.polyhedron).unwrap();
		let verts: &Vec<VertexData> = &self.verts;
		let edges: &mut Vec<EdgeData> = &mut self.edges;

		edges.clear();
		edges.reserve_exact(properties.edge_count);

		let distance_squared_threshold: f32 = properties.edge_length * properties.edge_length + 0.000001;

		for vert_index_1 in 0 .. properties.vert_count - 1 {
			let vert_1: &Vec3 = &verts[vert_index_1].vec;

			for vert_index_2 in vert_index_1 + 1 .. properties.vert_count {
				if vert_1.distance_squared(verts[vert_index_2].vec) <= distance_squared_threshold {
					edges.push(EdgeData(vert_index_1, vert_index_2));
				}
			}
		}

		trace!(target: log_path!("generate_edges"), "{:?} edges: {:#?}", properties.polyhedron, edges);

		if edges.len() != properties.edge_count {
			return Err(log_error!(
				target: log_path!("generate_edges"),
				Level::Warn, format!("Found edge count ({}) did not match expected edge count ({})", edges.len(), properties.edge_count)
			));
		}

		Ok(())
	}

	fn generate_faces(&mut self) -> LogErrorResult {
		let properties:			&Properties							= Properties::get(self.polyhedron).unwrap();
		let verts:				&Vec<VertexData>					= &self.verts;
		let edges:				&Vec<EdgeData>						= &self.edges;
		let vert_indices:		&mut Vec<usize>						= &mut self.vert_indices;
		let faces:				&mut Vec<FaceData>					= &mut self.faces;
		let all_edges_used:		u64									= (1 << properties.edge_count) - 1;
		let log_target:			String								= log_path!("generate_faces").into();

		let mut edge_to_index:	fnv::FnvHashMap<EdgeData, usize>	= fnv::FnvHashMap::default();
		let mut edge_matrix:	Vec<u64>							= vec![0; properties.vert_count];
		let mut forward_edges:	u64									= 0;
		let mut backward_edges:	u64									= 0;

		vert_indices.clear();
		faces.clear();
		faces.reserve_exact(properties.face_count);

		for (edge_index, edge) in edges.iter().enumerate() {
			edge_matrix[edge.0] |= 1 << edge.1;
			edge_matrix[edge.1] |= 1 << edge.0;
			edge_to_index.insert(*edge, edge_index);
		}

		macro_rules! log_edge_status {
			() => {
				if log_enabled!(target: log_concat!(log_target, "edge_status"), log::Level::Trace) {
					let mut status_update: String = "Edge Status:\n   ".to_string();
					let cell_width: usize = if properties.vert_count >= 0xF { 3 } else { 2 };

					for vert_index_x in 0 .. properties.vert_count {
						status_update.push_str(format!("{0:^1$x}", vert_index_x, cell_width).as_str());
					}

					status_update.push('\n');

					for (vert_index_y, adjacent_verts) in edge_matrix.iter().enumerate() {
						status_update.push_str(format!("{:>2x} ", vert_index_y).as_str());

						for vert_index_x in 0 .. properties.vert_count {
							status_update.push_str(format!("{0:^1$}",
								if (*adjacent_verts & (1 << vert_index_x)) != 0 { 'X' } else { '.' },
								cell_width
							).as_str());
						}

						status_update.push('\n');
					}

					status_update.push_str(format!("\n{:22}F B\n", "").as_str());

					for (edge_index, edge) in edges.iter().enumerate() {
						status_update.push_str(format!("{:>3x}: {:2?} {} {}\n", edge_index, edge,
							if (forward_edges & (1 << edge_index)) != 0 { 'X' } else { '.' },
							if (backward_edges & (1 << edge_index)) != 0 { 'X' } else { '.' }
						).as_str());
					}

					trace!(target: log_concat!(log_target, "edge_status"), "{}", status_update);
				}
			};
		}

		log_edge_status!();

		while forward_edges != all_edges_used && backward_edges != all_edges_used {
			const INVALID_VERT_INDEX: usize = usize::MAX;
			let initial_edge: EdgeData = edges[forward_edges.trailing_ones() as usize];
			let start: usize = vert_indices.len();
			let mut end: usize = start;
			let mut prev_vert_index: usize = 0;
			let mut curr_vert_index: usize = initial_edge.0;
			let mut next_vert_index: usize = initial_edge.1;

			macro_rules! cycle_values {
				() => {
					trace!(target: log_concat!(log_target, "cycle_values"), "Cycling {} <- {} <- {}", prev_vert_index, curr_vert_index, next_vert_index);
					prev_vert_index = curr_vert_index;
					curr_vert_index = next_vert_index;
				};
			}

			macro_rules! record_vert {
				() => {
					vert_indices.push(curr_vert_index);
					end += 1;
					trace!(target: log_concat!(log_target, "record_vert"), "Recording vertex index {} (end is now {})", curr_vert_index, end);
				};
			}

			macro_rules! record_edge {
				() => {
					// EdgeData::new() automatically orders the indices
					let edge: EdgeData = EdgeData::new(prev_vert_index, curr_vert_index);

					trace!(target: log_concat!(log_target, "record_edge"), "Recording edge {:?}", edge);

					edge_matrix[prev_vert_index] &= !(1_u64 << curr_vert_index);

					if let Some(edge_index) = edge_to_index.get(&edge) {
						if prev_vert_index <= curr_vert_index {
							forward_edges |= 1_u64 << edge_index;
						} else {
							backward_edges |= 1_u64 << edge_index;
						}
					} else {
						return Err(log_error!(
							target: log_concat!(log_target, "record_edge"),
							Level::Warn, format!("No valid next vertex for previous vertex {} ({}) and current vertex {} ({}) in polyhedron {:?}",
								prev_vert_index,
								verts[prev_vert_index].vec,
								curr_vert_index,
								verts[curr_vert_index].vec,
								properties.polyhedron
							)
						));
					}
				};
			}

			macro_rules! find_next_vert {
				() => {
					next_vert_index = {
						let mut candidate_vert_indices: u64 = edge_matrix[curr_vert_index];

						if candidate_vert_indices == 0 {
							warn!(target: log_concat!(log_target, "find_next_vert"), "No candidate next vertices for vertex {} in polyhedron {:?}", curr_vert_index, properties.polyhedron);

							INVALID_VERT_INDEX
						} else {
							let curr_vert_vector: Vec3 = verts[curr_vert_index].vec;
							let prev_vert_to_curr_vert_vector: Vec3 = curr_vert_vector - verts[prev_vert_index].vec;
							let mut best_vert_index: usize = INVALID_VERT_INDEX;
							let mut best_vert_angle: f32 = 0.0;

							while candidate_vert_indices != 0 {
								let candidate_vert_index: usize = candidate_vert_indices.trailing_zeros() as usize;

								trace!(target: log_concat!(log_target, "find_next_vert"), "Considering candidate vertex {}", candidate_vert_index);

								candidate_vert_indices &= !(1 << candidate_vert_index);

								// Don't double back
								if candidate_vert_index == prev_vert_index {
									trace!(target: log_concat!(log_target, "find_next_vert"), "Double-back, rejecting");

									continue;
								}

								let candidate_vert_vector: Vec3 = verts[candidate_vert_index].vec;

								// TODO: Might need to revisit this because glam is FOOLISH and implements left-hand rule instead of right-hand rule. Ruined my night learning this.
								// Only proceed if this is a counter-clockwise candidate
								if prev_vert_to_curr_vert_vector.cross(candidate_vert_vector).dot(curr_vert_vector) <= 0.0 {
									trace!(target: log_concat!(log_target, "find_next_vert"), "Clockwise, rejecting");

									continue;
								}

								let candidate_vert_angle: f32 = prev_vert_to_curr_vert_vector.angle_between(candidate_vert_vector);

								if best_vert_index == INVALID_VERT_INDEX || best_vert_angle <= candidate_vert_angle {
									best_vert_index = candidate_vert_index;
									best_vert_angle = candidate_vert_angle;
								} else {
									trace!(target: log_concat!(log_target, "find_next_vert"), "Worse angle ({} (best) > {} (candidate))", best_vert_angle, candidate_vert_angle);
								}
							}

							best_vert_index
						}
					}
				};
			}

			record_vert!();

			loop {
				cycle_values!();
				record_vert!();
				record_edge!();
				find_next_vert!();

				if next_vert_index == initial_edge.0 {
					break;
				} else if next_vert_index == INVALID_VERT_INDEX {
					return Err(log_error!(
						target: log_target,
						Level::Warn, format!("No valid next vertex for previous vertex {} ({}) and current vertex {} ({}) in polyhedron {:?}",
							prev_vert_index,
							verts[prev_vert_index].vec,
							curr_vert_index,
							verts[curr_vert_index].vec,
							properties.polyhedron
						)
					));
				}
			}

			cycle_values!();
			record_edge!();
			faces.push(FaceData::new(&verts, &vert_indices, start, end));
			trace!(target: log_target.as_str(), "Adding face {:?}", faces.last().unwrap());
			log_edge_status!();
		}

		let dual_verts: Vec<VertexData> = {
			if let Some(dual_properties) = Properties::get(properties.polyhedron.dual()) {
				let mut verts: Vec<VertexData> = Vec::<VertexData>::with_capacity(dual_properties.vert_count);

				Data::generate_verts_for_properties(dual_properties, &mut verts);

				verts
			} else {
				return Err(log_error!(
					target: log_target,
					Level::Warn, format!("Polyhedron {:?}'s dual is Invalid", properties.polyhedron)
				));
			}
		};

		faces.sort_by_cached_key(|face_data: &FaceData| -> usize {
			Data::get_closest_vert_index_for_verts(&dual_verts, &face_data.norm)
		});

		let should_be_initial_vert: Box<dyn Fn(usize, usize) -> bool> = match self.polyhedron {
			Polyhedron::Icosahedron => Box::new(|face_index: usize, vert_index: usize| -> bool {
				if face_index < properties.vert_count {
					((vert_index as isize) - (face_index as isize)).abs() > 1
				} else {
					vert_index >> 2 == 1
				}
			}) as Box<dyn Fn(usize, usize) -> bool>,
			Polyhedron::Dodecahedron => Box::new(|face_index: usize, vert_index: usize| -> bool {
				vert_index < properties.face_count && ((vert_index as isize) - (face_index as isize)).abs() > 1
			}) as Box<dyn Fn(usize, usize) -> bool>,
			Polyhedron::Icosidodecahedron => Box::new(|face_index: usize, vert_index: usize| -> bool {
				if face_index < 2 * polyhedra::properties::ICOSAHEDRON.vert_count {
					vert_index % 5 == 0
				} else {
					const X_MAJOR_VERT_INDEX_END: usize = polyhedra::properties::ICOSIDODECAHEDRON.vert_count / 3;

					vert_index < X_MAJOR_VERT_INDEX_END
				}
			}) as Box<dyn Fn(usize, usize) -> bool>,
			Polyhedron::RhombicTriacontahedron => Box::new(|face_index: usize, vert_index: usize| -> bool {
				if face_index % 5 == 0 {
					vert_index & 0b1 == 0
				} else {
					vert_index < polyhedra::properties::ICOSAHEDRON.vert_count
				}
			}) as Box<dyn Fn(usize, usize) -> bool>,
			Polyhedron::Invalid => {
				return Err(log_error!(target: log_target, Level::Error, format!("Couldn't assign closure to rotate vertex indices due to an Invalid Polyhedron")));
			},
		};

		for (face_index, face_data) in faces.iter_mut().enumerate() {
			let face_slice: &mut [usize] = face_data.get_slice_mut(vert_indices);

			if let Some(slice_index) = face_slice.iter().position(|vert_index: &usize| -> bool {
				should_be_initial_vert(face_index, *vert_index)
			}) {
				face_slice.rotate_left(slice_index);
			} else {
				return Err(log_error!(
					target: log_target,
					Level::Warn, format!("Couldn't find vertex in face {}'s slice, {:?}, that satisfies the condition necessary for it to be the initial vertex",
						face_index,
						face_slice
					)
				));
			}

			face_data.quat = {
				let first_vert: Vec3 = verts[*face_slice.first().unwrap()].vec;
				let negative_face_average: Vec3 = -(face_slice
					.iter()
					.map(|vert_index: &usize| -> &Vec3 {
						&verts[*vert_index].vec
					})
					.sum::<Vec3>() / face_slice.len() as f32);

				Quat::from_rotation_mat4(&Mat4::look_at_rh(
					Vec3::ZERO,
					negative_face_average,
					(first_vert + negative_face_average).normalize()
				))
			};
		}

		let log_target: String = log_concat!(log_target, "validation").into();

		if faces.len() != properties.face_count {
			return Err(log_error!(
				target: log_target,
				Level::Warn, format!("Found face count ({}) did not match expected face count ({})", faces.len(), properties.face_count)
			));
		}

		for face_size_index in 0 .. properties.face_sizes.len() - 1 {
			let min_face_index: usize = properties.face_sizes[face_size_index].initial_face_index;
			let max_face_index: usize = properties.face_sizes[face_size_index + 1].initial_face_index;
			let face_size: usize = properties.face_sizes[face_size_index].face_size;

			if min_face_index >= properties.face_count || max_face_index > properties.face_count {
				return Err(log_error!(
					target: log_target,
					Level::Warn, format!("Face size data {} is invalid", face_size_index)
				));
			}

			for (face_index, face_data) in faces[min_face_index .. max_face_index]
				.iter()
				.enumerate()
				.map(|(face_slice_index, face_data): (usize, &FaceData)| -> (usize, &FaceData) {
					(face_slice_index + min_face_index, face_data)
				})
			{
				if face_data.get_size() != face_size {
					return Err(log_error!(
						target: log_target,
						Level::Warn, format!("Face {}'s size ({}) did not match expected face size ({})", face_index, face_data.get_size(), face_size)
					));
				}

				let initial_vert_index: usize = vert_indices[face_data.range.start];

				if !should_be_initial_vert(face_index, initial_vert_index) {
					return Err(log_error!(
						target: log_target,
						Level::Warn, format!("Face {}'s initial vertex index ({}) did not satisfy the requisite condition for it to be the initial vertex index", face_index, initial_vert_index)
					));
				}
			}
		}

		Ok(())
	}

	fn reset(&mut self) -> () {
		// To make sure a failed Data doesn't hold on to allocated memory,
		// and save the lines of doing clear() then shrink_to_fit() on all vec members,
		// just make a new one
		*self = Data::default();
	}
}

#[macro_export(local_inner_macros)]
macro_rules! get_data {
	($polyhedron:ident) => {
		match Data::get(Polyhedron::$polyhedron) {
			Some(data) => data,
			None => { return Err(log_error!(Level::Error, std::format!("Data::get(Polyhedron::{:?}) was None", Polyhedron::$polyhedron))); }
		}
	};
}

lazy_static!{
	static ref ICOSAHEDRON:				Option<Data> = Data::new_option(Polyhedron::Icosahedron);
	static ref DODECAHEDRON:			Option<Data> = Data::new_option(Polyhedron::Dodecahedron);
	static ref ICOSIDODECAHEDRON:		Option<Data> = Data::new_option(Polyhedron::Icosidodecahedron);
	static ref RHOMBIC_TRIACONTAHEDRON:	Option<Data> = Data::new_option(Polyhedron::RhombicTriacontahedron);
}