use {
	crate::{
		math::{
			*,
			polyhedra::{
				properties::{
					self,
					Properties
				},
				Polyhedron,
				PolyhedronOption
			}
		},
		prelude::*,
		util::StaticDataLibrary
	},
	std::{
		f32::consts::TAU,
		mem::{
			MaybeUninit,
			transmute
		}
	},
	bevy::{
		prelude::*,
		render::{
			mesh::{
				Indices,
				Mesh
			},
			render_resource::PrimitiveTopology
		}
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

	pub fn get_slice_mut<'a>(&self, vert_indices: &'a mut Vec<usize>) -> &'a mut [usize] {
		&mut vert_indices[self.get_range()]
	}

	pub fn contains_vert(&self, vert_indices: &Vec<usize>, vert_index: usize) -> bool {
		self.get_slice(vert_indices).iter().any(|index: &usize| *index == vert_index)
	}

	pub fn get_size(&self) -> usize { self.range.end - self.range.start }

	pub fn get_rotation(&self, quat: &Quat) -> usize {
		let size: usize = self.get_size();
		let collapsed_result_up: Vec3 = Mat4::look_at_rh(
			Vec3::ZERO,
			-self.norm,
			self.norm.cross(self.quat * Vec3::Y)
		).transform_vector3(*quat * Vec3::Y);

		((TAU - collapsed_result_up.y.atan2(collapsed_result_up.x)) * size as f32 / TAU).round() as usize % size
	}

	pub fn get_rotation_quat(&self, rotation: u32) -> Quat {
		Quat::from_axis_angle(self.norm, rotation as f32 * -TAU / self.get_size() as f32)
	}

	pub fn get_rotated_quat(&self, rotation: u32) -> Quat {
		self.get_rotation_quat(rotation) * self.quat
	}
}

#[derive(Debug)]
pub struct Data {
	pub verts:			Vec<VertexData>,
	pub edges:			Vec<EdgeData>,
	pub vert_indices:	Vec<usize>,
	pub faces:			Vec<FaceData>
}

pub type OptionalPredicate<T> = Option<Box<dyn Fn(&T) -> bool>>;

impl Data {
	pub fn initialize() -> () { DataLibrary::build(); }

	/// get()
	/// 
	/// It is the caller's responsibility to ensure Data::initialize() was called first. Calling it a second time
	/// has no side effects
	pub fn get(polyhedron: Polyhedron) -> &'static Self { &DataLibrary::get().0[polyhedron as usize] }

	pub fn get_closest_vert_index(&self, vec: &Vec3, filter: OptionalPredicate<VertexData>) -> usize {
		Data::get_closest_vert_index_for_verts(&self.verts, vec, filter)
	}

	pub fn get_closest_vert_index_for_verts(
		verts:	&Vec<VertexData>,
		vec:	&Vec3,
		filter:	OptionalPredicate<VertexData>
	) -> usize {
		use std::cmp::Ordering;

		verts
			.iter()
			.filter(|vert: &&VertexData| -> bool {
				match &filter {
					Some(filter_func) => filter_func(vert),
					None => true
				}
			})
			.map(|vert: &VertexData| -> f32 {
				vert.norm.dot(*vec)
			})
			.enumerate()
			.max_by(|
				(_vert_index_a, norm_dot_vec_a): &(usize, f32),
				(_vert_index_b, norm_dot_vec_b): &(usize, f32)
			| -> Ordering {
				norm_dot_vec_a.partial_cmp(&norm_dot_vec_b).unwrap_or(Ordering::Equal)
			})
			.map(|(vert_index, _norm_dot_vec): (usize, f32)| -> usize {
				vert_index
			})
			.unwrap()
	}

	pub fn get_closest_face_index(&self, vec: &Vec3, filter: OptionalPredicate<FaceData>) -> usize {
		Data::get_closest_face_index_for_faces(&self.faces, vec, filter)
	}

	pub fn get_closest_face_index_for_faces(
		faces:	&Vec<FaceData>,
		vec:	&Vec3,
		filter:	OptionalPredicate<FaceData>
	) -> usize {
		use std::cmp::Ordering;

		faces
			.iter()
			.filter(|face: &&FaceData| -> bool {
				match &filter {
					Some(filter_func) => filter_func(face),
					None => true
				}
			})
			.map(|face: &FaceData| -> f32 {
				face.norm.dot(*vec)
			})
			.enumerate()
			.max_by(|
				(_face_index_a, norm_dot_vec_a): &(usize, f32),
				(_face_index_b, norm_dot_vec_b): &(usize, f32)
			| -> Ordering {
				norm_dot_vec_a.partial_cmp(&norm_dot_vec_b).unwrap_or(Ordering::Equal)
			})
			.map(|(face_index, _norm_dot_vec): (usize, f32)| -> usize {
				face_index
			})
			.unwrap()
	}

	pub fn get_pos_and_rot(&self, quat: &Quat, filter: OptionalPredicate<FaceData>) -> (usize, usize) {
		let pos: usize = self.get_closest_face_index(&(*quat * Vec3::Z), filter);

		(pos, self.faces[pos].get_rotation(quat))
	}

	pub fn as_mesh(&self, primitive_topology: PrimitiveTopology) -> Option<Mesh> {
		if primitive_topology == PrimitiveTopology::LineStrip
			|| primitive_topology == PrimitiveTopology::TriangleStrip {
			return None;
		}

		let mut mesh:		Mesh = Mesh::new(primitive_topology);
		let mut positions:	Vec<[f32; 3]> = Vec::<[f32; 3]>::new();
		let mut normals:	Vec<[f32; 3]> = Vec::<[f32; 3]>::new();
		let mut uvs:		Vec<[f32; 2]> = Vec::<[f32; 2]>::new();
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
}

#[cfg(test)]
impl Data {
	fn validate_polyhedra() -> LogErrorResult {
		let log_target: String = log_path!("validate_polyhedra").to_string();
		let mut data: MaybeUninit<[Data; 4]> = MaybeUninit::<[Data; 4]>::zeroed();
		let data: &mut [Data; 4] = unsafe { data.assume_init_mut() };

		for polyhedron_usize in 0_usize .. 4_usize {
			let mut data_builder: DataBuilder = DataBuilder {
				data: &mut data[polyhedron_usize],
				polyhedron: PolyhedronOption::from(polyhedron_usize as u8).0.unwrap()
			};

			data_builder.generate_checked()?;
		}

		let validate_dual_polyhedra = |polyhedron_a: Polyhedron| -> LogErrorResult {
			let polyhedron_b:	Polyhedron	= polyhedron_a.dual();
			let properties_a:	&Properties	= Properties::get(polyhedron_a);
			let properties_b:	&Properties	= Properties::get(polyhedron_b);
			let data_a:			&Data		= &data[polyhedron_a as usize];
			let data_b:			&Data		= &data[polyhedron_b as usize];

			if properties_a.vert_count != properties_b.face_count {
				return Err(log_error!(
					target: log_target,
					Level::Warn,
					format!(
						"Polyhedron {:?}'s vertex count doesn't equal polyhedron {:?}'s face count",
						polyhedron_a,
						polyhedron_b
					)
				));
			}

			for index in 0 .. properties_a.vert_count {
				if !data_a.verts[index].norm.abs_diff_eq(data_b.faces[index].norm, f32::EPSILON) {
					return Err(log_error!(
						target: log_target,
						Level::Warn,
						format!(
							"Vert {}'s normal vector ({:?}) of polyhedron {:?} isn't close enough to \
								face {}'s normal vector ({:?}) of polyhedron {:?}",
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
}

struct DataBuilder<'a> {
	data:		&'a mut Data,
	polyhedron:	Polyhedron
}

impl<'a> DataBuilder<'a> {
	fn generate(&mut self) -> () {
		self.generate_verts();
		self.generate_edges();
		warn_expect_ok!(self.generate_faces());
	}

	fn generate_verts(&mut self) -> () {
		let properties:	&Properties				= self.polyhedron.properties();
		let verts:		&mut Vec<VertexData>	= &mut self.data.verts;

		verts.clear();
		verts.reserve_exact(properties.vert_count);
		Self::generate_verts_for_properties(properties, verts);
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
				Self::generate_verts_for_properties(&polyhedra::properties::ICOSAHEDRON, verts);
				Self::generate_verts_for_properties(&polyhedra::properties::DODECAHEDRON, verts);
			}
		}
	}

	fn generate_edges(&mut self) -> () {
		let properties:	&Properties			= self.polyhedron.properties();
		let verts:		&Vec<VertexData>	= &self.data.verts;
		let edges:		&mut Vec<EdgeData>	= &mut self.data.edges;

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
	}

	fn generate_faces(&mut self) -> LogErrorResult {
		let properties:				&Properties							= self.polyhedron.properties();
		let should_be_initial_vert:	fn(usize, usize) -> bool			= self.get_should_be_initial_vert();
		let verts:					&Vec<VertexData>					= &self.data.verts;
		let edges:					&Vec<EdgeData>						= &self.data.edges;
		let vert_indices:			&mut Vec<usize>						= &mut self.data.vert_indices;
		let faces:					&mut Vec<FaceData>					= &mut self.data.faces;
		let all_edges_used:			u64									= (1_u64 << properties.edge_count) - 1_u64;
		let log_target:				String								= log_path!("generate_faces").into();

		let mut edge_to_index:		fnv::FnvHashMap<EdgeData, usize>	= fnv::FnvHashMap::default();
		let mut edge_matrix:		Vec<u64>							= vec![0_u64; properties.vert_count];
		let mut forward_edges:		u64									= 0_u64;
		let mut backward_edges:		u64									= 0_u64;

		vert_indices.clear();
		faces.clear();
		faces.reserve_exact(properties.face_count);

		for (edge_index, edge) in edges.iter().enumerate() {
			edge_matrix[edge.0] |= 1_u64 << edge.1;
			edge_matrix[edge.1] |= 1_u64 << edge.0;
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

					log::trace!(target: log_concat!(log_target, "edge_status"), "{}", status_update);
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
					log::trace!(
						target: log_concat!(log_target, "cycle_values"),
						"Cycling {} <- {} <- {}",
						prev_vert_index,
						curr_vert_index,
						next_vert_index
					);
					prev_vert_index = curr_vert_index;
					curr_vert_index = next_vert_index;
				};
			}

			macro_rules! record_vert {
				() => {
					vert_indices.push(curr_vert_index);
					end += 1;
					log::trace!(
						target: log_concat!(log_target, "record_vert"),
						"Recording vertex index {} (end is now {})",
						curr_vert_index,
						end
					);
				};
			}

			macro_rules! record_edge {
				() => {
					// EdgeData::new() automatically orders the indices
					let edge: EdgeData = EdgeData::new(prev_vert_index, curr_vert_index);

					log::trace!(target: log_concat!(log_target, "record_edge"), "Recording edge {:?}", edge);

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
							Level::Warn,
							format!(
								"No valid next vertex for previous vertex {} ({}) and current vertex {} ({}) in \
									polyhedron {:?}",
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
							log::warn!(
								target: log_concat!(log_target, "find_next_vert"),
								"No candidate next vertices for vertex {} in polyhedron {:?}",
								curr_vert_index,
								properties.polyhedron
							);

							INVALID_VERT_INDEX
						} else {
							let curr_vert_vector: Vec3 = verts[curr_vert_index].vec;
							let prev_vert_to_curr_vert_vector: Vec3 = curr_vert_vector - verts[prev_vert_index].vec;
							let mut best_vert_index: usize = INVALID_VERT_INDEX;
							let mut best_vert_angle: f32 = 0.0;

							while candidate_vert_indices != 0 {
								let candidate_vert_index: usize = candidate_vert_indices.trailing_zeros() as usize;

								log::trace!(
									target: log_concat!(log_target, "find_next_vert"),
									"Considering candidate vertex {}",
									candidate_vert_index
								);

								candidate_vert_indices &= !(1 << candidate_vert_index);

								// Don't double back
								if candidate_vert_index == prev_vert_index {
									log::trace!(
										target: log_concat!(log_target, "find_next_vert"),
										"Double-back, rejecting"
									);

									continue;
								}

								let candidate_vert_vector: Vec3 = verts[candidate_vert_index].vec;

								/* TODO: Might need to revisit this because glam is FOOLISH and implements left-hand
								rule instead of right-hand rule. Ruined my night learning this. Only proceed if this is
								a counter-clockwise candidate */
								if prev_vert_to_curr_vert_vector
									.cross(candidate_vert_vector)
									.dot(curr_vert_vector)
									<= 0.0 {
									log::trace!(
										target: log_concat!(log_target, "find_next_vert"),
										"Clockwise, rejecting"
									);

									continue;
								}

								let candidate_vert_angle: f32 = prev_vert_to_curr_vert_vector
									.angle_between(candidate_vert_vector);

								if best_vert_index == INVALID_VERT_INDEX || best_vert_angle <= candidate_vert_angle {
									best_vert_index = candidate_vert_index;
									best_vert_angle = candidate_vert_angle;
								} else {
									log::trace!(
										target: log_concat!(log_target, "find_next_vert"),
										"Worse angle ({} (best) > {} (candidate))",
										best_vert_angle, candidate_vert_angle
									);
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
						Level::Warn, 
						format!(
							"No valid next vertex for previous vertex {} ({}) and current vertex {} ({}) in polyhedron \
								{:?}",
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
			log::trace!(target: log_target.as_str(), "Adding face {:?}", faces.last().unwrap());
			log_edge_status!();
		}

		let dual_verts: Vec<VertexData> = {
			let dual_properties: &Properties = Properties::get(properties.polyhedron.dual());
			let mut verts: Vec<VertexData> = Vec::<VertexData>::with_capacity(dual_properties.vert_count);

			Self::generate_verts_for_properties(dual_properties, &mut verts);

			verts
		};

		faces.sort_by_cached_key(|face_data: &FaceData| -> usize {
			Data::get_closest_vert_index_for_verts(&dual_verts, &face_data.norm, None)
		});

		for (face_index, face_data) in faces.iter_mut().enumerate() {
			let face_slice: &mut [usize] = face_data.get_slice_mut(vert_indices);

			if let Some(slice_index) = face_slice.iter().position(|vert_index: &usize| -> bool {
				should_be_initial_vert(face_index, *vert_index)
			}) {
				face_slice.rotate_left(slice_index);
			} else {
				return Err(log_error!(
					target: log_target,
					Level::Warn,
					format!(
						"Couldn't find vertex in face {}'s slice, {:?}, that satisfies the condition necessary for it \
							to be the initial vertex",
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

				Quat::from_mat4(&Mat4::look_at_rh(
					Vec3::ZERO,
					negative_face_average,
					(first_vert + negative_face_average).normalize()
				).inverse())
			};
		}

		Ok(())
	}

	fn get_should_be_initial_vert(&self) -> fn(usize, usize) -> bool {
		match self.polyhedron {
			Polyhedron::Icosahedron				=> |face_index: usize, vert_index: usize| -> bool {
				if face_index < properties::ICOSAHEDRON.vert_count {
					((vert_index as isize) - (face_index as isize)).abs() > 1
				} else {
					vert_index >> 2 == 1
				}
			},
			Polyhedron::Dodecahedron				=> |face_index: usize, vert_index: usize| -> bool {
				vert_index < properties::DODECAHEDRON.face_count
					&& ((vert_index as isize) - (face_index as isize)).abs() > 1
			},
			Polyhedron::Icosidodecahedron		=> |face_index: usize, vert_index: usize| -> bool {
				if face_index < 2 * properties::ICOSAHEDRON.vert_count {
					vert_index % 5 == 0
				} else {
					const X_MAJOR_VERT_INDEX_END: usize = properties::ICOSIDODECAHEDRON.vert_count / 3;
		
					vert_index < X_MAJOR_VERT_INDEX_END
				}
			},
			Polyhedron::RhombicTriacontahedron	=> |face_index: usize, vert_index: usize| -> bool {
				if face_index % 5 == 0 {
					vert_index & 0b1 == 0
				} else {
					vert_index < properties::ICOSAHEDRON.vert_count
				}
			},
		}
	}
}

#[cfg(test)]
impl<'a> DataBuilder<'a> {
	fn generate_checked(&mut self) -> LogErrorResult {
		self.generate_elements_checked(
			Self::generate_verts_checked,
			"vertices",
			|data: &Data| -> (&Vec<VertexData>, ) {
				(&data.verts, )
			}
		)?;
		self.generate_elements_checked(
			Self::generate_edges_checked,
			"edges",
			|data: &Data| -> (&Vec<VertexData>, &Vec<EdgeData>) {
				(&data.verts, &data.edges)
			}
		)?;
		self.generate_elements_checked(
			Self::generate_faces_checked,
			"faces",
			|data: &Data| -> (&Vec<EdgeData>, &Vec<usize>, &Vec<FaceData>) {
				(&data.edges, &data.vert_indices, &data.faces)
			}
		)?;

		Ok(())
	}

	fn generate_elements_checked<'b, T: std::fmt::Debug>(
		&'b mut self,
		func:				fn(&mut Self) -> LogErrorResult,
		elements:			&str,
		get_relevant_data:	fn(&'b Data) -> T
	) -> LogErrorResult {
		if let Err(log_error) = func(self) {
			log_error.log();

			Err(log_error!(
				target: log_path!("generate"),
				Level::Warn, format!("Failed to generate {} for polyhedron {:?}", elements, self.polyhedron),
				Level::Info, format!("Relevant data: {:#?}", get_relevant_data(self.data))
			))
		} else {
			Ok(())
		}
	}

	fn generate_verts_checked(&mut self) -> LogErrorResult {
		self.generate_verts();

		let properties:	&Properties				= self.polyhedron.properties();
		let verts:		&Vec<VertexData>		= &self.data.verts;

		log::trace!(target: log_path!("generate_verts"), "{:?} verts: {:#?}", properties.polyhedron, verts);

		if verts.len() == properties.vert_count {
			Ok(())
		} else {
			Err(log_error!(
				target: log_path!("generate_verts"),
				Level::Warn, format!(
					"Found vertex count ({}) did not match expected vertex count ({})",
					verts.len(),
					properties.vert_count
				)
			))
		}
	}

	fn generate_edges_checked(&mut self) -> LogErrorResult {
		self.generate_edges();

		let properties:	&Properties			= self.polyhedron.properties();
		let edges:		&Vec<EdgeData>		= &self.data.edges;

		log::trace!(target: log_path!("generate_edges"), "{:?} edges: {:#?}", properties.polyhedron, edges);

		if edges.len() == properties.edge_count {
			Ok(())
		} else {
			Err(log_error!(
				target: log_path!("generate_edges"),
				Level::Warn, format!(
					"Found edge count ({}) did not match expected edge count ({})",
					edges.len(),
					properties.edge_count
				)
			))
		}
	}

	fn generate_faces_checked(&mut self) -> LogErrorResult {
		self.generate_faces()?;

		let properties:				&Properties					= self.polyhedron.properties();
		let vert_indices:			&Vec<usize>					= &self.data.vert_indices;
		let faces:					&Vec<FaceData>				= &self.data.faces;
		let log_target:				String						= log_path!("generate_faces_checked").into();
		let should_be_initial_vert:	fn(usize, usize) -> bool	= self.get_should_be_initial_vert();

		if faces.len() != properties.face_count {
			return Err(log_error!(
				target: log_target,
				Level::Warn,
				format!(
					"Found face count ({}) did not match expected face count ({})",
					faces.len(),
					properties.face_count
				)
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
						Level::Warn,
						format!(
							"Face {}'s size ({}) did not match expected face size ({})",
							face_index,
							face_data.get_size(),
							face_size
						)
					));
				}

				let initial_vert_index: usize = vert_indices[face_data.range.start];

				if !should_be_initial_vert(face_index, initial_vert_index) {
					return Err(log_error!(
						target: log_target,
						Level::Warn,
						format!(
							"Face {}'s initial vertex index ({}) did not satisfy the requisite condition for it to be \
							the initial vertex index",
							face_index,
							initial_vert_index
						)
					));
				}
			}
		}

		Ok(())
	}
}

struct DataLibrary([Data; 4_usize]);

impl DataLibrary {
	fn new() -> Self {
		let mut data_array: [MaybeUninit<Data>; 4_usize] = unsafe {
			MaybeUninit::<[MaybeUninit<Data>; 4_usize]>::uninit().assume_init()
		};

		for (polyhedron_index, data) in data_array.iter_mut().enumerate() {
			DataBuilder {
				data: unsafe {
					std::ptr::write(data.as_mut_ptr(), Data {
						verts:			Vec::<VertexData>::new(),
						edges:			Vec::<EdgeData>::new(),
						vert_indices:	Vec::<usize>::new(),
						faces:			Vec::<FaceData>::new()
					});
	
					data.assume_init_mut()
				},
				polyhedron: PolyhedronOption::from(polyhedron_index as u8).0.unwrap()
			}.generate();
		}

		DataLibrary(unsafe { transmute::<[MaybeUninit<Data>; 4_usize], [Data; 4_usize]>(data_array) })
	}
}

impl StaticDataLibrary for DataLibrary {
	fn get() -> &'static Self { &DATA_LIBRARY }
}

lazy_static! {
	static ref DATA_LIBRARY: DataLibrary = DataLibrary::new();
}

pub struct DataPlugin;

impl Plugin for DataPlugin {
	fn build(&self, _app: &mut App) -> () {
		DataLibrary::build();
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_data() -> () {
		init_env_logger();
		assert!(Data::validate_polyhedra().is_ok());
	}
}