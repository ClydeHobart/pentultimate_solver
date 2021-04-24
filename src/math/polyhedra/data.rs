use {
	crate::math::{
		*,
		polyhedra::properties::{
			Polyhedron,
			Properties
		}
	},
	std::module_path
};

type Range = std::ops::Range<usize>;

#[derive(Debug, Default)]
pub struct VertexData {
	vector:	Vec3,
	normal:	Vec3
}

impl From<Vec3> for VertexData {
	fn from(vector: Vec3) -> Self {
		Self {
			vector,
			normal: vector.normalize_or_zero()
		}
	}
}

#[derive(Clone, Copy, Eq, Debug, Default, Hash, PartialEq)]
pub struct EdgeData(u32, u32);

impl EdgeData {
	pub fn new(vert_index_1: u32, vert_index_2: u32) -> Self {
		EdgeData(std::cmp::min(vert_index_1, vert_index_2), std::cmp::max(vert_index_1, vert_index_2))
	}

	pub fn contains_vert(&self, vert_index: u32) -> bool {
		self.0 == vert_index || self.1 == vert_index
	}
}

#[derive(Debug, Default)]
pub struct FaceData {
	range:	Range,
	normal:	Vec3
}

impl FaceData {
	pub fn new(verts: &Vec<VertexData>, vert_indices: &Vec<u32>, start: usize, end: usize) -> Self {
		let mut vector_sum: Vec3 = Vec3::ZERO;

		for vert_index in vert_indices.get(start .. end).unwrap().iter() {
			vector_sum += verts[*vert_index as usize].vector;
		}

		FaceData {
			range: start .. end,
			normal: vector_sum.normalize_or_zero()
		}
	}
}

#[derive(Debug)]
pub struct Data {
	polyhedron:		Polyhedron,
	verts:			Vec<VertexData>,
	edges:			Vec<EdgeData>,
	vert_indices:	Vec<u32>,
	faces:			Vec<FaceData>
}

pub type DataRefOption = Option<&'static Data>;

impl Data {
	pub fn get(polyhedron: Polyhedron) -> DataRefOption {
		let option: &Option<Data> = match polyhedron {
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

	const fn default() -> Self {
		Self {
			polyhedron:		Polyhedron::Invalid,
			verts:			Vec::<VertexData>::new(),
			edges:			Vec::<EdgeData>::new(),
			vert_indices:	Vec::<u32>::new(),
			faces:			Vec::<FaceData>::new()
		}
	}

	fn new_option(polyhedron: Polyhedron) -> Option<Data> {
		let mut data: Data = Data::default();

		data.polyhedron = polyhedron;

		if data.generate() {
			Some(data)
		} else {
			data.reset();

			None
		}
	}

	fn generate(&mut self) -> bool {
		fn try_generate_elements<T: std::fmt::Debug>(result: bool, elements: &str, data_struct: &Data, important_data: &T) -> bool {
			return result || {
				warn!("Failed to generate {} for polyhedron {:?}", elements, data_struct.polyhedron);
				info!("Important data: {:#?}", important_data);

				false
			};
		}

		try_generate_elements(
			self.generate_verts(),
			"vertices",
			self,
			&self.verts
		) && try_generate_elements(
			self.generate_edges(),
			"edges",
			self,
			&(&self.verts, &self.edges)
		) && try_generate_elements(
			self.generate_faces(),
			"faces",
			self,
			&(&self.edges, &self.vert_indices, &self.faces)
		)
	}

	fn generate_verts(&mut self) -> bool {
		let properties: &'static Properties = Properties::get(self.polyhedron).unwrap();

		self.verts.clear();
		self.verts.reserve_exact(properties.vert_count);

		match self.polyhedron {
			Polyhedron::Icosahedron => {
				let base_vector: Vec3 = properties.base_vectors[0];
				let mut perm_mat: Mat3 = Mat3::IDENTITY;

				for _perm in 0 .. 3 {
					for vert in 0 .. 4 {
						self.verts.push(
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
			}, _ => {
				return false;
			}
		}

		self.verts.len() == properties.vert_count
	}

	fn generate_edges(&mut self) -> bool {
		let properties: &'static Properties =  Properties::get(self.polyhedron).unwrap();
		let verts: &Vec<VertexData> = &self.verts;

		self.edges.clear();
		self.edges.reserve_exact(properties.edge_count);

		let distance_squared_threshold: f32 = properties.edge_length * properties.edge_length + 0.000001;

		for vert_index_1 in 0 .. properties.vert_count - 1 {
			let vert_1: &Vec3 = &verts[vert_index_1].vector;

			for vert_index_2 in vert_index_1 + 1 .. properties.vert_count {
				if vert_1.distance_squared(verts[vert_index_2].vector) <= distance_squared_threshold {
					self.edges.push(EdgeData(vert_index_1 as u32, vert_index_2 as u32));
				}
			}
		}

		self.edges.len() == properties.edge_count
	}

	fn generate_faces(&mut self) -> bool {
		let properties: &'static Properties =  Properties::get(self.polyhedron).unwrap();
		let verts: &Vec<VertexData> = &self.verts;
		let edges: &Vec<EdgeData> = &self.edges;

		self.vert_indices.clear();
		self.faces.clear();
		self.faces.reserve_exact(properties.face_count);

		let mut edge_to_index: fnv::FnvHashMap<EdgeData, usize> = fnv::FnvHashMap::default();
		let mut edge_matrix: Vec<u64> = vec![0; properties.vert_count];

		let all_edges_used: u64 = (1 << properties.edge_count) - 1;

		let mut forward_edges: u64 = 0;
		let mut backward_edges: u64 = 0;

		for (edge_index, edge) in edges.iter().enumerate() {
			edge_matrix[edge.0 as usize] |= 1 << edge.1;
			edge_matrix[edge.1 as usize] |= 1 << edge.0;
			edge_to_index.insert(*edge, edge_index);
		}

		fn generate_edge_status_update(edges: &Vec<EdgeData>, edge_matrix: &Vec<u64>, forward_edges: u64, backward_edges: u64) -> String {
			let mut status_update: String = "Edge Status:\n   ".to_string();

			let vert_count: usize = edge_matrix.len();

			let cell_width: usize = if vert_count >= 0xF { 3 } else { 2 };

			for vert_index_x in 0 .. vert_count {
				status_update.push_str(format!("{0:^1$x}", vert_index_x, cell_width).as_str());
			}

			status_update.push('\n');

			for (vert_index_y, adjacent_verts) in edge_matrix.iter().enumerate() {
				status_update.push_str(format!("{:>2x} ", vert_index_y).as_str());

				for vert_index_x in 0 .. vert_count {
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

			status_update
		}

		// Check for Error first, since that ought to be on unless all logging is off
		if log_enabled!(log::Level::Error) {
			let edge_status_target: String = module_path!().to_owned() + "::edge_status";

			if log_enabled!(target: edge_status_target.as_str(), log::Level::Trace) {
				trace!(target: edge_status_target.as_str(), "{}", generate_edge_status_update(&edges, &edge_matrix, forward_edges, forward_edges));
			}
		}

		while forward_edges != all_edges_used && backward_edges != all_edges_used {
			const INVALID_VERT_INDEX: u32 = u32::MAX;
			let initial_edge: EdgeData = edges[forward_edges.trailing_ones() as usize];
			let start: usize = self.vert_indices.len();
			let mut end: usize = start;
			let mut prev_vert_index: u32 = 0;
			let mut curr_vert_index: u32 = initial_edge.0;
			let mut next_vert_index: u32 = initial_edge.1;

			fn cycle_values(prev_vert_index: &mut u32, curr_vert_index: &mut u32, next_vert_index: u32) -> () {
				trace!("Cycling {} <- {} <- {}", prev_vert_index, curr_vert_index, next_vert_index);

				*prev_vert_index = *curr_vert_index;
				*curr_vert_index = next_vert_index;
			}

			fn record_vert(vert_indices: &mut Vec<u32>, end: &mut usize, curr_vert_index: u32) -> () {
				vert_indices.push(curr_vert_index);
				*end += 1;

				trace!("Recording vertex index {} (end is now {})", curr_vert_index, end);
			}

			fn record_edge(
				edge_to_index: &fnv::FnvHashMap<EdgeData, usize>,
				edge_matrix: &mut Vec<u64>,
				forward_edges: &mut u64,
				backward_edges: &mut u64,
				prev_vert_index: u32,
				curr_vert_index: u32,
				polyhedron: Polyhedron
			) -> () {
				// EdgeData::new() automatically orders the indices
				let edge: EdgeData = EdgeData::new(prev_vert_index, curr_vert_index);

				trace!("Recording edge {:?}", edge);

				edge_matrix[prev_vert_index as usize] &= !(1_u64 << curr_vert_index);
				// edge_matrix[curr_vert_index as usize] &= !(1_u64 << prev_vert_index);

				if let Some(edge_index) = edge_to_index.get(&edge) {
					*(
						if prev_vert_index <= curr_vert_index {
							forward_edges
						} else {
							backward_edges
						}
					) |= 1_u64 << edge_index;
				} else {
					warn!("No stored index for edge {:?} in polyhedron {:?}", edge, polyhedron);
				}
			}

			fn find_next_vert(verts: &Vec<VertexData>, edge_matrix: &Vec<u64>, prev_vert_index: u32, curr_vert_index: u32, polyhedron: Polyhedron) -> u32 {
				let mut candidate_vert_indices: u64 = edge_matrix[curr_vert_index as usize];

				if candidate_vert_indices == 0 {
					warn!("No candidate next vertices for vertex {} in polyhedron {:?}", curr_vert_index, polyhedron);

					return INVALID_VERT_INDEX;
				}

				let curr_vert_vector: Vec3 = verts[curr_vert_index as usize].vector;
				let prev_vert_to_curr_vert_vector: Vec3 = curr_vert_vector - verts[prev_vert_index as usize].vector;
				let mut best_vert_index: u32 = INVALID_VERT_INDEX;
				let mut best_vert_angle: f32 = 0.0;

				while candidate_vert_indices != 0 {
					let candidate_vert_index: u32 = candidate_vert_indices.trailing_zeros();

					trace!("Considering candidate vertex {}", candidate_vert_index);

					candidate_vert_indices &= !(1 << candidate_vert_index);

					// Don't double back
					if candidate_vert_index == prev_vert_index {
						trace!("Double-back, rejecting");

						continue;
					}

					let candidate_vert_vector: Vec3 = verts[candidate_vert_index as usize].vector;

					// Only proceed if this is a counter-clockwise candidate
					if prev_vert_to_curr_vert_vector.cross(candidate_vert_vector).dot(curr_vert_vector) <= 0.0 {
						trace!("Clockwise, rejecting");

						continue;
					}

					let candidate_vert_angle: f32 = prev_vert_to_curr_vert_vector.angle_between(candidate_vert_vector);

					if best_vert_index == INVALID_VERT_INDEX || best_vert_angle <= candidate_vert_angle {
						best_vert_index = candidate_vert_index;
						best_vert_angle = candidate_vert_angle;
					} else {
						trace!("Worse angle ({} (best) > {} (candidate))", best_vert_angle, candidate_vert_angle);
					}
				}

				best_vert_index
			}

			record_vert(&mut self.vert_indices, &mut end, curr_vert_index);

			loop {
				cycle_values(&mut prev_vert_index, &mut curr_vert_index, next_vert_index);
				record_vert(&mut self.vert_indices, &mut end, curr_vert_index);
				record_edge(&edge_to_index, &mut edge_matrix, &mut forward_edges, &mut backward_edges, prev_vert_index, curr_vert_index, properties.polyhedron);
				next_vert_index = find_next_vert(&self.verts, &edge_matrix, prev_vert_index, curr_vert_index, properties.polyhedron);

				if next_vert_index == initial_edge.0 {
					break;
				} else if next_vert_index == INVALID_VERT_INDEX {
					warn!("No valid next vertex for previous vertex {} ({}) and current vertex {} ({}) in polyhedron {:?}",
						prev_vert_index,
						verts[prev_vert_index as usize].vector,
						curr_vert_index,
						verts[curr_vert_index as usize].vector,
						properties.polyhedron);

					return false;
				}
			}

			cycle_values(&mut prev_vert_index, &mut curr_vert_index, next_vert_index);
			record_edge(&edge_to_index, &mut edge_matrix, &mut forward_edges, &mut backward_edges, prev_vert_index, curr_vert_index, properties.polyhedron);

			self.faces.push(FaceData::new(&verts, &self.vert_indices, start, end));

			trace!("Adding face {:?}", self.faces.last().unwrap());

			// Check for Error first, since that ought to be on unless all logging is off
			if log_enabled!(log::Level::Error) {
				let edge_status_target: String = module_path!().to_owned() + "::edge_status";
	
				if log_enabled!(target: edge_status_target.as_str(), log::Level::Trace) {
					trace!(target: edge_status_target.as_str(), "{}", generate_edge_status_update(&edges, &edge_matrix, forward_edges, forward_edges));
				}
			}
		}

		self.faces.len() == properties.face_count
	}

	fn reset(&mut self) -> () {
		// To make sure a failed Data doesn't hold on to allocated memory,
		// and save the lines of doing clear() then shrink_to_fit() on all vec members,
		// just make a new one
		*self = Data::default();
	}
}

lazy_static!{
	static ref ICOSAHEDRON:				Option<Data> = Data::new_option(Polyhedron::Icosahedron);
	static ref DODECAHEDRON:			Option<Data> = Data::new_option(Polyhedron::Dodecahedron);
	static ref ICOSIDODECAHEDRON:		Option<Data> = Data::new_option(Polyhedron::Icosidodecahedron);
	static ref RHOMBIC_TRIACONTAHEDRON:	Option<Data> = Data::new_option(Polyhedron::RhombicTriacontahedron);
}