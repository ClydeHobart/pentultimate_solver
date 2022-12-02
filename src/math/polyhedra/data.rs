use {
    crate::{
        math::{
            polyhedra::{
                properties::{self, Properties},
                Polyhedron,
            },
            *,
        },
        prelude::*,
        util::StaticDataLibrary,
    },
    bevy::{
        prelude::*,
        render::{
            mesh::{Indices, Mesh},
            render_resource::PrimitiveTopology,
        },
    },
    log::Level,
    std::{
        cmp::Ordering,
        f32::consts::TAU,
        mem::{transmute, MaybeUninit},
        ops::Range,
    },
    strum::IntoEnumIterator,
};

/// A pair of distinct vertex indices in ascending order
#[derive(Clone, Copy, Eq, Debug, Default, Hash, Ord, PartialEq, PartialOrd)]
pub struct EdgeData(usize, usize);

impl EdgeData {
    /// Returns whether `self` contains the specified vertex index
    ///
    /// # Arguments
    ///
    /// * `vert_index` - The vertex index to query for
    pub fn contains_vert(&self, vert_index: usize) -> bool {
        self.0 == vert_index || self.1 == vert_index
    }
}

impl TryFrom<(usize, usize)> for EdgeData {
    type Error = ();

    /// Returns an `Ok`-wrapped `EdgeData` with the two supplied vertex indices in ascending order,
    /// or `Err(())` if the two indices are the same
    ///
    /// # Arguments
    ///
    /// * `vert_index_1` - The first of two vertex indices, not necessarily the lesser of the two
    /// * `vert_index_2` - The second of two vertex indices, not necessarily the greater of the two
    fn try_from((vert_index_1, vert_index_2): (usize, usize)) -> Result<Self, Self::Error> {
        match vert_index_1.cmp(&vert_index_2) {
            Ordering::Less => Ok(Self(vert_index_1, vert_index_2)),
            Ordering::Equal => Err(()),
            Ordering::Greater => Ok(Self(vert_index_2, vert_index_1)),
        }
    }
}

/// A bit array to refer to a set of edges
pub type EdgeBitArray = BitArr!(for properties::MAX_EDGE_COUNT, in u32);

/// A struct containing information describing a polyhedron face
#[derive(Debug, Default)]
pub struct FaceData {
    /// A quaternion representing the orientation of this face
    ///
    /// This is computed to be "looking" in the opposite direction of `norm`, with the first
    /// vertex index referred to by `range` being aligned upwards
    pub quat: Quat,

    /// The normal vector of this face
    ///
    /// This is computed as the normalized average of the comprising vertex vectors, assuming the
    /// face is a regular polygon, aligned with
    pub norm: Vec3,

    /// The range of vertex indices within the corresponding `Data::vert_indices` field, describing
    /// a list of vertices that comprise this face.
    ///
    /// The corresponding vertices within the specified slice wrap in counter-clockwise order
    /// (right-hand rule)
    pub range: Range<usize>,

    /// The set of edges comprising this face
    pub edges: EdgeBitArray,
}

impl FaceData {
    /// Returns a new `FaceData`
    ///
    /// # Arguments
    ///
    /// * `verts` - A slice of vertex vectors
    /// * `vert_indices` - A slice of indices into `verts`
    /// * `range` - A range into `vert_indices` specifying the vertices (within `verts`) that
    ///     comprise the new face
    /// * `edges` - The edges that comprise the new face
    pub fn new(
        verts: &[Vec3],
        vert_indices: &[usize],
        range: Range<usize>,
        edges: EdgeBitArray,
    ) -> Self {
        let norm: Vec3 = vert_indices[range.clone()]
            .iter()
            .map(|vert_index: &usize| -> &Vec3 { &verts[*vert_index] })
            .sum::<Vec3>()
            .normalize_or_zero();

        FaceData {
            quat: Quat::IDENTITY,
            norm,
            range,
            edges,
        }
    }

    /// Returns the range of vertex indices for `self`
    pub fn get_range(&self) -> Range<usize> {
        self.range.clone()
    }

    /// Borrows a slice of vertex indices for `self`
    ///
    /// # Arguments
    ///
    /// * `vert_indices` - The slice of vertex indices to borrow from
    pub fn get_slice<'a>(&self, vert_indices: &'a [usize]) -> &'a [usize] {
        &vert_indices[self.get_range()]
    }

    /// Mutably borrows a slice of vertex indices for `self`
    ///
    /// # Arguments
    ///
    /// * `vert_indices` - The slice of vertex indices to mutably borrow from
    pub fn get_slice_mut<'a>(&self, vert_indices: &'a mut [usize]) -> &'a mut [usize] {
        &mut vert_indices[self.get_range()]
    }

    /// Returns whether `self` contains a given vertex
    ///
    /// # Arguments
    ///
    /// * `vert_indices` - The slice of vertex indices that `self` was constructed with
    /// * `vert_index` - The vertex index to query for
    pub fn contains_vert(&self, vert_indices: &[usize], vert_index: usize) -> bool {
        self.get_slice(vert_indices)
            .iter()
            .any(|index: &usize| *index == vert_index)
    }

    /// Returns the size (the number of vertices) of `self`
    #[inline(always)]
    pub fn get_size(&self) -> usize {
        self.range.len()
    }

    /// Returns the angle in radians of a full rotation divided into `self.get_size()` equally-sized
    /// partial rotations
    #[inline(always)]
    pub fn get_partial_rotation_angle(&self) -> f32 {
        TAU / self.get_size() as f32
    }

    /// Returns the number of consecutive clockwise rotations of angle
    /// `self.get_partial_rotation_angle()` radians about `self.norm` until an accumulator
    /// quaternion initialized with `self.quat` is as close to a given quaternion as possible
    ///
    /// # Arguments
    ///
    /// * `quat` - The quaternion to find the number of partial rotations of
    ///
    /// # Examples
    ///
    /// ```
    /// # use {
    /// #     glam::{EulerRot, Quat, Vec2, Vec3, Vec3Swizzles},
    /// #     pentultimate_solver::math::polyhedra::{
    /// #         data::Data,
    /// #         Polyhedron,
    /// #     },
    /// #     rand::{rngs::ThreadRng, thread_rng, Rng},
    /// #     strum::IntoEnumIterator,
    /// #     std::{cmp::Ordering, f32::consts::TAU},
    /// # };
    /// const SMALL_ERROR: f32 = 1.0E-6_f32;
    ///
    /// let mut thread_rng: ThreadRng = thread_rng();
    /// let mut gen_angle = || -> f32 { TAU * thread_rng.gen::<f32>() };
    /// let mut gen_quat =
    ///     || -> Quat { Quat::from_euler(EulerRot::XYZ, gen_angle(), gen_angle(), gen_angle()) };
    /// let mut dots: Vec<f32> = Vec::new();
    ///
    /// for polyhedron in Polyhedron::iter() {
    ///     let data: &Data = Data::get(polyhedron);
    ///
    ///     for face_data in data.faces.iter() {
    ///         let to_xy_quat: Quat = Quat::from_rotation_arc(face_data.norm, Vec3::Z);
    ///         let to_xy = |quat: Quat| -> Vec2 { (to_xy_quat * quat * Vec3::Y).xy() };
    ///         let rand_quat: Quat = gen_quat();
    ///         let rand_quat_xy: Vec2 = to_xy(rand_quat);
    ///         let size: usize = face_data.get_size();
    ///
    ///         dots.clear();
    ///         dots.reserve(size);
    ///
    ///         for rotation in 0_usize..face_data.get_size() {
    ///             let quat: Quat = face_data.get_rotated_quat(rotation as u32);
    ///
    ///             assert_eq!(face_data.get_rotation(&quat), rotation);
    ///
    ///             dots.push(to_xy(quat).dot(rand_quat_xy));
    ///         }
    ///
    ///         let closest_rotation_to_rand_quat: usize = dots
    ///             .iter()
    ///             .enumerate()
    ///             .max_by(
    ///                 |(_rotation_a, dot_a): &(usize, &f32),
    ///                     (_rotation_b, dot_b): &(usize, &f32)|
    ///                     -> Ordering { dot_a.total_cmp(dot_b) },
    ///             )
    ///             .unwrap()
    ///             .0;
    ///         let rotation_of_rand_quat: usize = face_data.get_rotation(&rand_quat);
    ///
    ///         assert!(
    ///             // This will usually be the case
    ///             closest_rotation_to_rand_quat == rotation_of_rand_quat ||
    ///                 // In an unlucky scenario, this might occur
    ///                 (
    ///                     dots[closest_rotation_to_rand_quat] -
    ///                     dots[rotation_of_rand_quat]
    ///                 ).abs() <= SMALL_ERROR
    ///         );
    ///     }
    /// }
    /// ```
    pub fn get_rotation(&self, quat: &Quat) -> usize {
        let size: usize = self.get_size();
        let collapsed_result_up: Vec3 =
            Mat4::look_at_rh(Vec3::ZERO, -self.norm, self.norm.cross(self.quat * Vec3::Y))
                .transform_vector3(*quat * Vec3::Y);

        ((TAU - collapsed_result_up.y.atan2(collapsed_result_up.x))
            / self.get_partial_rotation_angle())
        .round() as usize
            % size
    }

    /// Returns a quaternion to perform a given number of consecutive clockwise rotations of angle
    /// `self.get_partial_rotation_angle()` radians about `self.norm`
    ///
    /// # Arguments
    ///
    /// * `rotation` - The number of partial rotations to perform
    ///
    /// # Examples
    ///
    /// ```
    /// # use {
    /// #     glam::{Quat, Vec3},
    /// #     pentultimate_solver::math::polyhedra::{
    /// #         data::Data,
    /// #         Polyhedron,
    /// #     },
    /// #     rand::{rngs::ThreadRng, thread_rng, Rng},
    /// #     strum::IntoEnumIterator,
    /// #     std::f32::consts::TAU,
    /// # };
    /// const SMALL_ERROR: f32 = 1.0E-6_f32;
    ///
    /// let mut thread_rng: ThreadRng = thread_rng();
    ///
    /// // From https://mathworld.wolfram.com/SpherePointPicking.html
    /// let mut gen_normalized_vec3 = || -> Vec3 {
    ///     let theta: f32 = TAU * thread_rng.gen::<f32>();
    ///     let phi: f32 = (2.0_f32 * thread_rng.gen::<f32>() - 1.0_f32).acos();
    ///     let sin_phi: f32 = phi.sin();
    ///
    ///     Vec3::new(sin_phi * theta.cos(), sin_phi * theta.sin(), phi.cos())
    /// };
    ///
    /// for polyhedron in Polyhedron::iter() {
    ///     let data: &Data = Data::get(polyhedron);
    ///
    ///     for face_data in data.faces.iter() {
    ///         let size: usize = face_data.get_size();
    ///
    ///         assert_eq!(face_data.get_rotation_quat(0_u32), Quat::IDENTITY);
    ///
    ///         let rand_normalized_vec3: Vec3 = gen_normalized_vec3();
    ///
    ///         assert!((face_data.get_rotation_quat(size as u32) * rand_normalized_vec3)
    ///             .abs_diff_eq(rand_normalized_vec3, SMALL_ERROR));
    ///
    ///         let local_up: Vec3 = face_data.quat * Vec3::Y;
    ///         let rotated_local_up: Vec3 = face_data.get_rotation_quat(1_u32) * local_up;
    ///
    ///         assert!(
    ///             (
    ///                 local_up.angle_between(rotated_local_up)
    ///                     - face_data.get_partial_rotation_angle()
    ///             )
    ///                 .abs()
    ///                 <= SMALL_ERROR
    ///         );
    ///     }
    /// }
    /// ```
    pub fn get_rotation_quat(&self, rotation: u32) -> Quat {
        Quat::from_axis_angle(
            self.norm,
            rotation as f32 * -self.get_partial_rotation_angle(),
        )
    }

    /// Returns this face's quaternion rotated a given number of consecutive clockwise rotations of
    /// angle `self.get_partial_rotation_angle()` radians
    ///
    /// # Arguments
    ///
    /// * `rotation` - The number of partial rotations to perform
    ///
    /// # Examples
    ///
    /// ```
    /// use {
    ///     pentultimate_solver::math::polyhedra::{data::Data, Polyhedron},
    ///     std::f32::consts::{PI, TAU},
    ///     strum::IntoEnumIterator,
    /// };
    /// const SMALL_ERROR: f32 = 1.0E-6_f32;
    ///
    /// for polyhedron in Polyhedron::iter() {
    ///     let data: &Data = Data::get(polyhedron);
    ///
    ///     for face_data in data.faces.iter() {
    ///         let size: usize = face_data.get_size();
    ///
    ///         assert_eq!(face_data.get_rotated_quat(0_u32), face_data.quat);
    ///
    ///         let partial_rotation_angle: f32 = face_data.get_partial_rotation_angle();
    ///         let expected_angle_between = |rotation: u32| -> f32 {
    ///             let full_rotation_angle: f32 = rotation as f32 * partial_rotation_angle;
    ///
    ///             if full_rotation_angle > PI {
    ///                 TAU - full_rotation_angle
    ///             } else {
    ///                 full_rotation_angle
    ///             }
    ///         };
    ///
    ///         for rotation in 1_u32..size as u32 {
    ///             assert!(
    ///                 (face_data
    ///                     .get_rotated_quat(rotation)
    ///                     .angle_between(face_data.quat)
    ///                     - expected_angle_between(rotation))
    ///                 .abs()
    ///                     <= SMALL_ERROR
    ///             );
    ///         }
    ///     }
    /// }
    /// ```
    pub fn get_rotated_quat(&self, rotation: u32) -> Quat {
        self.get_rotation_quat(rotation) * self.quat
    }
}

#[derive(Debug)]
pub struct Data {
    pub verts: Vec<Vec3>,
    pub edges: Vec<EdgeData>,
    pub vert_indices: Vec<usize>,
    pub faces: Vec<FaceData>,
}

pub type IndexedPredicate<'a, T> = &'a dyn Fn(usize, &T) -> bool;
pub type OptionalIndexedPredicate<'a, T> = Option<IndexedPredicate<'a, T>>;

impl Data {
    pub fn initialize() {
        DataLibrary::build();
    }

    /// get()
    ///
    /// It is the caller's responsibility to ensure Data::initialize() was called first. Calling it
    /// a second time has no side effects
    pub fn get(polyhedron: Polyhedron) -> &'static Self {
        &DataLibrary::get().0[polyhedron as usize]
    }

    pub fn get_closest_vert_index(
        &self,
        vec: &Vec3,
        filter: OptionalIndexedPredicate<Vec3>,
    ) -> usize {
        Data::get_closest_vert_index_for_verts(&self.verts, vec, filter)
    }

    pub fn get_closest_vert_index_for_verts(
        verts: &[Vec3],
        vec: &Vec3,
        filter: OptionalIndexedPredicate<Vec3>,
    ) -> usize {
        let filter: IndexedPredicate<Vec3> =
            filter.unwrap_or(&|_: usize, _: &Vec3| -> bool { true });

        verts
            .iter()
            .enumerate()
            .filter(|(vert_index, vert): &(usize, &Vec3)| -> bool { filter(*vert_index, vert) })
            .map(|(vert_index, vert): (usize, &Vec3)| -> (usize, f32) {
                (vert_index, vert.dot(*vec))
            })
            .max_by(
                |(_vert_index_a, norm_dot_vec_a): &(usize, f32),
                 (_vert_index_b, norm_dot_vec_b): &(usize, f32)|
                 -> Ordering {
                    norm_dot_vec_a
                        .partial_cmp(norm_dot_vec_b)
                        .unwrap_or(Ordering::Equal)
                },
            )
            .map(|(vert_index, _norm_dot_vec): (usize, f32)| -> usize { vert_index })
            .unwrap()
    }

    pub fn get_edge_index(&self, edge_data: &EdgeData) -> Result<usize, usize> {
        self.edges.binary_search(edge_data)
    }

    pub fn get_closest_face_index(
        &self,
        vec: &Vec3,
        filter: OptionalIndexedPredicate<FaceData>,
    ) -> usize {
        Data::get_closest_face_index_for_faces(&self.faces, vec, filter)
    }

    pub fn get_closest_face_index_for_faces(
        faces: &[FaceData],
        vec: &Vec3,
        filter: OptionalIndexedPredicate<FaceData>,
    ) -> usize {
        let filter: IndexedPredicate<FaceData> =
            filter.unwrap_or(&|_: usize, _: &FaceData| -> bool { true });

        faces
            .iter()
            .enumerate()
            .filter(|(face_index, face): &(usize, &FaceData)| -> bool { filter(*face_index, face) })
            .map(|(face_index, face): (usize, &FaceData)| -> (usize, f32) {
                (face_index, face.norm.dot(*vec))
            })
            .max_by(
                |(_face_index_a, norm_dot_vec_a): &(usize, f32),
                 (_face_index_b, norm_dot_vec_b): &(usize, f32)|
                 -> Ordering {
                    norm_dot_vec_a
                        .partial_cmp(norm_dot_vec_b)
                        .unwrap_or(Ordering::Equal)
                },
            )
            .map(|(face_index, _norm_dot_vec): (usize, f32)| -> usize { face_index })
            .unwrap()
    }

    pub fn get_pos_and_rot(
        &self,
        quat: &Quat,
        filter: OptionalIndexedPredicate<FaceData>,
    ) -> (usize, usize) {
        let pos: usize = self.get_closest_face_index(&(*quat * Vec3::Z), filter);

        (pos, self.faces[pos].get_rotation(quat))
    }

    pub fn as_mesh(&self, primitive_topology: PrimitiveTopology) -> Option<Mesh> {
        if primitive_topology == PrimitiveTopology::LineStrip
            || primitive_topology == PrimitiveTopology::TriangleStrip
        {
            return None;
        }

        let mut mesh: Mesh = Mesh::new(primitive_topology);
        let mut positions: Vec<[f32; 3]> = Vec::<[f32; 3]>::new();
        let mut normals: Vec<[f32; 3]> = Vec::<[f32; 3]>::new();
        let mut uvs: Vec<[f32; 2]> = Vec::<[f32; 2]>::new();
        let mut append_vert = |vert_index: usize| {
            let vert: &Vec3 = &self.verts[vert_index];
            positions.push(*vert.as_ref());
            normals.push(*vert.normalize_or_zero().as_ref());
            uvs.push([0.0, 0.0]);
        };
        let mut append_all_verts = || {
            for vert_index in 0..self.verts.len() {
                append_vert(vert_index);
            }
        };

        match primitive_topology {
            PrimitiveTopology::PointList => {
                append_all_verts();
            }
            PrimitiveTopology::LineList => {
                self.edges.iter().for_each(|edge_data: &EdgeData| {
                    append_vert(edge_data.0);
                    append_vert(edge_data.1);
                });
            }
            PrimitiveTopology::TriangleList => {
                let vert_indices: &Vec<usize> = &self.vert_indices;
                let mut indices: Vec<u32> = Vec::<u32>::new();

                for face_data in self.faces.iter() {
                    let initial_index: u32 = positions.len() as u32;

                    for vert_index in vert_indices[face_data.get_range()].iter() {
                        positions.push(*self.verts[*vert_index].as_ref());
                        normals.push(*face_data.norm.as_ref());
                        uvs.push([0.0, 0.0]);
                    }

                    for slice_index in 1..(face_data.get_size() - 1) as u32 {
                        indices.push(initial_index);
                        indices.push(initial_index + slice_index);
                        indices.push(initial_index + slice_index + 1);
                    }
                }

                mesh.set_indices(Some(Indices::U32(indices)));
            }
            _ => {
                return None;
            }
        }

        mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
        mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
        mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);

        Some(mesh)
    }

    fn new() -> Self {
        Self {
            verts: Vec::<Vec3>::new(),
            edges: Vec::<EdgeData>::new(),
            vert_indices: Vec::<usize>::new(),
            faces: Vec::<FaceData>::new(),
        }
    }
}

#[cfg(test)]
impl Data {
    fn validate_polyhedra() -> LogErrorResult {
        let log_target: String = log_path!("validate_polyhedra").to_string();
        let mut data_array: [MaybeUninit<Data>; 4_usize] =
            unsafe { MaybeUninit::<[MaybeUninit<Data>; 4_usize]>::uninit().assume_init() };

        for (data, polyhedron) in data_array.iter_mut().zip(Polyhedron::iter()) {
            DataBuilder {
                data: data.write(Data::new()),
                polyhedron,
            }
            .generate_checked()?;
        }

        // Safe: we just initialized each element in data_array
        let data_array: [Data; 4_usize] = unsafe { transmute(data_array) };

        let validate_dual_polyhedra = |polyhedron_a: Polyhedron| -> LogErrorResult {
            let polyhedron_b: Polyhedron = polyhedron_a.dual();
            let properties_a: &Properties = Properties::get(polyhedron_a);
            let properties_b: &Properties = Properties::get(polyhedron_b);
            let data_a: &Data = &data_array[polyhedron_a as usize];
            let data_b: &Data = &data_array[polyhedron_b as usize];

            if properties_a.vert_count != properties_b.face_count {
                return Err(log_error!(
                    target: log_target,
                    Level::Warn,
                    format!(
                        "Polyhedron {:?}'s vertex count doesn't equal polyhedron {:?}'s face count",
                        polyhedron_a, polyhedron_b
                    )
                ));
            }

            for (index, (vert_a, face_b)) in
                data_a.verts.iter().zip(data_b.faces.iter()).enumerate()
            {
                let vert_norm_a: Vec3 = vert_a.normalize_or_zero();

                if !vert_norm_a.abs_diff_eq(face_b.norm, f32::EPSILON) {
                    return Err(log_error!(
                        target: log_target,
                        Level::Warn,
                        format!(
                            "Vert {}'s normal vector ({:?}) of polyhedron {:?} isn't close enough \
                                to face {}'s normal vector ({:?}) of polyhedron {:?}",
                            index, vert_norm_a, polyhedron_a, index, face_b.norm, polyhedron_b
                        )
                    ));
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
    data: &'a mut Data,
    polyhedron: Polyhedron,
}

impl<'a> DataBuilder<'a> {
    fn generate(&mut self) {
        self.generate_verts();
        self.generate_edges();
        warn_expect_ok!(self.generate_faces());
    }

    fn generate_verts(&mut self) {
        let properties: &Properties = self.polyhedron.properties();
        let verts: &mut Vec<Vec3> = &mut self.data.verts;

        verts.clear();
        verts.reserve_exact(properties.vert_count);
        Self::generate_verts_for_properties(properties, verts);
    }

    fn generate_verts_for_properties(properties: &Properties, verts: &mut Vec<Vec3>) {
        match properties.polyhedron {
            Polyhedron::Icosahedron => {
                let base_vector: Vec3 = properties.base_vectors[0];
                let mut perm_mat: Mat3 = Mat3::IDENTITY;

                for _perm in 0..3 {
                    for vert in 0..4 {
                        verts.push(
                            perm_mat
                                * Mat3::from(ReflectionMat3::new(
                                    vert & 0b10 != 0,
                                    vert & 0b01 != 0,
                                    false,
                                ))
                                * base_vector,
                        );
                    }

                    perm_mat = PERMUTE_AXES * perm_mat;
                }
            }
            Polyhedron::Dodecahedron => {
                let base_vector: Vec3 = properties.base_vectors[0];
                let mut perm_mat: Mat3 = Mat3::IDENTITY;

                for _perm in 0..3 {
                    for vert in 0..4 {
                        verts.push(
                            perm_mat
                                * Mat3::from(ReflectionMat3::new(
                                    vert & 0b10 != 0,
                                    false,
                                    vert & 0b01 != 0,
                                ))
                                * base_vector,
                        );
                    }

                    perm_mat = PERMUTE_AXES * perm_mat;
                }

                let base_vector: Vec3 = properties.base_vectors[1];

                for vert in 0..8 {
                    verts.push(
                        Mat3::from(ReflectionMat3::new(
                            vert & 0b100 != 0,
                            vert & 0b010 != 0,
                            vert & 0b001 != 0,
                        )) * base_vector,
                    );
                }
            }
            Polyhedron::Icosidodecahedron => {
                let base_vector_1: Vec3 = properties.base_vectors[0];
                let base_vector_2: Vec3 = properties.base_vectors[1];
                let mut perm_mat: Mat3 = Mat3::IDENTITY;

                for _perm in 0..3 {
                    for vert in 0..8 {
                        if vert & 0b11 == 0 {
                            verts.push(
                                perm_mat
                                    * Mat3::from(ReflectionMat3::new(
                                        vert & 0b100 != 0,
                                        false,
                                        false,
                                    ))
                                    * base_vector_1,
                            );
                        }

                        verts.push(
                            perm_mat
                                * Mat3::from(ReflectionMat3::new(
                                    vert & 0b100 != 0,
                                    vert & 0b010 != 0,
                                    vert & 0b001 != 0,
                                ))
                                * base_vector_2,
                        );
                    }

                    perm_mat = PERMUTE_AXES * perm_mat;
                }
            }
            Polyhedron::RhombicTriacontahedron => {
                Self::generate_verts_for_properties(&polyhedra::properties::ICOSAHEDRON, verts);
                Self::generate_verts_for_properties(&polyhedra::properties::DODECAHEDRON, verts);
            }
        }
    }

    fn generate_edges(&mut self) {
        let properties: &Properties = self.polyhedron.properties();
        let verts: &Vec<Vec3> = &self.data.verts;
        let edges: &mut Vec<EdgeData> = &mut self.data.edges;

        edges.clear();
        edges.reserve_exact(properties.edge_count);

        let distance_squared_threshold: f32 =
            properties.edge_length * properties.edge_length + 0.000001;

        for (vert_index_1, vert_1) in verts.iter().enumerate() {
            let vert_index_1_plus_1: usize = vert_index_1 + 1_usize;

            for (vert_index_2_offset, vert_2) in verts[vert_index_1_plus_1..].iter().enumerate() {
                if vert_1.distance_squared(*vert_2) <= distance_squared_threshold {
                    edges.push(EdgeData(
                        vert_index_1,
                        vert_index_1_plus_1 + vert_index_2_offset,
                    ));
                }
            }
        }
    }

    fn generate_faces(&mut self) -> LogErrorResult {
        type VertBitArray = BitArr!(for properties::MAX_VERT_COUNT, in u32);

        let properties: &Properties = self.polyhedron.properties();
        let should_be_initial_vert: fn(usize, usize) -> bool = self.get_should_be_initial_vert();
        let verts: &Vec<Vec3> = &self.data.verts;
        let edges: &Vec<EdgeData> = &self.data.edges;
        let vert_indices: &mut Vec<usize> = &mut self.data.vert_indices;
        let faces: &mut Vec<FaceData> = &mut self.data.faces;
        let all_edges_used: EdgeBitArray = {
            let mut all_edges_used: EdgeBitArray = EdgeBitArray::default();

            all_edges_used[0_usize..properties.edge_count].fill(true);

            all_edges_used
        };
        let log_target: String = log_path!("generate_faces").into();

        let mut edge_to_index: fnv::FnvHashMap<EdgeData, usize> = fnv::FnvHashMap::default();
        let mut edge_matrix: Vec<VertBitArray> =
            vec![VertBitArray::default(); properties.vert_count];
        let mut forward_edges: EdgeBitArray = EdgeBitArray::default();
        let mut backward_edges: EdgeBitArray = EdgeBitArray::default();

        vert_indices.clear();
        faces.clear();
        faces.reserve_exact(properties.face_count);

        for (edge_index, edge) in edges.iter().enumerate() {
            edge_matrix[edge.0].set(edge.1, true);
            edge_matrix[edge.1].set(edge.0, true);
            edge_to_index.insert(*edge, edge_index);
        }

        macro_rules! log_edge_status {
            () => {
                if log_enabled!(
                    target: log_concat!(log_target, "edge_status"),
                    log::Level::Trace
                ) {
                    let mut status_update: String = "Edge Status:\n   ".to_string();
                    let cell_width: usize = if properties.vert_count >= 0xF { 3 } else { 2 };

                    for vert_index_x in 0..properties.vert_count {
                        status_update
                            .push_str(format!("{0:^1$x}", vert_index_x, cell_width).as_str());
                    }

                    status_update.push('\n');

                    for (vert_index_y, adjacent_verts) in edge_matrix.iter().enumerate() {
                        status_update.push_str(format!("{:>2x} ", vert_index_y).as_str());

                        for vert_index_x in 0..properties.vert_count {
                            status_update.push_str(
                                format!(
                                    "{0:^1$}",
                                    if adjacent_verts[vert_index_x] {
                                        'X'
                                    } else {
                                        '.'
                                    },
                                    cell_width
                                )
                                .as_str(),
                            );
                        }

                        status_update.push('\n');
                    }

                    status_update.push_str(format!("\n{:22}F B\n", "").as_str());

                    for (edge_index, edge) in edges.iter().enumerate() {
                        status_update.push_str(
                            format!(
                                "{:>3x}: {:2?} {} {}\n",
                                edge_index,
                                edge,
                                if forward_edges[edge_index] { 'X' } else { '.' },
                                if backward_edges[edge_index] { 'X' } else { '.' }
                            )
                            .as_str(),
                        );
                    }

                    log::trace!(
                        target: log_concat!(log_target, "edge_status"),
                        "{}",
                        status_update
                    );
                }
            };
        }

        log_edge_status!();

        while forward_edges != all_edges_used && backward_edges != all_edges_used {
            const INVALID_VERT_INDEX: usize = usize::MAX;
            let initial_edge: EdgeData = edges[forward_edges.leading_ones()];
            let start: usize = vert_indices.len();
            let mut end: usize = start;
            let mut prev_vert_index: usize = 0;
            let mut curr_vert_index: usize = initial_edge.0;
            let mut next_vert_index: usize = initial_edge.1;
            let mut face_edges: EdgeBitArray = EdgeBitArray::default();

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
                    let edge: EdgeData = ((prev_vert_index, curr_vert_index))
                        .try_into()
                        .map_err(|_: ()| -> LogError {
                            log_error!(
                                target: log_concat!(log_target, "record_edge"),
                                Level::Warn,
                                format!(
                                    "Cannot record edge with only the vertex {} in polyhedron {:?}",
                                    prev_vert_index,
                                    properties.polyhedron
                                )
                            )
                        })?;

                    log::trace!(
                        target: log_concat!(log_target, "record_edge"),
                        "Recording edge {:?}",
                        edge
                    );

                    edge_matrix[prev_vert_index].set(curr_vert_index, false);

                    if let Some(edge_index) = edge_to_index.get(&edge).cloned() {
                        if prev_vert_index <= curr_vert_index {
                            forward_edges.set(edge_index, true);
                        } else {
                            backward_edges.set(edge_index, true);
                        }

                        face_edges.set(edge_index, true);
                    } else {
                        return Err(log_error!(
                            target: log_concat!(log_target, "record_edge"),
                            Level::Warn,
                            format!(
                                "No valid next vertex for previous vertex {} ({}) and current \
                                    vertex {} ({}) in polyhedron {:?}",
                                prev_vert_index,
                                verts[prev_vert_index],
                                curr_vert_index,
                                verts[curr_vert_index],
                                properties.polyhedron
                            )
                        ));
                    }
                };
            }

            macro_rules! find_next_vert {
                () => {
                    next_vert_index = {
                        let mut candidate_vert_indices: VertBitArray = edge_matrix[curr_vert_index];

                        if candidate_vert_indices.not_any() {
                            log::warn!(
                                target: log_concat!(log_target, "find_next_vert"),
                                "No candidate next vertices for vertex {} in polyhedron {:?}",
                                curr_vert_index,
                                properties.polyhedron
                            );

                            INVALID_VERT_INDEX
                        } else {
                            let curr_vert_vector: Vec3 = verts[curr_vert_index];
                            let prev_vert_to_curr_vert_vector: Vec3 =
                                curr_vert_vector - verts[prev_vert_index];
                            let mut best_vert_index: usize = INVALID_VERT_INDEX;
                            let mut best_vert_angle: f32 = 0.0;

                            while candidate_vert_indices.any() {
                                let candidate_vert_index: usize =
                                    candidate_vert_indices.leading_zeros() as usize;

                                log::trace!(
                                    target: log_concat!(log_target, "find_next_vert"),
                                    "Considering candidate vertex {}",
                                    candidate_vert_index
                                );

                                candidate_vert_indices.set(candidate_vert_index, false);

                                // Don't double back
                                if candidate_vert_index == prev_vert_index {
                                    log::trace!(
                                        target: log_concat!(log_target, "find_next_vert"),
                                        "Double-back, rejecting"
                                    );

                                    continue;
                                }

                                let curr_vert_to_candidate_vert_vector: Vec3 =
                                    verts[candidate_vert_index] - curr_vert_vector;

                                if prev_vert_to_curr_vert_vector
                                    .cross(curr_vert_vector)
                                    .dot(curr_vert_to_candidate_vert_vector)
                                    >= 0.0_f32
                                {
                                    log::trace!(
                                        target: log_concat!(log_target, "find_next_vert"),
                                        "Clockwise, rejecting"
                                    );

                                    continue;
                                }

                                let candidate_vert_angle: f32 = prev_vert_to_curr_vert_vector
                                    .angle_between(curr_vert_to_candidate_vert_vector);

                                if best_vert_index == INVALID_VERT_INDEX
                                    || best_vert_angle <= candidate_vert_angle
                                {
                                    best_vert_index = candidate_vert_index;
                                    best_vert_angle = candidate_vert_angle;
                                } else {
                                    log::trace!(
                                        target: log_concat!(log_target, "find_next_vert"),
                                        "Worse angle ({} (best) > {} (candidate))",
                                        best_vert_angle,
                                        candidate_vert_angle
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
                            "No valid next vertex for previous vertex {} ({}) and current vertex \
                                {} ({}) in polyhedron {:?}",
                            prev_vert_index,
                            verts[prev_vert_index],
                            curr_vert_index,
                            verts[curr_vert_index],
                            properties.polyhedron
                        )
                    ));
                }
            }

            cycle_values!();
            record_edge!();
            faces.push(FaceData::new(verts, vert_indices, start..end, face_edges));
            log::trace!(target: log_target.as_str(), "Adding face {:?}", faces.last().unwrap());
            log_edge_status!();
        }

        let dual_verts: Vec<Vec3> = {
            let dual_properties: &Properties = Properties::get(properties.polyhedron.dual());
            let mut verts: Vec<Vec3> = Vec::<Vec3>::with_capacity(dual_properties.vert_count);

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
                        "Couldn't find vertex in face {}'s slice, {:?}, that satisfies the \
                            condition necessary for it to be the initial vertex",
                        face_index, face_slice
                    )
                ));
            }

            face_data.quat = {
                let first_vert: Vec3 = verts[*face_slice.first().unwrap()];
                let negative_face_average: Vec3 = -(face_slice
                    .iter()
                    .map(|vert_index: &usize| -> &Vec3 { &verts[*vert_index] })
                    .sum::<Vec3>()
                    / face_slice.len() as f32);

                Quat::from_mat4(
                    &Mat4::look_at_rh(
                        Vec3::ZERO,
                        negative_face_average,
                        (first_vert + negative_face_average).normalize(),
                    )
                    .inverse(),
                )
            };
        }

        Ok(())
    }

    fn get_should_be_initial_vert(&self) -> fn(usize, usize) -> bool {
        match self.polyhedron {
            Polyhedron::Icosahedron => |face_index: usize, vert_index: usize| -> bool {
                if face_index < properties::ICOSAHEDRON.vert_count {
                    ((vert_index as isize) - (face_index as isize)).abs() > 1
                } else {
                    vert_index >> 2 == 1
                }
            },
            Polyhedron::Dodecahedron => |face_index: usize, vert_index: usize| -> bool {
                vert_index < properties::DODECAHEDRON.face_count
                    && ((vert_index as isize) - (face_index as isize)).abs() > 1
            },
            Polyhedron::Icosidodecahedron => |face_index: usize, vert_index: usize| -> bool {
                if face_index < 2 * properties::ICOSAHEDRON.vert_count {
                    vert_index % 5 == 0
                } else {
                    const X_MAJOR_VERT_INDEX_END: usize =
                        properties::ICOSIDODECAHEDRON.vert_count / 3;

                    vert_index < X_MAJOR_VERT_INDEX_END
                }
            },
            Polyhedron::RhombicTriacontahedron => |face_index: usize, vert_index: usize| -> bool {
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
            |data: &Data| -> (&Vec<Vec3>,) { (&data.verts,) },
        )?;
        self.generate_elements_checked(
            Self::generate_edges_checked,
            "edges",
            |data: &Data| -> (&Vec<Vec3>, &Vec<EdgeData>) { (&data.verts, &data.edges) },
        )?;
        self.generate_elements_checked(
            Self::generate_faces_checked,
            "faces",
            |data: &Data| -> (&Vec<EdgeData>, &Vec<usize>, &Vec<FaceData>) {
                (&data.edges, &data.vert_indices, &data.faces)
            },
        )?;

        Ok(())
    }

    fn generate_elements_checked<'b, T: std::fmt::Debug>(
        &'b mut self,
        func: fn(&mut Self) -> LogErrorResult,
        elements: &str,
        get_relevant_data: fn(&'b Data) -> T,
    ) -> LogErrorResult {
        if let Err(log_error) = func(self) {
            log_error.log();

            Err(log_error!(
                target: log_path!("generate"),
                Level::Warn,
                format!(
                    "Failed to generate {} for polyhedron {:?}",
                    elements, self.polyhedron
                ),
                Level::Info,
                format!("Relevant data: {:#?}", get_relevant_data(self.data))
            ))
        } else {
            Ok(())
        }
    }

    fn generate_verts_checked(&mut self) -> LogErrorResult {
        self.generate_verts();

        let properties: &Properties = self.polyhedron.properties();
        let verts: &Vec<Vec3> = &self.data.verts;

        log::trace!(
            target: log_path!("generate_verts"),
            "{:?} verts: {:#?}",
            properties.polyhedron,
            verts
        );

        if verts.len() == properties.vert_count {
            Ok(())
        } else {
            Err(log_error!(
                target: log_path!("generate_verts"),
                Level::Warn,
                format!(
                    "Found vertex count ({}) did not match expected vertex count ({})",
                    verts.len(),
                    properties.vert_count
                )
            ))
        }
    }

    fn generate_edges_checked(&mut self) -> LogErrorResult {
        self.generate_edges();

        let properties: &Properties = self.polyhedron.properties();
        let edges: &Vec<EdgeData> = &self.data.edges;

        log::trace!(
            target: log_path!("generate_edges"),
            "{:?} edges: {:#?}",
            properties.polyhedron,
            edges
        );

        if edges.len() == properties.edge_count {
            Ok(())
        } else {
            Err(log_error!(
                target: log_path!("generate_edges"),
                Level::Warn,
                format!(
                    "Found edge count ({}) did not match expected edge count ({})",
                    edges.len(),
                    properties.edge_count
                )
            ))
        }
    }

    fn generate_faces_checked(&mut self) -> LogErrorResult {
        self.generate_faces()?;

        let properties: &Properties = self.polyhedron.properties();
        let vert_indices: &Vec<usize> = &self.data.vert_indices;
        let faces: &Vec<FaceData> = &self.data.faces;
        let log_target: String = log_path!("generate_faces_checked").into();
        let should_be_initial_vert: fn(usize, usize) -> bool = self.get_should_be_initial_vert();

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

        for face_size_index in 0..properties.face_sizes.len() - 1 {
            let min_face_index: usize = properties.face_sizes[face_size_index].initial_face_index;
            let max_face_index: usize =
                properties.face_sizes[face_size_index + 1].initial_face_index;
            let face_size: usize = properties.face_sizes[face_size_index].face_size;

            if min_face_index >= properties.face_count || max_face_index > properties.face_count {
                return Err(log_error!(
                    target: log_target,
                    Level::Warn,
                    format!("Face size data {} is invalid", face_size_index)
                ));
            }

            for (face_index, face_data) in faces[min_face_index..max_face_index]
                .iter()
                .enumerate()
                .map(
                    |(face_slice_index, face_data): (usize, &FaceData)| -> (usize, &FaceData) {
                        (face_slice_index + min_face_index, face_data)
                    },
                )
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
                            "Face {}'s initial vertex index ({}) did not satisfy the requisite \
                            condition for it to be the initial vertex index",
                            face_index, initial_vert_index
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
        let mut data_array: [MaybeUninit<Data>; 4_usize] =
            unsafe { MaybeUninit::<[MaybeUninit<Data>; 4_usize]>::uninit().assume_init() };

        for (data, polyhedron) in data_array.iter_mut().zip(Polyhedron::iter()) {
            DataBuilder {
                data: data.write(Data::new()),
                polyhedron,
            }
            .generate();
        }

        DataLibrary(unsafe {
            transmute::<[MaybeUninit<Data>; 4_usize], [Data; 4_usize]>(data_array)
        })
    }
}

impl StaticDataLibrary for DataLibrary {
    type Target = &'static Self;

    fn get() -> &'static Self {
        &DATA_LIBRARY
    }
}

lazy_static! {
    static ref DATA_LIBRARY: DataLibrary = DataLibrary::new();
}

pub struct DataPlugin;

impl DataPlugin {
    pub fn startup() {
        DataLibrary::build();
    }
}

impl Plugin for DataPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_system(Self::startup);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_data() {
        init_env_logger();
        break_assert!(Data::validate_polyhedra().is_ok());
    }
}
