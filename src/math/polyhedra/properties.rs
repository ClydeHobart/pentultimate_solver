use crate::math::{polyhedra::Polyhedron, *};

/// Given a `data: polyhedra::data::Data` struct, a `FaceSizeData` struct represents the beginning
/// of a range of faces within `data.faces` that have the same number of vertices.
pub struct FaceSizeData {
    /// The index within the `faces` field of a `Data` instance that `self` refers to
    ///
    /// Given two consecutive elements of a `[Self]`, the `initial_face_index` of the latter element
    /// defines when the range of the former element ends.
    pub initial_face_index: usize,

    /// The number of vertices on any face within the range specified by `self`
    ///
    /// The last element in a `[Self]` should have a `face_size` of 0, since it just indicates when
    /// the range of the previous element ends.
    pub face_size: usize,
}

impl FaceSizeData {
    /// A `const` constructor function for a `Self` that doesn't require typing the field names
    pub const fn new(initial_face_index: usize, face_size: usize) -> Self {
        Self {
            initial_face_index,
            face_size,
        }
    }
}

/// A `Properties` struct specifies key defining information for a `Polyhedron`.
pub struct Properties {
    /// The `Polyhedron` that `self` describes
    pub polyhedron: Polyhedron,

    /// The number of vertices that the polyhedron has
    pub vert_count: usize,

    /// The number of edges that the polyhedron has
    pub edge_count: usize,

    /// The number of faces that the polyhedron has
    pub face_count: usize,

    /// The `Vec3` vectors needed to construct all the vertices of the polyhedron
    pub base_vectors: &'static [Vec3],

    /// The resultant edge length of the polyhedron using `self.base_vectors`
    pub edge_length: f32,

    /// `FaceSizeData` elements describing how many vertices are on the different faces of the
    /// polyhedron
    ///
    /// Face-transitive polyhedra will have two elements, while non-face-transitive polyhedra will
    /// have more than two elements (see comment on `FaceSizeData::initial_face_index` for more
    /// info).
    pub face_sizes: &'static [FaceSizeData],
}

impl Properties {
    /// A `const` getter for the corresponding `&'static Self` of a given `Polyhedron` enum.
    pub const fn get(polyhedron: Polyhedron) -> &'static Self {
        match polyhedron {
            Polyhedron::Icosahedron => &ICOSAHEDRON,
            Polyhedron::Dodecahedron => &DODECAHEDRON,
            Polyhedron::Icosidodecahedron => &ICOSIDODECAHEDRON,
            Polyhedron::RhombicTriacontahedron => &RHOMBIC_TRIACONTAHEDRON,
        }
    }
}

/// The `Properties` of `Polyhedron::Icosahedron`
pub const ICOSAHEDRON: Properties = Properties {
    polyhedron: Polyhedron::Icosahedron,
    vert_count: 12,
    edge_count: 30,
    face_count: 20,
    base_vectors: &[const_vec3!([PHI as f32, 1.0, 0.0])],
    edge_length: 2.0,
    face_sizes: &[FaceSizeData::new(0, 3), FaceSizeData::new(20, 0)],
};

/// The `Properties` of `Polyhedron::Dodecahedron`
pub const DODECAHEDRON: Properties = Properties {
    polyhedron: Polyhedron::Dodecahedron,
    vert_count: ICOSAHEDRON.face_count,
    edge_count: ICOSAHEDRON.edge_count,
    face_count: ICOSAHEDRON.vert_count,
    base_vectors: &[
        const_vec3!([PHI as f32, 0.0, ONE_OVER_PHI as f32]),
        const_vec3!([1.0, 1.0, 1.0]),
    ],
    edge_length: (2.0 * ONE_OVER_PHI) as f32,
    face_sizes: &[
        FaceSizeData::new(0, 5),
        FaceSizeData::new(ICOSAHEDRON.vert_count, 0),
    ],
};

/// The `Properties` of `Polyhedron::Icosidodecahedron`
pub const ICOSIDODECAHEDRON: Properties = Properties {
    polyhedron: Polyhedron::Icosidodecahedron,
    vert_count: ICOSAHEDRON.edge_count,
    edge_count: 2 * ICOSAHEDRON.edge_count,
    face_count: DODECAHEDRON.face_count + ICOSAHEDRON.face_count,
    base_vectors: &[
        const_vec3!([PHI as f32, 0.0, 0.0]),
        const_vec3!([(0.5 * PHI * PHI) as f32, 0.5, (0.5 * PHI) as f32]),
    ],
    edge_length: 1.0,
    face_sizes: &[
        FaceSizeData::new(0, 5),
        FaceSizeData::new(DODECAHEDRON.face_count, 3),
        FaceSizeData::new(DODECAHEDRON.face_count + ICOSAHEDRON.face_count, 0),
    ],
};

/// The `Properties` of `Polyhedron::RhombicTriacontahedron`
pub const RHOMBIC_TRIACONTAHEDRON: Properties = Properties {
    polyhedron: Polyhedron::RhombicTriacontahedron,
    vert_count: ICOSIDODECAHEDRON.face_count,
    edge_count: ICOSIDODECAHEDRON.edge_count,
    face_count: ICOSIDODECAHEDRON.vert_count,
    base_vectors: &[
        ICOSAHEDRON.base_vectors[0],
        DODECAHEDRON.base_vectors[0],
        DODECAHEDRON.base_vectors[1],
    ],
    edge_length: const_sqrt_f64(1.0 + ONE_OVER_PHI * ONE_OVER_PHI) as f32,
    face_sizes: &[
        FaceSizeData::new(0, 4),
        FaceSizeData::new(ICOSIDODECAHEDRON.vert_count, 0),
    ],
};

/// The maximum number of vertices of the polyhedra enumerated by `Polyhedron` (32)
pub const MAX_VERT_COUNT: usize = crate::max!(
    ICOSAHEDRON.vert_count,
    DODECAHEDRON.vert_count,
    ICOSIDODECAHEDRON.vert_count,
    RHOMBIC_TRIACONTAHEDRON.vert_count
);

/// The maximum number of edges of the polyhedra enumerated by `Polyhedron` (60)
pub const MAX_EDGE_COUNT: usize = crate::max!(
    ICOSAHEDRON.edge_count,
    DODECAHEDRON.edge_count,
    ICOSIDODECAHEDRON.edge_count,
    RHOMBIC_TRIACONTAHEDRON.edge_count
);
