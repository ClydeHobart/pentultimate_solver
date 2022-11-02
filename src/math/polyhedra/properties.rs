use crate::math::{polyhedra::Polyhedron, *};

pub struct FaceSizeData {
    pub initial_face_index: usize,
    pub face_size: usize,
}

impl FaceSizeData {
    pub const fn new(initial_vert_index: usize, face_size: usize) -> Self {
        Self {
            initial_face_index: initial_vert_index,
            face_size,
        }
    }
}

pub struct Properties {
    pub polyhedron: Polyhedron,
    pub vert_count: usize,
    pub edge_count: usize,
    pub face_count: usize,
    pub base_vectors: &'static [Vec3],
    pub edge_length: f32,
    pub face_sizes: &'static [FaceSizeData],
}

impl Properties {
    pub const fn get(polyhedron: Polyhedron) -> &'static Self {
        match polyhedron {
            Polyhedron::Icosahedron => &ICOSAHEDRON,
            Polyhedron::Dodecahedron => &DODECAHEDRON,
            Polyhedron::Icosidodecahedron => &ICOSIDODECAHEDRON,
            Polyhedron::RhombicTriacontahedron => &RHOMBIC_TRIACONTAHEDRON,
        }
    }
}

pub const ICOSAHEDRON: Properties = Properties {
    polyhedron: Polyhedron::Icosahedron,
    vert_count: 12,
    edge_count: 30,
    face_count: 20,
    base_vectors: &[const_vec3!([PHI as f32, 1.0, 0.0])],
    edge_length: 2.0,
    face_sizes: &[FaceSizeData::new(0, 3), FaceSizeData::new(20, 0)],
};

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

pub const MAX_VERT_COUNT: usize = crate::max!(
    ICOSAHEDRON.vert_count,
    DODECAHEDRON.vert_count,
    ICOSIDODECAHEDRON.vert_count,
    RHOMBIC_TRIACONTAHEDRON.vert_count
);

pub const MAX_EDGE_COUNT: usize = crate::max!(
    ICOSAHEDRON.edge_count,
    DODECAHEDRON.edge_count,
    ICOSIDODECAHEDRON.edge_count,
    RHOMBIC_TRIACONTAHEDRON.edge_count
);
