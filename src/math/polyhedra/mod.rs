use {
    bevy_inspector_egui::Inspectable,
    serde::{Deserialize, Serialize},
    strum_macros::EnumIter,
};

/// A module for the `Data` struct, its related types, and its static instances
pub mod data;

/// A module for the `Properties` struct, its related types, and its static instances
pub mod properties;

/// One of the polyhedra used by this crate
#[derive(
    Clone,
    Copy,
    Debug,
    Deserialize,
    EnumIter,
    Eq,
    Hash,
    Inspectable,
    Ord,
    PartialEq,
    PartialOrd,
    Serialize,
)]
pub enum Polyhedron {
    /// A [regular icosahedron](https://en.wikipedia.org/wiki/Regular_icosahedron) (20 regular
    /// triangles)
    Icosahedron,

    /// A [regular dodecahedron](https://en.wikipedia.org/wiki/Regular_dodecahedron) (12 regular
    /// pentagons)
    Dodecahedron,

    /// An [icosidodecahedron](https://en.wikipedia.org/wiki/Icosidodecahedron) (12 regular
    /// pentagons, 20 regular triangles)
    Icosidodecahedron,

    /// A [rhombic triacontahedron](https://en.wikipedia.org/wiki/Rhombic_triacontahedron) (30
    /// rhombi)
    RhombicTriacontahedron,
}

impl Polyhedron {
    /// Convert `self` to its [dual polyhedron](https://en.wikipedia.org/wiki/Dual_polyhedron)
    pub fn dual(self) -> Self {
        match self {
            Self::Icosahedron => Self::Dodecahedron,
            Self::Dodecahedron => Self::Icosahedron,
            Self::Icosidodecahedron => Self::RhombicTriacontahedron,
            Self::RhombicTriacontahedron => Self::Icosidodecahedron,
        }
    }

    /// Get the static `Properties` instance for `self`
    #[inline(always)]
    pub const fn properties(self) -> &'static properties::Properties {
        properties::Properties::get(self)
    }
}

impl Default for Polyhedron {
    fn default() -> Self {
        Self::Icosidodecahedron
    }
}
