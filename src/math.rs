pub use bevy::math::prelude::*;

pub mod polyhedra;

pub const PHI:			f64 = 1.6180339887498950_f64;
pub const ONE_OVER_PHI:	f64 = 0.61803398874989490_f64;

#[derive(Clone, Copy)]
pub struct ReflectionMat3 {
	pub matrix: Mat3
}

pub const PERMUTE_AXES: Mat3 = bevy::math::const_mat3!([0.0, 1.0, 0.0], [0.0, 0.0, 1.0], [1.0, 0.0, 0.0]);

impl ReflectionMat3 {
	pub fn new(reflect_x: bool, reflect_y: bool, reflect_z: bool) -> ReflectionMat3 {
		ReflectionMat3 {
			matrix: Mat3::from_diagonal(bevy::math::vec3(
					if reflect_x { -1.0 } else { 1.0 },
					if reflect_y { -1.0 } else { 1.0 },
					if reflect_z { -1.0 } else { 1.0 }
				))
		}
	}
}

impl From<[bool; 3]> for ReflectionMat3 {
	fn from(reflect_axes: [bool; 3]) -> Self {
		Self::new(reflect_axes[0], reflect_axes[1], reflect_axes[2])
	}
}

impl From<BVec3> for ReflectionMat3 {
	fn from(reflect_axes_vector: BVec3) -> Self {
		type BoolArray = [bool; 3];

		Self::from(BoolArray::from(reflect_axes_vector))
	}
}

impl AsRef<Mat3> for ReflectionMat3 {
	fn as_ref(&self) -> &Mat3 {
		&self.matrix
	}
}

impl From<ReflectionMat3> for Mat3 {
	fn from(reflection_matrix: ReflectionMat3) -> Self {
		reflection_matrix.matrix
	}
}

// Modification of C++ code from https://www.geeksforgeeks.org/find-root-of-a-number-using-newtons-method/
pub const fn const_sqrt_f64(square: f64) -> f64 {
	let mut guess: f64 = square;
	let mut root: f64;

	loop {
		root = 0.5 * (guess + (square / guess));

		if root == guess {
			break;
		}

		guess = root;
	}

	root
}