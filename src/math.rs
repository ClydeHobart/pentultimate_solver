pub use bevy::math::{
	*,
	prelude::*
};

pub mod polyhedra;

pub const PHI:					f64 = 1.6180339887498950_f64;
pub const ONE_OVER_PHI:			f64 = 0.61803398874989490_f64;
pub const ONE_OVER_PHI_SQUARED:	f64 = 0.38196601125010515_f64;

#[macro_export]
macro_rules! clamp {
	($val:expr, $min:expr, $max:expr) => {
		std::cmp::min(
			std::cmp::max(
				$val, $min
			),
			$max
		)
	};
}

#[derive(Clone, Copy)]
pub struct ReflectionMat3 {
	pub matrix: Mat3
}

pub const PERMUTE_AXES: Mat3 = const_mat3!([0.0, 1.0, 0.0], [0.0, 0.0, 1.0], [1.0, 0.0, 0.0]);

impl ReflectionMat3 {
	pub fn new(reflect_x: bool, reflect_y: bool, reflect_z: bool) -> ReflectionMat3 {
		ReflectionMat3 {
			matrix: Mat3::from_diagonal(vec3(
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

pub trait ExtendVec4 : Sized {
	fn from_xyz(xyz: &Vec3) -> Self { Self::from_xyz_w(xyz, 0.0f32) }
	fn from_xyw(xyw: &Vec3) -> Self { Self::from_xyw_z(xyw, 0.0f32) }
	fn from_xzw(xzw: &Vec3) -> Self { Self::from_xzw_y(xzw, 0.0f32) }
	fn from_yzw(yzw: &Vec3) -> Self { Self::from_yzw_x(yzw, 0.0f32) }

	fn from_xyz_w(xyz: &Vec3, w: f32) -> Self;
	fn from_xyw_z(xyw: &Vec3, z: f32) -> Self;
	fn from_xzw_y(xzw: &Vec3, y: f32) -> Self;
	fn from_yzw_x(yzw: &Vec3, x: f32) -> Self;
}

impl ExtendVec4 for Vec4 {
	fn from_xyz_w(xyz: &Vec3, w: f32) -> Self { Self::new(xyz[0], xyz[1], xyz[2], w) }
	fn from_xyw_z(xyw: &Vec3, z: f32) -> Self { Self::new(xyw[0], xyw[1], z, xyw[2]) }
	fn from_xzw_y(xzw: &Vec3, y: f32) -> Self { Self::new(xzw[0], y, xzw[1], xzw[2]) }
	fn from_yzw_x(yzw: &Vec3, x: f32) -> Self { Self::new(x, yzw[0], yzw[1], yzw[2]) }
}

pub mod two_d {
	pub use super::*;

	pub enum ComputeInterpolantResult {
		Interpolants([f32; 2]),
		Coincident,
		Parallel
	}

	pub fn compute_interpolant(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2) -> ComputeInterpolantResult {
		// This uses the math from Paul Bourke (though not the code from Damian Coventry) from the City College of New York:
		// http://www-cs.ccny.cuny.edu/~wolberg/capstone/intersection/Intersection%20point%20of%20two%20lines.html
		let diff_2_1: Vec2 = p2 - p1;
		let diff_4_3: Vec2 = p4 - p3;
		let diff_1_3: Vec2 = p1 - p3;
		let denominator: f32 = diff_4_3.y * diff_2_1.x - diff_4_3.x * diff_2_1.y;
		let numerators: Vec2 = Vec2::new(
			diff_4_3.x * diff_1_3.y - diff_4_3.y * diff_1_3.x,
			diff_2_1.x * diff_1_3.y - diff_2_1.y * diff_1_3.x
		);

		if denominator.abs() > f32::EPSILON {
			ComputeInterpolantResult::Interpolants(*(numerators / denominator).as_ref())
		} else if numerators.abs_diff_eq(Vec2::ZERO, f32::EPSILON) {
			ComputeInterpolantResult::Coincident
		} else {
			ComputeInterpolantResult::Parallel
		}
	}

	pub enum ComputeIntersectionResult {
		Intersection(Vec2),
		Coincident,
		Disjoint
	}

	impl From<ComputeIntersectionResult> for Option<Vec2> {
		fn from(cir: ComputeIntersectionResult) -> Self {
			match cir {
				ComputeIntersectionResult::Intersection(intersection) => Some(intersection),
				_ => None
			}
		}
	}

	pub fn compute_intersection(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, extrapolate: bool, optional_cir: Option<ComputeInterpolantResult>) -> ComputeIntersectionResult {
		match optional_cir.unwrap_or_else(|| compute_interpolant(p1, p2, p3, p4)) {
			ComputeInterpolantResult::Interpolants(interpolants) => {
				if extrapolate ||
					interpolants[0].abs() <= f32::EPSILON && (1.0_f32 - interpolants[0]).abs() <= f32::EPSILON &&
					interpolants[1].abs() <= f32::EPSILON && (1.0_f32 - interpolants[1]).abs() <= f32::EPSILON
				{
					ComputeIntersectionResult::Intersection(p1 * (1.0_f32 - interpolants[0]) + p2 * interpolants[0])
				} else {
					ComputeIntersectionResult::Disjoint
				}
			},
			ComputeInterpolantResult::Coincident	=> ComputeIntersectionResult::Coincident,
			ComputeInterpolantResult::Parallel		=> ComputeIntersectionResult::Disjoint
		}
	}

	pub fn compute_segment_intersection(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, optional_cir: Option<ComputeInterpolantResult>) -> ComputeIntersectionResult {
		compute_intersection(p1, p2, p3, p4, false, optional_cir)
	}

	pub fn compute_line_intersection(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, optional_cir: Option<ComputeInterpolantResult>) -> ComputeIntersectionResult {
		compute_intersection(p1, p2, p3, p4, true, optional_cir)
	}
}