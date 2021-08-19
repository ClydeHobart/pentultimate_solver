pub use {
	core::arch::x86_64::{
		self,
		__m128i as m128,
		__m256i as m256
	},
	std::mem::{
		size_of,
		transmute,
		transmute_copy,
		MaybeUninit
	}
};

macro_rules! m256_from_array {
	($array:expr, $element_type:ty) => {
		{
			type Array = [$element_type; size_of::<m256>() / size_of::<$element_type>()];

			static_assertions::assert_eq_size!(Array, m256);

			unsafe {
				std::mem::transmute::<Array, m256>($array)
			}
		}
	};
}

pub mod consts {
	use super::*;

	pub const ZERO: m256 = m256_from_array!([0, 0, 0, 0], u64);
	pub const TWELVE_FIVES_TWENTY_THREES: [m256; 4] = [
		m256_from_array!([
			5, 5, 5, 5,
			5, 5, 5, 5
		], u32),
		m256_from_array!([
			5, 5, 5, 5,
			3, 3, 3, 3
		], u32),
		m256_from_array!([
			3, 3, 3, 3,
			3, 3, 3, 3
		], u32),
		m256_from_array!([
			3, 3, 3, 3,
			3, 3, 3, 3
		], u32)
	];

	pub mod inflate {
		use super::*;

		/*
		Stages for converting between deflated::PuzzleState and inflated::PuzzleState (when only AVX2 is available):
			* Stage 1: Cast *deflated_puzzle_state: deflated::PuzzleState into stage_1: m256
			* Stage 2: Space out data from u8s to u32s
				* Part 1: Run _mm256_permute4x64_epi64() for the 3 u64s not stored in the lowest 8B of stage_1, storing the result in part_1: [m256; 4] (part_1[0] is just stage_1)
				* Part 2: Run _mm256_cvtepu8_epi32() on part_1, storing the result in part_2: [m256; 4]
			* Stage 3: Multiply pent and tri u16s by 26 and 43, respectively, storing the result in stage_3: [m256; 4]
			* Stage 4: Right shift stage_3 by 7, storing the result in result[0], where result is a [[m256; 4]; 2]
			* Stage 5: Multiply result[0] by 5 and 3 for the pents and tris, respectively, storing the result in stage_5: [m256; 4]
			* Stage 6: Subtract stage_5 from part_2, storing the result in result[1]
			* Stage 7: Transmute result
		*/
		pub const STAGE_2_1_1_IMM8: i32 = 1_i32;
		pub const STAGE_2_1_2_IMM8: i32 = 2_i32;
		pub const STAGE_2_1_3_IMM8: i32 = 3_i32;
		pub const STAGE_3_B: [m256; 4] = [
			m256_from_array!([
				26, 26, 26, 26,
				26, 26, 26, 26
			], u32),
			m256_from_array!([
				26, 26, 26, 26,
				43, 43, 43, 43
			], u32),
			m256_from_array!([
				43, 43, 43, 43,
				43, 43, 43, 43
			], u32),
			m256_from_array!([
				43, 43, 43, 43,
				43, 43, 43, 43
			], u32)
		];
		pub const STAGE_4_IMM8: i32 = 7_i32;
		pub const STAGE_5_B: [m256; 4] = TWELVE_FIVES_TWENTY_THREES;
	}

	pub mod deflate {
		use super::*;

		/*
		Stages for converting between inflated::PuzzleState and deflated::PuzzleState (when only AVX2 is available):
			* Stage 1: Cast inflated_puzzle_state: &inflated::PuzzleState into stage_1: *const [[m256; 4]; 2]
			* Stage 2: Multiply stage_1[0] by 5 and 3 for the pents and tris, respectively, storing the result in stage_2: [m256; 4]
			* Stage 3: Add stage_2 and stage_1[1], storing the result in stage_3: [m256; 4]
			* Stage 4: Pack the data from u32s to u8s

				|*--------------- m256 0, u128 0
				| |*------------- m256 2, u128 0
				| | |*----------- m256 0, u128 1
				| | | |*--------- m256 2, u128 1
				| | | | |*------- m256 1, u128 0
				| | | | | |*----- m256 3, u128 0
				| | | | | | |*--- m256 1, u128 1
				| | | | | | | |*- m256 3, u128 1
				| | | | | | | | | // 8 bundles of 4 u32s (8 u128s, part_1)
				| |X X| | |X X| |
				| | | | | | | | | // 8 bundles of 4 u16s (8 u64s, part_2)
				| | |\ X X /| | |
				| | | X X X | | |
				| | |/ X X \| | |
				| | | | | | | | | // 8 bundles of 4 u8s (8 u32s, part_3)

				* Part 1: Run _mm256_permute2x128_si256::<0x20_i32>() and _mm256_permute2x128_si256::<0x31_i32>(), storing the result in part_1: [m256; 4]
				* Part 2: Run _mm256_packus_epi32(), storing the result in part_2: [m256; 2]
				* Part 3: Run _mm256_packus_epi16(), storing the result in result: m256
			* Stage 5: Transmute result

		*/
		pub const STAGE_2_B: [m256; 4] = TWELVE_FIVES_TWENTY_THREES;
		pub const STAGE_4_1_0_IMM8: i32 = 0x20_i32;
		pub const STAGE_4_1_1_IMM8: i32 = 0x31_i32;
	}
}

pub mod inflate {
	#[macro_export]
	macro_rules! util_simd_inflate_inflate_puzzle_state {
		($deflated_puzzle_state:expr, $deflated_puzzle_state_type:ty, $inflated_puzzle_state_type:ty) => {
			{
				use simd::{
					*,
					consts::{
						*,
						inflate::*
					}
				};

				static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);
				static_assertions::assert_eq_size!($inflated_puzzle_state_type, [[m256; 4]; 2]);

				unsafe {
					let mut result: [[m256; 4]; 2] = [[ZERO, ZERO, ZERO, ZERO], [ZERO, ZERO, ZERO, ZERO]];

					// Stage 1: Cast deflated_puzzle_state: &deflated::PuzzleState into stage_1: *const m256
					let mut stage_1: m256 = transmute_copy::<$deflated_puzzle_state_type, m256>($deflated_puzzle_state);

					// Stage 2: Space out data from u8s to u32s
					//   Part 1: Run _mm256_permute4x64_epi64() for the 3 u64s not stored in the lowest 8B of stage_1, storing the result in part_1: [m256; 4] (part_1[0] is just stage_1)
					let part_1: [m256; 4] = [
						stage_1,
						x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_1_IMM8>(stage_1),
						x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_2_IMM8>(stage_1),
						x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_3_IMM8>(stage_1)
					];

					//   Part 2: Run _mm256_cvtepu8_epi32() on part_1, storing the result in part_2: [m256; 4]
					let part_2: [m256; 4] = [
						x86_64::_mm256_cvtepu8_epi32(*(&part_1[0] as *const m256 as *const m128)),
						x86_64::_mm256_cvtepu8_epi32(*(&part_1[1] as *const m256 as *const m128)),
						x86_64::_mm256_cvtepu8_epi32(*(&part_1[2] as *const m256 as *const m128)),
						x86_64::_mm256_cvtepu8_epi32(*(&part_1[3] as *const m256 as *const m128))
					];

					// Stage 3: Multiply pent and tri u32s from part_2 by 26 and 43, respectively, storing the result in stage_3: [m256; 4]
					let stage_3: [m256; 4] = [
						x86_64::_mm256_mullo_epi32(part_2[0], STAGE_3_B[0]),
						x86_64::_mm256_mullo_epi32(part_2[1], STAGE_3_B[1]),
						x86_64::_mm256_mullo_epi32(part_2[2], STAGE_3_B[2]),
						x86_64::_mm256_mullo_epi32(part_2[3], STAGE_3_B[3])
					];

					// Stage 4: Right shift stage_3 by 7, storing the result in result[0]
					result[0] = [
						x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[0]),
						x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[1]),
						x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[2]),
						x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[3])
					];

					// Stage 5: Multiply result[0] by 5 and 3 for the pents and tris, respectively, storing the result in stage_5: [m256; 4]
					let stage_5: [m256; 4] = [
						x86_64::_mm256_mullo_epi32(result[0][0], STAGE_5_B[0]),
						x86_64::_mm256_mullo_epi32(result[0][1], STAGE_5_B[1]),
						x86_64::_mm256_mullo_epi32(result[0][2], STAGE_5_B[2]),
						x86_64::_mm256_mullo_epi32(result[0][3], STAGE_5_B[3])
					];

					// Stage 6: Subtract stage_5 from part_2, storing the result in result[1]
					result[1] = [
						x86_64::_mm256_sub_epi32(part_2[0], stage_5[0]),
						x86_64::_mm256_sub_epi32(part_2[1], stage_5[1]),
						x86_64::_mm256_sub_epi32(part_2[2], stage_5[2]),
						x86_64::_mm256_sub_epi32(part_2[3], stage_5[3])
					];

					// Stage 7: Transmute result
					transmute::<[[m256; 4]; 2], $inflated_puzzle_state_type>(result)
				}
			}
		};
	}

	#[macro_export]
	macro_rules! util_simd_inflate_inflate_puzzle_state_no_init {
		($deflated_puzzle_state:expr, $deflated_puzzle_state_type:ty, $inflated_puzzle_state_type:ty) => {
			{
				use simd::{
					*,
					consts::{
						*,
						inflate::*
					}
				};

				static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);
				static_assertions::assert_eq_size!($inflated_puzzle_state_type, [[m256; 4]; 2]);

				unsafe {
					let mut result: [[m256; 4]; 2] = MaybeUninit::<[[m256; 2]; 2]>::uninit().assume_init();

					// Stage 1: Cast deflated_puzzle_state: &deflated::PuzzleState into stage_1: *const m256
					let stage_1: *const m256 = $deflated_puzzle_state as *const $deflated_puzzle_state_type as *const m256;

					// Stage 2: Space out data from u8s to u16s
					//   Part 1: Run _mm256_unpacklo_epi8() and _mm256_unpackhi_epi8(), storing the result in part_1: [m256; 2]
					let part_1: [m256; 2] = [
						x86_64::_mm256_unpacklo_epi8(*stage_1, STAGE_2_1_B),
						x86_64::_mm256_unpackhi_epi8(*stage_1, STAGE_2_1_B)
					];

					//   Part 2: Run _mm256_permute2x128_si256::<0x20>() and _mm256_permute2x128_si256::<0x31>() on part_1, storing the result in part_2: [m256; 2]
					let part_2: [m256; 2] = [
						x86_64::_mm256_permute2x128_si256::<STAGE_2_2_0_IMM8>(part_1[0], part_1[1]),
						x86_64::_mm256_permute2x128_si256::<STAGE_2_2_1_IMM8>(part_1[0], part_1[1])
					];

					// Stage 3: Multiply pent and tri u16s by 26 and 43, respectively, storing the result in stage_3: [m256; 2]
					let stage_3: [m256; 2] = [
						x86_64::_mm256_mullo_epi16(part_2[0], STAGE_3_B[0]),
						x86_64::_mm256_mullo_epi16(part_2[1], STAGE_3_B[1])
					];

					// Stage 4: Right shift stage_3 by 7, storing the result in result[0]
					result[0] = [
						x86_64::_mm256_srai_epi16::<STAGE_4_IMM8>(stage_3[0]),
						x86_64::_mm256_srai_epi16::<STAGE_4_IMM8>(stage_3[1])
					];

					// STage 5: Multiply result[0] by 5 and 3 for the pents and tris, respectively, storing the result in stage_5: [m256; 2]
					let stage_5: [m256; 2] = [
						x86_64::_mm256_mullo_epi16(result[0][0], STAGE_5_B[0]),
						x86_64::_mm256_mullo_epi16(result[0][1], STAGE_5_B[1])
					];

					// Stage 6: Subtract stage_5 from part_2, storing the result in result[1]
					result[1] = [
						x86_64::_mm256_sub_epi16(part_2[0], stage_5[0]),
						x86_64::_mm256_sub_epi16(part_2[1], stage_5[1])
					];

					// Stage 7: Transmute result
					transmute::<[[m256; 2]; 2], $inflated_puzzle_state_type>(result)
				}
			}
		};
	}

	#[macro_export]
	macro_rules! util_simd_inflate_inflate_puzzle_state_no_arrays {
		($deflated_puzzle_state:expr, $deflated_puzzle_state_type:ty, $inflated_puzzle_state_type:ty) => {
			{
				use simd::{
					*,
					consts::{
						*,
						inflate::*
					}
				};

				const STAGE_3_B_0: m256 = STAGE_3_B[0];
				const STAGE_3_B_1: m256 = STAGE_3_B[1];
				const STAGE_5_B_0: m256 = STAGE_5_B[0];
				const STAGE_5_B_1: m256 = STAGE_5_B[1];

				static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);
				static_assertions::assert_eq_size!($inflated_puzzle_state_type, [[m256; 2]; 2]);

				unsafe {
					#[repr(align(32))]
					struct M256Result {
						result_0_0: m256,
						result_0_1: m256,
						result_1_0: m256,
						result_1_1: m256
					}

					let mut result: M256Result = M256Result {
						result_0_0: ZERO,
						result_0_1: ZERO,
						result_1_0: ZERO,
						result_1_1: ZERO
					};

					// Stage 1: Cast deflated_puzzle_state: &deflated::PuzzleState into stage_1: *const m256
					let stage_1: *const m256 = $deflated_puzzle_state as *const $deflated_puzzle_state_type as *const m256;

					// Stage 2: Space out data from u8s to u16s
					//   Part 1: Run _mm256_unpacklo_epi8() and _mm256_unpackhi_epi8(), storing the result in part_1: [m256; 2]
					let part_1_0: m256 = x86_64::_mm256_unpacklo_epi8(*stage_1, STAGE_2_1_B);
					let part_1_1: m256 = x86_64::_mm256_unpackhi_epi8(*stage_1, STAGE_2_1_B);

					//   Part 2: Run _mm256_permute2x128_si256::<0x20>() and _mm256_permute2x128_si256::<0x31>() on part_1, storing the result in part_2: [m256; 2]
					let part_2_0: m256 = x86_64::_mm256_permute2x128_si256::<STAGE_2_2_0_IMM8>(part_1_0, part_1_1);
					let part_2_1: m256 = x86_64::_mm256_permute2x128_si256::<STAGE_2_2_1_IMM8>(part_1_0, part_1_1);

					// Stage 3: Multiply pent and tri u16s by 26 and 43, respectively, storing the result in stage_3: [m256; 2]
					let stage_3_0: m256 = x86_64::_mm256_mullo_epi16(part_2_0, STAGE_3_B_0);
					let stage_3_1: m256 = x86_64::_mm256_mullo_epi16(part_2_1, STAGE_3_B_1);

					// Stage 4: Right shift stage_3 by 7, storing the result in result[0]
					result.result_0_0 = x86_64::_mm256_srai_epi16::<STAGE_4_IMM8>(stage_3_0);
					result.result_0_1 = x86_64::_mm256_srai_epi16::<STAGE_4_IMM8>(stage_3_1);

					// Stage 5: Multiply result[0] by 5 and 3 for the pents and tris, respectively, storing the result in stage_5: [m256; 2]
					let stage_5_0: m256 = x86_64::_mm256_mullo_epi16(result.result_0_0, STAGE_5_B_0);
					let stage_5_1: m256 = x86_64::_mm256_mullo_epi16(result.result_0_1, STAGE_5_B_1);

					// Stage 6: Subtract stage_5 from part_2, storing the result in result[1]
					result.result_1_0 = x86_64::_mm256_sub_epi16(part_2_0, stage_5_0);
					result.result_1_1 = x86_64::_mm256_sub_epi16(part_2_1, stage_5_1);

					// Stage 7: Transmute result
					transmute::<M256Result, $inflated_puzzle_state_type>(result)
				}
			}
		};
	}
}

pub mod deflate {
	#[macro_export]
	macro_rules! util_simd_deflate_deflate_puzzle_state {
		($inflated_puzzle_state:expr, $inflated_puzzle_state_type:ty, $deflated_puzzle_state_type:ty) => {
			{
				use simd::{
					*,
					consts::deflate::*
				};

				static_assertions::assert_eq_size!($inflated_puzzle_state_type, [[m256; 4]; 2]);
				static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);

				unsafe {
					// Stage 1: Cast inflated_puzzle_state: &inflated::PuzzleState into stage_1: *const [[m256; 4]; 2]
					let stage_1: *const [[m256; 4]; 2] = $inflated_puzzle_state as *const $inflated_puzzle_state_type as *const [[m256; 4]; 2];

					// error_expr!(fmt: ":#018x?", *stage_1);

					// Stage 2: Multiply stage_1[0] by 5 and 3 for the pents and tris, respectively, storing the result in stage_2: [m256; 2]
					let stage_2: [m256; 4] = [
						x86_64::_mm256_mullo_epi16((*stage_1)[0][0], STAGE_2_B[0]),
						x86_64::_mm256_mullo_epi16((*stage_1)[0][1], STAGE_2_B[1]),
						x86_64::_mm256_mullo_epi16((*stage_1)[0][2], STAGE_2_B[2]),
						x86_64::_mm256_mullo_epi16((*stage_1)[0][3], STAGE_2_B[3])
					];

					// error_expr!(fmt: ":#018x?", stage_2);

					// Stage 3: Add stage_2 and stage_1[1], storing the result in stage_3: [m256; 2]
					let stage_3: [m256; 4] = [
						x86_64::_mm256_add_epi16(stage_2[0], (*stage_1)[1][0]),
						x86_64::_mm256_add_epi16(stage_2[1], (*stage_1)[1][1]),
						x86_64::_mm256_add_epi16(stage_2[2], (*stage_1)[1][2]),
						x86_64::_mm256_add_epi16(stage_2[3], (*stage_1)[1][3])
					];

					// error_expr!(fmt: ":#018x?", stage_3);

					// Stage 4: Pack the data from u32s to u8s
					//   Part 1: Run _mm256_permute2x128_si256::<0x20_i32>() and _mm256_permute2x128_si256::<0x31_i32>(), storing the result in part_1: [m256; 4]
					let part_1: [m256; 4] = [
						x86_64::_mm256_permute2x128_si256::<STAGE_4_1_0_IMM8>(stage_3[0], stage_3[2]),
						x86_64::_mm256_permute2x128_si256::<STAGE_4_1_1_IMM8>(stage_3[0], stage_3[2]),
						x86_64::_mm256_permute2x128_si256::<STAGE_4_1_0_IMM8>(stage_3[1], stage_3[3]),
						x86_64::_mm256_permute2x128_si256::<STAGE_4_1_1_IMM8>(stage_3[1], stage_3[3])
					];

					// error_expr!(fmt: ":#018x?", part_1);

					//   Part 2: Run _mm256_packus_epi32(), storing the result in part_2: [m256; 2]
					let part_2: [m256; 2] = [
						x86_64::_mm256_packus_epi32(part_1[0], part_1[1]),
						x86_64::_mm256_packus_epi32(part_1[2], part_1[3])
					];

					// error_expr!(fmt: ":#018x?", part_2);

					//   Part 3: Run _mm256_packus_epi16(), storing the result in result: m256
					let result: m256 = x86_64::_mm256_packus_epi16(part_2[0], part_2[1]);

					// error_expr!(fmt: ":#018x?", result);

					// Stage 5: Transmute result
					transmute::<m256, $deflated_puzzle_state_type>(result)
				}
			}
		};
	}
}