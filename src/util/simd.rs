use prelude::*;

macro_rules! m256_from_array {
    ($array:expr, $element_type:ty) => {{
        type Array = [$element_type; size_of::<m256>() / size_of::<$element_type>()];

        static_assertions::assert_eq_size!(Array, m256);

        unsafe { std::mem::transmute::<Array, m256>($array) }
    }};
}

pub mod consts {
    use super::*;

    pub mod prelude {
        use super::*;

        pub const ZERO: m256 = m256_from_array!([0, 0, 0, 0], u64);
        pub const PIECE_SIDE_COUNTS: [m256; 4] = [
            m256_from_array!([5, 5, 5, 5, 5, 5, 5, 5], u32),
            m256_from_array!([5, 5, 5, 5, 3, 3, 3, 3], u32),
            m256_from_array!([3, 3, 3, 3, 3, 3, 3, 3], u32),
            m256_from_array!([3, 3, 3, 3, 3, 3, 3, 3], u32),
        ];
        pub const PIECE_OFFSETS: [m256; 4] = [
            m256_from_array!([00, 00, 00, 00, 00, 00, 00, 00], u32),
            m256_from_array!([00, 00, 00, 00, 12, 12, 12, 12,], u32),
            m256_from_array!([12, 12, 12, 12, 12, 12, 12, 12,], u32),
            m256_from_array!([12, 12, 12, 12, 12, 12, 12, 12,], u32),
        ];
    }

    pub mod deflated {
        /*
        Stages for converting between inflated::PuzzleState and deflated::PuzzleState (when only AVX2 is available):
            * Stage 1: Cast inflated_puzzle_state: &inflated::PuzzleState into stage_1: *const [[m256; 4]; 2]
            * Stage 2: Left shift (*stage_1)[0] by 3, storing the result in stage_2: [m256; 4]
            * Stage 3: Compute the bitwise OR of stage_2 and (*stage_1)[1], storign the result in stage_3: [m256; 4]
            * Stage 4: Pack the data from u32s to u8s

                +--------------- m256 0, u128 0
                |   +----------- m256 0, u128 1
                |   |   +------- m256 1, u128 0
                |   |   |   +--- m256 1, u128 1
                | +-|---|---|--- m256 2, u128 0
                | | | +-|---|--- m256 2, u128 1
                | | | | | +-|--- m256 3, u128 0
                | | | | | | | +- m256 3, u128 1
                | | | | | | | | <- 8 bundles of 4 u32s (8 u128s, stage_4_1)
                |  X  | |  X  |
                | | | | | | | | <- 8 bundles of 4 u16s (8 u64s, stage_4_2)
                | |  \ X /  | |
                | |   X X   | |
                | |  / X \  | |
                | | | | | | | | <- 8 bundles of 4 u8s (8 u32s, result)

                * Part 1: Run _mm256_permute2x128_si256::<0x20_i32>() and _mm256_permute2x128_si256::<0x31_i32>(), storing the result in stage_4_1: [m256; 4]
                * Part 2: Run _mm256_packus_epi32(), storing the result in stage_4_2: [m256; 2]
                * Part 3: Run _mm256_packus_epi16(), storing the result in result: m256
            * Stage 5: Transmute result
        */

        pub const STAGE_2_IMM8: i32 = 3_i32;
        pub const STAGE_4_1_0_IMM8: i32 = 0x20_i32;
        pub const STAGE_4_1_1_IMM8: i32 = 0x31_i32;
    }

    pub mod inflated {
        use super::{prelude::*, *};

        pub mod inflate_puzzle_state {
            use super::*;

            /*
            Stages for converting between deflated::PuzzleState and inflated::PuzzleState (when only AVX2 is available):
                * Stage 1: Cast *deflated_puzzle_state: deflated::PuzzleState into stage_1: m256
                * Stage 2: Space out data from u8s to u32s
                    * Part 1: Run _mm256_permute4x64_epi64() for the 3 u64s not stored in the lowest 8B of stage_1, storing the result in stage_2_1: [m256; 4] (stage_2_1[0] is just stage_1)
                    * Part 2: Run _mm256_cvtepu8_epi32() on stage_2_1, storing the result in stage_2_2: [m256; 4]
                * Stage 3: Right shift stage_2_2 by 3, storing the result in result[0], where result is a [[m256; 4]; 2]
                * Stage 4: Compute the bitwise AND of stage_2_2 and 7 for each piece, storign the result in result[1]
                * Stage 5: Transmute result
            */

            pub const STAGE_2_1_1_IMM8: i32 = 1_i32;
            pub const STAGE_2_1_2_IMM8: i32 = 2_i32;
            pub const STAGE_2_1_3_IMM8: i32 = 3_i32;
            pub const STAGE_3_IMM8: i32 = 3_i32;
            pub const STAGE_4_B: [m256; 4] = [
                m256_from_array!(
                    [0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111],
                    u32
                ),
                m256_from_array!(
                    [0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111],
                    u32
                ),
                m256_from_array!(
                    [0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111],
                    u32
                ),
                m256_from_array!(
                    [0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111, 0b111],
                    u32
                ),
            ];
        }

        pub mod apply_transformation {
            use super::*;

            /*
            Stages for applying a transformation to an inflated::PuzzleState (when only AVX2 is available):
                * Stage 1: cast self: &mut PuzzleState into stage_1: *mut [[m256; 4]; 2]
                * Stage 2: cast transformation: &Transformation into stage_2: *const [[m256]; 2]
                * Stage 3: Run _mm256_i32gather_epi32::<4_i32>((*stage_2)[1], (*stage_1)[0]), storing the result in stage_3: [m256; 4]
                * Stage 4: Add stage_3 to (*stage_1)[1], storing the result in stage_4: [m256; 4]
                * Stage 5: Run _mm256_cmpgt_epi32() comparing stage_4 against (4 and 2 for pents and tris), storing the result in stage_5: [m256; 4]
                * Stage 6: Compute the bitwise AND of stage_5 and (5 and 3 for pents and tris), storing the result in stage_6: [m256; 4]
                * Stage 7: Run _mm256_sub_epi32(), subtracting stage_6 from stage_4, storing the result in (*stage_1)[1]
                * Stage 8: Run _mm256_i32gather_epi32::<4_i32>((*stage_2)[0], (*stage_1)[0]), storing the result in (*stage_1)[0]
            */

            pub const STAGE_3_IMM8: i32 = 4_i32;
            pub const STAGE_5_B: [m256; 4] = [
                m256_from_array!([4, 4, 4, 4, 4, 4, 4, 4], u32),
                m256_from_array!([4, 4, 4, 4, 2, 2, 2, 2], u32),
                m256_from_array!([2, 2, 2, 2, 2, 2, 2, 2], u32),
                m256_from_array!([2, 2, 2, 2, 2, 2, 2, 2], u32),
            ];
            pub const STAGE_6_B: [m256; 4] = PIECE_SIDE_COUNTS;
            pub const STAGE_8_IMM8: i32 = 4_i32;
        }
    }
}

pub mod deflated {
    #[macro_export]
    macro_rules! util_simd_deflated_deflate_puzzle_state {
        ($inflated_puzzle_state:expr, $inflated_puzzle_state_type:ty, $deflated_puzzle_state_type:ty) => {
            {
                use simd::{
                    prelude::*,
                    consts::deflated::*
                };

                static_assertions::assert_eq_size!($inflated_puzzle_state_type, [[m256; 4]; 2]);
                static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);

                unsafe {
                    // Stage 1: Cast inflated_puzzle_state: &inflated::PuzzleState into stage_1: *const [[m256; 4]; 2]
                    let stage_1: *const [[m256; 4]; 2] = $inflated_puzzle_state as *const $inflated_puzzle_state_type as *const [[m256; 4]; 2];

                    // error_expr!(fmt: ":#018x?", *stage_1);

                    // Stage 2: Left shift (*stage_1)[0] by 3, storing the result in stage_2: [m256; 4]
                    let stage_2: [m256; 4] = [
                        x86_64::_mm256_slli_epi32::<STAGE_2_IMM8>((*stage_1)[0][0]),
                        x86_64::_mm256_slli_epi32::<STAGE_2_IMM8>((*stage_1)[0][1]),
                        x86_64::_mm256_slli_epi32::<STAGE_2_IMM8>((*stage_1)[0][2]),
                        x86_64::_mm256_slli_epi32::<STAGE_2_IMM8>((*stage_1)[0][3])
                    ];

                    // error_expr!(fmt: ":#018x?", stage_2);

                    // Stage 3: Compute the bitwise OR of stage_2 and (*stage_1)[1], storign the result in stage_3: [m256; 4]
                    let stage_3: [m256; 4] = [
                        x86_64::_mm256_or_si256(stage_2[0], (*stage_1)[1][0]),
                        x86_64::_mm256_or_si256(stage_2[1], (*stage_1)[1][1]),
                        x86_64::_mm256_or_si256(stage_2[2], (*stage_1)[1][2]),
                        x86_64::_mm256_or_si256(stage_2[3], (*stage_1)[1][3])
                    ];

                    // error_expr!(fmt: ":#018x?", stage_3);

                    // Stage 4: Pack the data from u32s to u8s
                    //   Part 1: Run _mm256_permute2x128_si256::<0x20_i32>() and _mm256_permute2x128_si256::<0x31_i32>(), storing the result in stage_4_1: [m256; 4]
                    let stage_4_1: [m256; 4] = [
                        x86_64::_mm256_permute2x128_si256::<STAGE_4_1_0_IMM8>(stage_3[0], stage_3[2]),
                        x86_64::_mm256_permute2x128_si256::<STAGE_4_1_1_IMM8>(stage_3[0], stage_3[2]),
                        x86_64::_mm256_permute2x128_si256::<STAGE_4_1_0_IMM8>(stage_3[1], stage_3[3]),
                        x86_64::_mm256_permute2x128_si256::<STAGE_4_1_1_IMM8>(stage_3[1], stage_3[3])
                    ];

                    // error_expr!(fmt: ":#018x?", stage_4_1);

                    //   Part 2: Run _mm256_packus_epi32(), storing the result in stage_4_2: [m256; 2]
                    let stage_4_2: [m256; 2] = [
                        x86_64::_mm256_packus_epi32(stage_4_1[0], stage_4_1[1]),
                        x86_64::_mm256_packus_epi32(stage_4_1[2], stage_4_1[3])
                    ];

                    // error_expr!(fmt: ":#018x?", stage_4_2);

                    //   Part 3: Run _mm256_packus_epi16(), storing the result in result: m256
                    let result: m256 = x86_64::_mm256_packus_epi16(stage_4_2[0], stage_4_2[1]);

                    // error_expr!(fmt: ":#018x?", result);

                    // Stage 6: Transmute result
                    transmute::<m256, $deflated_puzzle_state_type>(result)
                }
            }
        };
    }
}

pub mod inflated {
    #[macro_export]
    macro_rules! util_simd_inflated_inflate_puzzle_state {
        ($deflated_puzzle_state:expr, $deflated_puzzle_state_type:ty, $inflated_puzzle_state_type:ty) => {
            {
                use simd::{
                    prelude::*,
                    consts::{
                        prelude::*,
                        inflated::inflate_puzzle_state::*
                    }
                };

                static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);
                static_assertions::assert_eq_size!($inflated_puzzle_state_type, [[m256; 4]; 2]);

                unsafe {
                    let mut result: [[m256; 4]; 2] = [[ZERO, ZERO, ZERO, ZERO], [ZERO, ZERO, ZERO, ZERO]];

                    // Stage 1: Cast *deflated_puzzle_state: deflated::PuzzleState into stage_1: m256
                    let stage_1: m256 = transmute_copy::<$deflated_puzzle_state_type, m256>($deflated_puzzle_state);

                    // Stage 2: Space out data from u8s to u32s
                    //   Part 1: Run _mm256_permute4x64_epi64() for the 3 u64s not stored in the lowest 8B of stage_1, storing the result in stage_2_1: [m256; 4] (stage_2_1[0] is just stage_1)
                    let stage_2_1: [m256; 4] = [
                        stage_1,
                        x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_1_IMM8>(stage_1),
                        x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_2_IMM8>(stage_1),
                        x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_3_IMM8>(stage_1)
                    ];

                    //   Part 2: Run _mm256_cvtepu8_epi32() on stage_2_1, storing the result in stage_2_2: [m256; 4]
                    let stage_2_2: [m256; 4] = [
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[0] as *const m256 as *const m128)),
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[1] as *const m256 as *const m128)),
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[2] as *const m256 as *const m128)),
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[3] as *const m256 as *const m128))
                    ];

                    // Stage 3: Right shift stage_2_2 by 3, storing the result in result[0], where result is a [[m256; 4]; 2]
                    result[0] = [
                        x86_64::_mm256_srli_epi32::<STAGE_3_IMM8>(stage_2_2[0]),
                        x86_64::_mm256_srli_epi32::<STAGE_3_IMM8>(stage_2_2[1]),
                        x86_64::_mm256_srli_epi32::<STAGE_3_IMM8>(stage_2_2[2]),
                        x86_64::_mm256_srli_epi32::<STAGE_3_IMM8>(stage_2_2[3])
                    ];

                    // Stage 4: Compute the bitwise AND of stage_2_2 and 7 for each piece, storign the result in result[1]
                    result[1] = [
                        x86_64::_mm256_and_si256(stage_2_2[0], STAGE_4_B[0]),
                        x86_64::_mm256_and_si256(stage_2_2[1], STAGE_4_B[1]),
                        x86_64::_mm256_and_si256(stage_2_2[2], STAGE_4_B[2]),
                        x86_64::_mm256_and_si256(stage_2_2[3], STAGE_4_B[3])
                    ];

                    // Stage 7: Transmute result
                    transmute::<[[m256; 4]; 2], $inflated_puzzle_state_type>(result)
                }
            }
        };
    }

    #[macro_export]
    macro_rules! util_simd_inflated_inflate_puzzle_state_no_init {
        ($deflated_puzzle_state:expr, $deflated_puzzle_state_type:ty, $inflated_puzzle_state_type:ty) => {
            {
                use simd::{
                    prelude::*,
                    consts::{
                        *,
                        inflated::inflate_puzzle_state::*
                    }
                };

                static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);
                static_assertions::assert_eq_size!($inflated_puzzle_state_type, [[m256; 4]; 2]);

                panic!();

                /*
                unsafe {
                    let mut result: [[m256; 4]; 2] = MaybeUninit::<[[m256; 4]; 2]>::uninit().assume_init();

                    // Stage 1: Cast *deflated_puzzle_state: deflated::PuzzleState into stage_1: m256
                    let stage_1: m256 = transmute_copy::<$deflated_puzzle_state_type, m256>($deflated_puzzle_state);

                    // Stage 2: Space out data from u8s to u32s
                    //   Part 1: Run _mm256_permute4x64_epi64() for the 3 u64s not stored in the lowest 8B of stage_1, storing the result in stage_2_1: [m256; 4] (stage_2_1[0] is just stage_1)
                    let stage_2_1: [m256; 4] = [
                        stage_1,
                        x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_IMM8>(stage_1),
                        x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_IMM8>(stage_1),
                        x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_IMM8>(stage_1)
                    ];

                    //   Part 2: Run _mm256_cvtepu8_epi32() on stage_2_1, storing the result in stage_2_2: [m256; 4]
                    let stage_2_2: [m256; 4] = [
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[0] as *const m256 as *const m128)),
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[1] as *const m256 as *const m128)),
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[2] as *const m256 as *const m128)),
                        x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1[3] as *const m256 as *const m128))
                    ];

                    // Stage 3: Multiply stage_2_2 by (26 and 43 for pents and tris), storing the result in stage_3: [m256; 4]
                    let stage_3: [m256; 4] = [
                        x86_64::_mm256_mullo_epi32(stage_2_2[0], STAGE_3_B[0]),
                        x86_64::_mm256_mullo_epi32(stage_2_2[1], STAGE_3_B[1]),
                        x86_64::_mm256_mullo_epi32(stage_2_2[2], STAGE_3_B[2]),
                        x86_64::_mm256_mullo_epi32(stage_2_2[3], STAGE_3_B[3])
                    ];

                    // Stage 4: Right shift stage_3 by 7, storing the result in stage_4: [m256; 4]
                    let stage_4: [m256; 4] = [
                        x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[0]),
                        x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[1]),
                        x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[2]),
                        x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3[3])
                    ];

                    // Stage 5: Add (0 and 12 for pents and tris) to stage_4, storing the result in result[0], where result is a [[m256; 4]; 2]
                    result[0] = [
                        x86_64::_mm256_add_epi32(stage_4[0], STAGE_5_B[0]),
                        x86_64::_mm256_add_epi32(stage_4[1], STAGE_5_B[1]),
                        x86_64::_mm256_add_epi32(stage_4[2], STAGE_5_B[2]),
                        x86_64::_mm256_add_epi32(stage_4[3], STAGE_5_B[3])
                    ];

                    // Stage 6: Multiply stage_4 by (5 and 3 for the pents and tris), storing the result in stage_6: [m256; 4]
                    let stage_6: [m256; 4] = [
                        x86_64::_mm256_mullo_epi32(stage_4[0], STAGE_6_B[0]),
                        x86_64::_mm256_mullo_epi32(stage_4[1], STAGE_6_B[1]),
                        x86_64::_mm256_mullo_epi32(stage_4[2], STAGE_6_B[2]),
                        x86_64::_mm256_mullo_epi32(stage_4[3], STAGE_6_B[3])
                    ];

                    // Stage 7: Subtract stage_6 from stage_2_2, storing the result in result[1]
                    result[1] = [
                        x86_64::_mm256_sub_epi32(stage_2_2[0], stage_6[0]),
                        x86_64::_mm256_sub_epi32(stage_2_2[1], stage_6[1]),
                        x86_64::_mm256_sub_epi32(stage_2_2[2], stage_6[2]),
                        x86_64::_mm256_sub_epi32(stage_2_2[3], stage_6[3])
                    ];

                    // Stage 7: Transmute result
                    transmute::<[[m256; 4]; 2], $inflated_puzzle_state_type>(result)
                }
                */
            }
        };
    }

    #[macro_export]
    macro_rules! util_simd_inflated_inflate_puzzle_state_no_arrays {
        ($deflated_puzzle_state:expr, $deflated_puzzle_state_type:ty, $inflated_puzzle_state_type:ty) => {
            {
                use simd::{
                    prelude::*,
                    consts::{
                        prelude::*,
                        inflated::inflate_puzzle_state::*
                    }
                };

                /*
                const STAGE_3_B_0: m256 = STAGE_3_B[0];
                const STAGE_3_B_1: m256 = STAGE_3_B[1];
                const STAGE_3_B_2: m256 = STAGE_3_B[2];
                const STAGE_3_B_3: m256 = STAGE_3_B[3];

                const STAGE_5_B_0: m256 = STAGE_5_B[0];
                const STAGE_5_B_1: m256 = STAGE_5_B[1];
                const STAGE_5_B_2: m256 = STAGE_5_B[2];
                const STAGE_5_B_3: m256 = STAGE_5_B[3];

                const STAGE_6_B_0: m256 = STAGE_6_B[0];
                const STAGE_6_B_1: m256 = STAGE_6_B[1];
                const STAGE_6_B_2: m256 = STAGE_6_B[2];
                const STAGE_6_B_3: m256 = STAGE_6_B[3];
                */

                #[repr(align(32))]
                struct M256Result {
                    result_0_0:    m256,
                    result_0_1:    m256,
                    result_0_2:    m256,
                    result_0_3:    m256,
                    result_1_0:    m256,
                    result_1_1:    m256,
                    result_1_2:    m256,
                    result_1_3:    m256
                }

                static_assertions::assert_eq_size!($deflated_puzzle_state_type, m256);
                static_assertions::assert_eq_size!($inflated_puzzle_state_type, M256Result);

                panic!();

                /*
                unsafe {
                    let mut result: M256Result = M256Result {
                        result_0_0: ZERO,
                        result_0_1: ZERO,
                        result_0_2: ZERO,
                        result_0_3: ZERO,
                        result_1_0: ZERO,
                        result_1_1: ZERO,
                        result_1_2: ZERO,
                        result_1_3: ZERO
                    };

                    // Stage 1: Cast *deflated_puzzle_state: deflated::PuzzleState into stage_1: m256
                    let stage_1: m256 = transmute_copy::<$deflated_puzzle_state_type, m256>($deflated_puzzle_state);

                    // Stage 2: Space out data from u8s to u32s
                    //   Part 1: Run _mm256_permute4x64_epi64() for the 3 u64s not stored in the lowest 8B of stage_1, storing the result in stage_2_1: [m256; 4] (stage_2_1[0] is just stage_1)
                    let stage_2_1_0: m256 = stage_1;
                    let stage_2_1_1: m256 = x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_IMM8>(stage_1);
                    let stage_2_1_2: m256 = x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_IMM8>(stage_1);
                    let stage_2_1_3: m256 = x86_64::_mm256_permute4x64_epi64::<STAGE_2_1_IMM8>(stage_1);

                    //   Part 2: Run _mm256_cvtepu8_epi32() on stage_2_1, storing the result in stage_2_2: [m256; 4]
                    let stage_2_2_0: m256 = x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1_0 as *const m256 as *const m128));
                    let stage_2_2_1: m256 = x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1_1 as *const m256 as *const m128));
                    let stage_2_2_2: m256 = x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1_2 as *const m256 as *const m128));
                    let stage_2_2_3: m256 = x86_64::_mm256_cvtepu8_epi32(*(&stage_2_1_3 as *const m256 as *const m128));

                    // Stage 3: Multiply pent and tri u32s from part_2 by 26 and 43, respectively, storing the result in stage_3: [m256; 4]
                    let stage_3_0: m256 = x86_64::_mm256_mullo_epi32(stage_2_2_0, STAGE_3_B_0);
                    let stage_3_1: m256 = x86_64::_mm256_mullo_epi32(stage_2_2_1, STAGE_3_B_1);
                    let stage_3_2: m256 = x86_64::_mm256_mullo_epi32(stage_2_2_2, STAGE_3_B_2);
                    let stage_3_3: m256 = x86_64::_mm256_mullo_epi32(stage_2_2_3, STAGE_3_B_3);

                    // Stage 4: Right shift stage_3 by 7, storing the result in stage_4: [m256; 4]
                    let stage_4_0: m256 = x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3_0);
                    let stage_4_1: m256 = x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3_1);
                    let stage_4_2: m256 = x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3_2);
                    let stage_4_3: m256 = x86_64::_mm256_srai_epi32::<STAGE_4_IMM8>(stage_3_3);

                    // Stage 5: Add (0 and 12 for pents and tris) to stage_4, storing the result in result[0], where result is a [[m256; 4]; 2]
                    result.result_0_0 = x86_64::_mm256_add_epi32(stage_4_0, STAGE_5_B_0);
                    result.result_0_1 = x86_64::_mm256_add_epi32(stage_4_1, STAGE_5_B_1);
                    result.result_0_2 = x86_64::_mm256_add_epi32(stage_4_2, STAGE_5_B_2);
                    result.result_0_3 = x86_64::_mm256_add_epi32(stage_4_3, STAGE_5_B_3);

                    // Stage 6: Multiply stage_4 by (5 and 3 for the pents and tris), storing the result in stage_6: [m256; 4]
                    let stage_6_0: m256 = x86_64::_mm256_mullo_epi32(stage_4_0, STAGE_6_B_0);
                    let stage_6_1: m256 = x86_64::_mm256_mullo_epi32(stage_4_1, STAGE_6_B_1);
                    let stage_6_2: m256 = x86_64::_mm256_mullo_epi32(stage_4_2, STAGE_6_B_2);
                    let stage_6_3: m256 = x86_64::_mm256_mullo_epi32(stage_4_3, STAGE_6_B_3);

                    // Stage 7: Subtract stage_6 from stage_2_2, storing the result in result[1]
                    result.result_1_0 = x86_64::_mm256_sub_epi32(stage_2_2_0, stage_6_0);
                    result.result_1_1 = x86_64::_mm256_sub_epi32(stage_2_2_1, stage_6_1);
                    result.result_1_2 = x86_64::_mm256_sub_epi32(stage_2_2_2, stage_6_2);
                    result.result_1_3 = x86_64::_mm256_sub_epi32(stage_2_2_3, stage_6_3);

                    // Stage 8: Transmute result
                    transmute::<M256Result, $inflated_puzzle_state_type>(result)
                }
                */
            }
        };
    }

    #[macro_export]
    macro_rules! util_simd_inflated_apply_transformation {
        ($ref_mut_self:expr, $self_type:ty, $ref_transformation:expr, $transformation_type:ty) => {
            use simd::{
                prelude::*,
                consts::inflated::apply_transformation::*
            };

            static_assertions::assert_eq_size!($self_type, $transformation_type, [[m256; 4]; 2]);

            unsafe {
                // Stage 1: cast self: &mut PuzzleState into stage_1: *mut [[m256; 4]; 2]
                let stage_1: *mut [[m256; 4]; 2] = $ref_mut_self as *mut $self_type as *mut [[m256; 4]; 2];

                // Stage 2: cast transformation: &Transformation into stage_2: *const [[m256]; 2]
                let stage_2: *const [[m256; 4]; 2] = $ref_transformation as *const $transformation_type as *const [[m256; 4]; 2];

                // Stage 3: Run _mm256_i32gather_epi32::<4_i32>((*stage_2)[1], (*stage_1)[0]), storing the result in stage_3: [m256; 4]
                let stage_3: [m256; 4] = [
                    x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*stage_2)[1] as *const [m256; 4] as *const i32, (*stage_1)[0][0]),
                    x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*stage_2)[1] as *const [m256; 4] as *const i32, (*stage_1)[0][1]),
                    x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*stage_2)[1] as *const [m256; 4] as *const i32, (*stage_1)[0][2]),
                    x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*stage_2)[1] as *const [m256; 4] as *const i32, (*stage_1)[0][3])
                ];

                // Stage 4: Add stage_3 to (*stage_1)[1], storing the result in stage_4: [m256; 4]
                let stage_4: [m256; 4] = [
                    x86_64::_mm256_add_epi32((*stage_1)[1][0], stage_3[0]),
                    x86_64::_mm256_add_epi32((*stage_1)[1][1], stage_3[1]),
                    x86_64::_mm256_add_epi32((*stage_1)[1][2], stage_3[2]),
                    x86_64::_mm256_add_epi32((*stage_1)[1][3], stage_3[3])
                ];

                // Stage 5: Run _mm256_cmpgt_epi32() comparing stage_4 against (4 and 2 for pents and tris), storing the result in stage_5: [m256; 4]
                let stage_5: [m256; 4] = [
                    x86_64::_mm256_cmpgt_epi32(stage_4[0], STAGE_5_B[0]),
                    x86_64::_mm256_cmpgt_epi32(stage_4[1], STAGE_5_B[1]),
                    x86_64::_mm256_cmpgt_epi32(stage_4[2], STAGE_5_B[2]),
                    x86_64::_mm256_cmpgt_epi32(stage_4[3], STAGE_5_B[3])
                ];

                // Stage 6: Compute the bitwise AND of stage_5 and (5 and 3 for pents and tris), storing the result in stage_6: [m256; 4]
                let stage_6: [m256; 4] = [
                    x86_64::_mm256_and_si256(stage_5[0], STAGE_6_B[0]),
                    x86_64::_mm256_and_si256(stage_5[1], STAGE_6_B[1]),
                    x86_64::_mm256_and_si256(stage_5[2], STAGE_6_B[2]),
                    x86_64::_mm256_and_si256(stage_5[3], STAGE_6_B[3])
                ];

                // Stage 7: Run _mm256_sub_epi32(), subtracting stage_6 from stage_4, storing the result in (*stage_1)[1]
                (*stage_1)[1] = [
                    x86_64::_mm256_sub_epi32(stage_4[0], stage_6[0]),
                    x86_64::_mm256_sub_epi32(stage_4[1], stage_6[1]),
                    x86_64::_mm256_sub_epi32(stage_4[2], stage_6[2]),
                    x86_64::_mm256_sub_epi32(stage_4[3], stage_6[3])
                ];

                // Stage 8: Run _mm256_i32gather_epi32::<4_i32>((*stage_2)[0], (*stage_1)[0]), storing the result in (*stage_1)[0]
                (*stage_1)[0] = [
                    x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*stage_2)[0] as *const [m256; 4] as *const i32, (*stage_1)[0][0]),
                    x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*stage_2)[0] as *const [m256; 4] as *const i32, (*stage_1)[0][1]),
                    x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*stage_2)[0] as *const [m256; 4] as *const i32, (*stage_1)[0][2]),
                    x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*stage_2)[0] as *const [m256; 4] as *const i32, (*stage_1)[0][3])
                ];
            }
        };
    }

    #[macro_export(local_inner_macros)]
    macro_rules! util_simd_inflated_add {
        ($src_state:ident, $transformation:ident, $dest_state:ident) => {
            // Stage 3: Run _mm256_i32gather_epi32::<4_i32>((*stage_2)[1], (*stage_1)[0]), storing the result in stage_3: [m256; 4]
            let stage_3: [m256; 4] = [
                x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*$transformation)[1] as *const [m256; 4] as *const i32, (*$src_state)[0][0]),
                x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*$transformation)[1] as *const [m256; 4] as *const i32, (*$src_state)[0][1]),
                x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*$transformation)[1] as *const [m256; 4] as *const i32, (*$src_state)[0][2]),
                x86_64::_mm256_i32gather_epi32::<STAGE_3_IMM8>(&(*$transformation)[1] as *const [m256; 4] as *const i32, (*$src_state)[0][3])
            ];

            // Stage 4: Add stage_3 to (*stage_1)[1], storing the result in stage_4: [m256; 4]
            let stage_4: [m256; 4] = [
                x86_64::_mm256_add_epi32((*$src_state)[1][0], stage_3[0]),
                x86_64::_mm256_add_epi32((*$src_state)[1][1], stage_3[1]),
                x86_64::_mm256_add_epi32((*$src_state)[1][2], stage_3[2]),
                x86_64::_mm256_add_epi32((*$src_state)[1][3], stage_3[3])
            ];

            // Stage 5: Run _mm256_cmpgt_epi32() comparing stage_4 against (4 and 2 for pents and tris), storing the result in stage_5: [m256; 4]
            let stage_5: [m256; 4] = [
                x86_64::_mm256_cmpgt_epi32(stage_4[0], STAGE_5_B[0]),
                x86_64::_mm256_cmpgt_epi32(stage_4[1], STAGE_5_B[1]),
                x86_64::_mm256_cmpgt_epi32(stage_4[2], STAGE_5_B[2]),
                x86_64::_mm256_cmpgt_epi32(stage_4[3], STAGE_5_B[3])
            ];

            // Stage 6: Compute the bitwise AND of stage_5 and (5 and 3 for pents and tris), storing the result in stage_6: [m256; 4]
            let stage_6: [m256; 4] = [
                x86_64::_mm256_and_si256(stage_5[0], STAGE_6_B[0]),
                x86_64::_mm256_and_si256(stage_5[1], STAGE_6_B[1]),
                x86_64::_mm256_and_si256(stage_5[2], STAGE_6_B[2]),
                x86_64::_mm256_and_si256(stage_5[3], STAGE_6_B[3])
            ];

            // Stage 7: Run _mm256_sub_epi32(), subtracting stage_6 from stage_4, storing the result in (*stage_1)[1]
            (*$dest_state)[1] = [
                x86_64::_mm256_sub_epi32(stage_4[0], stage_6[0]),
                x86_64::_mm256_sub_epi32(stage_4[1], stage_6[1]),
                x86_64::_mm256_sub_epi32(stage_4[2], stage_6[2]),
                x86_64::_mm256_sub_epi32(stage_4[3], stage_6[3])
            ];

            // Stage 8: Run _mm256_i32gather_epi32::<4_i32>((*stage_2)[0], (*stage_1)[0]), storing the result in (*stage_1)[0]
            (*$dest_state)[0] = [
                x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*$transformation)[0] as *const [m256; 4] as *const i32, (*$src_state)[0][0]),
                x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*$transformation)[0] as *const [m256; 4] as *const i32, (*$src_state)[0][1]),
                x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*$transformation)[0] as *const [m256; 4] as *const i32, (*$src_state)[0][2]),
                x86_64::_mm256_i32gather_epi32::<STAGE_8_IMM8>(&(*$transformation)[0] as *const [m256; 4] as *const i32, (*$src_state)[0][3])
            ];
        };

        ($ref_self:expr, $self_type:ty, $ref_transformation:expr, $transformation_type:ty) => {
            {
                use simd::{
                    prelude::*,
                    consts::inflated::apply_transformation::*
                };

                static_assertions::assert_eq_size!($self_type, $transformation_type, [[m256; 4]; 2]);

                unsafe {
                    // Stage 1: cast self: &mut PuzzleState into stage_1: *mut [[m256; 4]; 2]
                    let stage_1: *const [[m256; 4]; 2] = $ref_self as *const $self_type as *const [[m256; 4]; 2];

                    // Stage 2: cast transformation: &Transformation into stage_2: *const [[m256]; 2]
                    let stage_2: *const [[m256; 4]; 2] = $ref_transformation as *const $transformation_type as *const [[m256; 4]; 2];

                    let mut dest_state: $self_type = <$self_type>::default();
                    let ptr_dest_state: *mut [[m256; 4]; 2] = &mut dest_state as *mut $self_type as *mut [[m256; 4]; 2];

                    util_simd_inflated_add!(stage_1, stage_2, ptr_dest_state);

                    dest_state
                }
            }
        };
    }

    #[macro_export(local_inner_macros)]
    macro_rules! util_simd_inflated_add_assign {
        ($ref_mut_self:expr, $self_type:ty, $ref_transformation:expr, $transformation_type:ty) => {{
            use simd::{consts::inflated::apply_transformation::*, prelude::*};

            static_assertions::assert_eq_size!($self_type, $transformation_type, [[m256; 4]; 2]);

            unsafe {
                // Stage 1: cast self: &mut PuzzleState into stage_1: *mut [[m256; 4]; 2]
                let stage_1: *mut [[m256; 4]; 2] =
                    $ref_mut_self as *mut $self_type as *mut [[m256; 4]; 2];

                // Stage 2: cast transformation: &Transformation into stage_2: *const [[m256]; 2]
                let stage_2: *const [[m256; 4]; 2] =
                    $ref_transformation as *const $transformation_type as *const [[m256; 4]; 2];

                util_simd_inflated_add!(stage_1, stage_2, stage_1);
            }
        }};
    }
}

pub mod prelude {
    pub use std::{
        arch::x86_64::{self, __m128i as m128, __m256i as m256},
        mem::{size_of, transmute, transmute_copy, MaybeUninit},
    };
}
