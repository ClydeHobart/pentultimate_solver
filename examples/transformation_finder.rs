use pentultimate_solver::{
	prelude::*,
	puzzle::{
		consts::*,
		explorer::{
			Explorer,
			ExplorerParams,
			IsEndState,
			RunResult
		},
		transformation::{
			GenusIndex,
			GenusIndexConsts,
			GenusIndexBitArray,
			GenusIndexBitArrayConsts,
			Library
		},
		InflatedPuzzleState
	},
	util::StaticDataLibrary
};


#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
fn is_end_state(inflated_puzzle_state: &InflatedPuzzleState) -> bool {
	use pentultimate_solver::util::simd::prelude::*;

	const MASK: m256 = unsafe { transmute::<[u32; 8_usize], m256>([
		u32::MAX, u32::MAX, u32::MAX, u32::MAX, 0_u32, 0_u32, 0_u32, 0_u32
	]) };

	const COMPARISON: [m256; 2_usize] = unsafe { transmute::<[u32; 16_usize], [m256; 2_usize]>([
		0_u32, 1_u32, 2_u32, 3_u32, 4_u32, 5_u32, 6_u32, 7_u32,
		8_u32, 9_u32, 10_u32, 11_u32, 0_u32, 0_u32, 0_u32, 0_u32
	]) };

	let rot_sum: [u32; 8_usize] = unsafe {
		let inflated_puzzle_state_m256s: &[[m256; 4_usize]; 2_usize] = transmute::<
			&InflatedPuzzleState,
			&[[m256; 4_usize]; 2_usize]
		>(inflated_puzzle_state);

		if x86_64::_mm256_movemask_epi8(
			x86_64::_mm256_and_si256(
				x86_64::_mm256_cmpeq_epi32(
					inflated_puzzle_state_m256s[0_usize][0_usize],
					COMPARISON[0_usize]
				),
				x86_64::_mm256_cmpeq_epi32(
					x86_64::_mm256_and_si256(
						inflated_puzzle_state_m256s[0_usize][1_usize],
						MASK
					),
					COMPARISON[1_usize]
				)
			)
		) != -1_i32 {
			return false;
		}

		transmute::<m256, [u32; 8_usize]>(
			x86_64::_mm256_hadd_epi32(
			inflated_puzzle_state_m256s[1_usize][0_usize],
			x86_64::_mm256_and_si256(
				inflated_puzzle_state_m256s[1_usize][1_usize],
				MASK
				)
			)
		)
	};

	(rot_sum[0_usize] + rot_sum[1_usize] + rot_sum[2_usize] + rot_sum[3_usize]
		+ rot_sum[4_usize] + rot_sum[5_usize] + rot_sum[6_usize] + rot_sum[7_usize]
	) % PENTAGON_SIDE_COUNT_IPSC as u32 != 0_u32
}

#[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
fn is_end_state(inflated_puzzle_state: &InflatedPuzzleState) -> bool {
	let mut inflated_puzzle_state: InflatedPuzzleState = inflated_puzzle_state.clone();

	inflated_puzzle_state.standardize();

	let (pos, rot): (&InflatedPuzzleStateComponent, &InflatedPuzzleStateComponent) =
		inflated_puzzle_state.arrays();
	let mut pent_rots: u32 = 0_u32;

	for pentagon_index in PENTAGON_PIECE_RANGE {
		if pos[pentagon_index] != pentagon_index as InflatedPieceStateComponent {
			return false;
		}

		pent_rots += rot[pentagon_index] as u32;
	}

	pent_rots % PENTAGON_SIDE_COUNT as u32 != 0_u32
}

fn main() -> () {
	init_env_logger();
	<Library as StaticDataLibrary>::build();

	let mut explorer: Explorer = Explorer::new(ExplorerParams {
		is_end_state: Box::new(is_end_state) as IsEndState,
		candidate_genera: GenusIndexBitArray::try_from([GenusIndex::SIMPLE].as_slice())
			.unwrap_or(GenusIndexBitArray::NONE),
		.. ExplorerParams::default()
	});

	// let cycle_duration: Option<Duration> = Some(Duration::from_millis(32_u64));

	while matches!(explorer.run_cycle(None, false, true), RunResult::Pending) {}

	trace_expr!(explorer.run_result());
}
