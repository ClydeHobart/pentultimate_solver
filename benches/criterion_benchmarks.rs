use {
	std::time::Duration,
	criterion::{
		BatchSize,
		Bencher,
		Criterion,
		black_box,
		criterion_group,
		criterion_main
	},
	pentultimate_solver::{
		puzzle::{
			DeflatedPuzzleState,
			InflatedPuzzleState
		},
		util::simd,
		util_simd_deflated_deflate_puzzle_state as deflate_puzzle_state_current,
		util_simd_inflated_inflate_puzzle_state as inflate_puzzle_state_current,
	},
	rand::prelude::*
};

macro_rules! conversion_criterion {
	($group_name:ident, $from_type:ty, $to_type:ty, $($test:ident),+) => {
		fn $group_name(criterion: &mut Criterion) -> () {
			let mut thread_rng: ThreadRng = rand::thread_rng();
			let mut benchmark_group /* : BenchmarkGroup */ = criterion.benchmark_group(std::stringify!($group_name));

			benchmark_group.warm_up_time(Duration::from_secs(30_u64));
			benchmark_group.measurement_time(Duration::from_secs(120_u64));
			benchmark_group.sample_size(1000_usize);

			$(
				benchmark_group.bench_function(std::stringify!($test), |bencher: &mut Bencher| -> () {
					bencher.iter_batched(
						|| -> $from_type {
							(&mut thread_rng).into()
						},
						|from_value: $from_type| -> $to_type {
							$test!(black_box(&from_value), $from_type, $to_type)
						},
						BatchSize::SmallInput
					);
				});
			)+
		}
	};
}

conversion_criterion!(inflate, DeflatedPuzzleState, InflatedPuzzleState, inflate_puzzle_state_current);
conversion_criterion!(deflate, InflatedPuzzleState, DeflatedPuzzleState, deflate_puzzle_state_current);

criterion_group!(convert_puzzle_state, inflate, deflate);

criterion_main!(convert_puzzle_state);
