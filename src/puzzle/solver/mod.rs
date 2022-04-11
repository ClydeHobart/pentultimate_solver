use {
	std::{
		cmp::{
			Ordering,
			PartialEq,
			PartialOrd
		},
		convert::TryFrom,
		mem::{
			swap,
			take,
			transmute
		},
		time::{
			Duration,
			Instant
		}
	},
	bevy::prelude::*,
	bit_field::BitField,
	egui::{
		Color32,
		Grid,
		RichText,
		Ui,
		Vec2,
		Visuals
	},
	crate::{
		app::prelude::*,
		math::polyhedra::{
			data::Data,
			Polyhedron
		},
		prelude::*,
		puzzle::{
			consts::*,
			inflated::{
				PosAndRot,
				PuzzleState
			},
			transformation::{
				Action,
				FullAddr,
				FullMask,
				GenusIndexBitArray,
				GenusRange,
				HalfMask,
				Library
			}
		},
		ui::input::{
			PendingActions,
			PuzzleAction,
			PuzzleActionType
		},
		util::StaticDataLibrary
	},
	super::explorer::{
		Explorer,
		ExplorerParams,
		IsEndState,
		RunResult,
		ShouldExplore
	}
};

pub mod tools;

#[derive(Debug)]
#[repr(usize)]
enum SolverGenusIndex {
	Simple,
	Swap2PentPairs,
	CommutePentTrioTriangle,
	CommutePentTrioLine,
	Rotate2PentPairs,
	RotatePentPair,
	RotatePent,
	CommuteTriTrioFar,
	CommuteTriTrioNear,
	CommuteTriTrioAcute,
	RotateTriPairFar,
	RotateTriPairNear,
	Count
}

const PENTAGON_HALF_MASK:		HalfMask	= ((1 as HalfMask) << PENTAGON_PIECE_COUNT) - 1 as HalfMask;
const TRIANGLE_HALF_MASK:		HalfMask	= !PENTAGON_HALF_MASK;
const ZERO_HALF_MASK:			HalfMask	= 0 as HalfMask;
const COMPLETION_TIER_COUNT:	usize		= 4_usize;
const MAX_DEPTH:				u8			= 1_u8;

#[derive(Debug, Default)]
struct SolverData{
	completed_half_masks:	[HalfMask; COMPLETION_TIER_COUNT + 1_usize],
	focus_half_masks:		[HalfMask; COMPLETION_TIER_COUNT + 1_usize],
	genus_ranges:			[GenusRange; SolverGenusIndex::Count as usize]
}

impl SolverData {
	fn new() -> Self {
		let mut solver_data: Self = Self::default();
		let mut dot_products: [i8; PIECE_COUNT] = [0_i8; PIECE_COUNT];
		let mut pent_dot_products: Vec<i8> = Vec::<i8>::with_capacity(COMPLETION_TIER_COUNT + 1_usize);
		let mut tri_dot_products: Vec<i8> = Vec::<i8>::with_capacity(COMPLETION_TIER_COUNT + 1_usize);

		let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);
		let origin_norm: Vec3 = icosidodecahedron_data.faces[PENTAGON_INDEX_OFFSET].norm;

		for (piece_index, face_data) in icosidodecahedron_data.faces.iter().enumerate() {
			/* Multiply the dot product by -1 so that sorting the vectors (in ascending order) is in decreasing order
			of alignment with origin_norm  */
			let dot_product: i8 = (-i8::MAX as f32 * face_data.norm.dot(origin_norm)) as i8;
			dot_products[piece_index] = dot_product;

			fn push_dot_product(dot_products: &mut Vec<i8>, dot_product: i8) -> () {
				if !dot_products.contains(&dot_product) {
					dot_products.push(dot_product);
				}
			}

			if piece_index < TRIANGLE_INDEX_OFFSET {
				push_dot_product(&mut pent_dot_products, dot_product);
			} else {
				push_dot_product(&mut tri_dot_products, dot_product);
			}
		}

		assert_eq!(pent_dot_products.len(), COMPLETION_TIER_COUNT);
		assert_eq!(tri_dot_products.len(), COMPLETION_TIER_COUNT);

		pent_dot_products.sort();
		tri_dot_products.sort();

		for (piece_index, dot_product) in dot_products.iter().enumerate() {
			solver_data
				.focus_half_masks[
					(if piece_index < TRIANGLE_INDEX_OFFSET { &pent_dot_products } else { &tri_dot_products })
						.binary_search(dot_product)
						.unwrap()
				]
				.set_bit(piece_index, true);
		}

		solver_data.completed_half_masks = solver_data.focus_half_masks;
		solver_data.completed_half_masks.rotate_right(1_usize);

		for completion_half_mask_index in 0_usize .. COMPLETION_TIER_COUNT {
			solver_data.completed_half_masks[completion_half_mask_index + 1_usize] |= solver_data
				.completed_half_masks
				[completion_half_mask_index];
		}

		for solver_genus_index in 0_usize .. SolverGenusIndex::Count as usize {
			let solver_genus_index_string: String = format!(
				"{:?}",
				unsafe { transmute::<usize, SolverGenusIndex>(solver_genus_index) }
			);

			if let Ok(genus_range) = GenusRange::try_from(solver_genus_index_string.as_str()) {
				solver_data.genus_ranges[solver_genus_index] = genus_range;
			} else {
				log::warn!("Couldn't find genus index for string \"{}\"", solver_genus_index_string);
			}
		}

		solver_data
	}
}

lazy_static!{
	static ref SOLVER_DATA: SolverData = SolverData::new();
}

impl StaticDataLibrary for SolverData {
	fn pre_init() -> Option<Box<dyn FnOnce() -> ()>> { Some(Box::new(Library::build)) }

	fn get() -> &'static Self { &SOLVER_DATA }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[repr(u8)]
enum SolveStage {
	PositionPents,
	RotatePents,
	PositionTris,
	RotateTris,
	Solved
}

impl SolveStage {
	#[inline]
	const fn get_focus_affected_half_mask(self, full_mask: &FullMask) -> HalfMask {
		self.get_piece_half_mask() & self.get_full_mask_field(full_mask)
	}

	#[inline]
	const fn get_focus_unaffected_half_mask(self, full_mask: &FullMask) -> HalfMask {
		self.get_piece_half_mask() & !self.get_full_mask_field(full_mask)
	}

	#[inline]
	const fn get_full_mask_field(self, full_mask: &FullMask) -> HalfMask {
		match self {
			Self::PositionPents	| Self::PositionTris	=> full_mask.affected_poses,
			Self::RotatePents	| Self::RotateTris		=> full_mask.affected_pieces,
			Self::Solved								=> ZERO_HALF_MASK
		}
	}

	#[inline]
	const fn get_piece_half_mask(self) -> HalfMask {
		match self {
			Self::PositionPents	| Self::RotatePents	=> PENTAGON_HALF_MASK,
			Self::PositionTris	| Self::RotateTris	=> TRIANGLE_HALF_MASK,
			Self::Solved							=> ZERO_HALF_MASK
		}
	}
}

impl Default for SolveStage { fn default() -> Self { Self::PositionPents } }

impl From<FullMask> for SolveStage {
	fn from(full_mask: FullMask) -> Self {
		if SolveStage::RotatePents.get_focus_affected_half_mask(&full_mask) != ZERO_HALF_MASK {
			// Pentagons haven't been solved
			if SolveStage::PositionPents.get_focus_affected_half_mask(&full_mask) != ZERO_HALF_MASK {
				// Pentagons haven't been positioned
				Self::PositionPents
			} else {
				// Pentagons have been positioned
				Self::RotatePents
			}
		} else {
			// Pentagons have been solved
			if SolveStage::PositionTris.get_focus_affected_half_mask(&full_mask) != ZERO_HALF_MASK {
				// Triangles haven't been positioned
				Self::PositionTris
			} else {
				// Triangles have been positioned
				if SolveStage::RotateTris.get_focus_affected_half_mask(&full_mask) != ZERO_HALF_MASK {
					// Triangles haven't been rotated
					Self::RotateTris
				} else {
					// Triangles have been rotated
					Self::Solved
				}
			}
		}
	}
}

impl<'p, 'r> From<PosAndRot<'p, 'r>> for SolveStage {
	fn from(pos_and_rot: PosAndRot) -> Self { FullMask::from(pos_and_rot).into() }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
struct Completion {
	solve_stage:	SolveStage,
	tier:			u8
}

impl From<FullMask> for Completion {
	fn from(full_mask: FullMask) -> Self { (full_mask, SolveStage::from(full_mask)).into() }
}

impl From<(FullMask, SolveStage)> for Completion {
	fn from((full_mask, solve_stage): (FullMask, SolveStage)) -> Self {
		let solve_stage_half_mask: HalfMask = solve_stage.get_focus_affected_half_mask(&full_mask);

		for tier in (1_usize .. COMPLETION_TIER_COUNT).rev() {
			if SOLVER_DATA.completed_half_masks[tier] & solve_stage_half_mask == ZERO_HALF_MASK {
				return Self {
					solve_stage,
					tier: tier as u8
				}
			}
		}

		Self {
			solve_stage,
			tier: 0_u8
		}
	}
}

#[derive(Clone, Copy, Debug, Default)]
struct FullMaskAndCompletion {
	full_mask:	FullMask,
	completion:	Completion
}

impl FullMaskAndCompletion {
	#[inline]
	fn get_focus_affected_half_mask(&self) -> HalfMask {
		self.completion.solve_stage.get_focus_affected_half_mask(&self.full_mask)
	}

	#[inline]
	fn get_focus_unaffected_half_mask(&self) -> HalfMask {
		self.completion.solve_stage.get_focus_unaffected_half_mask(&self.full_mask)
	}

	fn get_incorrect_piece_count(&self) -> u32 {
		(SOLVER_DATA
			.completed_half_masks
			[self.completion.tier as usize + 1_usize]
			& self
				.get_focus_affected_half_mask()
		).count_ones()
	}

	fn immut_ui(&self, ui: &mut Ui) -> () {
		ui.collapsing("Full Mask And Completion", |ui: &mut Ui| -> () {
			ui.label(format!("Solve Stage: {:?}, Tier: {:?}", self.completion.solve_stage, self.completion.tier));
			ui.scope(|ui: &mut Ui| -> () {
				let (focus_poses, focus_pieces): (bool, bool) = match self.completion.solve_stage {
					SolveStage::PositionPents | SolveStage::PositionTris => (true, false),
					SolveStage::RotatePents | SolveStage::RotateTris => (false, true),
					_ => (false, false)
				};
				let piece_half_mask: HalfMask = self.completion.solve_stage.get_piece_half_mask();
				let completed_tier_half_mask: HalfMask = SOLVER_DATA
					.completed_half_masks
					[self.completion.tier as usize]
					& piece_half_mask;
				let focus_tier_half_mask: HalfMask = SOLVER_DATA
					.completed_half_masks
					[self.completion.tier as usize + 1_usize]
					& !completed_tier_half_mask
					& piece_half_mask;
				let (strong, normal, weak): (Color32, Color32, Color32) = {
					let visuals: &Visuals = ui.visuals();

					(visuals.strong_text_color(), visuals.text_color(), visuals.weak_text_color())
				};

				Grid::new(0).show(ui, |ui: &mut Ui| -> () {
					for (label, half_mask_string, focus)
						in [
							("Affected Poses", self.full_mask.affected_poses_string(), focus_poses),
							("Affected Pieces", self.full_mask.affected_pieces_string(), focus_pieces)
						]
					{
						ui.label(label);
						ui.horizontal(|ui: &mut Ui| -> () {
							ui.spacing_mut().item_spacing = Vec2::ZERO;
							

							for (bit, bit_char) in half_mask_string.chars().enumerate() {
								ui.label(RichText::new(bit_char).monospace().color(
									if focus {
										if focus_tier_half_mask.get_bit(bit) {
											strong
										} else if completed_tier_half_mask.get_bit(bit) {
											normal
										} else {
											weak
										}
									} else {
										weak
									}
								));
							}
						});
						ui.end_row();
					}
				});
			});
		});
	}

	fn should_explore(&self, full_addr: FullAddr) -> bool {
		if let Some(full_mask) = full_addr.get_full_mask() {
			(SOLVER_DATA.completed_half_masks[self.completion.tier as usize]
				| SOLVER_DATA.focus_half_masks[self.completion.tier as usize]
				& self.get_focus_unaffected_half_mask()
			) & self.completion.solve_stage.get_focus_affected_half_mask(full_mask) == ZERO_HALF_MASK
		} else {
			false
		}
	}
}

impl From<(FullMask, SolveStage)> for FullMaskAndCompletion {
	fn from((full_mask, solve_stage): (FullMask, SolveStage)) -> Self {
		Self {
			full_mask,
			completion: (full_mask, solve_stage).into()
		}
	}
}

impl<'p, 'r> From<(PosAndRot<'p, 'r>, SolveStage)> for FullMaskAndCompletion {
	fn from((pos_and_rot, solve_stage): (PosAndRot, SolveStage)) -> Self {
		(FullMask::from(pos_and_rot), solve_stage).into()
	}
}

impl From<FullMask> for FullMaskAndCompletion {
	fn from(full_mask: FullMask) -> Self {
		Self {
			full_mask,
			completion: full_mask.into()
		}
	}
}

impl<'p, 'r> From<PosAndRot<'p, 'r>> for FullMaskAndCompletion {
	fn from(pos_and_rot: PosAndRot) -> Self { FullMask::from(pos_and_rot).into() }
}

impl PartialEq for FullMaskAndCompletion {
	fn eq(&self, other: &Self) -> bool {
		self.completion == other.completion && self.get_incorrect_piece_count() == other.get_incorrect_piece_count()
	}
}

impl PartialOrd for FullMaskAndCompletion {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		let completion_partial_cmp: Option<Ordering> = self.completion.partial_cmp(&other.completion);

		if completion_partial_cmp.is_none() || completion_partial_cmp.as_ref().unwrap().is_ne() {
			completion_partial_cmp
		} else {
			other.get_incorrect_piece_count().partial_cmp(&self.get_incorrect_piece_count())
		}
	}
}

macro_rules! define_solve_stage_state {
	(
		$(#[$attr:meta])*
		$vis:vis enum $enum_name:ident {
			$($enum_variant:ident),* $(,)?
		}
	) => {
		$(#[$attr])*
		$vis enum $enum_name {
			$($enum_variant),*
		}

		impl $enum_name {
			fn try_get_next(self) -> Option<Self> {
				const COUNT: u8 = $enum_name::get_count();

				let next: u8 = self as u8 + 1_u8;

				if next == COUNT { None } else { Some(unsafe { transmute::<u8, Self>(next) }) }
			}

			const fn get_count() -> u8 {
				#![allow(path_statements)]

				let mut count: u8 = 0_u8;

				$(
					Self::$enum_variant;

					count += 1_u8;
				)*

				count
			}
		}

		impl Default for $enum_name { fn default() -> Self { unsafe { transmute::<u8, Self>(0_u8) } } }

		impl From<$enum_name> for GenusRange {
			fn from(e: $enum_name) -> Self {
				match e {
					$(
						$enum_name::$enum_variant =>
							SOLVER_DATA.genus_ranges[SolverGenusIndex::$enum_variant as usize].clone(),
					)*
				}
			}
		}
	}
}

define_solve_stage_state!{
	#[derive(Clone, Copy, Debug)]
	#[repr(u8)]
	enum PositionPentsStage {
		Simple,
		Swap2PentPairs,
		CommutePentTrioTriangle,
		CommutePentTrioLine
	}
}

define_solve_stage_state!{
	#[derive(Clone, Copy, Debug)]
	#[repr(u8)]
	enum RotatePentsStage {
		Rotate2PentPairs,
		RotatePentPair,
		RotatePent
	}
}

define_solve_stage_state!{
	#[derive(Clone, Copy, Debug)]
	#[repr(u8)]
	enum PositionTrisStage {
		CommuteTriTrioFar,
		CommuteTriTrioNear,
		CommuteTriTrioAcute
	}
}

define_solve_stage_state!{
	#[derive(Clone, Copy, Debug)]
	#[repr(u8)]
	enum RotateTrisStage {
		RotateTriPairFar,
		RotateTriPairNear
	}
}

#[derive(Clone, Copy, Debug)]
enum StatefulSolveStage {
	PositionPents(PositionPentsStage),
	RotatePents(RotatePentsStage),
	PositionTris(PositionTrisStage),
	RotateTris(RotateTrisStage)
}

impl StatefulSolveStage {
	fn try_get_next_within_stage(self) -> Option<Self> {
		match self {
			Self::PositionPents(position_pents_stage) =>
				position_pents_stage.try_get_next().map(Self::PositionPents),
			Self::RotatePents(rotate_pents_stage) =>
				rotate_pents_stage.try_get_next().map(Self::RotatePents),
			Self::PositionTris(position_tris_stage) =>
				position_tris_stage.try_get_next().map(Self::PositionTris),
			Self::RotateTris(rotate_tris_stage) =>
				rotate_tris_stage.try_get_next().map(Self::RotateTris)
		}
	}
}

impl<'p> From<(&'p PuzzleState, StatefulSolveStage)> for ExplorerParams<'p> {
	fn from((puzzle_state, stateful_solve_stage): (&'p PuzzleState, StatefulSolveStage)) -> Self {
		(
			puzzle_state,
			FullMaskAndCompletion::from((puzzle_state.arrays(), SolveStage::from(stateful_solve_stage))),
			stateful_solve_stage
		).into()
	}
}

impl<'p> From<(&'p PuzzleState, FullMaskAndCompletion, StatefulSolveStage)> for ExplorerParams<'p> {
	fn from((
		puzzle_state,
		full_mask_and_completion,
		stateful_solve_stage
	): (
		&'p PuzzleState,
		FullMaskAndCompletion,
		StatefulSolveStage
	)) -> Self { Self {
		start_state:		puzzle_state,
		should_explore:		Some(Box::new(move |_: &PuzzleState, full_addr: FullAddr| -> bool {
			full_mask_and_completion.should_explore(full_addr)
		}) as ShouldExplore),
		is_end_state:		Box::new(move |puzzle_state: &PuzzleState| -> bool {
			FullMaskAndCompletion::from(puzzle_state.arrays()) > full_mask_and_completion
		}) as IsEndState,
		candidate_genera:	GenusIndexBitArray::from(GenusRange::from(stateful_solve_stage)),
		max_depth:			MAX_DEPTH
	} }
}

impl From<StatefulSolveStage> for GenusRange {
	fn from(stateful_solve_stage: StatefulSolveStage) -> GenusRange {
		match stateful_solve_stage {
			StatefulSolveStage::PositionPents(position_pents_stage)=> position_pents_stage.into(),
			StatefulSolveStage::RotatePents(rotate_pents_stage) => rotate_pents_stage.into(),
			StatefulSolveStage::PositionTris(position_tris_stage) => position_tris_stage.into(),
			StatefulSolveStage::RotateTris(rotate_tris_stage) => rotate_tris_stage.into()
		}
	}
}

impl From<StatefulSolveStage> for SolverState {
	fn from(stateful_solve_stage: StatefulSolveStage) -> Self { Self::Solving(stateful_solve_stage) }
}

impl From<StatefulSolveStage> for SolveStage {
	fn from(stateful_solve_stage: StatefulSolveStage) -> Self {
		match stateful_solve_stage {
			StatefulSolveStage::PositionPents(_)	=> Self::PositionPents,
			StatefulSolveStage::RotatePents(_)		=> Self::RotatePents,
			StatefulSolveStage::PositionTris(_)		=> Self::PositionTris,
			StatefulSolveStage::RotateTris(_)		=> Self::RotateTris
		}
	}
}

#[derive(Clone, Copy, Debug)]
enum SolverState {
	Idle,
	Solving(StatefulSolveStage),
	Solved,
	DidNotFinish
}

impl SolverState {
	#[inline(always)]
	fn is_done(&self) -> bool { matches!(self, Self::Solved | Self::DidNotFinish) }

	#[inline(always)]
	fn is_solving(&self) -> bool { matches!(self, Self::Solving(_)) }

	#[inline(always)]
	fn is_solved(&self) -> bool { matches!(self, Self::Solved) }

	fn try_get_stateful_solve_stage(self) -> Option<StatefulSolveStage> {
		match self {
			Self::Solving(stateful_solve_stage) => Some(stateful_solve_stage),
			_ => None
		}
	}
}

impl From<SolveStage> for SolverState {
	fn from(solve_stage: SolveStage) -> SolverState {
		match solve_stage {
			SolveStage::PositionPents =>
				SolverState::Solving(StatefulSolveStage::PositionPents(PositionPentsStage::default())),
			SolveStage::RotatePents =>
				SolverState::Solving(StatefulSolveStage::RotatePents(RotatePentsStage::default())),
			SolveStage::PositionTris =>
				SolverState::Solving(StatefulSolveStage::PositionTris(PositionTrisStage::default())),
			SolveStage::RotateTris =>
				SolverState::Solving(StatefulSolveStage::RotateTris(RotateTrisStage::default())),
			SolveStage::Solved => SolverState::Solved
		}
	}
}

impl Default for SolverState { fn default() -> Self { SolverState::Idle } }

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
enum StatType {
	Total,
	SolveStage,
	Tier,
	Count
}

impl From<u8> for StatType {
	fn from(stat_type: u8) -> Self { unsafe { transmute::<u8, Self>(stat_type.min(Self::Count as u8)) } }
}

#[derive(Debug, Default)]
struct CountStats {
	solver_cycles:		u64,
	explorer_cycles:	u64,
	explorer_states:	u64
}

#[derive(Debug, Default)]
struct TypeStats {
	elapsed:	Duration,
	counts:		CountStats
}

impl TypeStats {
	fn immut_ui(&self, ui: &mut Ui) -> () {
		Grid::new(0_u32).show(ui, |ui: &mut Ui| -> () {
			ui.label("Elapsed:");
			ui.label(String::from_alt(self.elapsed));
			ui.end_row();
			ui.label("Solver Cycles:");
			ui.label(format!("{}", self.counts.solver_cycles));
			ui.end_row();
			ui.label("Explorer Cycles:");
			ui.label(format!("{}", self.counts.explorer_cycles));
			ui.end_row();
			ui.label("Explorer States:");
			ui.label(format!("{}", self.counts.explorer_states));
			ui.end_row();
		});
	}
}

#[derive(Debug, Default)]
struct Stats {
	type_stats_array:			[TypeStats; StatType::Count as usize],
	full_mask_and_completion:	FullMaskAndCompletion
}

impl Stats {
	fn type_stats_array_ui(&self, ui: &mut Ui) -> () {
		ui.collapsing("Type Stats Array", |ui: &mut Ui| -> () {
			ui.indent(0_u32, |ui: &mut Ui| -> () {
				for (stat_type, type_stats) in self.type_stats_array.iter().enumerate() {
					ui.collapsing(
						format!("{:?}", StatType::from(stat_type as u8)),
						|ui: &mut Ui| -> () {
							ui.indent(stat_type, |ui: &mut Ui| -> () {
								type_stats.immut_ui(ui);
							});
						}
					);
				}
			});
		});
	}
}

#[derive(Default)]
pub struct Solver {
	puzzle_state:	PuzzleState,
	path:			Vec<FullAddr>,
	explorer:		Explorer,
	solver_state:	SolverState,
	stats:			Stats
}

impl Solver {
	pub fn init(&mut self, puzzle_state: &PuzzleState) -> () { *self = Self::new(puzzle_state) }

	pub fn new(puzzle_state: &PuzzleState) -> Self {
		let full_mask_and_completion: FullMaskAndCompletion = puzzle_state.arrays().into();
		let solver_state: SolverState = full_mask_and_completion.completion.solve_stage.into();

		Self {
			puzzle_state:	puzzle_state.clone(),
			path:			Vec::<FullAddr>::new(),
			explorer:		if solver_state.is_solving() {
				Explorer::new(ExplorerParams::from((
					puzzle_state,
					full_mask_and_completion,
					solver_state.try_get_stateful_solve_stage().unwrap()
				)))
			} else {
				Explorer::default()
			},
			solver_state,
			stats:			Stats {
				full_mask_and_completion,
				.. Stats::default()
			}
		}
	}

	#[inline(always)]
	fn is_done(&self) -> bool { self.solver_state.is_done() }

	#[inline(always)]
	fn is_solved(&self) -> bool { self.solver_state.is_solved() }

	#[inline(always)]
	fn is_solving(&self) -> bool { self.solver_state.is_solving() }

	fn run_cycle(&mut self, mut max_cycle_duration: Duration) -> () {
		if !self.is_solving() {
			return;
		}

		let Solver {
			puzzle_state,
			path,
			explorer,
			solver_state,
			stats
		} = self;

		let mut stateful_solve_stage: StatefulSolveStage = solver_state.try_get_stateful_solve_stage().unwrap();
		let mut stat_type_starts: [Instant; StatType::Count as usize] = [Instant::now(); StatType::Count as usize];

		macro_rules! update_explorer { () => {
			explorer.init((&*puzzle_state, stats.full_mask_and_completion, stateful_solve_stage).into());
		} }

		while !max_cycle_duration.is_zero() {
			let start: Instant = Instant::now();
			let explorer_state_start: u64 = explorer.get_stats().states;
			let run_result: RunResult = explorer.run_cycle(
				Some(max_cycle_duration),
				false,
				false
			);
			let explorer_state_increase: u64 = explorer.get_stats().states - explorer_state_start;

			for type_stats in stats.type_stats_array.iter_mut() {
				type_stats.counts.explorer_cycles += 1_u64;
				type_stats.counts.explorer_states += explorer_state_increase;
			}

			macro_rules! update_max_cycle_duration { () => {
				max_cycle_duration = max_cycle_duration.saturating_sub(Instant::now() - start);
			} }

			match run_result {
				RunResult::Pending => {
					// We ran out of time. The explorer still has state space to explore, though, so pick up next cycle
					break;
				},
				RunResult::EndStateFound(mut end_state) => {
					update_max_cycle_duration!();

					*puzzle_state = end_state.state;
					path.append(&mut end_state.path);

					let old_full_mask_and_completion: FullMaskAndCompletion = {
						let mut new_full_mask_and_completion: FullMaskAndCompletion = puzzle_state.arrays().into();

						swap(&mut stats.full_mask_and_completion, &mut new_full_mask_and_completion);

						new_full_mask_and_completion
					};

					macro_rules! reset_type_stats { ($stat_type:expr) => {
						stats.type_stats_array[$stat_type] = TypeStats::default();
						stat_type_starts[$stat_type] = Instant::now();
					} }

					if stats.full_mask_and_completion.completion.solve_stage
						> old_full_mask_and_completion.completion.solve_stage {
						// We have advanced to a new stage, which may be solved
						*solver_state = stats.full_mask_and_completion.completion.solve_stage.into();

						for stat_type in StatType::SolveStage as usize .. StatType::Count as usize {
							reset_type_stats!(stat_type);
						}

						if solver_state.is_solving() {
							// Update stateful_solve_stage and explorer
							stateful_solve_stage = solver_state.try_get_stateful_solve_stage().unwrap();
							update_explorer!();
						} else {
							// We saved the city!
							break;
						}
					} else {
						if stats.full_mask_and_completion.completion.tier
							> old_full_mask_and_completion.completion.tier
						{
							reset_type_stats!(StatType::Tier as usize);
						}

						// Update the explorer, but no need to change anything else
						update_explorer!();
					}
				},
				RunResult::DidNotFinish(_) => {
					update_max_cycle_duration!();

					/* Either we need to upgrade to a higher caliber weapon, or we ran out of tools at our disposal and
					lost */
					if let Some(new_stateful_solve_stage) =
						stateful_solve_stage.try_get_next_within_stage()
					{
						stateful_solve_stage = new_stateful_solve_stage;
						update_explorer!();
					} else {
						*solver_state = SolverState::DidNotFinish;

						debug_expr!(stateful_solve_stage);
						log::info!("Did not finish solve:\n{:#?}", stats);

						break;
					}
				}
			}
		}

		let end: Instant = Instant::now();

		for (stat_type, type_stats) in stats.type_stats_array.iter_mut().enumerate() {
			type_stats.counts.solver_cycles += 1_u64;
			type_stats.elapsed += end - stat_type_starts[stat_type];
		}
	}

	fn build_pending_actions(&mut self, preferences: &Preferences) -> PendingActions {
		let pending_actions: PendingActions = PendingActions {
			animation_speed_data: preferences.speed.animation.clone(),
			actions: take(&mut self.path).into_iter().map(|transformation: FullAddr| -> Action {
				let mut camera_start: HalfAddr = *transformation.get_half_addr();

				if transformation.is_genus_index_simple() {
					camera_start.set_organism_index(0_usize);
				}

				Action {
					transformation,
					camera_start
				}
			}).collect(),
			.. PendingActions::default()
		};

		*self = Self::default();

		pending_actions
	}
}

pub struct SolverPlugin;

impl SolverPlugin {
	fn startup() -> () {
		SolverData::build();
	}

	fn update(
		preferences:	Res<Preferences>,
		mut solver:		ResMut<Solver>
	) -> () {
		solver.run_cycle(Duration::from_micros(
			(preferences.speed.solver_cycle_duration_millis * 1000.0_f32) as u64
		));
	}

	fn post_update(
		preferences:		Res<Preferences>,
		mut solver:			ResMut<Solver>,
		mut input_state:	ResMut<InputState>
	) -> () {
		if !solver.is_done() {
			return;
		}

		input_state.puzzle_action = Some(PuzzleAction {
			current_action: None,
			pending_actions: Some(Box::new(solver.build_pending_actions(&*preferences))),
			action_type: PuzzleActionType::Solve
		});
		input_state.is_solving = false;
	}
}

impl Plugin for SolverPlugin {
	fn build(&self, app: &mut App) -> () {
		app
			.insert_resource(Solver::default())
			.add_startup_system(Self::startup
				.system()
				.label(STRING_DATA.labels.solver_startup.as_ref())
				.after(STRING_DATA.labels.transformation_startup.as_ref())
			)
			.add_system(Self::update
				.system()
				.label(STRING_DATA.labels.solver_update.as_ref())
			)
			.add_system(Self::post_update
				.system()
				.label(STRING_DATA.labels.solver_post_update.as_ref())
			);
	}
}

#[cfg(test)]
mod tests {
	use {
		crate::{
			preferences::{
				FileMenuData,
				Preferences,
				RandomizationParams
			},
			puzzle::transformation::HalfAddrConsts,
			ui::input::PendingActions
		},
		super::*
	};

	fn test_solve_stage_solution(solve_stage: SolveStage) -> () {
		const SOLVE_ITERATIONS: u32 = 1000_u32;
		const RANDOM_TRANSFORMATION_COUNT: u8 = 50_u8;
		const ONE_SECOND: Duration = Duration::from_secs(1_u64);

		init_env_logger();
		SolverData::build();

		let camera_orientation: Quat = *HalfAddr::ORIGIN.get_orientation().unwrap();
		let preferences: Preferences = Preferences {
			file_menu: FileMenuData {
				randomization_params: RandomizationParams {
					random_transformation_count: RANDOM_TRANSFORMATION_COUNT,
					.. RandomizationParams::default()
				}
			},
			.. Preferences::default()
		};
		let mut solver: Solver = Solver::default();

		for iteration in 0_u32 .. SOLVE_ITERATIONS {
			solver.init(&PendingActions::randomize(
				&preferences,
				&PuzzleState::SOLVED_STATE,
				&camera_orientation
			).puzzle_state);

			fn solve_stage_from_solver(solver: &Solver) -> SolveStage {
				solver.stats.full_mask_and_completion.completion.solve_stage
			}

			let mut solve_cycles: u32 = 60_u32;

			while solver.is_solving() && solve_stage_from_solver(&solver) == solve_stage && solve_cycles != 0_u32 {
				solver.run_cycle(ONE_SECOND);
				solve_cycles -= 1_u32;
			}

			match solver.solver_state {
				SolverState::Idle => {
					unreachable!();
				},
				SolverState::Solving(stateful_solve_stage) => {
					if SolveStage::from(stateful_solve_stage) == solve_stage {
						assert!(solve_cycles == 0_u32);
						panic!("Couldn't solve {:?} after 1 minute of solve time on iteration {}\n\
							SolverState: {:#?}\n\
							SolverStats: {:#?}",
							solve_stage, iteration, solver.solver_state, solver.stats);
					}
				},
				SolverState::DidNotFinish => {
					if solve_stage_from_solver(&solver) == solve_stage {
						panic!("Did not solve {:?} on iteration {}\n\
							SolverState: {:#?}\n\
							SolverStats: {:#?}",
							solve_stage, iteration, solver.solver_state, solver.stats);
					}
				},
				_ => {}
			}
		}
	}

	#[test]
	fn test_position_pents_solution() -> () { test_solve_stage_solution(SolveStage::PositionPents); }

	#[test]
	fn test_rotate_pents_solution() -> () { test_solve_stage_solution(SolveStage::RotatePents); }

	#[test]
	fn test_position_tris_solution() -> () { test_solve_stage_solution(SolveStage::PositionTris); }

	#[test]
	fn test_rotate_tris_solution() -> () { test_solve_stage_solution(SolveStage::RotateTris); }
}