use {
    super::{
        transformation::{Addr, FullAddr, GenusIndex, GenusIndexBitArray, Library},
        DeflatedPuzzleState, InflatedPuzzleState,
    },
    crate::util::FromAlt,
    std::{
        collections::{hash_set::HashSet, VecDeque},
        time::{Duration, Instant},
    },
};

pub type ShouldExplore =
    Box<dyn Fn(&InflatedPuzzleState, FullAddr) -> bool + Send + Sync + 'static>;
pub type IsEndState = Box<dyn Fn(&InflatedPuzzleState) -> bool + Send + Sync + 'static>;

#[derive(Clone, Debug)]
pub struct EndState {
    pub state: InflatedPuzzleState,
    pub path: Vec<FullAddr>,
}

#[derive(Clone, Debug)]
pub enum DidNotFinishError {
    ExahustedStateSpace,
    MaxDepthReached,
}

#[derive(Clone, Debug)]
pub enum RunResult {
    Pending,
    EndStateFound(Box<EndState>),
    DidNotFinish(DidNotFinishError),
}

pub struct ExplorerParams<'a> {
    pub start_state: &'a InflatedPuzzleState,
    pub should_explore: Option<ShouldExplore>,
    pub is_end_state: IsEndState,
    pub candidate_genera: GenusIndexBitArray,
    pub max_depth: u8,
}

impl Default for ExplorerParams<'static> {
    fn default() -> Self {
        Self {
            start_state: &Explorer::START_STATE,
            should_explore: Explorer::SHOULD_EXPLORE,
            is_end_state: Explorer::default_is_end_state(),
            candidate_genera: Explorer::default_candidate_genera(),
            max_depth: Explorer::MAX_DEPTH,
        }
    }
}

#[derive(Default)]
pub struct Stats {
    pub elapsed: Duration,
    pub depth_elapsed: Duration,
    pub cycle: u64,
    pub states: u64,
    pub depth_states: u64,
}

pub struct Explorer {
    state_queue: VecDeque<DeflatedPuzzleState>,
    path_queue: VecDeque<u8>, // Stored as [depth byte][depth FullAddr payload bytes]
    seen: HashSet<DeflatedPuzzleState>,
    should_explore: Option<ShouldExplore>,
    is_end_state: IsEndState,
    pub stats: Stats,
    run_result: RunResult,
    candidate_genera: GenusIndexBitArray,
    depth: u8,
    max_depth: u8,
}

impl Explorer {
    const START_STATE: InflatedPuzzleState = InflatedPuzzleState::SOLVED_STATE;
    const SHOULD_EXPLORE: Option<ShouldExplore> = None;
    const MAX_DEPTH: u8 = 0_u8;

    #[inline]
    pub fn init(&mut self, params: ExplorerParams) {
        *self = Self::new(params);
    }

    pub fn new(params: ExplorerParams) -> Self {
        let ExplorerParams {
            start_state,
            should_explore,
            is_end_state,
            candidate_genera,
            max_depth,
        } = params;

        Self {
            state_queue: VecDeque::<DeflatedPuzzleState>::from([DeflatedPuzzleState::from(
                start_state,
            )]),
            path_queue: VecDeque::<u8>::from([0_u8]),
            seen: HashSet::from([{
                let mut start_state: InflatedPuzzleState = start_state.clone();

                start_state.standardize();

                DeflatedPuzzleState::from(&start_state)
            }]),
            should_explore,
            is_end_state,
            candidate_genera,
            max_depth,
            ..Self::default()
        }
    }

    #[inline]
    pub fn get_stats(&self) -> &Stats {
        &self.stats
    }

    #[inline]
    pub fn set_should_explore(&mut self, should_explore: Option<ShouldExplore>) {
        self.should_explore = should_explore;
    }

    #[inline]
    pub fn set_is_end_state(&mut self, is_end_state: IsEndState) {
        self.is_end_state = is_end_state;
    }

    #[inline]
    pub fn set_candidate_genera(&mut self, candidate_genera: GenusIndexBitArray) {
        self.candidate_genera = candidate_genera;
    }

    #[inline]
    pub fn set_max_depth(&mut self, max_depth: u8) {
        self.max_depth = max_depth;
    }

    pub fn run_cycle(
        &mut self,
        max_cycle_duration: Option<Duration>,
        log_cycle_stats: bool,
        log_depth_stats: bool,
    ) -> RunResult {
        if !matches!(self.run_result, RunResult::Pending) {
            return self.run_result.clone();
        }

        let cycle_start: Instant = Instant::now();
        let mut depth_start: Instant = cycle_start;
        let mut cycle_states: u64 = 0_u64;

        self.stats.cycle += 1_u64;

        while !self.state_queue.is_empty() {
            let state: InflatedPuzzleState =
                InflatedPuzzleState::from(&self.state_queue.pop_front().unwrap());
            let path_length: usize = self.path_queue.pop_front().unwrap_or(0_u8) as usize;
            let mut path: Vec<FullAddr> = Vec::<FullAddr>::with_capacity(path_length);
            let mut raw_full_addr: u16 = 0_u16;
            let byte_path: Vec<u8> = self
                .path_queue
                .drain(0_usize..path_length * 2_usize)
                .enumerate()
                .map(|(path_byte_index, path_byte): (usize, u8)| -> u8 {
                    if path_byte_index & 1_usize == 0_usize {
                        raw_full_addr = (path_byte as u16) << u8::BITS;
                    } else {
                        path.push(FullAddr::from(raw_full_addr | path_byte as u16));
                    }

                    path_byte
                })
                .collect::<Vec<u8>>();

            if path_length as u8 != self.depth {
                let now: Instant = Instant::now();

                self.stats.depth_elapsed += now - depth_start;
                depth_start = now;

                if log_depth_stats {
                    log::trace!(
                        "Depth {}:\n\
                            \tElapsed: {}\n\
                            \tStates Examined: {}",
                        self.depth,
                        String::from_alt(self.stats.depth_elapsed),
                        self.stats.depth_states
                    );
                }

                self.stats.depth_elapsed = Duration::ZERO;
                self.stats.depth_states = 0_u64;
                self.depth = path_length as u8;
            }

            cycle_states += 1_u64;
            self.stats.states += 1_u64;
            self.stats.depth_states += 1_u64;

            if (self.is_end_state)(&state) {
                let now: Instant = Instant::now();
                let cycle_elapsed: Duration = now - cycle_start;

                if log_cycle_stats {
                    log::trace!(
                        "Cycle {}:\n\
                            \tElapsed: {}\n\
                            \tStates Examined: {}\n",
                        self.stats.cycle,
                        String::from_alt(cycle_elapsed),
                        cycle_states
                    );
                }

                self.stats.depth_elapsed += now - depth_start;

                if log_depth_stats {
                    log::trace!(
                        "Depth {}:\n\
                            \tElapsed: {}\n\
                            \tStates Examined: {}",
                        self.depth,
                        String::from_alt(self.stats.depth_elapsed),
                        self.stats.depth_states
                    );
                }

                let end_state: EndState = EndState { state, path };

                self.stats.elapsed += cycle_elapsed;

                log::trace!(
                    "End State Found:\n\
                        \tCycle: {}\n\
                        \tDepth: {}\n\
                        \tElapsed: {}\n\
                        \tStates Examined: {}\n\
                        \tEnd State: {:#?}",
                    self.stats.cycle,
                    self.depth,
                    String::from_alt(self.stats.elapsed),
                    self.stats.states,
                    end_state
                );

                self.run_result = RunResult::EndStateFound(Box::<EndState>::new(end_state));

                return self.run_result.clone();
            }

            let mut full_addr: FullAddr = FullAddr::default();

            /* Always skip GenusIndex::REORIENTATION, since all transformations of that genus are
            "identity" transformations (for all states A and all transformations T in the set of all
            Reorientation transformations, standardize(A) == standardize(A + T)) */
            for genus_index in usize::from(GenusIndex::SIMPLE)..Library::get_genus_count() {
                if !self.candidate_genera.get_bit(genus_index) {
                    continue;
                }

                full_addr.set_genus_index(genus_index);

                for species_index in 0_usize..Library::SPECIES_PER_GENUS {
                    full_addr.set_species_index(species_index);

                    for organism_index in 0_usize..Library::ORGANISMS_PER_SPECIES {
                        full_addr.set_organism_index(organism_index);

                        if full_addr.is_identity_transformation()
                            || !self.should_explore.as_ref().map_or(
                                true,
                                |should_explore: &ShouldExplore| -> bool {
                                    should_explore(&state, full_addr)
                                },
                            )
                        {
                            continue;
                        }

                        let state_to_push: InflatedPuzzleState = &state + full_addr;

                        if !self.seen.insert(DeflatedPuzzleState::from(
                            &*state_to_push.clone().standardize(),
                        )) {
                            continue;
                        }

                        self.state_queue
                            .push_back(DeflatedPuzzleState::from(&state_to_push));
                        self.path_queue.push_back(self.depth + 1_u8);

                        for byte in &byte_path {
                            self.path_queue.push_back(*byte);
                        }

                        let full_addr_u16: u16 = u16::from(full_addr);

                        self.path_queue.push_back((full_addr_u16 >> u8::BITS) as u8);
                        self.path_queue
                            .push_back((full_addr_u16 & u8::MAX as u16) as u8);
                    }
                }
            }

            if let Some(max_cycle_duration) = max_cycle_duration.as_ref() {
                let now: Instant = Instant::now();
                let cycle_duration: Duration = now - cycle_start;

                if cycle_duration >= *max_cycle_duration {
                    self.stats.elapsed += cycle_duration;
                    self.stats.depth_elapsed += now - depth_start;

                    if log_cycle_stats {
                        log::trace!(
                            "Cycle {}:\n\
                                \tElapsed: {}\n\
                                \tStates Examined: {}",
                            self.stats.cycle,
                            String::from_alt(cycle_duration),
                            cycle_states
                        );
                    }

                    self.run_result = RunResult::Pending;

                    return RunResult::Pending;
                }
            }
        }

        self.run_result =
            RunResult::DidNotFinish(if self.max_depth != 0_u8 && self.depth == self.max_depth {
                DidNotFinishError::MaxDepthReached
            } else {
                DidNotFinishError::ExahustedStateSpace
            });

        self.run_result.clone()
    }

    pub fn run_result(&self) -> &RunResult {
        &self.run_result
    }

    fn default_candidate_genera() -> GenusIndexBitArray {
        GenusIndexBitArray::all()
    }

    fn default_is_end_state() -> IsEndState {
        Box::new(|_: &InflatedPuzzleState| -> bool { true })
    }
}

impl Default for Explorer {
    fn default() -> Self {
        Self {
            state_queue: VecDeque::<DeflatedPuzzleState>::new(),
            path_queue: VecDeque::<u8>::new(),
            seen: HashSet::<DeflatedPuzzleState>::new(),
            should_explore: Self::SHOULD_EXPLORE,
            is_end_state: Self::default_is_end_state(),
            stats: Stats::default(),
            run_result: RunResult::Pending,
            candidate_genera: Self::default_candidate_genera(),
            depth: 0_u8,
            max_depth: Self::MAX_DEPTH,
        }
    }
}
