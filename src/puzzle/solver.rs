use {
	crate::{
		app::prelude::*,
		puzzle::transformation::Library,
		util::StaticDataLibrary
	},
	std::{
		convert::TryFrom,
		mem::transmute
	}
};

#[derive(Debug)]
#[repr(usize)]
enum SolverGenusIndex {
	Swap2PentPairs,
	CommutePentTrio,
	Rotate2PentPairs,
	RotatePentPair,
	RotatePent,
	CommuteTriTrio,
	Count
}

pub struct SolverGenusIndices([GenusIndex; SolverGenusIndex::Count as usize]);

impl SolverGenusIndices {
	fn default() -> Self { Self([GenusIndex::INVALID; SolverGenusIndex::Count as usize]) }

	fn new() -> Self {
		let mut solver_genus_indices: Self = Self::default();

		for solver_genus_index in 0_usize .. Library::get_genus_count() {
			let solver_genus_index_string: String = format!(
				"{:?}",
				unsafe { transmute::<usize, SolverGenusIndex>(solver_genus_index) }
			);

			if let Ok(genus_index) = GenusIndex::try_from(solver_genus_index_string.as_str()) {
				solver_genus_indices.0[solver_genus_index] = genus_index;
			} else {
				log::warn!("Couldn't find genus index for string \"{}\"", solver_genus_index_string);
			}
		}

		solver_genus_indices
	}
}

lazy_static!{
	static ref SOLVER_GENUS_INDICES: SolverGenusIndices = SolverGenusIndices::new();
}

impl StaticDataLibrary for SolverGenusIndices {
	fn pre_init() -> Option<Box<dyn FnOnce() -> ()>> { Some(Box::new(Library::build)) }

	fn get() -> &'static Self { &SOLVER_GENUS_INDICES }
}