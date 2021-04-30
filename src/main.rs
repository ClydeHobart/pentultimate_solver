extern crate pentultimate_solver;

use pentultimate_solver::{
	prelude::*,
	math::polyhedra::{
		properties::Polyhedron,
		data::*
	},
	util
};

fn init() -> () {
	util::init_env_logger();
}

fn validate() -> Result<(), LogError> {
	Data::validate_polyhedra()?;

	Ok(())
}

fn main() -> () {
	init();

	if let Err(log_error) = validate() {
		log_error.log();

		return;
	}

	// println!("{:#?}", Data::get(Polyhedron::Icosidodecahedron));
}