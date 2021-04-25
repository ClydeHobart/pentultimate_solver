extern crate pentultimate_solver;

use pentultimate_solver::{
	math::polyhedra::{
		properties::Polyhedron,
		data::*
	},
	util
};

fn init() -> () {
	util::init_env_logger();
}

fn main() -> () {
	init();

	let data: DataRefOption = Data::get(Polyhedron::RhombicTriacontahedron);

	println!("{:#?}", data);
}