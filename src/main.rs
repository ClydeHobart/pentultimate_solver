use pentultimate_solver::{
	app,
	prelude::init_env_logger
};

fn init() -> () {
	init_env_logger();
}

fn main() -> () {
	init();
	app::main();
}