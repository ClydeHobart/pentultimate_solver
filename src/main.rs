use pentultimate_solver::{
	app,
	util
};

fn init() -> () {
	util::init_env_logger();
}

fn main() -> () {
	init();
	app::main();
}