use core::fmt;

use serde::Deserialize;

#[derive(Deserialize, Debug)]
enum LevelFilter {
	Off,
	Error,
	Warn,
	Info,
	Debug,
	Trace,
}

impl fmt::Display for LevelFilter {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(self, f)
	}
}

#[derive(Deserialize, Debug)]
struct Module {
	f:	LevelFilter,
	m:	std::collections::HashMap<String, Module>
}

#[macro_export]
macro_rules! log_path {
	() => {
		std::module_path!()
	};

	($item:expr) => {
		format!("{}::{}", std::module_path!(), $item).as_str()
	};
}

pub fn init_env_logger() -> () {
	const RUST_LOG_JSON_FILE_NAME: &str = "RUST_LOG.json5";

	match get_module(RUST_LOG_JSON_FILE_NAME) {
		Ok(module) => {
			set_env_var(module);
		},
		Err(error) => {
			println!("Error opening \"{}\": {}", RUST_LOG_JSON_FILE_NAME, error);
		}
	}
}

fn get_module(file_name: &str) -> Result<Module, Box<dyn std::error::Error>> {
	use std::{
		io::{
			BufReader,
			read_to_string
		},
		fs::File
	};

	Ok(
		json5::from_str(
			read_to_string::<BufReader<File>>(
				&mut BufReader::new(
					File::open(file_name)?
				)
			)?
			.as_str()
		)?
	)
}

fn set_env_var(module: Module) -> () {
	use std::{
		collections::hash_map,
		iter::Peekable
	};

	type Iter<'a> = hash_map::Iter<'a, String, Module>;
	type Item<'a> = <Iter<'a> as Iterator>::Item;
	type PeekableIter<'a> = Peekable<Iter<'a>>;

	let mut module_path: String = String::new();
	let mut iter_stack: Vec<PeekableIter> = Vec::new();
	let mut env_var_val: String = module.f.to_string();

	if !module.m.is_empty() {
		iter_stack.push(module.m.iter().peekable());

		while !iter_stack.is_empty() {
			fn process_iter_stack<'a, 'b>(iter_stack: &'b mut Vec<PeekableIter<'a>>, module_path: &mut String, env_var_val: &mut String) -> Option<PeekableIter<'a>>
				where 'a: 'b
			{
				let insert_colons: bool = iter_stack.len() > 1;
				let iter_option = iter_stack.last_mut().unwrap().peek();

				if iter_option.is_some() {
					let (sub_module_path, sub_module): &Item<'a> = iter_option.unwrap();

					module_path.push_str(format!("{}{}",
						if insert_colons { "::" } else { "" },
						sub_module_path
					).as_str());
					env_var_val.push_str(format!(",{}={}", module_path, sub_module.f).as_str());

					Some(sub_module.m.iter().peekable())
				} else {
					None
				}
			}

			let iter_option: Option<PeekableIter> = process_iter_stack(&mut iter_stack, &mut module_path, &mut env_var_val);

			if iter_option.is_some() {
				iter_stack.push(iter_option.unwrap());
			} else {
				iter_stack.pop();

				if !iter_stack.is_empty() {
					let remove_colons: bool = iter_stack.len() > 1;
					let iter = iter_stack.last_mut().unwrap();
					let module_path_len: usize = module_path.len();
					let bytes_to_cut: usize = std::cmp::min(iter.peek().unwrap().0.len() + (if remove_colons { 2 } else { 0 }), module_path_len);

					module_path.truncate(module_path_len - bytes_to_cut);
					iter.next();
				}
			}
		}
	}

	println!("RUST_LOG={}",
		env_var_val
			.split(',')
			.collect::<Vec<&str>>()
			.iter()
			.map(
				|token: &&str| -> String {
					format!("\n\t{}", token)
				}
			)
			.collect::<String>()
	);
	std::env::set_var("RUST_LOG", env_var_val);
	env_logger::init();
}