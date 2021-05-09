use {
	core::fmt,
	serde::Deserialize
};

#[derive(Deserialize, Debug)]
enum LevelFilter { // A facsimile definition of log::LevelFilter, for trait derivation
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

#[derive(Debug)]
#[repr(u8)]
enum Level { // A facsimile definition of log::Level, for trait derivation
	Error,
	Warn,
	Info,
	Debug,
	Trace,
}

impl From<u8> for Level {
	fn from(val: u8) -> Self {
		unsafe { std::mem::transmute(crate::clamp!(val, 0, Level::Trace as u8)) }
	}
}

impl From<log::Level> for Level {
	fn from(level: log::Level) -> Self {
		match level {
			log::Level::Error	=> Level::Error,
			log::Level::Warn	=> Level::Warn,
			log::Level::Info	=> Level::Info,
			log::Level::Debug	=> Level::Debug,
			log::Level::Trace	=> Level::Trace
		}
	}
}

#[derive(Deserialize, Debug)]
struct Module {
	f:	LevelFilter,
	m:	std::collections::HashMap<String, Module>
}

#[macro_export]
macro_rules! log_concat {
	($part_1:expr, $part_2:expr) => {
		format!("{}::{}", $part_1, $part_2).as_str()
	};
}

#[macro_export]
macro_rules! log_path {
	() => {
		std::module_path!()
	};

	($item:expr) => {
		crate::log_concat!(std::module_path!(), $item)
	};
}

#[derive(Clone, Default)]
pub struct LogError {
	pub target:	String,
	pub error:	String,
	pub warn:	String,
	pub info:	String,
	pub debug:	String,
	pub trace:	String
}

pub type LogErrorResult<T = ()> = Result<T, LogError>;

impl LogError {
	pub fn log(&self) -> () {
		let mut has_logged: bool = false;

		if !self.error.is_empty() { log::error! (target: self.target.as_str(), "{}", self.error); has_logged = true; }
		if !self.warn .is_empty() { log::warn!  (target: self.target.as_str(), "{}", self.warn ); has_logged = true; }
		if !self.info .is_empty() { log::info!  (target: self.target.as_str(), "{}", self.info ); has_logged = true; }
		if !self.debug.is_empty() { log::debug! (target: self.target.as_str(), "{}", self.debug); has_logged = true; }
		if !self.trace.is_empty() { log::trace! (target: self.target.as_str(), "{}", self.trace); has_logged = true; }
		if !has_logged { eprintln!("log() called on empty LogError with target \"{}\"", self.target); }
	}

	fn get_message(&self) -> String {
		for level_as_u8 in Level::Error as u8 ..= Level::Trace as u8 {
			if !self[Level::from(level_as_u8)].is_empty() {
				let mut message: String = format!("[{:?} {}]: {}", Level::from(level_as_u8), self.target, self[Level::from(level_as_u8)]);
				let mut other_messages: String = String::new();

				for other_level_as_u8 in level_as_u8 + 1 ..= Level::Trace as u8 {
					if !self[Level::from(other_level_as_u8)].is_empty() {
						let is_first_message: bool = other_messages.is_empty();

						other_messages.push_str(format!("{}{:?}",
							if is_first_message { "" } else { ", " },
							Level::from(other_level_as_u8)
						).as_str());
					}
				}

				if other_messages.is_empty() {
					message.push_str(format!("\n({} also available)", other_messages).as_str());
				}

				return message;
			}
		}

		format!("[{}] No message available", self.target)
	}
}

impl std::ops::Index<Level> for LogError {
	type Output = String;

	fn index(&self, level: Level) -> &Self::Output {
		match level {
			Level::Error	=> &self.error,
			Level::Warn		=> &self.warn,
			Level::Info		=> &self.info,
			Level::Debug	=> &self.debug,
			Level::Trace	=> &self.trace,
		}
	}
}

impl std::ops::IndexMut<Level> for LogError {
	fn index_mut(&mut self, level: Level) -> &mut Self::Output {
		match level {
			Level::Error	=> &mut self.error,
			Level::Warn		=> &mut self.warn,
			Level::Info		=> &mut self.info,
			Level::Debug	=> &mut self.debug,
			Level::Trace	=> &mut self.trace,
		}
	}
}

impl std::ops::Index<log::Level> for LogError {
	type Output = String;

	fn index(&self, level: log::Level) -> &Self::Output {
		&self[Level::from(level)]
	}
}

impl std::ops::IndexMut<log::Level> for LogError {
	fn index_mut(&mut self, level: log::Level) -> &mut Self::Output {
		&mut self[Level::from(level)]
	}
}

impl fmt::Debug for LogError {
	fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
		write!(formatter, "{}", self.get_message())
	}
}

impl fmt::Display for LogError {
	fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
		write!(formatter, "{}", self.get_message())
	}
}

#[macro_export(local_inner_macros)]
macro_rules! __log_error_set_message {
	($log_error:ident, $level:expr, $message:expr) => {
		$log_error[$level] = $message;
	};

	($log_error:ident, $level:expr, $message:expr, $($other_levels:expr, $other_messages:expr),+) => {
		__log_error_set_message!($log_error, $level, $message);
		__log_error_set_message!($log_error, $($other_levels, $other_messages),+);
	}
}

#[macro_export(local_inner_macros)]
macro_rules! log_error {
	(target: $target:expr, $($levels:expr, $messages:expr),+) => {{
		let mut log_error = LogError {
			target: $target.to_string(),
			..Default::default()
		};

		__log_error_set_message!(log_error, $($levels, $messages),+);

		log_error
	}};

	($($levels:expr, $messages:expr),+) => {
		log_error!(log_path!(), $($levels, $messages),+)
	};
}

pub fn init_env_logger() -> () {
	const RUST_LOG_JSON_FILE_NAME: &str = "RUST_LOG.json5";

	match get_module(RUST_LOG_JSON_FILE_NAME) {
		Ok(module) => {
			set_env_var(module);
		},
		Err(error) => {
			eprintln!("Error opening \"{}\": {}\nLogging will be disabled", RUST_LOG_JSON_FILE_NAME, error);
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

	let pretty_env_var_val: String = env_var_val
		.split(',')
		.collect::<Vec<&str>>()
		.iter()
		.map(
			|token: &&str| -> String {
				format!("\n\t{}", token)
			}
		)
		.collect::<String>();

	std::env::set_var("RUST_LOG", env_var_val);
	env_logger::init();
	log::info!(target: log_path!("set_env_var"), "RUST_LOG={}", pretty_env_var_val);
}