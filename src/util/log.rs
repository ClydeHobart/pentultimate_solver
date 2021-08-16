pub mod prelude {
	pub use {
		crate::{
			trace_expr,
			debug_expr,
			info_expr,
			warn_expr,
			error_expr
		},
		super::{
			LogError,
			LogErrorResult
		}
	};
}

use {
	crate::{
		prelude::*,
		strings::STRING_DATA
	},
	core::fmt,
	::log::Level as LogLevel,
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

impl From<LogLevel> for Level {
	fn from(level: LogLevel) -> Self {
		match level {
			LogLevel::Error	=> Level::Error,
			LogLevel::Warn	=> Level::Warn,
			LogLevel::Info	=> Level::Info,
			LogLevel::Debug	=> Level::Debug,
			LogLevel::Trace	=> Level::Trace
		}
	}
}

#[derive(Deserialize, Debug)]
struct Module {
	f:	LevelFilter,								// Filter
	m:	std::collections::HashMap<String, Module>	// Modules
}

impl Default for Module {
	fn default() -> Self {
		Self {
			f: LevelFilter::Off,
			m: std::collections::HashMap::<String, Module>::default()
		}
	}
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

		if !self.error.is_empty() { error! (target: self.target.as_str(), "{}", self.error); has_logged = true; }
		if !self.warn .is_empty() { warn!  (target: self.target.as_str(), "{}", self.warn ); has_logged = true; }
		if !self.info .is_empty() { info!  (target: self.target.as_str(), "{}", self.info ); has_logged = true; }
		if !self.debug.is_empty() { debug! (target: self.target.as_str(), "{}", self.debug); has_logged = true; }
		if !self.trace.is_empty() { trace! (target: self.target.as_str(), "{}", self.trace); has_logged = true; }
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

impl std::ops::Index<LogLevel> for LogError {
	type Output = String;

	fn index(&self, level: LogLevel) -> &Self::Output {
		&self[Level::from(level)]
	}
}

impl std::ops::IndexMut<LogLevel> for LogError {
	fn index_mut(&mut self, level: LogLevel) -> &mut Self::Output {
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

impl std::error::Error for LogError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> { None }
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
		log_error!(target: log_path!(), $($levels, $messages),+)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! log_dyn_error {
	($error:expr) => {
		match (&$error as &dyn std::any::Any).downcast_ref::<LogError>() {
			Some(log_error) => {
				log_error.log();
			},
			None => {
				::log::warn!("{:?}", $error);
			}
		}
	};
}

#[macro_export(local_inner_macros)]
macro_rules! log_result_err {
	($result:expr) => {
		match $result {
			Ok(value) => value,
			Err(error) => {
				log_dyn_error!(error);

				return;
			}
		}
	};

	($result:expr, $default:expr) => {
		match $result {
			Ok(value) => value,
			Err(error) => {
				log_dyn_error!(error);

				$default
			}
		}
	};
}

#[macro_export(local_inner_macros)]
macro_rules! log_option_none {
	($option:expr) => {
		match $option {
			Some(value) => value,
			None => {
				::log::warn!("\"{}\" was None", std::stringify!($option));

				return;
			}
		}
	};

	($option:expr, $default:expr) => {
		match $option {
			Ok(value) => value,
			None => {
				::log::warn!("\"{}\" was None", std::stringify!($option));

				$default
			}
		}
	};
}

#[macro_export(local_inner_macros)]
macro_rules! __log_expr_literals {
	($fmt:literal, $_:expr) => {
		std::concat!("{}: {", $fmt, "}")
	};

	($fmt:literal, $expr:expr, $($exprs:expr),+) => {
		std::concat!(__log_expr_literals!($fmt, $expr), "\n", __log_expr_literals!($fmt, $($exprs),+))
	};
}

#[macro_export(local_inner_macros)]
macro_rules! log_expr {
	($level:path, $fmt:literal, $($expr:expr),+) => {
		$level!(__log_expr_literals!($fmt, $($expr),+),
			$(
				std::stringify!($expr), $expr
			),+
		);
	};
}

#[macro_export(local_inner_macros)]
macro_rules! trace_expr {
	(fmt: $fmt:literal, $($expr:expr),+) => {
		log_expr!(::log::trace, $fmt, $($expr),+);
	};

	($($expr:expr),+) => {
		trace_expr!(fmt: ":#?", $($expr),+)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! debug_expr {
	(fmt: $fmt:literal, $($expr:expr),+) => {
		log_expr!(::log::debug, $fmt, $($expr),+);
	};

	($($expr:expr),+) => {
		debug_expr!(fmt: ":#?", $($expr),+)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! info_expr {
	(fmt: $fmt:literal, $($expr:expr),+) => {
		log_expr!(::log::info, $fmt, $($expr),+);
	};

	($($expr:expr),+) => {
		info_expr!(fmt: ":#?", $($expr),+)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! warn_expr {
	(fmt: $fmt:literal, $($expr:expr),+) => {
		log_expr!(::log::warn, $fmt, $($expr),+);
	};

	($($expr:expr),+) => {
		warn_expr!(fmt: ":#?", $($expr),+)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! error_expr {
	(fmt: $fmt:literal, $($expr:expr),+) => {
		log_expr!(::log::error, $fmt, $($expr),+);
	};

	($($expr:expr),+) => {
		error_expr!(fmt: ":#?", $($expr),+)
	};
}

#[macro_export]
macro_rules! init_log {
	() => {
		std::env::set_var("RUST_LOG", "Trace");
		let _ = env_logger::try_init();
	};
}

pub fn init_env_logger() -> () {
	set_env_var(from_ron_or_default(&STRING_DATA.files.rust_log));
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
	info!(target: log_path!("set_env_var"), "RUST_LOG={}", pretty_env_var_val);
}