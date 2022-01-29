pub mod prelude {
	pub use {
		crate::{
			trace_expr,
			debug_expr,
			info_expr,
			warn_expr,
			error_expr,
			trace_expect,
			debug_expect,
			info_expect,
			warn_expect,
			error_expect
		},
		super::{
			LogError,
			LogErrorResult,
			init_env_logger
		}
	};
}

use {
	crate::{
		prelude::*,
		strings::STRING_DATA
	},
	core::fmt,
	std::sync::Once,
	::log::{
		Level,
		LevelFilter
	},
	serde::Deserialize
};

fn usize_to_level(val: usize) -> Level {
	unsafe { std::mem::transmute(crate::clamp!(val, Level::Error as usize, Level::Trace as usize)) }
}

#[derive(Deserialize, Debug)]
struct Module {
	#[serde(default = "Module::default_level_filter")]
	f:	LevelFilter,								// Filter

	#[serde(default)]
	m:	std::collections::HashMap<String, Module>	// Modules
}

impl Module {
	fn default_level_filter() -> LevelFilter { LevelFilter::Off }
}

impl Default for Module {
	fn default() -> Self {
		Self {
			f: Self::default_level_filter(),
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
		for level_as_usize in Level::Error as usize ..= Level::Trace as usize {
			if !self[usize_to_level(level_as_usize)].is_empty() {
				let mut message: String = format!("[{:?} {}]: {}", usize_to_level(level_as_usize), self.target, self[usize_to_level(level_as_usize)]);
				let mut other_messages: String = String::new();

				for other_level_as_usize in level_as_usize + 1 ..= Level::Trace as usize {
					if !self[usize_to_level(other_level_as_usize)].is_empty() {
						let is_first_message: bool = other_messages.is_empty();

						other_messages.push_str(format!("{}{:?}",
							if is_first_message { "" } else { ", " },
							usize_to_level(other_level_as_usize)
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
			Some(value) => value,
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

#[cfg(debug_assertions)]
#[macro_export(local_inner_macros)]
macro_rules! log_expect {
	($level:path, $expr:expr) => {
		if $expr {
			true
		} else {
			$level!("\"{}\" was false", std::stringify!($expr));

			false
		}
	};
}

#[cfg(not(debug_assertions))]
#[macro_export(local_inner_macros)]
macro_rules! log_expect {
	($_:path, $expr:expr) => {
		$expr
	}
}

#[macro_export(local_inner_macros)]
macro_rules! trace_expect {
	($expr:expr) => {
		log_expect!(::log::trace, $expr)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! debug_expect {
	($expr:expr) => {
		log_expect!(::log::debug, $expr)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! info_expect {
	($expr:expr) => {
		log_expect!(::log::info, $expr)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! warn_expect {
	($expr:expr) => {
		log_expect!(::log::warn, $expr)
	};
}

#[macro_export(local_inner_macros)]
macro_rules! error_expect {
	($expr:expr) => {
		log_expect!(::log::error, $expr)
	};
}

pub fn init_env_logger() -> () {
	INIT_ENV_LOGGER.call_once(|| -> () {
		set_env_var(Module::from_file_or_default(&STRING_DATA.files.rust_log));
	});
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

static INIT_ENV_LOGGER: Once = Once::new();