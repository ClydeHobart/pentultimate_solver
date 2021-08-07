mod log;

use serde::Deserialize;

pub use crate::util::log::{
	LogError,
	LogErrorResult,
	init_env_logger
};

use {
	std::fmt::Debug,
	bevy::app::{
		AppExit,
		EventWriter
	},
	::log::Level
};

use crate::prelude::*;

pub trait FromAlt<T> {
	fn from_alt(value: T) -> Self;
}

pub trait ToResult<T> where Self: Sized {
	fn to_result(self) -> LogErrorResult<T>;
}

impl<T> ToResult<T> for Option<T> {
	fn to_result(self) -> LogErrorResult<T> {
		match self {
			Some(value) => Ok(value),
			None => Err(log_error!(Level::Debug, "Option was none".into()))
		}
	}
}

pub trait ToOption<T> where Self: Sized {
	fn to_option(self) -> Option<T>;
}

trait DisplayError where Self: Sized {
	fn display_error(self) -> ();
}

impl<T: Sized + Debug> DisplayError for T {
	#[cfg(feature = "specialization")]
	default fn display_error(self) -> () {
		eprintln!("{:#?}", self);
	}

	#[cfg(not(feature = "specialization"))]
	fn display_error(self) -> () {
		eprintln!("{:#?}", self);
	}
}

#[cfg(feature = "specialization")]
impl DisplayError for LogError {
	fn display_error(self) -> () {
		self.log();
	}
}

impl<T, E> ToOption<T> for Result<T, E> where E: Debug {
	fn to_option(self) -> Option<T> {
		if self.is_ok() {
			self.ok()
		} else {
			if let Some(error) = self.err() {
				error.display_error();
			}

			None
		}
	}
}

pub fn from_ron<T>(file_name: &str) -> Result<T, Box<dyn std::error::Error>>
	where
		T: for<'de> Deserialize<'de>
{
	use {
		std::{
			io::{
				BufReader,
				read_to_string
			},
			fs::File,
		},
		ron::de::from_str
	};

	Ok(
		from_str(
			read_to_string::<BufReader<File>>(
				&mut BufReader::<File>::new(
					File::open(
						file_name
					)?
				)
			)?
			.as_str()
		)?
	)
}

pub fn exit_app(mut exit: EventWriter<AppExit>) -> () {
	exit.send(AppExit);
}

#[cfg(test)]
mod tests {
	use {
		crate::prelude::*,
		super::{
			ToOption,
			ToResult
		},
		log::Level
	};

	#[test]
	fn to_result() -> () {
		macro_rules! test_some {
			($e:expr, $t:ty) => {
				assert!({
					let result: LogErrorResult<$t> = Some($e).to_result();
		
					result.is_ok() && result.unwrap() == $e
				});
			};
		}

		test_some!(5,			i32);
		test_some!("foo",		&str);
		test_some!(15.0_f32,	f32);

		assert!({
			let result: LogErrorResult = None.to_result();

			result.is_err() && result.unwrap_err().debug == "Option was none"
		});
	}

	fn to_option_test_ok() {
		macro_rules! test_ok {
			($expr:expr, $t:ty, $e:ty) => {
				assert!({
					let option: Option<$t> = Result::<$t, $e>::Ok($expr).to_option();

					option.is_some() && option.unwrap() == $expr
				});
			};
		}

		test_ok!(5,			i32,	f32);
		test_ok!("foo",		&str,	i32);
		test_ok!(15.0_f32,	f32,	&str);
	}

	macro_rules! test_err {
		($expr:expr, $t:ty, $e:ty) => {
			assert!({
				println!("\n******** BEGIN EXPECT ********\n{:#?}\n******** END EXPECT ********\n******** BEGIN ERROR ********", $expr);

				let option: Option<$t> = Result::<$t, $e>::Err($expr).to_option();

				println!("******** END ERROR ********");

				option.is_none()
			});
		};
	}

	fn to_option_test_err_std() -> () {
		test_err!(5,		f32,	i32);
		test_err!("foo",	i32,	&str);
		test_err!(15.0_f32,	&str,	f32);
	}

	#[cfg(feature = "specialization")]
	macro_rules! test_log_error {
		($expr:expr, $t:ty) => {
			assert!({
				println!("\n******** BEGIN EXPECT ********");

				$expr.log();

				println!("******** END EXPECT ********\n******** BEGIN ERROR ********");

				let option: Option<$t> = LogErrorResult::<$t>::Err($expr).to_option();

				println!("******** END ERROR ********");

				option.is_none()
			});
		};
	}

	#[cfg(not(feature = "specialization"))]
	macro_rules! test_log_error {
		($expr:expr, $t:ty) => {
			log_error!($expr, $t, LogError);
		};
	}

	fn to_option_test_err_log_error() -> () {
		test_log_error!(log_error!(Level::Error,	"Error Message".into()),	i32);
		test_log_error!(log_error!(Level::Warn,		"Warn Message".into()),		u32);
		test_log_error!(log_error!(Level::Info,		"Info Message".into()),		f32);
		test_log_error!(log_error!(Level::Debug,	"Debug Message".into()),	i64);
		test_log_error!(log_error!(Level::Trace,	"Trace Message".into()),	f64);
	}

	fn to_option_test_err() -> () {
		std::env::set_var("RUST_LOG", log_path!());
		env_logger::init();

		to_option_test_err_std();
		to_option_test_err_log_error();
	}

	#[test]
	fn to_option() -> () {
		to_option_test_ok();
		to_option_test_err();
	}
}