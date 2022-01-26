mod log;

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
pub mod simd;

pub mod prelude {
	pub use super::{
		log::prelude::*,
		FromAlt,
		IntoAlt,
		ShortSlerp,
		StaticDataLibrary,
		ToOption,
		ToResult,
		from_file,
		from_file_or_default,
		init_env_logger,
		red_to_green,
		untracked_ref,
		untracked_ref_mut
	};
}

pub mod inspectable_bin_map;
pub mod inspectable_bit_array;

pub use self::log::init_env_logger;

use {
	crate::prelude::*,
	std::{
		any::type_name,
		cmp::min,
		error::Error as StdError,
		ffi::OsStr,
		fmt::{
			Debug,
			Write
		},
		fs::File,
		mem::transmute,
		path::Path,
		str,
		time::Duration
	},
	num_format::{
		Buffer,
		Locale
	},
	bevy::{
		app::{
			AppExit,
			EventWriter
		},
		render::color::Color
	},
	egui::color::Color32,
	::log::Level,
	memmap::Mmap,
	serde::Deserialize,
	simple_error::SimpleError
};

pub trait FromAlt<T> {
	fn from_alt(value: T) -> Self;
}

impl FromAlt<Duration> for String {
	fn from_alt(value: Duration) -> Self {
		const CONVERSION_FACTORS: [u128; 7_usize] = [
			1000_u128,
			1000_u128,
			1000_u128,
			60_u128,
			60_u128,
			24_u128,
			u128::MAX
		];

		#[allow(dead_code)] // Not actually dead code due to the transmute
		#[derive(PartialEq)]
		enum Field {
			Nano,
			Micro,
			Milli,
			Sec,
			Min,
			Hr,
			Day
		}

		let mut fields: [u32; 7_usize] = [0_u32; 7_usize];
		let mut populated_fields: u8 = 0_u8;
		let mut nanos: u128 = value.as_nanos();

		for field_index in Field::Nano as usize ..= Field::Day as usize {
			let conversion_factor: u128 = CONVERSION_FACTORS[field_index];
			let field: u32 = (nanos % conversion_factor) as u32;

			fields[field_index] = field;
			populated_fields |= ((field != 0_u32) as u8) << field_index;
			nanos /= conversion_factor;
		}

		let		min_field_i8:		i8		= populated_fields.trailing_zeros() as i8;
		let		max_field_i8:		i8		= (i8::BITS - min(populated_fields.leading_zeros(), 7_u32) - 1_u32)
			as i8;
		let		max_field:			Field	= unsafe { transmute::<i8, Field>(max_field_i8) };
		let mut	field_i8:			i8		= max_field_i8;
		let mut	duration_string:	String	= String::new();

		macro_rules! print_min_field_digits {
			() => {{
				let field_count: u32 = fields[field_i8 as usize];
				let (digits, digit_count): (u32, usize) = {
					if field_count % 10_u32 != 0_u32 {
						(field_count, 3_usize)
					} else if field_count % 100_u32 != 0_u32 {
						(field_count / 10_u32, 2_usize)
					} else {
						(field_count / 100_u32, 1_usize)
					}
				};

				match max_field {
					Field::Sec		=> write!(duration_string,	"{0:01$}s",		digits,	digit_count),
					Field::Milli	=> write!(duration_string,	"{0:01$}ms",	digits,	digit_count),
					Field::Micro	=> write!(duration_string,	"{0:01$}μs",	digits,	digit_count),
					_				=> write!(duration_string,	"{0:01$}",		digits,	digit_count)
				}
			}}
		}

		while field_i8 >= 0 {
			let field: Field = unsafe { transmute::<i8, Field>(field_i8) };

			match field {
				Field::Day => {
					let mut buffer: Buffer = Buffer::default();

					buffer.write_formatted(&fields[Field::Day as usize], &Locale::en);
					write!(duration_string, "{}d", buffer.as_str()).unwrap();

					if field_i8 == min_field_i8 {
						field_i8 = 0_i8;
					} else {
						write!(duration_string, " ").unwrap();
					}
				},
				Field::Hr	=> { write!(duration_string, "{:02}:", fields[Field::Hr as usize]).unwrap(); },
				Field::Min	=> { write!(duration_string, "{:02}:", fields[Field::Min as usize]).unwrap(); },
				Field::Sec	=> {
					let is_min_field: bool = field_i8 == min_field_i8;
					let is_max_field: bool = field_i8 == max_field_i8;
					let secs: u32 = fields[Field::Sec as usize];

					match (is_min_field, is_max_field) {
						(false,	false)	=> write!(duration_string,	"{:02}.",	secs),
						(false,	true)	=> write!(duration_string,	"{}.",		secs),
						(true,	false)	=> write!(duration_string,	"{:02}",	secs),
						(true,	true)	=> write!(duration_string,	"{}s",		secs)
					}.unwrap();

					if is_min_field {
						field_i8 = 0_i8;
					}
				},
				Field::Milli => {
					let is_min_field: bool = field_i8 == min_field_i8;
					let is_max_field: bool = field_i8 == max_field_i8;
					let millis: u32 = fields[Field::Milli as usize];

					match (is_min_field, is_max_field) {
						(false, false) => write!(duration_string, "{:03}", millis),
						(false, true) => write!(duration_string, "{}.", millis),
						(true, false) => print_min_field_digits!(),
						(true, true) => write!(duration_string, "{}ms", millis)
					}.unwrap();

					if is_min_field {
						field_i8 = 0_i8;
					}
				},
				Field::Micro => {
					let is_min_field: bool = field_i8 == min_field_i8;
					let is_max_field: bool = field_i8 == max_field_i8;
					let micros: u32 = fields[Field::Micro as usize];

					match (is_min_field, is_max_field) {
						(false, false) => write!(duration_string, "{:03}", micros),
						(false, true) => write!(duration_string, "{}.", micros),
						(true, false) => print_min_field_digits!(),
						(true, true) => write!(duration_string, "{}μs", micros)
					}.unwrap();

					if is_min_field {
						field_i8 = 0_i8;
					}
				},
				Field::Nano => {
					let nanos: u32 = fields[Field::Nano as usize];

					if field_i8 == max_field_i8 {
						write!(duration_string, "{}ns", nanos)
					} else {
						print_min_field_digits!()
					}.unwrap();
				}
			}

			field_i8 -= 1_i8;
		}

		duration_string
	}
}

impl FromAlt<Color> for Color32 {
	fn from_alt(value: Color) -> Self {
		const U8_MAX_F32: f32 = u8::MAX as f32;
		let rgba: [f32; 4_usize] = value.as_rgba_f32();

		Color32::from_rgba_premultiplied(
			(rgba[0_usize] * U8_MAX_F32) as u8,
			(rgba[1_usize] * U8_MAX_F32) as u8,
			(rgba[2_usize] * U8_MAX_F32) as u8,
			(rgba[3_usize] * U8_MAX_F32) as u8
		)
	}
}

impl FromAlt<[u8; 4_usize]> for Color32 {
	fn from_alt(value: [u8; 4_usize]) -> Self { unsafe { transmute::<[u8; 4_usize], Self>(value) } }
}

pub trait IntoAlt<T> {
	fn into_alt(self) -> T;
}

impl<T, U> IntoAlt<U> for T
	where U: FromAlt<T>
{
	fn into_alt(self) -> U {
		U::from_alt(self)
	}
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

#[macro_export(local_inner_macros)]
macro_rules! option_to_result {
	($expr:expr) => {
		match $expr {
			Some(value) => Ok(value),
			None => Err(log_error!(::log::Level::Debug, std::format!("\"{}\" was None", std::stringify!($expr))))
		}
	};
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

pub trait StaticDataLibrary {
	/// Initializes the data if it needs to, otherwise does nothing
	fn initialize() -> ();
	/// Calls `initialize()`, then returns `get()`
	fn initialize_and_get() -> &'static Self {
		Self::initialize();

		Self::get()
	}
  
	/// Fetches the data, assuming it has already been initialized
	fn get() -> &'static Self;
}

#[derive(Clone, Copy)]
pub enum SerFmt {
	Bincode,
	JSON,
	JSON5,
	RON,
	TOML
}

impl SerFmt {
	const fn file_extension(self) -> &'static str{
		match self {
			Self::Bincode	=> "bc",
			Self::JSON		=> "json",
			Self::JSON5		=> "json5",
			Self::RON		=> "ron",
			Self::TOML		=> "toml"
		}
	}
}

impl TryFrom<&str> for SerFmt {
	type Error = ();

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		if value == Self::Bincode	.file_extension() { Ok(Self::Bincode	) } else
		if value == Self::JSON		.file_extension() { Ok(Self::JSON		) } else
		if value == Self::JSON5		.file_extension() { Ok(Self::JSON5		) } else
		if value == Self::RON		.file_extension() { Ok(Self::RON		) } else
		if value == Self::TOML		.file_extension() { Ok(Self::TOML		) } else

		{
			Err(())
		}
	}
}

pub fn from_file<T>(file_name: &str) -> Result<T, Box<dyn StdError>>
	where T: for<'de> Deserialize<'de>
{
	let function_call = || -> String { format!("from_file::<{}>(\"{}\")", type_name::<T>(), file_name) };
	let invalid_file_extension = || -> Box<dyn StdError> {
		Box::new(SimpleError::new(format!("{} doesn't have a valid file extension", function_call())))
	};
	let ser_fmt: SerFmt = SerFmt::try_from(
		Path::new(file_name)
			.extension()
			.map_or_else(
				|| -> Result<&str, Box<dyn StdError>> { Err(invalid_file_extension()) },
				|os_str: &OsStr| -> Result<&str, Box<dyn StdError>> {
					if let Some(unicode_str) = os_str.to_str() {
						Ok(unicode_str)
					} else {
						Err(invalid_file_extension())
					}
				}
			)?
	)
		.map_err(
			|_: ()| -> Box<dyn StdError> { invalid_file_extension() },
		)?;
	let file: File = File::open(file_name)?;
	let mmap: Mmap = unsafe { Mmap::map(&file) }?;
	let bytes: &[u8] = &mmap;

	macro_rules! deserialize {
		($($ser_fmt:ident, $result_expr:expr, $err_type:ty);*) => {
			match ser_fmt {
				$(
					SerFmt::$ser_fmt => {
						$result_expr.map_err(
							|error: $err_type| -> Box<dyn StdError> {
								Box::new(SimpleError::new(format!("{}: {:#?}", function_call(), error)))
							}
						)
					}
				)*
			}
		}
	}

	deserialize!(
		Bincode,	bincode::deserialize::<T>(bytes),				Box<bincode::ErrorKind>;
		JSON,		serde_json::from_slice::<T>(bytes),				serde_json::Error;
		JSON5,		json5::from_str::<T>(str::from_utf8(bytes)?),	json5::Error;
		RON,		ron::de::from_bytes::<T>(bytes),				ron::Error;
		TOML,		toml::from_slice::<T>(bytes),					toml::de::Error
	)
}

pub fn from_file_or_default<T>(file_name: &str) -> T
	where T: Default + for<'de> Deserialize<'de>
{
	match from_file(file_name) {
		Ok(value) => { value },
		Err(err) => {
			::log::warn!("Error encountered deserializing \"{}\": {:?}", file_name, err);

			T::default()
		}
	}
}

pub fn exit_app(mut exit: EventWriter<AppExit>) -> () {
	exit.send(AppExit);
}

pub trait ShortSlerp where Self: Sized {
	fn short_slerp(self, end: Self, s: f32) -> Self;
}

impl ShortSlerp for bevy::math::Quat {
	fn short_slerp(self, end: Self, s: f32) -> Self {
		if self.dot(end) >= 0.0_f32 {
			self.slerp(end, s)
		} else {
			(-self).slerp(end, s)
		}
	}
}

pub fn red_to_green(s: f32) -> Color {
	let s: f32 = {
		let s: f32 = s.clamp(0.0_f32, 1.0_f32);

		if s.is_nan() {
			0.0_f32
		} else {
			s
		}
	};

	Color::hsl(s * 120.0_f32, 1.0_f32, 0.5_f32)
}

// Macro adapted from https://stackoverflow.com/questions/66291962/how-do-i-use-macro-rules-to-define-a-struct-with-optional-cfg
#[macro_export(local_inner_macros)]
macro_rules! define_struct_with_default {
	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis $struct_name:ident {
			$(
				$(#[$field_attr:meta])?
				$field_vis:vis $field:ident : $field_type:ty = $value:expr,
			)*
		}
	) => {
		$(#[$struct_attr])*
		$struct_vis struct $struct_name {
			$(
				$(#[$field_attr])?
				$field_vis $field: $field_type,
			)*
		}
		
		impl Default for $struct_name {
			fn default() -> Self {
				Self {
					$(
						$field: $value.into(),
					)*
				}
			}
		}
	};

	(
		$(#[$struct_attr:meta])*
		$struct_vis:vis $struct_name:ident <$field_type:ty> {
			$(
				$(#[$field_attr:meta])?
				$field_vis:vis $field:ident = $value:expr,
			)*
		}
	) => {
		$(#[$struct_attr])*
		$struct_vis struct $struct_name {
			$(
				$(#[$field_attr])?
				$field_vis $field: $field_type,
			)*
		}
		
		impl Default for $struct_name {
			fn default() -> Self {
				Self {
					$(
						$field: $value.into(),
					)*
				}
			}
		}
	};
}

/// Returns a copy of an immutable reference that is no longer tracked by the borrow checker
///
/// Though the returned reference nominally has the 'static lifetime, this is just used to trick the borrow checker. It
/// is the users responsibility to ensure that the referenced data isn't mutated while the reference is in scope. This
/// can be useful in cases where a user wants to hold a mutable reference to one field of a struct, and an immutable
/// reference to another field
pub fn untracked_ref<T>(reference: &T) -> &'static T {
	unsafe { (reference as *const T as usize as *const T).as_ref() }.unwrap()
}

/// Returns a copy of a mutable reference that is no longer tracked by the borrow checker
///
/// Though the returned reference nominally has the 'static lifetime, this is just used to trick the borrow checker. It
/// is the users responsibility to ensure that the referenced data isn't mutated by other threads while the reference is
/// in scope. This can be useful in cases where a user wants to hold a mutable reference to one field of a struct, and 
/// an immutable reference to another field
pub fn untracked_ref_mut<T>(reference: &mut T) -> &'static mut T {
	unsafe { (reference as *mut T as usize as *mut T).as_mut() }.unwrap()
}

#[macro_export]
macro_rules! max { ($a:expr, $b:expr) => { if $a > $b { $a } else { $b } } }

#[cfg(test)]
mod tests {
	use {
		crate::prelude::*,
		super::{
			ToOption,
			ToResult
		},
		std::time::Duration,
		::log::Level
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
			test_err!($expr, $t, LogError);
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
		init_env_logger();
		to_option_test_err_std();
		to_option_test_err_log_error();
	}

	#[test]
	fn to_option() -> () {
		init_env_logger();
		to_option_test_ok();
		to_option_test_err();
	}

	#[test]
	fn string_from_alt_duration() -> () {
		assert_eq!(String::from_alt(Duration::from_nanos(1_u64)),						"1ns");
		assert_eq!(String::from_alt(Duration::from_nanos(12_u64)),						"12ns");
		assert_eq!(String::from_alt(Duration::from_nanos(123_u64)),						"123ns");
		assert_eq!(String::from_alt(Duration::from_nanos(1_234_u64)),					"1.234μs");
		assert_eq!(String::from_alt(Duration::from_nanos(12_345_u64)),					"12.345μs");
		assert_eq!(String::from_alt(Duration::from_nanos(123_456_u64)),					"123.456μs");
		assert_eq!(String::from_alt(Duration::from_nanos(1_234_567_u64)),				"1.234567ms");
		assert_eq!(String::from_alt(Duration::from_nanos(12_345_678_u64)),				"12.345678ms");
		assert_eq!(String::from_alt(Duration::from_nanos(123_456_789_u64)),				"123.456789ms");
		assert_eq!(String::from_alt(Duration::from_nanos(1_234_567_891_u64)),			"1.234567891s");
		assert_eq!(String::from_alt(Duration::from_nanos(12_345_678_912_u64)),			"12.345678912s");
		assert_eq!(String::from_alt(Duration::from_nanos(123_456_789_123_u64)),			"02:03.456789123");
		assert_eq!(String::from_alt(Duration::from_nanos(1_234_567_891_234_u64)),		"20:34.567891234");
		assert_eq!(String::from_alt(Duration::from_nanos(12_345_678_912_345_u64)),		"03:25:45.678912345");
		assert_eq!(String::from_alt(Duration::from_nanos(123_456_789_123_456_u64)),		"1d 10:17:36.789123456");
		assert_eq!(String::from_alt(Duration::from_nanos(1_234_567_891_234_567_u64)),	"14d 06:56:07.891234567");
		assert_eq!(String::from_alt(Duration::from_nanos(12_345_678_912_345_678_u64)),	"142d 21:21:18.912345678");
		assert_eq!(String::from_alt(Duration::from_nanos(123_456_789_123_456_789_u64)),	"1,428d 21:33:09.123456789");
	}
}