use {
    crate::prelude::*,
    bevy::{
        app::AppExit,
        ecs::{
            event::Events,
            world::{Mut, World},
        },
        render::color::Color,
    },
    bit_field::{BitArray, BitField},
    egui::Color32,
    num_traits::PrimInt,
    serde::{Deserialize, Serialize},
    simple_error::SimpleError,
    std::{
        any::type_name,
        cmp::min,
        convert::{AsRef, TryFrom},
        error::Error as StdError,
        ffi::OsStr,
        fmt::{Debug, Write},
        fs::File,
        io::{BufWriter, Write as IoWrite},
        mem::{transmute, ManuallyDrop, MaybeUninit},
        ops::Range,
        path::Path,
        str,
        sync::Once,
        time::{Duration, Instant},
    },
};

#[cfg(miri)]
use std::io::Read;

#[cfg(not(miri))]
use memmap::Mmap;

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
pub mod simd;

pub mod prelude {
    pub use super::{
        debug_break, exit_app, log::prelude::*, red_to_green, to_pretty_string, AsBitString,
        DefaultArray, FromAlt, FromFile, FromFileOrDefault, IntoAlt, SerFmt, ShortSlerp,
        StaticDataLibrary, ToFile, WithLengthAndCapacity,
    };
}

pub mod inspectable_bin_map;
pub mod inspectable_bit_array;
pub mod inspectable_num;
pub mod triangular_array;

mod log;

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
            u128::MAX,
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
            Day,
        }

        let mut fields: [u64; 7_usize] = [0_u64; 7_usize];
        let mut populated_fields: u8 = 0_u8;
        let mut nanos: u128 = value.as_nanos();

        for field_index in Field::Nano as usize..=Field::Day as usize {
            let conversion_factor: u128 = CONVERSION_FACTORS[field_index];
            let field: u64 = (nanos % conversion_factor) as u64;

            fields[field_index] = field;
            populated_fields |= ((field != 0_u64) as u8) << field_index;
            nanos /= conversion_factor;
        }

        let min_field_i8: i8 = populated_fields.trailing_zeros() as i8;
        let max_field_i8: i8 =
            (i8::BITS - min(populated_fields.leading_zeros(), 7_u32) - 1_u32) as i8;
        let max_field: Field = unsafe { transmute::<i8, Field>(max_field_i8) };
        let mut field_i8: i8 = max_field_i8;
        let mut duration_string: String = String::new();

        macro_rules! print_min_field_digits {
            () => {{
                let field_count: u64 = fields[field_i8 as usize];
                let (digits, digit_count): (u64, usize) = {
                    if field_count % 10_u64 != 0_u64 {
                        (field_count, 3_usize)
                    } else if field_count % 100_u64 != 0_u64 {
                        (field_count / 10_u64, 2_usize)
                    } else {
                        (field_count / 100_u64, 1_usize)
                    }
                };

                match max_field {
                    Field::Sec => write!(duration_string, "{0:01$}s", digits, digit_count),
                    Field::Milli => write!(duration_string, "{0:01$}ms", digits, digit_count),
                    Field::Micro => write!(duration_string, "{0:01$}μs", digits, digit_count),
                    _ => write!(duration_string, "{0:01$}", digits, digit_count),
                }
            }};
        }

        while field_i8 >= 0 {
            let field: Field = unsafe { transmute::<i8, Field>(field_i8) };

            match field {
                Field::Day => {
                    // This used to use num_format::Buffer, but Miri sees UB in that
                    const MAX_TRIO_COUNT: usize = {
                        let mut max_days: u64 = u64::MAX
                            / (CONVERSION_FACTORS[Field::Sec as usize]
                                * CONVERSION_FACTORS[Field::Min as usize]
                                * CONVERSION_FACTORS[Field::Hr as usize])
                                as u64;
                        let mut trio_count: usize = 0_usize;

                        while max_days != 0_u64 {
                            trio_count += 1_usize;
                            max_days /= 1000_u64;
                        }

                        trio_count
                    };
                    let mut digit_trios: [u16; MAX_TRIO_COUNT] = [0_u16; MAX_TRIO_COUNT];
                    let mut populated_trios: usize = 0_usize;
                    let mut days: u64 = fields[Field::Day as usize];

                    while days != 0_u64 {
                        digit_trios[populated_trios] = (days % 1000_u64) as u16;
                        days /= 1000_u64;
                        populated_trios += 1_usize;
                    }

                    /* This shouldn't happen, since that would mean the field isn't populated, but
                    handle it properly anyway */
                    if !warn_expect!(populated_trios > 0_usize) {
                        populated_trios = 1_usize;
                    }

                    for (trio_index, trio) in digit_trios[0_usize..populated_trios]
                        .iter()
                        .enumerate()
                        .rev()
                    {
                        write!(
                            duration_string,
                            "{0:1$}{2}",
                            trio,
                            if trio_index == populated_trios - 1_usize {
                                0_usize
                            } else {
                                3_usize
                            },
                            if trio_index != 0_usize { "," } else { "" }
                        )
                        .unwrap();
                    }

                    write!(duration_string, "d").unwrap();

                    if field_i8 == min_field_i8 {
                        field_i8 = 0_i8;
                    } else {
                        write!(duration_string, " ").unwrap();
                    }
                }
                Field::Hr => {
                    write!(duration_string, "{:02}:", fields[Field::Hr as usize]).unwrap();
                }
                Field::Min => {
                    write!(duration_string, "{:02}:", fields[Field::Min as usize]).unwrap();
                }
                Field::Sec => {
                    let is_min_field: bool = field_i8 == min_field_i8;
                    let is_max_field: bool = field_i8 == max_field_i8;
                    let secs: u64 = fields[Field::Sec as usize];

                    match (is_min_field, is_max_field) {
                        (false, false) => write!(duration_string, "{:02}.", secs),
                        (false, true) => write!(duration_string, "{}.", secs),
                        (true, false) => write!(duration_string, "{:02}", secs),
                        (true, true) => write!(duration_string, "{}s", secs),
                    }
                    .unwrap();

                    if is_min_field {
                        field_i8 = 0_i8;
                    }
                }
                Field::Milli => {
                    let is_min_field: bool = field_i8 == min_field_i8;
                    let is_max_field: bool = field_i8 == max_field_i8;
                    let millis: u64 = fields[Field::Milli as usize];

                    match (is_min_field, is_max_field) {
                        (false, false) => write!(duration_string, "{:03}", millis),
                        (false, true) => write!(duration_string, "{}.", millis),
                        (true, false) => print_min_field_digits!(),
                        (true, true) => write!(duration_string, "{}ms", millis),
                    }
                    .unwrap();

                    if is_min_field {
                        field_i8 = 0_i8;
                    }
                }
                Field::Micro => {
                    let is_min_field: bool = field_i8 == min_field_i8;
                    let is_max_field: bool = field_i8 == max_field_i8;
                    let micros: u64 = fields[Field::Micro as usize];

                    match (is_min_field, is_max_field) {
                        (false, false) => write!(duration_string, "{:03}", micros),
                        (false, true) => write!(duration_string, "{}.", micros),
                        (true, false) => print_min_field_digits!(),
                        (true, true) => write!(duration_string, "{}μs", micros),
                    }
                    .unwrap();

                    if is_min_field {
                        field_i8 = 0_i8;
                    }
                }
                Field::Nano => {
                    let nanos: u64 = fields[Field::Nano as usize];

                    if field_i8 == max_field_i8 {
                        write!(duration_string, "{}ns", nanos)
                    } else {
                        print_min_field_digits!()
                    }
                    .unwrap();
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
            (rgba[3_usize] * U8_MAX_F32) as u8,
        )
    }
}

impl FromAlt<[u8; 4_usize]> for Color32 {
    fn from_alt(value: [u8; 4_usize]) -> Self {
        unsafe { transmute::<[u8; 4_usize], Self>(value) }
    }
}

impl<T: PrimInt, U: PrimInt> FromAlt<Range<T>> for Option<Range<U>> {
    fn from_alt(range: Range<T>) -> Self {
        Some(U::from(range.start)?..U::from(range.end)?)
    }
}

pub trait IntoAlt<T> {
    fn into_alt(self) -> T;
}

impl<T, U> IntoAlt<U> for T
where
    U: FromAlt<T>,
{
    fn into_alt(self) -> U {
        U::from_alt(self)
    }
}

trait DisplayError
where
    Self: Sized,
{
    fn display_error(self);
}

impl<T: Sized + Debug> DisplayError for T {
    #[cfg(feature = "specialization")]
    default fn display_error(self) {
        eprintln!("{:#?}", self);
    }

    #[cfg(not(feature = "specialization"))]
    fn display_error(self) {
        eprintln!("{:#?}", self);
    }
}

#[cfg(feature = "specialization")]
impl DisplayError for LogError {
    fn display_error(self) {
        self.log();
    }
}

pub trait StaticDataLibrary: 'static {
    type Target;

    fn pre_init() -> Option<Box<dyn FnOnce()>> {
        None
    }

    fn init() -> Option<Box<dyn FnOnce()>> {
        Some(Box::new(|| {
            Self::get();
        }))
    }

    fn post_init() -> Option<Box<dyn FnOnce()>> {
        None
    }

    fn get() -> Self::Target;

    fn get_once() -> Option<&'static Once> {
        None /* Some types utilize lazy_static!, which maintains its own Once */
    }

    fn build() {
        miri_echo!();

        let build = || {
            macro_rules! call_build_stage {
                ($stage:ident) => {{
                    if let Some($stage) = Self::$stage() {
                        let start: Instant = Instant::now();

                        $stage();

                        let end: Instant = Instant::now();

                        ::log::info!(
                            "StaticDataLibrary::{}() called for {} in {}",
                            stringify!($stage),
                            std::any::type_name::<Self>(),
                            String::from_alt(end - start)
                        );
                    }
                }};
            }

            let start: Instant = Instant::now();

            call_build_stage!(pre_init);
            call_build_stage!(init);
            call_build_stage!(post_init);

            let end: Instant = Instant::now();

            ::log::info!(
                "StaticDataLibrary::build() called for {} in {}",
                std::any::type_name::<Self>(),
                String::from_alt(end - start)
            );
        };

        if let Some(once) = Self::get_once() {
            once.call_once(build);
        } else {
            build();
        }
    }
}

pub trait DefaultArray: Sized {
    fn default_array() -> Self;
}

impl<T: Default, const N: usize> DefaultArray for [T; N] {
    fn default_array() -> Self {
        type MU<T> = MaybeUninit<T>;

        let mut mu_array: MU<Self> = MU::<Self>::uninit();

        {
            for t in unsafe {
                /*
                * as *mut [MU<T>; N]: safe since they're the same size and we're about to write()
                  into each cell
                * as_mut(): safe since we know where this pointer comes from
                * unwrap(): safe since we know where this pointer comes from
                */
                (&mut mu_array as *mut MU<Self> as *mut [MU<T>; N])
                    .as_mut()
                    .unwrap()
            } {
                t.write(T::default());
            }
        }

        // Safe: the whole array has been initialized
        unsafe { mu_array.assume_init() }
    }
}

pub fn to_pretty_string<T: Debug + Sized>(value: T) -> String {
    format!("{:#?}", value)
}

#[derive(Clone, Copy)]
pub enum SerFmt {
    #[cfg(feature = "bincode")]
    Bincode,

    #[cfg(feature = "serde_json")]
    JSON,

    #[cfg(feature = "json5")]
    JSON5,

    #[cfg(feature = "ron")]
    RON,

    #[cfg(feature = "toml")]
    TOML,
}

impl SerFmt {
    pub const fn file_extension(self) -> &'static str {
        match self {
            #[cfg(feature = "bincode")]
            Self::Bincode => "bc",
            #[cfg(feature = "serde_json")]
            Self::JSON => "json",
            #[cfg(feature = "json5")]
            Self::JSON5 => "json5",
            #[cfg(feature = "ron")]
            Self::RON => "ron",
            #[cfg(feature = "toml")]
            Self::TOML => "toml",
        }
    }

    pub const fn file_extensions() -> &'static [&'static str] {
        const FILE_EXTENSIONS: &[&str] = &[
            #[cfg(feature = "bincode")]
            SerFmt::Bincode.file_extension(),
            #[cfg(feature = "serde_json")]
            SerFmt::JSON.file_extension(),
            #[cfg(feature = "json5")]
            SerFmt::JSON5.file_extension(),
            #[cfg(feature = "ron")]
            SerFmt::RON.file_extension(),
            #[cfg(feature = "toml")]
            SerFmt::TOML.file_extension(),
        ];

        FILE_EXTENSIONS
    }

    pub fn from_file_extension(file_extension: &str) -> Option<Self> {
        #[cfg(feature = "bincode")]
        if file_extension == Self::Bincode.file_extension() {
            return Some(Self::Bincode);
        }
        #[cfg(feature = "serde_json")]
        if file_extension == Self::JSON.file_extension() {
            return Some(Self::JSON);
        }
        #[cfg(feature = "json5")]
        if file_extension == Self::JSON5.file_extension() {
            return Some(Self::JSON5);
        }
        #[cfg(feature = "ron")]
        if file_extension == Self::RON.file_extension() {
            return Some(Self::RON);
        }
        #[cfg(feature = "toml")]
        if file_extension == Self::TOML.file_extension() {
            return Some(Self::TOML);
        }

        None
    }

    pub fn from_file_name(file_name: &str) -> Option<Self> {
        Path::new(file_name)
            .extension()
            .and_then(OsStr::to_str)
            .and_then(Self::from_file_extension)
    }
}

pub trait FromFile: for<'de> Deserialize<'de> {
    fn from_file(file_name: &str) -> Result<Self, Box<dyn StdError>> {
        let function_call =
            || -> String { format!("from_file::<{}>(\"{}\")", type_name::<Self>(), file_name) };
        let ser_fmt: SerFmt = SerFmt::from_file_name(file_name).ok_or_else(|| {
            Box::new(SimpleError::new(format!(
                "{} doesn't have a valid file extension",
                function_call()
            )))
        })?;
        let mut file: File = File::open(file_name)?;

        enum FileBytes {
            #[cfg(not(miri))]
            Mmap(Mmap),
            #[cfg(miri)]
            ByteVec(Vec<u8>),
        }

        impl AsRef<[u8]> for FileBytes {
            fn as_ref(&self) -> &[u8] {
                match self {
                    #[cfg(not(miri))]
                    Self::Mmap(mmap) => mmap,
                    #[cfg(miri)]
                    Self::ByteVec(byte_vec) => &byte_vec,
                }
            }
        }

        impl<'a> TryFrom<&'a mut File> for FileBytes {
            type Error = Box<dyn StdError>;

            fn try_from(file: &'a mut File) -> Result<Self, Self::Error> {
                #[cfg(not(miri))]
                {
                    Ok(Self::Mmap(unsafe { Mmap::map(file) }?))
                }

                #[cfg(miri)]
                {
                    // If we're running this in Miri, Mmap isn't a viable option since it uses FFI
                    let mut byte_vec: Vec<u8> = Vec::<u8>::new();

                    file.read_to_end(&mut byte_vec)?;

                    Ok(Self::ByteVec(byte_vec))
                }
            }
        }

        let file_bytes: FileBytes = (&mut file).try_into()?;
        let bytes: &[u8] = file_bytes.as_ref();
        let to_boxed_error = |string: String| -> Box<dyn StdError> {
            Box::new(SimpleError::new(format!("{}: {}", function_call(), string)))
        };

        macro_rules! deserialize {
            ($($feature:meta, $ser_fmt:ident, $result_expr:expr);*) => {
                match ser_fmt {
                    $(
                        #[$feature]
                        SerFmt::$ser_fmt => $result_expr
                            .map_err(to_pretty_string)
                            .map_err(to_boxed_error),
                    )*
                }
            }
        }

        deserialize!(
            cfg(feature = "bincode"),    Bincode, bincode::deserialize::<Self>(bytes);
            cfg(feature = "serde_json"), JSON,    serde_json::from_slice::<Self>(bytes);
            cfg(feature = "json5"),      JSON5,   json5::from_str::<Self>(str::from_utf8(bytes)?);
            cfg(feature = "ron"),        RON,     ron::de::from_bytes::<Self>(bytes);
            cfg(feature = "toml"),       TOML,    toml::from_slice::<Self>(bytes)
        )
    }
}

impl<T: for<'de> Deserialize<'de>> FromFile for T {}

pub trait FromFileOrDefault: Default + FromFile {
    fn from_file_or_default(file_name: &str) -> Self {
        match Self::from_file(file_name) {
            Ok(value) => value,
            Err(err) => {
                ::log::warn!(
                    "Error encountered deserializing \"{}\": {:?}",
                    file_name,
                    err
                );

                Self::default()
            }
        }
    }
}

impl<T: Default + FromFile> FromFileOrDefault for T {}

pub trait ToFile: Serialize {
    fn to_file(&self, file_name: &str) -> Result<(), Box<dyn StdError>> {
        #[cfg(feature = "ron")]
        lazy_static! {
            static ref PRETTY_CONFIG: ron::ser::PrettyConfig = ron::ser::PrettyConfig::new()
                .new_line("\n".into())
                .indentor("\t".into())
                .separate_tuple_members(true)
                .enumerate_arrays(true)
                .decimal_floats(true);
        }
        let function_call =
            || -> String { format!("to_file::<{}>(\"{}\")", type_name::<Self>(), file_name) };
        let ser_fmt: SerFmt = SerFmt::from_file_name(file_name).ok_or_else(|| {
            Box::new(SimpleError::new(format!(
                "{} doesn't have a valid file extension",
                function_call()
            )))
        })?;
        let to_boxed_error = |string: String| -> Box<dyn StdError> {
            Box::new(SimpleError::new(format!("{}: {}", function_call(), string)))
        };
        let mut buf_writer: BufWriter<File> = BufWriter::<File>::new(File::create(file_name)?);
        let mut write_all =
            |result: Result<String, Box<dyn StdError>>| -> Result<(), Box<dyn StdError>> {
                result.and_then(|string: String| -> Result<(), Box<dyn StdError>> {
                    buf_writer
                        .write_all(string.as_bytes())
                        .map_err(to_pretty_string)
                        .map_err(to_boxed_error)
                })
            };

        macro_rules! serialize {
            ($($feature:meta, $ser_fmt:ident, $result_expr:expr, $and_then:expr);*) => {
                #[allow(clippy::redundant_closure_call)]
                match ser_fmt {
                    $(
                        #[$feature]
                        SerFmt::$ser_fmt => ($and_then)($result_expr.map_err(to_pretty_string).map_err(to_boxed_error)),
                    )*
                }
            }
        }

        serialize!(
            cfg(feature = "bincode"),
            Bincode,
            bincode::serialize_into(buf_writer, self),
            |x| x;

            cfg(feature = "serde_json"),
            JSON,
            serde_json::to_writer(buf_writer, self),
            |x| x;

            cfg(feature = "json5"),
            JSON5,
            json5::to_string(&self),
            write_all;

            cfg(feature = "ron"),
            RON,
            ron::ser::to_writer_pretty(buf_writer, &self, PRETTY_CONFIG.clone()),
            |x| x;

            cfg(feature = "toml"),
            TOML,
            toml::ser::to_string_pretty(self),
            write_all
        )
    }
}

impl<T: Serialize> ToFile for T {}

pub fn exit_app(world: &mut World) {
    debug_expect_some!(
        world.get_resource_mut::<Events<AppExit>>(),
        |mut app_exit_events: Mut<Events<AppExit>>| {
            app_exit_events.send(AppExit);
        }
    );
}

pub trait ShortSlerp
where
    Self: Sized,
{
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

/// The reverse direction of format!("{:0b}")
pub trait AsBitString {
    fn as_bit_string(&self) -> String {
        self.as_bit_string_with_chars('0', '1')
    }

    fn as_bit_string_with_chars(&self, zero: char, one: char) -> String;
}

impl<T> AsBitString for T
where
    T: BitField + Clone + Copy + Sized,
    [T]: BitArray<T>,
{
    fn as_bit_string_with_chars(&self, zero: char, one: char) -> String {
        [*self].as_slice().as_bit_string_with_chars(zero, one)
    }
}

impl<T> AsBitString for [T]
where
    T: BitField + Clone + Copy + Sized,
    [T]: BitArray<T>,
{
    fn as_bit_string_with_chars(&self, zero: char, one: char) -> String {
        let mut bit_string: String =
            String::with_capacity(zero.len_utf8().max(one.len_utf8()) * self.bit_length());

        for bit in 0_usize..self.bit_length() {
            write!(
                &mut bit_string,
                "{}",
                if self.get_bit(bit) { one } else { zero }
            )
            .unwrap();
        }

        bit_string
    }
}

// Trait from https://stackoverflow.com/questions/45634083/is-there-a-concept-of-pod-types-in-rust
define_super_trait!(pub trait Pod: 'static, Copy, Send, Sized, Sync);

pub trait WithLengthAndCapacity {
    fn with_length_and_capacity(length: usize, capacity: usize) -> Self;
}

impl<T: Pod> WithLengthAndCapacity for Vec<T> {
    fn with_length_and_capacity(length: usize, capacity: usize) -> Self {
        let mut vec_with_capacity: ManuallyDrop<Vec<T>> =
            ManuallyDrop::new(Vec::<T>::with_capacity(capacity));
        let (ptr, capacity): (*mut T, usize) =
            (vec_with_capacity.as_mut_ptr(), vec_with_capacity.capacity());

        /* Safe because:
        * ManuallyDrop guarantees vec_with_capacity's allocated memory won't be freed when it goes
          out of scope
        * Pod guarantees there's no harm in incorrectly assuming allocated objects are initialized
          (other than the fact that they'll contain garbage values, but that's a programmer error)
        * capacity is the actual allocated capacity */
        unsafe { Vec::<T>::from_raw_parts(ptr, length.min(capacity), capacity) }
    }
}

#[cfg(debug_assertions)]
pub fn debug_break() {
    unsafe {
        core::intrinsics::breakpoint();
    }
}

#[cfg(debug_assertions)]
#[macro_export]
macro_rules! cond_break {
    ($cond:expr) => {{
        if $cond {
            $crate::util::debug_break();
        }
    }};
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! cond_break {
    ($cond:expr) => {};
}

#[macro_export]
macro_rules! break_assert {
    ($cond:expr $(, $($arg:tt)*)?) => {
        {
            {
                #[cfg(debug_assertions)]
                cond_break!(!($cond));
            }

            assert!($cond $(, $($arg)*)?);
        }
    }
}

#[macro_export]
macro_rules! ignore {
    ($tt:tt) => {};
}

// Macro adapted from https://stackoverflow.com/questions/66291962/how-do-i-use-macro-rules-to-define-a-struct-with-optional-cfg
#[macro_export]
macro_rules! define_struct_with_default {
    (
        $(#[$struct_attr:meta])*
        $struct_vis:vis struct $struct_name:ident {
            $(
                $(#[$field_attr:meta])?
                $field_vis:vis $field:ident : $field_type:ty = $value:expr
            ),* $(,)?
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
        $struct_vis:vis struct $struct_name:ident <$field_type:ty> {
            $(
                $(#[$field_attr:meta])?
                $field_vis:vis $field:ident = $value:expr
            ),* $(,)?
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

#[macro_export]
macro_rules! define_super_trait {
    ($vis:vis trait $super_trait:ident : 'static, $sub_trait:path $(, $other_sub_trait:path)*) => {
        $vis trait $super_trait: 'static + $sub_trait $(+ $other_sub_trait)* {}

        impl<T: 'static + $sub_trait $(+ $other_sub_trait)*> $super_trait for T {}
    };

    ($vis:vis trait $super_trait:ident : $sub_trait:path $(, $other_sub_trait:path)*) => {
        $vis trait $super_trait: $sub_trait $(+ $other_sub_trait)* {}

        impl<T: $sub_trait $(+ $other_sub_trait)*> $super_trait for T {}
    };
}

#[macro_export]
macro_rules! max {
    ($a:expr $(, $b:expr)*) => { {
        let mut _max_val = $a;

        $(
            {
                let _b_val = $b;

                if _max_val < _b_val {
                    _max_val = _b_val;
                }
            }
        )*

        _max_val
    } }
}

#[macro_export]
macro_rules! min {
    ($a:expr $(, $b:expr)*) => { {
        let mut _min_val = $a;

        $(
            {
                let _b_val = $b;

                if _min_val > _b_val {
                    _min_val = _b_val;
                }
            }
        )*

        _min_val
    } }
}

#[cfg(test)]
mod tests {
    use {crate::prelude::*, std::time::Duration};

    #[test]
    fn string_from_alt_duration() {
        assert_eq!(String::from_alt(Duration::from_nanos(1_u64)), "1ns");
        assert_eq!(String::from_alt(Duration::from_nanos(12_u64)), "12ns");
        assert_eq!(String::from_alt(Duration::from_nanos(123_u64)), "123ns");
        assert_eq!(String::from_alt(Duration::from_nanos(1_234_u64)), "1.234μs");
        assert_eq!(
            String::from_alt(Duration::from_nanos(12_345_u64)),
            "12.345μs"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(123_456_u64)),
            "123.456μs"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(1_234_567_u64)),
            "1.234567ms"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(12_345_678_u64)),
            "12.345678ms"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(123_456_789_u64)),
            "123.456789ms"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(1_234_567_891_u64)),
            "1.234567891s"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(12_345_678_912_u64)),
            "12.345678912s"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(123_456_789_123_u64)),
            "02:03.456789123"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(1_234_567_891_234_u64)),
            "20:34.567891234"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(12_345_678_912_345_u64)),
            "03:25:45.678912345"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(123_456_789_123_456_u64)),
            "1d 10:17:36.789123456"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(1_234_567_891_234_567_u64)),
            "14d 06:56:07.891234567"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(12_345_678_912_345_678_u64)),
            "142d 21:21:18.912345678"
        );
        assert_eq!(
            String::from_alt(Duration::from_nanos(123_456_789_123_456_789_u64)),
            "1,428d 21:33:09.123456789"
        );
    }

    #[cfg(any(
        feature = "bincode",
        feature = "json5",
        feature = "serde_json",
        feature = "ron",
        feature = "toml"
    ))]
    #[test]
    fn test_serialization_and_deserialization() {
        use {
            super::{Deserialize, FromFile, SerFmt, Serialize, StdError, ToFile},
            std::{
                collections::HashMap,
                env::temp_dir,
                fs::{create_dir_all, remove_dir_all},
                path::PathBuf,
            },
            uuid::{adapter::Simple, Uuid},
        };

        define_struct_with_default!(
            #[derive(Debug, Deserialize, PartialEq, Serialize)]
            pub struct Struct {
                field_u8:    u8    = u8::MAX,
                field_u16:   u16   = u16::MAX,
                field_u32:   u32   = u32::MAX,
                field_u64:   u64   = u64::MAX,
                field_usize: usize = usize::MAX,
                field_i8:    i8    = i8::MAX,
                field_i16:   i16   = i16::MAX,
                field_i32:   i32   = i32::MAX,
                field_i64:   i64   = i64::MAX,
                field_isize: isize = isize::MAX
            }
        );

        #[derive(Debug, Deserialize, PartialEq, Serialize)]
        struct StructNT(Struct);
        #[derive(Debug, Deserialize, PartialEq, Serialize)]
        struct StructTuple(Struct, Struct);
        #[derive(Debug, Deserialize, PartialEq, Serialize)]
        struct EmptyStruct;

        #[derive(Debug, Deserialize, PartialEq, Serialize)]
        enum Enum {
            None,
            Option(Option<Struct>),
            StructNT(StructNT),
            StructTuple(StructTuple),
            EmptyStruct(EmptyStruct),
            Tuple(Struct, Struct),
            #[allow(dead_code)]
            Struct {
                a: Struct,
                b: Struct,
            },
            Array([Struct; 2_usize]),
            Vec(Vec<Struct>),
        }

        let map: HashMap<String, Enum> = HashMap::from([
            ("None".into(), Enum::None),
            ("OptionNone".into(), Enum::Option(None)),
            ("OptionSome".into(), Enum::Option(Some(Struct::default()))),
            (
                "StructNT".into(),
                Enum::StructNT(StructNT(Struct::default())),
            ),
            (
                "StructTuple".into(),
                Enum::StructTuple(StructTuple(Struct::default(), Struct::default())),
            ),
            ("EmptyStruct".into(), Enum::EmptyStruct(EmptyStruct)),
            (
                "Tuple".into(),
                Enum::Tuple(Struct::default(), Struct::default()),
            ),
            (
                "Struct".into(),
                Enum::Struct {
                    a: Struct::default(),
                    b: Struct::default(),
                },
            ),
            (
                "Array".into(),
                Enum::Array([Struct::default(), Struct::default()]),
            ),
            (
                "Vec".into(),
                Enum::Vec(vec![Struct::default(), Struct::default()]),
            ),
        ]);
        let mut file_path: PathBuf = temp_dir();

        file_path.push({
            let mut buf: [u8; Simple::LENGTH] = [b'_'; Simple::LENGTH];

            Uuid::new_v4()
                .to_simple_ref()
                .encode_upper(&mut buf)
                .to_string()
        });
        create_dir_all(file_path.clone()).unwrap();
        file_path.push("tmp");

        macro_rules! test_ser_fmt {
            ($ser_fmt:ident) => {{
                file_path.set_extension(SerFmt::$ser_fmt.file_extension());

                let file_path_str: &str = file_path.as_path().to_str().unwrap();
                let serialization_result: Result<(), Box<dyn StdError>> =
                    map.to_file(file_path_str);

                break_assert!(
                    serialization_result.is_ok(),
                    "Serializing {} yielded error: {:#?}",
                    file_path_str,
                    serialization_result.unwrap_err()
                );

                let deserialization_result: Result<HashMap<String, Enum>, Box<dyn StdError>> =
                    HashMap::<String, Enum>::from_file(file_path_str);

                break_assert!(
                    deserialization_result.is_ok(),
                    "Deserializing {} yielded error: {:#?}",
                    file_path_str,
                    deserialization_result.err().unwrap()
                );

                let new_map: HashMap<String, Enum> = deserialization_result.unwrap();

                assert_eq!(
                    new_map, map,
                    "New map does not match old map.\n\nNew map: {:#?}\n\nOld map: {:#?}",
                    new_map, map
                );
            }};
        }

        #[cfg(feature = "bincode")]
        test_ser_fmt!(Bincode);

        #[cfg(feature = "serde_json")]
        test_ser_fmt!(JSON);

        #[cfg(feature = "json5")]
        test_ser_fmt!(JSON5);

        #[cfg(feature = "ron")]
        test_ser_fmt!(RON);

        #[cfg(feature = "toml")]
        test_ser_fmt!(TOML);

        file_path.pop();
        remove_dir_all(file_path).unwrap();
    }
}
