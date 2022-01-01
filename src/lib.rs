#![feature(core_intrinsics, const_fn_floating_point_arithmetic, io_read_to_string)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate log;

#[macro_use]
extern crate static_assertions as sa;

pub mod app;
pub mod math;
pub mod piece;
pub mod preferences;
pub mod prelude;
pub mod puzzle;
pub mod strings;
pub mod ui;
pub mod util;

#[cfg(debug_assertions)]
pub mod debug;