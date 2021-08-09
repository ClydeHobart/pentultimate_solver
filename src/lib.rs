#![feature(const_fn_floating_point_arithmetic, io_read_to_string)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate log;

pub mod app;
pub mod colors;
pub mod math;
pub mod piece;
pub mod prelude;
pub mod puzzle;
pub mod strings;
pub mod ui;
pub mod util;