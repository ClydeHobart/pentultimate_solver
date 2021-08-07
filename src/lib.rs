#![feature(const_fn_floating_point_arithmetic, io_read_to_string, specialization)]

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
pub mod util;