#![feature(
    core_intrinsics,
    const_fn_floating_point_arithmetic,
    maybe_uninit_slice,
    slice_flatten
)]

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
pub mod tools;
pub mod ui;
pub mod util;
