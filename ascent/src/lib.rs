//! Ascent enables writing logic programs in the style of Datalog in Rust.
//!
//! See the documentation for [`ascent`], one of the main macros of this crate, for more information.

#![deny(unused_crate_dependencies)]
pub mod internal;
#[doc(hidden)]
pub mod rel;
pub mod aggregators;
mod convert;
mod rel_index_read;
mod exps;
#[cfg(feature = "par")]
mod c_rel_index;
#[cfg(feature = "par")]
mod c_rel_index_read;
#[cfg(feature = "par")]
mod c_rel_full_index;
#[cfg(feature = "par")]
mod c_rel_index_combined;
#[cfg(feature = "par")]
mod c_lat_index;
#[cfg(feature = "par")]
mod c_rel_no_index;
mod to_rel_index;
mod tuple_of_borrowed;
mod rel_index_boilerplate;

pub use ascent_base::*;
pub use ascent_macro::{ascent, ascent_run, ascent_source};
#[cfg(feature = "par")]
pub use ascent_macro::{ascent_par, ascent_run_par};
#[cfg(feature = "par")]
pub use dashmap;
#[cfg(feature = "par")]
pub use rayon;
pub use {boxcar, hashbrown};
