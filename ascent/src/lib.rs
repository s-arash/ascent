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
mod c_rel_index;
mod exps;
mod c_rel_full_index;
mod c_rel_index_combined;
mod c_lat_index;
mod c_rel_no_index;
mod to_rel_index;
mod tuple_of_borrowed;
mod rel_index_boilerplate;

pub use ascent_macro::ascent;
pub use ascent_macro::ascent_run;
pub use ascent_macro::ascent_par;
pub use ascent_macro::ascent_run_par;

pub use ascent_base::*;

pub use hashbrown;
pub use dashmap;
pub use boxcar;
pub use rayon;
