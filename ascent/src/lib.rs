// #![feature(hash_raw_entry)]
#![allow(mixed_script_confusables)]
// #![feature(adt_const_params)]

pub mod internal;
pub mod rel;
pub mod aggregators;
mod convert;
pub mod rel_index_read;
mod c_rel_index;
mod exps;
mod c_rel_full_index;
mod c_rel_index_combined;
mod c_lat_index;
mod c_rel_no_index;
pub mod to_rel_index;
pub mod tuple_of_borrowed;
mod rel_index_boilerplate;

pub use ascent_macro::ascent;
pub use ascent_macro::ascent_run;
pub use ascent_macro::ascent_par;
pub use ascent_macro::ascent_run_par;

pub use ascent_base::*;

pub mod rayon {
   pub use rayon::*;
}

pub mod boxcar {
   pub use boxcar::Vec;
}