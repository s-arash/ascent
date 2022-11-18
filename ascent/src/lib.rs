// #![feature(hash_raw_entry)]
#![allow(mixed_script_confusables)]
// #![feature(adt_const_params)]

pub mod internal;
pub mod aggregators;
mod convert;
mod rel_index_read;
mod c_rel_index;
mod exps;
mod c_rel_full_index;
mod c_rel_index_combined;
mod c_lat_index;

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