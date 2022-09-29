// #![feature(hash_raw_entry)]
#![allow(mixed_script_confusables)]
// #![feature(adt_const_params)]

pub mod internal;
pub mod aggregators;
mod convert;
mod rel_index_read;
// mod hash_map_plus;
mod experimental_multimap;
mod experimental_rel_index2;
mod experimental_rel_full_index2;
mod experimental_rel_full_index_bt;
mod tests;
mod experimental_rel_full_index3;
pub mod experimental_dict;
mod experimental_index_shape;
pub mod dict_merge;
mod utils;

pub use ascent_macro::ascent;
pub use ascent_macro::ascent_run;

pub use ascent_base::*;