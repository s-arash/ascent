//! data structures for [`ascent`](https://github.com/s-arash/ascent) relations, 
//! made possible by Ascent's [BYODS](https://dl.acm.org/doi/pdf/10.1145/3622840) feature

#![cfg_attr(not(test), deny(unused_crate_dependencies))]

// See Cargo.toml for why this is needed. 
use syn as _;

mod union_find;
#[doc(hidden)]
pub mod eqrel_ind;
mod iterator_from_dyn;
mod test;
#[doc(hidden)]
pub mod eqrel_binary;
#[doc(hidden)]
pub mod eqrel_ternary;
mod rel_boilerplate;
#[doc(hidden)]
pub mod fake_vec;
#[doc(hidden)]
pub mod trrel_binary;
#[doc(hidden)]
pub mod trrel_ternary_ind;
mod utils;

#[doc(hidden)]
pub mod trrel_binary_ind;
#[doc(hidden)]
pub mod binary_rel;
#[cfg(feature = "par")]
#[doc(hidden)]
pub mod ceqrel_ind;
pub mod trrel_union_find;
#[doc(hidden)]
pub mod trrel_union_find_binary_ind;

pub mod uf;
pub mod trrel;
pub mod eqrel;
pub mod trrel_uf;
pub mod adaptor;
