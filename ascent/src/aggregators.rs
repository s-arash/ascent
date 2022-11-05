//! This module provides aggregators that can be used in Ascent rules.
//! 
//! eg: Writing the average of `foo` in `bar`
//! ```
//! use ascent::ascent;
//! use ascent::aggregators::*;
//! ascent!{
//!   relation foo(i32);
//!   relation bar(i32);
//!   // ...
//!   bar(m as i32) <-- agg m = mean(x) in foo(x);   
//! }
//! ```

use std::ops::Add;
use std::iter::Sum;

/// computes the minimum of the input column
pub fn min<'a, N: 'a>(inp: impl Iterator<Item = (&'a N,)>) -> impl Iterator<Item = N>
where N: Ord + Clone 
{
   inp.map(|tuple| tuple.0).min().cloned().into_iter()
}

/// computes the maximum of the input column
pub fn max<'a, N: 'a>(inp: impl Iterator<Item = (&'a N,)>) -> impl Iterator<Item = N>
where N: Ord + Clone 
{
   inp.map(|tuple| tuple.0).max().cloned().into_iter()
}

/// computes the sum of the input column
pub fn sum<'a, N: 'a>(inp: impl Iterator<Item = (&'a N,)>) -> impl Iterator<Item = N>
where N: Ord + Add + Clone + Sum<N>
{
   let sum = inp.map(|tuple| tuple.0).cloned().sum::<N>();
   std::iter::once(sum)
}

/// returns the number of tuples
pub fn count<'a, N: 'a>(inp: impl Iterator<Item = (&'a N,)>) -> impl Iterator<Item = usize>
{
   let (size_floor, size_ceiling)= inp.size_hint();
   let size_ceiling = size_ceiling.unwrap_or(usize::MAX);
   let count = if size_floor == size_ceiling {
      size_floor
   } else {
      inp.count()
   };
   std::iter::once(count)
}

/// computes the average of the input column, returning an `f64`
pub fn mean<'a, N: 'a>(inp: impl Iterator<Item = (&'a N,)>) -> impl Iterator<Item = f64>
where N: Clone + Into<f64>
{
   let (sum, count) = inp.fold((0.0, 0usize), |(sum, count), tuple| (tuple.0.clone().into() + sum, count + 1));
   let res = if count == 0 {None} else {
      Some(sum / count as f64)
   };
   res.into_iter()
}

/// computes the value at the given percentile of the input column
pub fn percentile<'a, TItem: 'a, TInputIter>(p: f64) -> impl Fn(TInputIter) -> std::option::IntoIter<TItem>
where
   TInputIter: Iterator<Item = (&'a TItem,)>, TItem: Ord + Clone,
{
   move |inp| {
      let mut sorted: Vec<_> = inp.map(|tuple| tuple.0.clone()).collect();
      sorted.sort();
      let p_index = (sorted.len() as f64 * p / 100.0) as usize;
      if sorted.len() > 0 {
         Some(sorted.swap_remove(p_index))
      } else {
         None
      }.into_iter()
   }
}

/// backs negations (eg `!foo(x)`) in `ascent`
pub fn not<'a>(mut inp: impl Iterator<Item = ()>) -> impl Iterator<Item = ()>
{
   let any = inp.next().is_some();
   if any {None} else {Some(())}.into_iter()
}