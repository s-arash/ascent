use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

#[warn(warnings)]
#[allow(unused_imports)]
#[allow(dead_code)]
#[allow(redundant_semicolons)]
fn test<T: FactTypes>() {
   use ascent::{aggregators::*, lattice::set::Set, Dual};

   let _ = ascent::ascent_run_par! {
      struct Polonius<T: FactTypes>;

      relation subset(T::Origin, T::Origin, T::Point);// = ctx.subset_base.clone();
      relation cfg_edge(T::Point, T::Point);
      relation origin_live_on_entry(T::Origin, T::Point);
      relation origin_contains_loan_on_entry(T::Origin, T::Loan, T::Point);
      relation loan_live_at(T::Loan, T::Point);
      relation loan_invalidated_at(T::Loan, T::Point);
      relation errors(T::Loan, T::Point);
      relation placeholder_origin(T::Origin);
      relation subset_error(T::Origin, T::Origin, T::Point);
      relation loan_killed_at(T::Loan, T::Point);// = loan_killed_at.iter().cloned().collect();
      relation known_placeholder_subset(T::Origin, T::Origin);// = known_placeholder_subset.iter().cloned().collect();

      subset(origin1, origin3, point) <--
         subset(origin1, origin2, point),
         subset(origin2, origin3, point),
         if origin1 != origin3;

      subset(origin1, origin2, point2) <--
         subset(origin1, origin2, point1),
         cfg_edge(point1, point2),
         origin_live_on_entry(origin1, point2),
         origin_live_on_entry(origin2, point2);

      origin_contains_loan_on_entry(origin2, loan, point) <--
         origin_contains_loan_on_entry(origin1, loan, point),
         subset(origin1, origin2, point);

      origin_contains_loan_on_entry(origin, loan, point2) <--
         origin_contains_loan_on_entry(origin, loan, point1),
         cfg_edge(point1, point2),
         !loan_killed_at(loan, point1),
         origin_live_on_entry(origin, point2);

      loan_live_at(loan, point) <--
         origin_contains_loan_on_entry(origin, loan, point),
         origin_live_on_entry(origin, point);

      errors(loan, point) <--
         loan_invalidated_at(loan, point),
         loan_live_at(loan, point);

      subset_error(origin1, origin2, point) <--
         subset(origin1, origin2, point),
         placeholder_origin(origin1),
         placeholder_origin(origin2),
         !known_placeholder_subset(origin1, origin2),
         if origin1 != origin2;
   };
}

fn main() {}
