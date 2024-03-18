use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   relation foo(i32, Option<i32>);
   relation bar(i32, i32);

   foo(1, None);
   foo(2, Some(2));
   foo(3, Some(30));

   bar(*x, *y) <-- foo(x, ?Some(y)) if y != x;
   bar(*x, *y) <-- foo(x, y_opt) if let Some(y) = y_opt if y != x;
}

fn main() {}
