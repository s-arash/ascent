use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   relation foo(i32, i32);
   relation bar(i32, i32);

   macro foo_($x: expr, $y: expr) {
      foo($x, $y)
   }

   macro foo($x: expr, $y: expr) {
      let _x = $x, let _y = $y, foo_!(_x, _y)
   }

   foo(0, 1);
   foo(1, 2);
   foo(2, 3);
   foo(3, 4);

   bar(x, y) <-- foo(x, y), foo!(x + 1, y + 1), foo!(x + 2, y + 2), foo!(x + 3, y + 3);
}

fn main() {}
