use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent::Dual;

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   relation edge(i32, i32, u32);
   lattice shortest_path(i32, i32, Dual<u32>);

   shortest_path(x, y, Dual(*len)) <-- edge(x, y, len);
   shortest_path(x, z, Dual(len + plen)) <-- edge(x, y, len), shortest_path(y, z, ?Dual(plen));
}

fn main() {}
