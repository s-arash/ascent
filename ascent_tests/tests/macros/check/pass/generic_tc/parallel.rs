use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent_par! {
   struct TC<TNode> where TNode: Clone + std::cmp::Eq + std::hash::Hash + Sync + Send;

   relation edge(TNode, TNode);
   relation path(TNode, TNode);

   path(x, z) <-- edge(x, y), path(y, z);
   // path(x, z) <-- path(x, y), path(y, z);
}

fn main() {}
