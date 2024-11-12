//! Aggregate `agg` clause

use ascent::{ascent, Dual};

pub type Node = &'static str;

ascent! {
    // Facts:

    relation edge(Node, Node, u32);

    // Rules:

    lattice shortest_path(Node, Node, Dual<u32>);

    shortest_path(x, y, Dual(*w)) <-- edge(x, y, w);

    shortest_path(x, z, Dual(w + l)) <--
        edge(x, y, w),
        shortest_path(y, z, ?Dual(l));
}

fn main() {}
