//! Generic program
//!
//! The following example encodes reachability of a directed graph that is generic over its nodes.

use std::hash::Hash;

use ascent::ascent;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Node(&'static str);

ascent! {
    struct AscentProgram<N: Clone + Eq + Hash>;

    // Or alternatively (and with the same semantics!):
    // struct AscentProgram<N> where N: Clone + Eq + Hash;

    // If you want to keep the trait bounds out of the program's
    // type signature and limit them to its `impl` blocks,
    // you could alternatively specify the program's type like this:
    //
    // struct AscentProgram<N>;
    // impl<N> AscentProgram<N: Clone + Eq + Hash>;
    //
    // Or alternatively (and with the same semantics!):
    //
    // struct AscentProgram<N>;
    // impl<N> AscentProgram<N> where N: Clone + Eq + Hash;

    // Where desirable you could even do a mix of both:
    //
    // struct AscentProgram<N: Eq + Hash>;
    // impl<N> AscentProgram<N: Clone + Eq + Hash>;

    // Facts:

    relation node(N);
    relation edge(N, N);

    // Rules:

    relation reachable(N, N);

    reachable(x, y) <-- edge(x, y);
    reachable(x, z) <-- reachable(x, y), edge(y, z);
}

fn main() {
   let mut prog: AscentProgram<Node> = AscentProgram::default();

   prog.node = vec![(Node("A"),), (Node("B"),), (Node("C"),)];

   prog.edge = vec![(Node("A"), Node("B")), (Node("B"), Node("C"))];

   prog.run();

   let AscentProgram { mut reachable, .. } = prog;

   reachable.sort_by_key(|(_, key)| key.0);
   reachable.sort_by_key(|(key, _)| key.0);

   assert_eq!(reachable, vec![(Node("A"), Node("B")), (Node("A"), Node("C")), (Node("B"), Node("C")),]);
}
