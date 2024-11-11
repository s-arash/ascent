//! Transitive Graph Closure
//!
//! The following example encodes the transitive closure of a directed graph.

use ascent::ascent;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Node(&'static str);

ascent! {
    // Facts:

    relation node(Node);
    relation edge(Node, Node);

    // Rules:

    relation reachable(Node, Node);

    // The transitive closure can be written using a so-called linear rule:
    reachable(x, y) <-- edge(x, y);
    reachable(x, z) <-- reachable(x, y), edge(y, z);

    // or a non-linear rule:
    // reachable(x, y) <-- edge(x, y);
    // reachable(x, z) <-- reachable(x, y), reachable(y, z);

    // While both variants are semantically equivalent the linear rule
    // tends to be more performant since in the non-linear variant the fact `reachable(x, y)`
    // is redundantly discovered again at every iteration (and thus n − 1 times).

    relation closure_of_a(Node);

    closure_of_a(y) <-- reachable(Node("A"), y);
}

fn main() {
   let mut prog = AscentProgram::default();

   prog.node = vec![(Node("A"),), (Node("B"),), (Node("C"),)];

   prog.edge = vec![(Node("A"), Node("B")), (Node("B"), Node("C"))];

   prog.run();

   let AscentProgram { mut reachable, mut closure_of_a, .. } = prog;

   reachable.sort_by_key(|(key, _)| key.0);
   reachable.sort_by_key(|(_, key)| key.0);

   assert_eq!(reachable, vec![(Node("A"), Node("B")), (Node("A"), Node("C")), (Node("B"), Node("C")),]);

   closure_of_a.sort_by_key(|(key,)| key.0);

   assert_eq!(closure_of_a, vec![(Node("B"),), (Node("C"),),]);
}
