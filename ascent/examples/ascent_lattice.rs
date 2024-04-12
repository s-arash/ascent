//! Aggregate `agg` clause

use ascent::{ascent, Dual};

type Node = &'static str;

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

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.edge = vec![
        ("A", "B", 1),
        ("A", "D", 4),
        ("B", "C", 1),
        ("B", "D", 1),
        ("C", "D", 2),
    ];

    prog.run();

    let AscentProgram { mut shortest_path, ..} = prog;

    shortest_path.sort_by_key(|(_, key, _)| *key);
    shortest_path.sort_by_key(|(key, _, _)| *key);
    
    assert_eq!(shortest_path, vec![
        ("A", "B", Dual(1)),
        ("A", "C", Dual(2)),
        ("A", "D", Dual(2)),
        ("B", "C", Dual(1)),
        ("B", "D", Dual(1)),
        ("C", "D", Dual(2)),
    ]);
}
