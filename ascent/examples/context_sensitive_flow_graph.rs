//! Context Sensitive Flow Graph
//!
//! The following example demonstrates one way of integrating context information into a control flow graph.
//!
//! In this example the flow relation describes a graph where each node consists of a pair
//! of an instruction (`Instr`) and some (abstract) context (`Context`).
//!
//! Although correct, the increased number of attributes causes larger code bases,
//! and thus an increased risk of typos leading to hard-to-identify bugs.
//!
//! The fact that each node is represented by a pair of elements can be made explicit by
//! utilizing records, as demonstrated in `context_sensitive_flow_graph_with_records.rs`.
//!
//! (Adapted from https://souffle-lang.github.io/examples#context-sensitive-flow-graph)

use ascent::ascent;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Instr(&'static str);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Context(&'static str);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Res {
    Ok,
    Err,
}

ascent! {
    // Facts:

    relation succ(Instr, Context, Instr, Context);

    // Rules:
    
    relation flow(Instr, Context, Instr, Context);
    
    flow(i1, c1, i2, c2) <-- succ(i1, c1, i2, c2);
    flow(i1, c1, i3, c3) <-- flow(i1, c1, i2, c2), flow(i2, c2, i3, c3);

    relation res(Res);
    
    res(Res::Ok) <-- flow(Instr("w1"), Context("c1"), Instr("r2"), Context("c1"));
    res(Res::Err) <-- flow(Instr("w1"), Context("c1"), Instr("r2"), Context("c2"));
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.succ = vec![
        (Instr("w1"), Context("c1"), Instr("w2"), Context("c1")),
        (Instr("w2"), Context("c1"), Instr("r1"), Context("c1")),
        (Instr("r1"), Context("c1"), Instr("r2"), Context("c1")),

        (Instr("w1"), Context("c2"), Instr("w2"), Context("c2")),
        (Instr("w2"), Context("c2"), Instr("r1"), Context("c2")),
        (Instr("r1"), Context("c2"), Instr("r2"), Context("c2")),
    ];

    prog.run();

    let AscentProgram { res, ..} = prog;

    assert_eq!(res, vec![
        (Res::Ok,),
    ]);
}
