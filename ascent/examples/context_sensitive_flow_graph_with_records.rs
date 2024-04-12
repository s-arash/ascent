//! Context Sensitive Flow Graph with Records
//!
//! The following example demonstrates one way of integrating context information into a control flow graph.
//!
//! The type program point (`ProgPoint`) aggregates an instruction and an (abstract) context (`Context`) into a new entity
//! which is utilized as the node type of the flow graph. Note that in this version flow is a simpler,
//! binary relation and typos mixing up instructions and contexts of different program points are effectively prevented.
//!
//! Also, as we will see below, the flow relation could now be modelled utilizing a generalized graph component,
//! thereby inheriting a library of derived relations.
//!
//! (Adapted from https://souffle-lang.github.io/examples#context-sensitive-flow-graph-with-records)

use ascent::ascent;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct Instr(&'static str);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct Context(&'static str);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct ProgPoint(Instr, Context);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
enum Res {
    Ok,
    Err,
}

ascent! {
    // Facts:

    relation succ(ProgPoint, ProgPoint);

    // Rules:
    
    relation flow(ProgPoint, ProgPoint);
    
    flow(p1, p2) <-- succ(p1, p2);
    flow(p1, p3) <-- flow(p1, p2), flow(p2, p3);

    relation res(Res);
    
    res(Res::Ok) <-- flow(ProgPoint(Instr("w1"), Context("c1")), ProgPoint(Instr("r2"), Context("c1")));
    res(Res::Err) <-- flow(ProgPoint(Instr("w1"), Context("c1")), ProgPoint(Instr("r2"), Context("c2")));
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.succ = vec![
        (ProgPoint(Instr("w1"), Context("c1")), ProgPoint(Instr("w2"), Context("c1"))),
        (ProgPoint(Instr("w2"), Context("c1")), ProgPoint(Instr("r1"), Context("c1"))),
        (ProgPoint(Instr("r1"), Context("c1")), ProgPoint(Instr("r2"), Context("c1"))),

        (ProgPoint(Instr("w1"), Context("c2")), ProgPoint(Instr("w2"), Context("c2"))),
        (ProgPoint(Instr("w2"), Context("c2")), ProgPoint(Instr("r1"), Context("c2"))),
        (ProgPoint(Instr("r1"), Context("c2")), ProgPoint(Instr("r2"), Context("c2"))),
    ];

    prog.run();

    let AscentProgram { res, ..} = prog;

    assert_eq!(res, vec![
        (Res::Ok,),
    ]);
}
