//! Simple Typed VarPointsTo
//!
//! The following example encodes the most simple version of a var-points-to analysis.
//!
//! The example starts by declaring types for named variables, objects and fields.
//! Based on those, four input relations `assign`, `new`, `ld` and `st` are declared and
//! filled with data corresponding to the small code snippet outlined below:
//!
//! ```
//! let mut v1 = h1();
//!
//! let v2 = h2();
//! v1 = v2;
//!
//! let v3 = h3();
//! v1.f = v3;
//!
//! let v4 = v1.f;
//! ```
//!
//! The analysis itself is broken up in two parts:
//!
//! - computation of aliases
//! - computation of the var-points-to relation based on aliases
//!
//! Note that in particular for the last rule of the alias relation the utilization
//! of typed attributes ensures that connections between attributes are consistently established.
//!
//! Problems caused by e.g. getting the wrong order of parameters can be effectively prevented.
//!
//! (Adapted from https://souffle-lang.github.io/examples#simple-typed-varpointsto)

use ascent::ascent;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct Var(&'static str);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct Obj(&'static str);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct Field(&'static str);

ascent! {
    // Facts:

    relation assign(Var, Var);
    relation new(Var, Obj);
    relation ld(Var, Var, Field);
    relation st(Var, Field, Var);

    // Rules:

    relation alias(Var, Var);

    alias(x, x) <-- assign(x, _);
    alias(x, x) <-- assign(_, x);
    alias(x, y) <-- assign(x, y);
    alias(x, y) <-- ld(x, a, f), alias(a, b), st(b, f, y);

    relation points_to(Var, Obj);

    points_to(x, y) <-- new(x, y);
    points_to(x, y) <-- alias(x, z), points_to(z, y);
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.assign = vec![
        (Var("v1"), Var("v2"))
    ];

    prog.new = vec![
        (Var("v1"), Obj("h1")),
        (Var("v2"), Obj("h2")),
        (Var("v3"), Obj("h3")),
    ];

    prog.st = vec![
        (Var("v1"), Field("f"), Var("v3")),
    ];

    prog.ld = vec![
        (Var("v4"), Var("v1"), Field("f")),
    ];

    prog.run();

    let AscentProgram { mut alias, mut points_to, .. } = prog;

    alias.sort_by_key(|(_, key)| key.0);
    alias.sort_by_key(|(key, _)| key.0);

    assert_eq!(alias, vec![
        (Var("v1"), Var("v1")),
        (Var("v1"), Var("v2")),
        (Var("v2"), Var("v2")),
        (Var("v4"), Var("v3")),
    ]);

    points_to.sort_by_key(|(_, key)| key.0);
    points_to.sort_by_key(|(key, _)| key.0);

    assert_eq!(points_to, vec![
        (Var("v1"), Obj("h1")),
        (Var("v1"), Obj("h2")),
        (Var("v2"), Obj("h2")),
        (Var("v3"), Obj("h3")),
        (Var("v4"), Obj("h3")),
    ]);
}
