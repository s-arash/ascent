//! Sequences using Recursive Records
//!
//! The following example demonstrates the utilization of recursive records for
//! building sequences of strings over a given alphabet.
//!
//! The Seq type is a recursive type where each instance is either nil or a pair of
//! a leading `Letter` and a tailing `Seq`.
//!
//! The relation `seq` is defined to contain all sequences of length 5 or less
//! over a given alphabet defined by the relation letter.
//! The relation `len` is essentially a function assigning each sequence its length.
//!
//! Finally, the res relation illustrates how to create constant values for recursive record types.
//!
//! (Adapted from https://souffle-lang.github.io/examples#context-sensitive-flow-graph-with-records)

use std::rc::Rc;

use ascent::ascent;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Letter(&'static str);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Seq(Letter, Option<Rc<Seq>>);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Res(&'static str);

ascent! {
    // Facts:

    relation letter(Letter);
    
    // Rules:
    
    relation seq(Option<Rc<Seq>>);
    
    seq(None);
    seq(Some(Rc::new(Seq(l.clone(), s.clone())))) <-- letter(l), seq(s), len(s, n), if *n < 5;

    relation len(Option<Rc<Seq>>, usize);
    
    len(None, 0);
    len(Some(Rc::clone(&s)), n + 1) <-- letter(l), len(r, n), let s = Rc::new(Seq(l.clone(), r.clone())), seq(Some(Rc::clone(&s)));

    relation res(Res);

    res(Res("-")) <-- seq(nil);
    res(Res("a")) <-- seq(Some(Rc::new(Seq(Letter("a"), None))));
    res(Res("b")) <-- seq(Some(Rc::new(Seq(Letter("b"), None))));
    res(Res("c")) <-- seq(Some(Rc::new(Seq(Letter("c"), None))));
    res(Res("ab")) <-- seq(Some(Rc::new(Seq(Letter("a"), Some(Rc::new(Seq(Letter("b"), None)))))));
    res(Res("aba")) <-- seq(Some(Rc::new(Seq(Letter("a"), Some(Rc::new(Seq(Letter("b"), Some(Rc::new(Seq(Letter("a"), None))))))))));
    res(Res("abc")) <-- seq(Some(Rc::new(Seq(Letter("a"), Some(Rc::new(Seq(Letter("b"), Some(Rc::new(Seq(Letter("c"), None))))))))));
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.letter = vec![
        (Letter("a"),),
        (Letter("b"),),
    ];

    prog.run();

    let AscentProgram { mut res, ..} = prog;

    res.sort_by_key(|(key,)| key.0);

    assert_eq!(res, vec![
        (Res("-"),),
        (Res("a"),),
        (Res("ab"),),
        (Res("aba"),),
        (Res("b"),),
    ]);
}
