//! Lists using Recursive Enums
//!
//! The following example demonstrates the utilization of recursive records for
//! building lists of strings over a given alphabet.
//!
//! The `List<T>` type is a recursive type where each instance is either `List::Nil`
//! or a pair of a leading `T` and a tailing `List::Cons`.
//!
//! The relation `list` is defined to contain all lists of length 5 or less
//! over a given alphabet defined by the relation `char`.
//! The relation `len` is essentially a function assigning each list its length.
//!
//! Finally, the `res` relation illustrates how to create constant values for recursive record types.
//!
//! (Adapted from https://souffle-lang.github.io/examples#sequences-using-recursive-records)

use std::rc::Rc;

use ascent::ascent;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum List<T> {
  Cons(T, Rc<List<T>>),
  Nil
}

macro_rules! cons {
    ($h: expr, $t: expr) => {
        Rc::new(List::Cons($h, $t))
    }
}

macro_rules! nil {
    () => {
        Rc::new(List::Nil)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Res(&'static str);

ascent! {
    // Facts:

    relation char(char);
    
    // Rules:
    
    relation list(Rc<List<char>>);
    
    list(nil!());
    list(cons!(c.clone(), l.clone())) <-- char(c), list(l), len(l, n), if *n < 5;

    relation len(Rc<List<char>>, usize);
    
    len(nil!(), 0);
    len(l.clone(), n + 1) <-- char(c), len(r, n), let l = cons!(c.clone(), r.clone()), list(&l);
    
    relation res(Res);

    res(Res("-")) <-- list(nil!());
    res(Res("a")) <-- list(cons!('a', nil!()));
    res(Res("b")) <-- list(cons!('b', nil!()));
    res(Res("c")) <-- list(cons!('c', nil!()));
    res(Res("ab")) <-- list(cons!('a', cons!('b', nil!())));
    res(Res("aba")) <-- list(cons!('a', cons!('b', cons!('a', nil!()))));
    res(Res("abc")) <-- list(cons!('a', cons!('b', cons!('c', nil!()))));
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.char = vec![
        ('a',),
        ('b',),
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
