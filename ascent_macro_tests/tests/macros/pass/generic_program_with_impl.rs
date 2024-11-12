//! Generic program with impl declaration

use std::hash::Hash;

use ascent::ascent;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Node(&'static str);

ascent! {
    struct AscentProgram<T> where T: Clone;
    impl<T> AscentProgram<T> where T: Clone + Eq + Hash;

    // Facts:

    relation value(T);
}

fn main() {}
