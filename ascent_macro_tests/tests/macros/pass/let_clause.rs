//! Binding `let` clause

use ascent::ascent;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum List {
   Nil,
   Cons(usize, Box<List>),
}

impl List {
   fn as_vec(&self) -> Vec<usize> {
      let mut items = vec![];

      let mut list = self;
      while let Self::Cons(head, tail) = list {
         items.push(*head);
         list = tail;
      }

      items
   }
}

ascent! {
    // Rules:

    relation list(List, usize);

    list(List::Nil, 0);
    list(List::Cons(*l, Box::new(t.clone())), h) <-- list(t, l), let h = *l + 1, if h <= 5;
}

fn main() {}
