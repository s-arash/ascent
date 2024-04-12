//! Binding `let` clause

use ascent::ascent;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
enum List {
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

fn main() {
    let mut prog = AscentProgram::default();

    prog.run();

    let AscentProgram { mut list, ..} = prog;

    list.sort_by_key(|(_, key)| *key);

    let lists: Vec<_> = list.into_iter().map(|(list, len)| {
        (list.as_vec(), len)
    }).collect();

    assert_eq!(lists, vec![
        (vec![], 0),
        (vec![0], 1),
        (vec![1, 0], 2),
        (vec![2, 1, 0], 3),
        (vec![3, 2, 1, 0], 4),
        (vec![4, 3, 2, 1, 0], 5),
    ]);
}
