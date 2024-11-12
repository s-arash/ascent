//! Macro rule

use std::rc::Rc;

use ascent::ascent;

ascent! {
    // Facts:

    relation unique(isize);

    // Macros:

    macro shared($x: expr) {
        shared(Rc::new($x))
    }

    // Rules:

    relation shared(Rc<isize>);

    shared!(*x) <-- unique(x);
}

fn main() {}
