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

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.unique = vec![
        (1,),
        (2,),
        (3,),
        (4,),
        (5,),
    ];

    prog.run();

    let AscentProgram { mut shared, ..} = prog;

    shared.sort_by_key(|(key,)| Rc::clone(key));

    assert_eq!(shared, vec![
        (Rc::new(1),),
        (Rc::new(2),),
        (Rc::new(3),),
        (Rc::new(4),),
        (Rc::new(5),),
    ]);
}
