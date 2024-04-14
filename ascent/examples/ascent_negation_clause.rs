//! Negation clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(i32);
    
    // Rules:
    
    relation even(i32);

    even(x) <-- number(x), if x % 2 == 0;

    relation odd(i32);

    odd(x) <-- number(x), !even(x);
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.number = vec![
        (1,),
        (2,),
        (3,),
        (4,),
        (5,),
    ];

    prog.run();

    let AscentProgram { mut even, mut odd, ..} = prog;

    even.sort_by_key(|(key,)| *key);

    assert_eq!(even, vec![
        (2,),
        (4,),
    ]);

    odd.sort_by_key(|(key,)| *key);

    assert_eq!(odd, vec![
        (1,),
        (3,),
        (5,),
    ]);
}
