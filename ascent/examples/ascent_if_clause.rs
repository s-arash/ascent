//! Conditional `if` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(isize);
    
    // Rules:
    
    relation even(isize);

    even(x) <-- number(x) if x % 2 == 0;
    
    relation odd(isize);

    odd(x) <-- number(x) if x % 2 != 0;
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
