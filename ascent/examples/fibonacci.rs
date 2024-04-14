//! Conditional `if` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(isize);
    
    // Rules:

    relation fib(isize, isize);
    
    fib(0, 1) <-- number(0);
    fib(1, 1) <-- number(1);
    fib(x, y + z) <-- number(x), if *x >= 2, fib(x - 1, y), fib(x - 2, z);
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.number = (0..6).map(|n| (n,)).collect();

    prog.run();

    let AscentProgram { mut fib, ..} = prog;

    fib.sort_by_key(|(key, _)| *key);

    assert_eq!(fib, vec![
        (0, 1),
        (1, 1),
        (2, 2),
        (3, 3),
        (4, 5),
        (5, 8),
    ]);
}
