//! FizzBuzz

use ascent::ascent;

ascent! {
    // Facts:

    relation number(isize);

    // Rules:

    relation divisible(isize, isize);

    divisible(x, 3) <-- number(x), if x % 3 == 0;
    divisible(x, 5) <-- number(x), if x % 5 == 0;

    relation fizz(isize);

    fizz(x) <-- number(x), divisible(x, 3), !divisible(x, 5);

    relation buzz(isize);

    buzz(x) <-- number(x), !divisible(x, 3), divisible(x, 5);

    relation fizz_buzz(isize);

    fizz_buzz(x) <-- number(x), divisible(x, 3), divisible(x, 5);

    relation other(isize);

    other(x) <-- number(x), !divisible(x, 3), !divisible(x, 5);
}

fn main() {
   let mut prog = AscentProgram::default();

   prog.number = (1..=15).map(|n| (n,)).collect();

   prog.run();

   let AscentProgram { mut fizz, mut buzz, mut fizz_buzz, mut other, .. } = prog;

   fizz.sort_by_key(|(key,)| *key);

   assert_eq!(fizz, vec![(3,), (6,), (9,), (12,),]);

   buzz.sort_by_key(|(key,)| *key);

   assert_eq!(buzz, vec![(5,), (10,),]);

   fizz_buzz.sort_by_key(|(key,)| *key);

   assert_eq!(fizz_buzz, vec![(15,),]);

   other.sort_by_key(|(key,)| *key);

   assert_eq!(other, vec![(1,), (2,), (4,), (7,), (8,), (11,), (13,), (14,)]);
}
