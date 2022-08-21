#![cfg(test)]
use ascent::ascent;
use ascent::ascent_run;
use itertools::Itertools;

use crate::assert_rels_eq;
use crate::utils::rels_equal;

fn percentile<'a, TInputIter>(p: f32) -> impl Fn(TInputIter) -> std::option::IntoIter<i32>
where
   TInputIter: Iterator<Item = (&'a i32,)>,
{
   move |inp| {
      let sorted = inp.map(|tuple| *tuple.0).sorted().collect_vec();
      let p_index = (sorted.len() as f32 * p / 100.0) as usize;
      let p_index = p_index.clamp(0, sorted.len() - 1);
      sorted.get(p_index).cloned().into_iter()
   }
}

#[test]
fn test_ascent_agg3(){
   let res = ascent_run!{
      relation foo(i32, i32);
      relation bar(i32, i32, i32);
      relation baz(i32, i32);
      foo(1, 2);
      foo(10, 11);

      bar(1, x, y),
      bar(10, x * 10, y * 10),
      bar(100, x * 100, y * 100) <-- for (x, y) in (1..100).map(|x| (x, x * 2));

      baz(a, x_75th_p) <--
         foo(a, _),
         agg x_75th_p = (percentile(75.0))(x) in bar(a, x, _);
   };
   // println!("{}", res.summary());
   println!("baz: {:?}", res.baz);
   assert!(rels_equal([(1, 75), (10, 750)], res.baz));
}

#[test]
fn test_ascent_agg4(){
   use ascent::aggregators::*;
   let res = ascent_run!{
      relation foo(i32, i32);
      relation bar(i32, i32, i32);
      relation baz(i32, i32, i32);
      foo(1, 2);
      foo(10, 11);

      bar(1, x, y),
      bar(10, x * 10, y * 10),
      bar(100, x * 100, y * 100) <-- for (x, y) in (1..100).map(|x| (x, x * 2));

      baz(a, x_mean as i32, y_mean as i32) <--
         foo(a, _),
         agg x_mean = mean(x) in bar(a, x, _),
         agg y_mean = mean(y) in bar(a, _, y);

   };
   // println!("{}", res.summary());
   println!("baz: {:?}", res.baz);
   assert!(rels_equal([(1, 50, 100), (10, 500, 1000)], res.baz));
}

#[test]
fn test_ascent_negation(){
   use ascent::aggregators::*;
   let res = ascent_run!{
      relation foo(i32, i32);
      relation bar(i32, i32, i32);
      relation baz(i32, i32);
      relation baz2(i32, i32);

      foo(0, 1);
      foo(1, 2);
      foo(10, 11);
      foo(100, 101);

      bar(1, 2, 102); 
      bar(10, 11, 20);
      bar(10, 11, 12);

      baz(x, y) <--
         foo(x, y),
         !bar(x, y, _);

      // equivalent to:
      baz2(x, y) <--
         foo(x, y),
         agg () = not() in bar(x, y, _);
   };
   // println!("{}", res.summary());
   println!("baz: {:?}", res.baz);
   assert!(rels_equal([(0, 1), (100, 101)], res.baz));
   assert!(rels_equal([(0, 1), (100, 101)], res.baz2));
}

#[test]
fn test_ascent_negation2(){
   use ascent::aggregators::*;
   let res = ascent_run!{
      relation foo(i32, i32);
      relation bar(i32, i32);
      relation baz(i32, i32);
      relation baz2(i32, i32);

      foo(0, 1);
      foo(1, 2);
      foo(10, 11);
      foo(100, 101);

      bar(1, 2); 
      bar(10, 11);
      bar(10, 11);

      baz(x, y) <--
         foo(x, y),
         !bar(x, y);

      // equivalent to:
      baz2(x, y) <--
         foo(x, y),
         agg () = not() in bar(x, y);
   };
   // println!("{}", res.summary());
   println!("baz: {:?}", res.baz);
   assert_rels_eq!([(0, 1), (100, 101)], res.baz);
   assert_rels_eq!([(0, 1), (100, 101)], res.baz2);
}

#[test]
fn test_ascent_negation3(){
   use ascent::aggregators::*;
   let res = ascent_run!{
      relation foo(i32, i32);
      relation bar(i32, i32, i32);
      relation baz(i32, i32);

      foo(0, 1);
      foo(1, 2);
      foo(10, 11);
      foo(100, 101);

      bar(1, 2, 3); 
      bar(10, 11, 13);

      baz(x, y) <--
         foo(x, y),
         !bar(x, y, y + 1);
   };
   // println!("{}", res.summary());
   println!("baz: {:?}", res.baz);
   assert!(rels_equal([(0, 1), (10, 11), (100, 101)], res.baz));
}

#[test]
fn test_ascent_agg_simple(){
   use ascent::aggregators::*;
   let res = ascent_run!{
      relation foo(i32);
      foo(0); foo(10);

      relation bar(i32);
      bar(m as i32) <-- agg m = mean(x) in foo(x);   
   };
   assert!(rels_equal([(5,)], res.bar));
}

// Must fail to compile:
// #[test]
// fn test_ascent_agg_not_stratifiable(){
//    use ascent::aggregators::*;
//    let res = ascent_run!{
//       relation foo(i32, i32, i32);
//       relation bar(i32, i32);
//       relation baz(i32);

//       baz(x) <--
//          foo(x, _, _),
//          !bar(_, x);
         
//       bar(x, x + 1) <-- baz(x); 
//    };
//    assert!(rels_equal([(5,)], res.bar));
// }