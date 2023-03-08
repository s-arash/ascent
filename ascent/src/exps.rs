#![cfg(test)]
#![allow(dead_code)]

use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::time::Instant;

use rayon::prelude::*;

use crate::c_rel_index::CRelIndex;
use crate::internal::{RelIndexWrite, Freezable};
use crate::rel_index_read::RelIndexRead;
use std::sync::atomic::Ordering::Relaxed;

// #[test]
fn bench_aovec() {
   type AOVec<T> = boxcar::Vec<T>;
   let size = 125_000_000;

   println!("pushing ...");
   let before = Instant::now();
   let mut vec = vec![];
   for i in 0..size {
      vec.push(i);
   }
   let elapsed = before.elapsed();
   println!("vec time: {:?}", elapsed);

   let before = Instant::now();
   let vec = AOVec::new();
   for i in 0..size {
      vec.push(i);
   }
   let elapsed = before.elapsed();
   println!("ao vec time: {:?}", elapsed);

   /////////////////////////////////
   
   println!("\nparallel pushing ...");

   let before = Instant::now();
   let vec = Mutex::new(vec![]);
   (0..size).into_par_iter().for_each(|i| {
      vec.lock().unwrap().push(i);
   });
   let elapsed = before.elapsed();
   assert_eq!(vec.lock().unwrap().len(), size);
   println!("parallel Mutex<Vec> time: {:?}", elapsed);


   let before = Instant::now();
   let vec = AOVec::new();
   (0..size).into_par_iter().for_each(|i| {
      vec.push(i);
   });
   let elapsed = before.elapsed();
   assert_eq!(vec.len(), size);
   println!("parallel ao vec time: {:?}", elapsed);
}

// #[test]
fn bench_atomic_changed() {
   type AOVec<T> = boxcar::Vec<T>;
   let size = 125_000_000;

   {

      let before = Instant::now();
      let vec = AOVec::new();
      let changed = AtomicBool::new(false);
      (0..size).into_par_iter().for_each(|i| {
         vec.push(i);
         changed.store(true, Relaxed);
      });
      let elapsed = before.elapsed();
      println!("changed: {}", changed.load(Relaxed));
      assert_eq!(vec.len(), size);
      println!("atomic changed ao vec time: {:?}", elapsed);
   }

   {
      let before = Instant::now();
      let vec = AOVec::new();
      let changed = (0..size).into_par_iter().fold_with(false, |_changed, i| {
         vec.push(i);
         true
      });
      // let changed = changed.reduce(|| false, |x, y| x | y);
      println!("changed count: {}", changed.count());
      let elapsed = before.elapsed();
      // println!("changed: {}", changed);
      assert_eq!(vec.len(), size);
      println!("therad-local changed ao vec time: {:?}", elapsed);
   }
}


// #[test]
fn bench_crel_index() {
   let mut rel_index = CRelIndex::default();

   let before = Instant::now();
   for i in 0..1000_000 {
      RelIndexWrite::index_insert(&mut rel_index, i, i);
   }
   let elapsed = before.elapsed();
   println!("insert time: {:?}", elapsed);

   let iters = 1000_000;

   let before = Instant::now();
   let mut _sum = 0;
   for _ in 0..iters {
      crate::internal::Freezable::freeze(&mut rel_index as _);
      _sum += rel_index.index_get(&42).unwrap().next().unwrap();
      rel_index.unfreeze();
   }
   
   let elapsed = before.elapsed();

   println!("freeze_unfreeze for {} iterations time: {:?}", iters, elapsed);
}

// #[test]
fn bench_par_iter() {

   let arr = (1..1000_000).collect::<Vec<_>>();

   let before = Instant::now();
   arr.par_iter().for_each(|x| {
      if *x == 42 {
         println!("x is 42");
      }
   });
   println!("par_iter took {:?}", before.elapsed());

   let before = Instant::now();
   arr.iter().par_bridge().for_each(|x| {
      if *x == 42 {
         println!("x is 42");
      }
   });
   println!("par_bridge took {:?}", before.elapsed());
}

#[test]
fn bench_par_flat_map() {

   fn calc_sum(x: usize) -> usize {
      let mut res = 0;
      for i in 0..=(x >> 5) {
         res += i;
      }
      if res >= usize::MAX {
         panic!("boo");
      }
      res
   }

   let len = 40;
   let mut arr = Vec::with_capacity(len);

   for _ in 0..len {
      let vec = (0..1000).collect::<Vec<_>>();
      arr.push(vec);
   }

   
   let before = Instant::now();
   arr.iter().flat_map(|v| v.iter()).for_each(|x| {
      calc_sum(*x);
   });
   println!("ser flat_map took {:?}", before.elapsed());

   let before = Instant::now();
   arr.par_iter().flat_map(|v| v.par_iter()).for_each(|x| {
      calc_sum(*x);
   });
   println!("par flat_map took {:?}", before.elapsed());

   let before = Instant::now();
   arr.par_iter().flat_map_iter(|v| v.iter()).for_each(|x| {
      calc_sum(*x);
   });
   println!("par flat_map_iter took {:?}", before.elapsed());

}

#[test]
fn exp_rayon_scope() {
   rayon::scope(|__scope| {
      __scope.spawn(|_| {

      });
      __scope.spawn(|_| {

      });
   });
}