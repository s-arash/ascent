#![cfg(test)]

use std::sync::{Mutex, RwLock};
use std::time::Instant;

use rayon::prelude::{IntoParallelIterator, ParallelIterator, IntoParallelRefIterator, ParallelBridge};

use crate::c_rel_index::CRelIndex;
use crate::internal::CRelFullIndexWrite;
use crate::rel_index_read::RelIndexRead;

#[test]
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

#[test]
fn bench_crel_index() {
   let mut rel_index = CRelIndex::default();

   let before = Instant::now();
   for i in 0..1000_000 {
      rel_index.insert_if_not_present(&i, i);
   }
   let elapsed = before.elapsed();
   println!("insert time: {:?}", elapsed);

   let iters = 1000_000;

   let before = Instant::now();
   let mut _sum = 0;
   for _ in 0..iters {
      rel_index.freeze();
      _sum += rel_index.index_get(&42).unwrap().next().unwrap();
      rel_index.unfreeze();
   }
   let elapsed = before.elapsed();

   println!("freeze_unfreeze for {} iterations time: {:?}", iters, elapsed);
}

#[test]
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
