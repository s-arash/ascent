mod tracking_alloc;

use std::alloc::System;
use std::time::Instant;

use ascent::ascent;
use ascent_byods_rels::eqrel;
use itertools::Itertools;
use tracking_alloc::TrackingAllocator;

type User = u32;
type Tx = u32;

#[global_allocator]
static GLOBAL: TrackingAllocator<System> = TrackingAllocator(System);

ascent! {
   struct BlockChainAnalysis;

   relation transaction(Tx, User);

   #[ds(eqrel)]
   relation same_user(User, User);

   same_user(u1, u2) <--
      transaction(tx, u1),
      transaction(tx, u2);

   relation same_user_materialized(User, User);
   // same_user_materialized(u1, u2) <-- same_user(u1, u2);

   // same_user(y, y) <-- same_user(&42, y);
   // same_user(x, z) <-- same_user(x, y), same_user(y, z);
}

ascent! {
   struct BlockChainAnalysisExplicit;

   relation transaction(Tx, User);
   relation same_user_explicit(User, User);

   same_user_explicit(u1, u2) <--
      transaction(tx, u1),
      transaction(tx, u2);

   // symmetry, reflexivity:
   same_user_explicit(u2, u1),
   same_user_explicit(u1, u1) <-- same_user_explicit(u1, u2);

   //transitivity:
   same_user_explicit(u1, u3) <-- same_user_explicit(u1, u2), same_user_explicit(u2, u3);
}

fn mem_pretty(bytes: usize) -> String {
   format!("{}KiB", bytes / 1024)
}

   fn main() {

   for count in [100, 200, 400, 600, 800, 1200, 1600, 2000] {
      println!("\n=========== COUNT: {count} ============");
      let mem_use_before = tracking_alloc::current_alloc();
      let transaction = (0..count).flat_map(|tx| (tx..(tx + 2).min(count)).map(move |u| (tx, u))).collect_vec();

      let mut prog = BlockChainAnalysis::default();
      prog.transaction = transaction.iter().cloned().collect();

      let before = Instant::now();
      prog.run();
      println!("prog took {:?} s", before.elapsed().as_secs_f32());
      // println!("same_user len: {}", prog.same_user.len());
      println!("same_user_count_exact: {}", prog.__same_user_ind_common.count_exact());
      println!("same_user_materialized len: {}", prog.same_user_materialized.len());
      println!("mem use by eqrel version: {}", mem_pretty(tracking_alloc::current_alloc() - mem_use_before));

      println!("----------------");

      let mut prog_explicit = BlockChainAnalysisExplicit::default();
      prog_explicit.transaction = transaction.iter().cloned().collect();

      let before = Instant::now();
      let mem_use_before = tracking_alloc::current_alloc();
      prog_explicit.run();
      println!("prog_explicit took {:?} s", before.elapsed().as_secs_f32());
      println!("same_user_explicit len: {}", prog_explicit.same_user_explicit.len());
      println!("mem use by explicit version: {}", mem_pretty(tracking_alloc::current_alloc() - mem_use_before));

      // println!("same_user_explicit: {:?}", prog.same_user_explicit);
   }
   println!("\nmax mem use: {}", tracking_alloc::max_alloc());
}
