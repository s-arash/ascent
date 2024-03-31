mod tracking_alloc;

use ascent::ascent;
use ascent_byods_rels::trrel_uf;
use itertools::Itertools;
use tracking_alloc::TrackingAllocator;
use std::alloc::System;
use std::time::Instant;

#[global_allocator]
static GLOBAL: TrackingAllocator<System> = TrackingAllocator(System);

ascent! {
   struct TrRelUFTest;

   relation inp(u32, u32);

   #[ds(trrel_uf)]
   relation tr(u32, u32);

   tr(x, y) <-- inp(x, y);

   relation tr_materialized(u32, u32);
   // tr_materialized(x, y) <-- tr(x, y);
}

ascent! {
   struct TrRelUFTestExplicit;

   relation inp(u32, u32);

   relation tr_explicit(u32, u32);
   relation tr_explicit_proto(u32, u32);

   tr_explicit_proto(x, y) <-- inp(x, y);

   tr_explicit(x, y) <-- tr_explicit_proto(x, y);
   tr_explicit(x, z) <-- tr_explicit_proto(x, y), tr_explicit(y, z);
}

fn main() {
   let test_cases = vec![
      ((0..5000).map(|x| (x, x + 1)).collect_vec(), "chain 5000"),
      ((0..5000).map(|x| (x, x + 1)).chain([(5000, 0)]).collect_vec(), "loop 5000"),
      (
         (0..20)
            .flat_map(|x| {
               let start = x * 500;
               (start..start + 500).map(|x| (x, x + 1)).chain([(start + 500, start)])
            })
            .collect_vec(),
         "loops",
      ),
   ];

   for (inp, name) in test_cases {
      println!("\n======= {name} =======");

      tracking_alloc::reset_max_alloc();
      let mem_use_before = tracking_alloc::current_alloc();
      let mut prog = TrRelUFTest::default();
      prog.inp = inp.clone();
      let before = Instant::now();
      prog.run();
      println!("tr_materialized len: {}", prog.tr_materialized.len());
      println!("tr with union find count exact: {}", prog.__tr_ind_common.count_exact());
      println!("tr with union find took {:?}", before.elapsed());
      println!("tr with union find max mem use: {}MB", (tracking_alloc::max_alloc() - mem_use_before) / (1 << 20));

      println!("----------------");
      tracking_alloc::reset_max_alloc();
      let mem_use_before = tracking_alloc::current_alloc();
      let mut explicit_prog = TrRelUFTestExplicit::default();
      explicit_prog.inp = inp.clone();
      let before = Instant::now();
      explicit_prog.run();
      println!("tr_explicit len: {}", explicit_prog.tr_explicit.len());
      println!("tr explicit took {:?}", before.elapsed());
      println!("tr explicit max mem use: {}MB", (tracking_alloc::max_alloc() - mem_use_before) / (1 << 20));
   }
}
