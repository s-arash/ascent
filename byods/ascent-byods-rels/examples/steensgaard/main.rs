use std::alloc::System;
use std::borrow::Borrow;

use ascent::ascent;
use ascent::internal::Instant;
use itertools::Itertools;

use ascent_byods_rels::eqrel;
use ascent_byods_rels::tracking_alloc::{self, TrackingAllocator};

#[global_allocator]
static GLOBAL: TrackingAllocator<System> = TrackingAllocator(System);

type Symbol = &'static str;

// examples adapted from Souffle's eqrel paper (https://souffle-lang.github.io/pdf/pact19.pdf)
ascent! {
   #![measure_rule_times]
   /// Steensgaard analysis using `eqrel`
   struct Steensgaard;

   relation alloc(Symbol, Symbol);
   // x := y;
   relation assign(Symbol, Symbol);
   // x := y.f;
   relation load(Symbol, Symbol, Symbol);
   // x.f := y;
   relation store(Symbol, Symbol, Symbol);

   #[ds(eqrel)]
   relation vpt(Symbol, Symbol);

   // assignments
   vpt(x,y) <--
   assign(x,y);

   // allocation sites
   vpt(x,y) <--
   alloc(x,y);

   // load/store pairs
   vpt(y,p) <--
   store(x,f, y),
   load(p,q,f),
   vpt(x,q);
}

ascent! {
   #![measure_rule_times]
   /// Explicit Steensgaard analysis
   struct SteensgaardExplicit;

   relation alloc(Symbol, Symbol);
   // x := y;
   relation assign(Symbol, Symbol);
   // x := y.f;
   relation load(Symbol, Symbol, Symbol);
   // x.f := y;
   relation store(Symbol, Symbol, Symbol);

   relation vpt(Symbol, Symbol);

   vpt(y, x), vpt(x, x) <-- vpt(x, y);
   vpt(x, z) <-- vpt(x, y), vpt(y, z);

   // assignments
   vpt(x,y) <--
   assign(x,y);

   // allocation sites
   vpt(x,y) <--
   alloc(x,y);

   // load/store pairs
   vpt(y,p) <--
   store(x,f, y),
   load(p,q,f),
   vpt(x,q);
}

fn read_csv<T>(path: &str) -> impl Iterator<Item = T>
where
   for<'de> T: serde::de::Deserialize<'de>,
{
   csv::ReaderBuilder::new()
      .delimiter(b'\t')
      .has_headers(false)
      .double_quote(false)
      .quoting(false)
      .from_path(path)
      .unwrap()
      .into_deserialize()
      .map(|x| x.unwrap())
}

fn main() {
   let path = "./steensgaard/openjdk_javalang_steensgaard/";
   let get_path = |x: &str| format!("{path}{x}");

   println!("Running eqrel version.");
   let mem_use_before = tracking_alloc::current_alloc();
   let start_time = Instant::now();
   let mut prog = Steensgaard::default();

   prog.alloc = read_csv::<(String, String)>(&get_path("alloc.facts"))
      .map(|(x, y)| (leak(x), leak(y)))
      .collect_vec();
   prog.assign = read_csv::<(String, String)>(&get_path("assign.facts"))
      .map(|(x, y)| (leak(x), leak(y)))
      .collect_vec();
   prog.load = read_csv::<(String, String, String)>(&get_path("load.facts"))
      .map(|(x, y, z)| (leak(x), leak(y), leak(z)))
      .collect_vec();
   prog.store = read_csv::<(String, String, String)>(&get_path("store.facts"))
      .map(|(x, y, z)| (leak(x), leak(y), leak(z)))
      .collect_vec();

   prog.run();

   println!("mem use: {:.2} Mib", (tracking_alloc::max_alloc() - mem_use_before) as f64 / 2f64.powi(20));
   println!("everything took: {:?}", start_time.elapsed());
   println!("vpt size: {}", prog.__vpt_ind_common.count_exact());

   tracking_alloc::reset_max_alloc();

   // Explicit version:
   println!("");
   println!("Running Explicit version. This will take FOREVER!");
   let mem_use_before = tracking_alloc::current_alloc();
   let start_time = Instant::now();
   let mut prog = SteensgaardExplicit::default();

   prog.alloc = read_csv::<(String, String)>(&get_path("alloc.facts"))
      .map(|(x, y)| (leak(x), leak(y)))
      .collect_vec();
   prog.assign = read_csv::<(String, String)>(&get_path("assign.facts"))
      .map(|(x, y)| (leak(x), leak(y)))
      .collect_vec();
   prog.load = read_csv::<(String, String, String)>(&get_path("load.facts"))
      .map(|(x, y, z)| (leak(x), leak(y), leak(z)))
      .collect_vec();
   prog.store = read_csv::<(String, String, String)>(&get_path("store.facts"))
      .map(|(x, y, z)| (leak(x), leak(y), leak(z)))
      .collect_vec();

   prog.run();

   println!("mem use: {:.2} Mib", (tracking_alloc::max_alloc() - mem_use_before) as f64 / 2f64.powi(20));
   println!("everything took: {:?}", start_time.elapsed());
   println!("vpt size: {}", prog.vpt.len());
}

fn leak<T: Borrow<TB> + 'static, TB: ?Sized>(x: T) -> &'static TB {
   let leaked: &'static T = Box::leak(Box::new(x));
   leaked.borrow()
}
