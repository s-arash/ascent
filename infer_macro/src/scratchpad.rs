use std::collections::{HashMap, HashSet};
fn move_index_contents<K, V>(hm1: &mut HashMap<K, HashSet<V>>, hm2: &mut HashMap<K, HashSet<V>>)
where
   K: Eq + std::hash::Hash,
   V: Eq + std::hash::Hash,
{
   for (k, v) in hm1.drain() {
      let set = hm2.entry(k).or_default();
      set.extend(v);
   }
}
fn comment(_: &str) {}
#[derive(Default)]
pub struct DLProgram {
   pub baz: Vec<(i32, Vec<i32>)>,
   pub baz_indices_0_1: std::collections::HashMap<(i32, Vec<i32>), std::collections::HashSet<usize>>,
   pub baz_indices_: std::collections::HashMap<(), std::collections::HashSet<usize>>,
   pub bar: Vec<(i32, i32)>,
   pub bar_indices_0_1: std::collections::HashMap<(i32, i32), std::collections::HashSet<usize>>,
   pub foo1: Vec<(i32, i32, i32)>,
   pub foo1_indices_0_1_2: std::collections::HashMap<(i32, i32, i32), std::collections::HashSet<usize>>,
   pub foo1_indices_: std::collections::HashMap<(), std::collections::HashSet<usize>>,
   pub foo2: Vec<(i32, Option<i32>)>,
   pub foo2_indices_0: std::collections::HashMap<(i32,), std::collections::HashSet<usize>>,
   pub foo2_indices_0_1: std::collections::HashMap<(i32, Option<i32>), std::collections::HashSet<usize>>,
   pub baz2: Vec<(i32, Vec<i32>, i32)>,
   pub baz2_indices_0_1_2: std::collections::HashMap<(i32, Vec<i32>, i32), std::collections::HashSet<usize>>,
}
impl DLProgram {
   pub fn run(&mut self) {
      let baz2_indices_0_1_2_delta: &mut HashMap<(i32, Vec<i32>, i32), HashSet<usize>> = &mut self.baz2_indices_0_1_2;
      let mut baz2_indices_0_1_2_total: HashMap<(i32, Vec<i32>, i32), HashSet<usize>> = HashMap::default();
      let mut baz2_indices_0_1_2_new: HashMap<(i32, Vec<i32>, i32), HashSet<usize>> = HashMap::default();
      let foo1_indices_0_1_2_delta: &mut HashMap<(i32, i32, i32), HashSet<usize>> = &mut self.foo1_indices_0_1_2;
      let mut foo1_indices_0_1_2_total: HashMap<(i32, i32, i32), HashSet<usize>> = HashMap::default();
      let mut foo1_indices_0_1_2_new: HashMap<(i32, i32, i32), HashSet<usize>> = HashMap::default();
      let baz_indices__delta: &mut HashMap<(), HashSet<usize>> = &mut self.baz_indices_;
      let mut baz_indices__total: HashMap<(), HashSet<usize>> = HashMap::default();
      let mut baz_indices__new: HashMap<(), HashSet<usize>> = HashMap::default();
      let foo1_indices__delta: &mut HashMap<(), HashSet<usize>> = &mut self.foo1_indices_;
      let mut foo1_indices__total: HashMap<(), HashSet<usize>> = HashMap::default();
      let mut foo1_indices__new: HashMap<(), HashSet<usize>> = HashMap::default();
      let foo2_indices_0_delta: &mut HashMap<(i32,), HashSet<usize>> = &mut self.foo2_indices_0;
      let mut foo2_indices_0_total: HashMap<(i32,), HashSet<usize>> = HashMap::default();
      let mut foo2_indices_0_new: HashMap<(i32,), HashSet<usize>> = HashMap::default();
      let bar_indices_0_1_delta: &mut HashMap<(i32, i32), HashSet<usize>> = &mut self.bar_indices_0_1;
      let mut bar_indices_0_1_total: HashMap<(i32, i32), HashSet<usize>> = HashMap::default();
      let mut bar_indices_0_1_new: HashMap<(i32, i32), HashSet<usize>> = HashMap::default();
      let baz_indices_0_1_delta: &mut HashMap<(i32, Vec<i32>), HashSet<usize>> = &mut self.baz_indices_0_1;
      let mut baz_indices_0_1_total: HashMap<(i32, Vec<i32>), HashSet<usize>> = HashMap::default();
      let mut baz_indices_0_1_new: HashMap<(i32, Vec<i32>), HashSet<usize>> = HashMap::default();
      let foo2_indices_0_1_delta: &mut HashMap<(i32, Option<i32>), HashSet<usize>> = &mut self.foo2_indices_0_1;
      let mut foo2_indices_0_1_total: HashMap<(i32, Option<i32>), HashSet<usize>> = HashMap::default();
      let mut foo2_indices_0_1_new: HashMap<(i32, Option<i32>), HashSet<usize>> = HashMap::default();
      loop {
         let mut changed = false;
         comment("bar <-- foo1_indices__total, foo2_indices_0_total");
         let matching = foo1_indices__total.get(&());
         if matching.is_some() {
            let matching = matching.unwrap();
            for &ind in matching.iter() {
               let row = &self.foo1[ind];
               let x = &row.0;
               let y = &row.1;
               let z = &row.2;
               let matching = foo2_indices_0_total.get(&(x.clone(),));
               if matching.is_some() {
                  let matching = matching.unwrap();
                  for &ind in matching.iter() {
                     let row = &self.foo2[ind];
                     let w = &row.1;
                     if *z != 4 {
                        let new_row: (i32, i32) = (*x, y + z + w.unwrap_or(0));
                        if bar_indices_0_1_new.contains_key(&new_row)
                           || bar_indices_0_1_delta.contains_key(&new_row)
                           || bar_indices_0_1_total.contains_key(&new_row)
                        {
                           continue;
                        }
                        let new_row_ind = self.bar.len();
                        bar_indices_0_1_new
                           .entry((new_row.0.clone(), new_row.1.clone()))
                           .or_default()
                           .insert(new_row_ind);
                        self.bar.push(new_row);
                        changed = true;
                     }
                  }
               }
            }
         }
         comment("bar <-- foo1_indices__delta, foo2_indices_0_total");
         let matching = foo1_indices__delta.get(&());
         if matching.is_some() {
            let matching = matching.unwrap();
            for &ind in matching.iter() {
               let row = &self.foo1[ind];
               let x = &row.0;
               let y = &row.1;
               let z = &row.2;
               let matching = foo2_indices_0_total.get(&(x.clone(),));
               if matching.is_some() {
                  let matching = matching.unwrap();
                  for &ind in matching.iter() {
                     let row = &self.foo2[ind];
                     let w = &row.1;
                     if *z != 4 {
                        let new_row: (i32, i32) = (*x, y + z + w.unwrap_or(0));
                        if bar_indices_0_1_new.contains_key(&new_row)
                           || bar_indices_0_1_delta.contains_key(&new_row)
                           || bar_indices_0_1_total.contains_key(&new_row)
                        {
                           continue;
                        }
                        let new_row_ind = self.bar.len();
                        bar_indices_0_1_new
                           .entry((new_row.0.clone(), new_row.1.clone()))
                           .or_default()
                           .insert(new_row_ind);
                        self.bar.push(new_row);
                        changed = true;
                     }
                  }
               }
            }
         }
         comment("bar <-- foo1_indices__total, foo2_indices_0_delta");
         let matching = foo1_indices__total.get(&());
         if matching.is_some() {
            let matching = matching.unwrap();
            for &ind in matching.iter() {
               let row = &self.foo1[ind];
               let x = &row.0;
               let y = &row.1;
               let z = &row.2;
               let matching = foo2_indices_0_delta.get(&(x.clone(),));
               if matching.is_some() {
                  let matching = matching.unwrap();
                  for &ind in matching.iter() {
                     let row = &self.foo2[ind];
                     let w = &row.1;
                     if *z != 4 {
                        let new_row: (i32, i32) = (*x, y + z + w.unwrap_or(0));
                        if bar_indices_0_1_new.contains_key(&new_row)
                           || bar_indices_0_1_delta.contains_key(&new_row)
                           || bar_indices_0_1_total.contains_key(&new_row)
                        {
                           continue;
                        }
                        let new_row_ind = self.bar.len();
                        bar_indices_0_1_new
                           .entry((new_row.0.clone(), new_row.1.clone()))
                           .or_default()
                           .insert(new_row_ind);
                        self.bar.push(new_row);
                        changed = true;
                     }
                  }
               }
            }
         }
         comment("bar <-- foo1_indices__delta, foo2_indices_0_delta");
         let matching = foo1_indices__delta.get(&());
         if matching.is_some() {
            let matching = matching.unwrap();
            for &ind in matching.iter() {
               let row = &self.foo1[ind];
               let x = &row.0;
               let y = &row.1;
               let z = &row.2;
               let matching = foo2_indices_0_delta.get(&(x.clone(),));
               if matching.is_some() {
                  let matching = matching.unwrap();
                  for &ind in matching.iter() {
                     let row = &self.foo2[ind];
                     let w = &row.1;
                     if *z != 4 {
                        let new_row: (i32, i32) = (*x, y + z + w.unwrap_or(0));
                        if bar_indices_0_1_new.contains_key(&new_row)
                           || bar_indices_0_1_delta.contains_key(&new_row)
                           || bar_indices_0_1_total.contains_key(&new_row)
                        {
                           continue;
                        }
                        let new_row_ind = self.bar.len();
                        bar_indices_0_1_new
                           .entry((new_row.0.clone(), new_row.1.clone()))
                           .or_default()
                           .insert(new_row_ind);
                        self.bar.push(new_row);
                        changed = true;
                     }
                  }
               }
            }
         }
         comment("baz2 <-- baz_indices__total");
         let matching = baz_indices__total.get(&());
         if matching.is_some() {
            let matching = matching.unwrap();
            for &ind in matching.iter() {
               let row = &self.baz[ind];
               let x = &row.0;
               let exp = &row.1;
               if *x < 100 {
                  let new_row: (i32, Vec<i32>, i32) = (*x, exp.clone(), exp.len() as i32);
                  if baz2_indices_0_1_2_new.contains_key(&new_row)
                     || baz2_indices_0_1_2_delta.contains_key(&new_row)
                     || baz2_indices_0_1_2_total.contains_key(&new_row)
                  {
                     continue;
                  }
                  let new_row_ind = self.baz2.len();
                  baz2_indices_0_1_2_new
                     .entry((new_row.0.clone(), new_row.1.clone(), new_row.2.clone()))
                     .or_default()
                     .insert(new_row_ind);
                  self.baz2.push(new_row);
                  changed = true;
               }
            }
         }
         comment("baz2 <-- baz_indices__delta");
         let matching = baz_indices__delta.get(&());
         if matching.is_some() {
            let matching = matching.unwrap();
            for &ind in matching.iter() {
               let row = &self.baz[ind];
               let x = &row.0;
               let exp = &row.1;
               if *x < 100 {
                  let new_row: (i32, Vec<i32>, i32) = (*x, exp.clone(), exp.len() as i32);
                  if baz2_indices_0_1_2_new.contains_key(&new_row)
                     || baz2_indices_0_1_2_delta.contains_key(&new_row)
                     || baz2_indices_0_1_2_total.contains_key(&new_row)
                  {
                     continue;
                  }
                  let new_row_ind = self.baz2.len();
                  baz2_indices_0_1_2_new
                     .entry((new_row.0.clone(), new_row.1.clone(), new_row.2.clone()))
                     .or_default()
                     .insert(new_row_ind);
                  self.baz2.push(new_row);
                  changed = true;
               }
            }
         }
         move_index_contents(baz2_indices_0_1_2_delta, &mut baz2_indices_0_1_2_total);
         baz2_indices_0_1_2_delta.clear();
         std::mem::swap(&mut baz2_indices_0_1_2_new, baz2_indices_0_1_2_delta);
         move_index_contents(foo1_indices_0_1_2_delta, &mut foo1_indices_0_1_2_total);
         foo1_indices_0_1_2_delta.clear();
         std::mem::swap(&mut foo1_indices_0_1_2_new, foo1_indices_0_1_2_delta);
         move_index_contents(baz_indices__delta, &mut baz_indices__total);
         baz_indices__delta.clear();
         std::mem::swap(&mut baz_indices__new, baz_indices__delta);
         move_index_contents(foo1_indices__delta, &mut foo1_indices__total);
         foo1_indices__delta.clear();
         std::mem::swap(&mut foo1_indices__new, foo1_indices__delta);
         move_index_contents(foo2_indices_0_delta, &mut foo2_indices_0_total);
         foo2_indices_0_delta.clear();
         std::mem::swap(&mut foo2_indices_0_new, foo2_indices_0_delta);
         move_index_contents(bar_indices_0_1_delta, &mut bar_indices_0_1_total);
         bar_indices_0_1_delta.clear();
         std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
         move_index_contents(baz_indices_0_1_delta, &mut baz_indices_0_1_total);
         baz_indices_0_1_delta.clear();
         std::mem::swap(&mut baz_indices_0_1_new, baz_indices_0_1_delta);
         move_index_contents(foo2_indices_0_1_delta, &mut foo2_indices_0_1_total);
         foo2_indices_0_1_delta.clear();
         std::mem::swap(&mut foo2_indices_0_1_new, foo2_indices_0_1_delta);
         if !changed {
            break;
         }
      }
      self.baz2_indices_0_1_2 = baz2_indices_0_1_2_total;
      self.foo1_indices_0_1_2 = foo1_indices_0_1_2_total;
      self.baz_indices_ = baz_indices__total;
      self.foo1_indices_ = foo1_indices__total;
      self.foo2_indices_0 = foo2_indices_0_total;
      self.bar_indices_0_1 = bar_indices_0_1_total;
      self.baz_indices_0_1 = baz_indices_0_1_total;
      self.foo2_indices_0_1 = foo2_indices_0_1_total;
   }
   pub fn update_indices(&mut self) {
      for (i, tuple) in self.baz.iter().enumerate() {
         let selection_tuple = (tuple.0.clone(), tuple.1.clone());
         self.baz_indices_0_1.entry(selection_tuple).or_default().insert(i);
         let selection_tuple = ();
         self.baz_indices_.entry(selection_tuple).or_default().insert(i);
      }
      for (i, tuple) in self.bar.iter().enumerate() {
         let selection_tuple = (tuple.0.clone(), tuple.1.clone());
         self.bar_indices_0_1.entry(selection_tuple).or_default().insert(i);
      }
      for (i, tuple) in self.foo1.iter().enumerate() {
         let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
         self.foo1_indices_0_1_2.entry(selection_tuple).or_default().insert(i);
         let selection_tuple = ();
         self.foo1_indices_.entry(selection_tuple).or_default().insert(i);
      }
      for (i, tuple) in self.foo2.iter().enumerate() {
         let selection_tuple = (tuple.0.clone(),);
         self.foo2_indices_0.entry(selection_tuple).or_default().insert(i);
         let selection_tuple = (tuple.0.clone(), tuple.1.clone());
         self.foo2_indices_0_1.entry(selection_tuple).or_default().insert(i);
      }
      for (i, tuple) in self.baz2.iter().enumerate() {
         let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
         self.baz2_indices_0_1_2.entry(selection_tuple).or_default().insert(i);
      }
   }
}
