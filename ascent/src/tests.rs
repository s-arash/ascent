#![cfg(test)]

use instant::Instant;

use crate::experimental_rel_full_index3::RelFullIndex3;
use crate::experimental_rel_full_index_bt::RelFullIndexBT;
use crate::internal::{RelFullIndexRead, RelFullIndexWrite, RelIndexWrite, HashBrownRelFullIndexType};
use crate::rel_index_read::{RelIndexReadAll, RelIndexRead};

type Key = (usize, usize);
fn test_rel_full_index<RFI>()
where RFI: RelFullIndexRead<Key = Key> + RelFullIndexWrite<Key = Key> + RelFullIndexRead<Key = Key> +
           for<'a> RelIndexReadAll<'a, Key = Key> + for<'a> RelIndexRead<'a, Key = Key> + RelIndexWrite<Key = Key>
{
   let mut ind = RFI::default();

   let vals = (0..10000).map(|x| ((x, x), x)).collect::<Vec<_>>();

   let (vals_part1, vals_part2) = vals.split_at(vals.len() / 2);


   for (k, v) in vals_part1.iter() {
      <RFI as RelIndexWrite>::index_insert(&mut ind, *k, v.clone());
   }

   for (k, v) in vals_part1.iter() {
      ind.insert_if_not_present(k, v.clone());
   }
   let mut ind2 = RFI::default();
   for (k, v) in vals_part2.iter() {
      ind2.insert_if_not_present(k, v.clone());
   }

   RFI::move_index_contents(&mut ind2, &mut ind);

   for (k, v) in vals.iter() {
      assert!(ind.contains_key(k));
      let v_retreived = ind.index_get(k).unwrap().collect::<Vec<_>>();
      assert_eq!(&v_retreived[..], &[*v]);

      assert!(ind.contains_key(k));
   }
   let vals_retreived = ind.iter_all().collect::<Vec<_>>();

   for (rk, rv) in vals_retreived.into_iter() {
      let matching = vals.iter().find(|(k, _v)| rk == k).unwrap();
      assert_eq!(&rv.collect::<Vec<_>>()[..], &[matching.1]);
   }
}

fn bench_rel_full_index<RFI>()
where RFI: RelFullIndexRead<Key = Key> + RelFullIndexWrite<Key = Key> + RelFullIndexRead<Key = Key> +
           for<'a> RelIndexReadAll<'a, Key = Key> + for<'a> RelIndexRead<'a, Key = Key> + RelIndexWrite<Key = Key>
{
   let mut ind = RFI::default();

   let vals = {
      let count = 10_000_000;
      let mut vals = Vec::with_capacity(count);
      for x in 0..count { vals.push(((x, x), x)); }
      vals
   };

   let (vals_part1, vals_part2) = vals.split_at(vals.len() / 2);


   let before = Instant::now();
   for (k, v) in vals_part1.iter() {
      <RFI as RelIndexWrite>::index_insert(&mut ind, *k, v.clone());
   }

   for (k, v) in vals_part1.iter() {
      ind.insert_if_not_present(k, v.clone());
   }

   let mut ind2s = vec![];
   let ind2s_count = 1000;
   for ind2_i in 0..ind2s_count {
      let mut ind2 = RFI::default();
      let lb = vals_part2.len() * ind2_i / ind2s_count;
      for (k, v) in vals_part2[lb..(lb + vals_part2.len() / ind2s_count)].iter() {
         ind2.insert_if_not_present(k, v.clone());
      }
      ind2s.push(ind2);
   }
   println!("insert took {:?}", before.elapsed());

   let before = Instant::now();
   for mut ind2 in ind2s.into_iter() {
      RFI::move_index_contents(&mut ind2, &mut ind);
   }
   println!("move_index_contents took {:?}", before.elapsed());

   let before = Instant::now();
   for (k, v) in vals.iter() {
      assert!(ind.contains_key(k));
      let v_retreived = ind.index_get(k).unwrap().collect::<Vec<_>>();
      assert_eq!(&v_retreived[..], &[*v]);
   }
   println!("index_get and contains_key took {:?}", before.elapsed());


   let before = Instant::now();
   let _vals_retreived = ind.iter_all().collect::<Vec<_>>();
   println!("iter_all took {:?}", before.elapsed());
}

#[test]
fn test_rel_full_index_bt() {
   test_rel_full_index::<RelFullIndexBT<Key>>();
}

#[test]
fn test_rel_full_index3() {
   test_rel_full_index::<RelFullIndex3<Key>>();
}

#[test]
fn test_hashbrown_rel_full_index() {
   test_rel_full_index::<HashBrownRelFullIndexType<Key>>();
}

#[test]
fn bench_rel_full_index3() {
   bench_rel_full_index::<RelFullIndex3<Key>>();
}

#[test]
fn bench_hashbrown_rel_full_index() {
   bench_rel_full_index::<HashBrownRelFullIndexType<Key>>();
}