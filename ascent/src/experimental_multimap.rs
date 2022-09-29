#![allow(dead_code, unused_variables)]

use rustc_hash::FxHasher;

use std::collections::HashMap;
use std::hash::BuildHasherDefault;

pub trait MultiDictRead<'a> {
   type K: 'a;
   type V: 'a;
   type TIter: Iterator<Item = (Self::K, Self::TMapIter)> + 'a;
   type TMapIter : Iterator<Item = &'a Self::V> + 'a;

   fn iter(&'a self) -> Self::TIter;

   fn get(&'a self, key: &Self::K) -> Option<Self::TMapIter>;

   fn iter_test(& self) -> Self::TIter { todo!() }
}

pub trait MultiDictWrite {
   type K;
   type V;

   fn add(&mut self, key: Self::K, val: Self::V);
}

pub trait DictRead<'a> {
   type K: 'a;
   type V: 'a;
   fn get(&'a self, key: &Self::K) -> Option<&'a Self::V>;

   type TIter: Iterator<Item = (&'a Self::K, &'a Self::V)> + 'a;
   fn iter(&'a self) -> Self::TIter;
}

pub trait DictWrite<'a> {
   type K: 'a;
   type V: 'a;
   fn get_mut(&'a mut self, key: &Self::K) -> Option<&'a mut Self::V>;
}

struct MMA2<TMM>(TMM);

impl<'a, TDict, TDict2> DictRead<'a> for MMA2<TDict>
where TDict: DictRead<'a, V = TDict2> + 'a, TDict2: DictRead<'a> + 'a {
   type K = (TDict::K, TDict2::K);
   type V = TDict2::V;

   fn get(&'a self, key: &Self::K) -> Option<&'a Self::V> {
      self.0.get(&key.0).and_then(|dict2| dict2.get(&key.1))
   }

   type TIter = Box<dyn Iterator<Item = (&'a Self::K, &'a Self::V)> + 'a>;

   fn iter(&'a self) -> Self::TIter {
      // let res = self.0.iter().flat_map(|kv| kv.1.iter().map(|kv2| ((kv.0, kv2.0), kv2.1))).collect::<Vec<_>>();
      // Box::new(res.into_iter())
      todo!()
   }
}

impl<'a, TDict, TDict2> DictWrite<'a> for MMA2<TDict>
where TDict: DictWrite<'a, V = TDict2> + 'a, TDict2: DictWrite<'a> + 'a {
   type K = (TDict::K, TDict2::K);
   type V = TDict2::V;

   fn get_mut(&'a mut self, key: &Self::K) -> Option<&'a mut Self::V> {
      self.0.get_mut(&key.0).and_then(|dict2| dict2.get_mut(&key.1))
   }
}

impl<TMM, TMM2>  MMA2<TMM>
where for<'a> TMM: MultiDictRead<'a, V = TMM2>,
      for<'a> TMM2: MultiDictRead<'a> 
{
   // type K = (& TMM::K, & TMM2::K);

   #[allow(dead_code)]
   fn get<'a>(&'a self, k: (&'a <TMM as MultiDictRead<'a>>::K, &'a <TMM2 as MultiDictRead<'a>>::K)) -> Option<impl Iterator<Item = &'a <TMM2 as MultiDictRead<'a>>::V> + 'a> {
      let res = match self.0.get(&k.0){
         Some(mm2_iter) => Some(mm2_iter.flat_map(move |mm2| mm2.get(&k.1).into_iter().flatten())),
         None => None,
      };
      res
   }
}

struct DictOfMultiDict<TDict>(TDict);

impl<TDict, TMultiDict> DictOfMultiDict<TDict>
where for<'aa> TDict: DictRead<'aa, V = TMultiDict> + 'static,
      for<'aa> TMultiDict: MultiDictRead<'aa> + 'static,
      for <'aa> <TDict as DictRead<'aa>>::K: Clone
{
   fn iter(&self) -> () {
      let res0 = self.0.iter().flat_map(|(k1, v1)| v1.iter_test()).collect::<Vec<_>>();
      let mut res_vec = vec![];
      for (k1, v1) in self.0.iter() {
         let mut v1_iter = v1.iter();
         while let Some((k2, v)) = v1_iter.next() {
            res_vec.push(((k1.clone(), k2), v));
         }
         // res_vec.extend(v1.iter().map(|(k2, v)| ((k1.clone(), k2), v)));
      }
      // let res = self.0.iter().flat_map(|(k1, v1)| v1.iter().map(move |(k2, v)| ((k1.clone(), k2), v))).collect::<Vec<_>>();
      // Box::new(res.into_iter())
      todo!()
   }

}

impl<'a, TDict, TMultiDict> MultiDictRead<'a> for DictOfMultiDict<TDict>
where for<'aa> TDict: DictRead<'aa, V = TMultiDict> + 'static,
      for<'aa> TMultiDict: MultiDictRead<'aa> + 'static,
      TMultiDict: 'a,
      for <'aa> <TDict as DictRead<'aa>>::K: Clone
{
   type K = (<TDict as DictRead<'static>>::K, <TMultiDict as MultiDictRead<'static>>::K);

   type V = <TMultiDict as MultiDictRead<'a>>::V;

   type TIter = Box<dyn Iterator<Item = (Self::K, Self::TMapIter)> + 'a>;

   // type TMapIter = Box<dyn Iterator<Item = &'a Self::V> + 'a>;
   // type TMapIter = <TMultiDict as MultiDictRead<'a>>::TMapIter;
   type TMapIter = <TMultiDict as MultiDictRead<'a>>::TMapIter;

   fn iter(&'a self) -> Self::TIter {
      let res0 = self.0.iter().flat_map(|(k1, v1)| v1.iter_test()).collect::<Vec<_>>();
      let mut res_vec = vec![];
      for (k1, v1) in self.0.iter() {
         let mut v1_iter = v1.iter();
         while let Some((k2, v)) = v1_iter.next() {
            res_vec.push(((k1.clone(), k2), v));
         }
         // res_vec.extend(v1.iter().map(|(k2, v)| ((k1.clone(), k2), v)));
      }
      // let res = self.0.iter().flat_map(|(k1, v1)| v1.iter().map(move |(k2, v)| ((k1.clone(), k2), v))).collect::<Vec<_>>();
      // Box::new(res.into_iter())
      todo!()
   }

   fn get(&'a self, key: &Self::K) -> Option<Self::TMapIter> {
      todo!()
   }
}

struct MMA2Write<'a, TMM>(&'a mut TMM);

impl<'a, TMM, TMM2> MultiDictWrite for MMA2Write<'a, TMM>
where TMM: MultiDictWrite<V = TMM2>, TMM2: MultiDictWrite {
   type K = (TMM::K, TMM2::K);
   type V = TMM2::V;

   fn add(&mut self, key: Self::K, val: Self::V) {
      todo!()
   }
}

type HashMampMM<K, V> = HashMap<K, Vec<V>, BuildHasherDefault<FxHasher>>;
// impl<'a, K: Hash + Eq + 'a, V: 'a> MultiDictRead<'a> for HashMampMM<K, V> {
//    type K = K;

//    type V = V;

//    type TIter = Map<std::collections::hash_map::Iter<'a, K, Vec<V>>, for<'aa, 'bb> fn((&'aa K, &'bb Vec<V>)) -> (&'aa K, std::slice::Iter<'bb, V>)>;

//    type TMapIter = std::slice::Iter<'a, V>;

//    fn iter(&'a self) -> Self::TIter {
//       let res : Map<std::collections::hash_map::Iter<'_, K, Vec<V>>, for<'aa, 'bb> fn((&'aa K, &'bb Vec<V>)) -> (&'aa K, std::slice::Iter<'bb, V>)>
//          = self.iter().map(|(k,v)| (k, v.iter()));
//       res
//    }

//    fn get(&'a self, key: &Self::K) -> Option<Self::TMapIter> {
//       self.get(key).map(|x| x.iter())
//    }
// }

// #[test]
// fn test_mma2() {
//    let mut hm: HashMampMM<u32, HashMampMM<u64, usize>> = Default::default();
//    hm.insert(1, vec![[(2, vec![3])].into_iter().collect()]);
//    hm.insert(10, vec![[(20, vec![30])].into_iter().collect()]);
//    let mma = MMA2(hm);
//    let res1 = mma.get((&1u32, &2u64)).unwrap().collect::<Vec<_>>();
//    assert_eq!(&res1[..], &[&3]);

//    let res2 = mma.get((&10u32, &20u64)).unwrap().collect::<Vec<_>>();
//    assert_eq!(&res2[..], &[&30]);
// }

impl<'a, TMM, TMM2: 'a, T1: 'a, T2: 'a> MultiDictRead<'a> for MMA2<TMM>
where TMM: MultiDictRead<'a, K = T1, V = TMM2>,
      TMM2: MultiDictRead<'a, K = T2>,
{
   type K = (&'a T1, &'a T2);
   type V = <TMM2 as MultiDictRead<'a>>::V;

   type TIter = Box<dyn Iterator<Item = ((&'a T1, &'a T2), TMM2::TMapIter)> + 'a>;

   type TMapIter = TMM2::TMapIter;

   fn iter(&'a self) -> Self::TIter {
      // let res = //: Box<dyn Iterator<Item = (&(T1, T2), TMM2::TMapIter)>> = 
      //    Box::new(self.0.iter().flat_map(|(x1, mms)| mms.flat_map(move |mm2| mm2.iter().map(move |(x2, v)| ((x1, x2), v)) )));
                  
                  // .map(|(k, v)| (&k, v)));
      // res
      todo!()

   }

   fn get(&self, _key: &Self::K) -> Option<Self::TMapIter> {
      todo!()
   }
}

// fn mma2_impl_iter_f<'a, TMM: 'a, TMM2: 'a, T1: 'a, T2: 'a>(inp : (&'a T1, &'a TMM::TIter)) 
//    -> impl Iterator<Item = ((&'a T1, &'a T2), &'a TMM2::V)>
// where TMM: MultiMap<'a, K = T1, V = TMM2>,
//       TMM2: MultiMap<'a, K = T2>
// {
//    todo!()
// }


fn _test() {
   let v = vec![(1, vec![2, 3, 4])];
   let _v2 = v.iter().flat_map(|(k, vs)| vs.iter().map(move |v| (k, v))).collect::<Vec<_>>();
}
// struct MMA3<TMM>(TMM);

// impl<'a, TMM, TMM2: 'a, T1: 'a, T2: 'a, T3: 'a> MultiMap<'a> for MMA3<TMM>
// where TMM: MultiMap<'a, K = (T1, T2), V = TMM2>,
//       TMM2: MultiMap<'a, K = T3>
// {
//    type K = (T1, T2, T3);
//    type V = <TMM as MultiMap<'a>>::V;
// }
