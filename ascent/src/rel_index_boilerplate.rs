use crate::internal::{RelFullIndexWrite, RelIndexWrite, RelFullIndexRead, RelIndexMerge, CRelIndexWrite, CRelFullIndexWrite};
use crate::rel_index_read::{RelIndexRead, RelIndexReadAll, CRelIndexRead, CRelIndexReadAll};

impl<'a, T> RelIndexWrite for &'a mut T where T: RelIndexWrite {
   type Key = T::Key;
   type Value = T::Value;

   #[inline(always)]
   fn index_insert(&mut self, key: Self::Key, value: Self::Value) { (**self).index_insert(key, value) }
}

impl<'a, T> RelFullIndexWrite for &'a mut T where T:RelFullIndexWrite {
   type Key = T::Key;
   type Value = T::Value;

   #[inline(always)]
   fn insert_if_not_present(&mut self, key: &Self::Key, v: Self::Value) -> bool {
      (**self).insert_if_not_present(key, v)
   }
}

impl<'a, T> CRelIndexWrite for &'a T where T: CRelIndexWrite {
   type Key = T::Key;
   type Value = T::Value;

   #[inline(always)]
   fn index_insert(&self, key: Self::Key, value: Self::Value) {
      (**self).index_insert(key, value)
   }
}

impl<'a, T> CRelFullIndexWrite for &'a T where T: CRelFullIndexWrite {
   type Key = T::Key;
   type Value = T::Value;

   #[inline(always)]
   fn insert_if_not_present(&self, key: &Self::Key, v: Self::Value) -> bool {
      (**self).insert_if_not_present(key, v)
   }
}

impl<'a, T> RelIndexMerge for &'a mut T where T:RelIndexMerge {
   fn move_index_contents(from: &mut Self, to: &mut Self) {
      T::move_index_contents(*from, *to)
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      T::merge_delta_to_total_new_to_delta(*new, *delta, *total)
   }

   fn init(new: &mut Self, delta: &mut Self, total: &mut Self) {
      T::init(new, delta, total)
   }
}

impl<'a, T> RelIndexRead<'a> for &'a T where T: RelIndexRead<'a> {
   type Key = T::Key;
   type Value = T::Value;
   type IteratorType = T::IteratorType;

   #[inline(always)]
   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      (**self).index_get(key)
   }

   #[inline(always)]
   fn len(&self) -> usize {
      (**self).len()
   }
}

impl<'a, T> RelIndexReadAll<'a> for &'a T where T: RelIndexReadAll<'a> {
   type Key = T::Key;
   type Value = T::Value;
   type ValueIteratorType = T::ValueIteratorType;
   type AllIteratorType = T::AllIteratorType;

   #[inline(always)]
   fn iter_all(&'a self) -> Self::AllIteratorType {
      (**self).iter_all()
   }
}

impl<'a, T> RelFullIndexRead<'a> for &'a T where T:RelFullIndexRead<'a> {
   type Key = T::Key;
   #[inline(always)]
   fn contains_key(&self, key: &Self::Key) -> bool { (**self).contains_key(key) }
}

impl<'a, T> CRelIndexRead<'a> for &'a T where T: CRelIndexRead<'a> {
   type Key = T::Key;
   type Value = T::Value;
   type IteratorType = T::IteratorType;
   
   #[inline(always)]
   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      (**self).c_index_get(key)
   }

}

impl<'a, T> CRelIndexReadAll<'a> for &'a T where T: CRelIndexReadAll<'a> {
   type Key = T::Key;
   type Value = T::Value;
   type ValueIteratorType = T::ValueIteratorType;
   type AllIteratorType = T::AllIteratorType;

   #[inline(always)]
   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      (**self).c_iter_all()
   }
}