use rayon::iter::ParallelIterator;


pub trait CRelIndexRead<'a>{
   type Key;
   type Value;
   type IteratorType: ParallelIterator<Item = Self::Value> + Clone + 'a;
   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType>;
}

pub trait CRelIndexReadAll<'a>{
   type Key: 'a;
   type Value;
   type ValueIteratorType: ParallelIterator<Item = Self::Value> + 'a;
   type AllIteratorType: ParallelIterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a;
   fn c_iter_all(&'a self) -> Self::AllIteratorType;
}