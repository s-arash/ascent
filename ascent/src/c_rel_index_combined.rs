use crate::rel_index_read::RelIndexCombined;
use crate::internal::{CRelIndexRead, CRelIndexReadAll};

use rayon::prelude::*;

impl <'a, Ind1, Ind2, K, V> CRelIndexRead<'a> for RelIndexCombined<'a, Ind1, Ind2> 
where Ind1: CRelIndexRead<'a, Key = K, Value = V>,  Ind2: CRelIndexRead<'a, Key = K, Value = V>, {
   type Key = K;

   type Value = V;

   type IteratorType = rayon::iter::Chain<rayon::iter::Flatten<rayon::option::IntoIter<<Ind1 as CRelIndexRead<'a>>::IteratorType>>, 
                                          rayon::iter::Flatten<rayon::option::IntoIter<<Ind2 as CRelIndexRead<'a>>::IteratorType>>>;

   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      match (self.ind1.c_index_get(key), self.ind2.c_index_get(key)) {
         (None, None) => None,
         (iter1, iter2) => {
            let res = iter1.into_par_iter().flatten().chain(iter2.into_par_iter().flatten());
            Some(res)
         }
      }
   }
}

impl <'a, Ind1, Ind2, K: 'a, V: 'a, VTI: ParallelIterator<Item = V> + 'a> CRelIndexReadAll<'a> for RelIndexCombined<'a, Ind1, Ind2> 
where Ind1: CRelIndexReadAll<'a, Key = K, ValueIteratorType = VTI>,  Ind2: CRelIndexReadAll<'a, Key = K, ValueIteratorType = VTI>
{
   type Key = K;
   type Value = V;

   type ValueIteratorType = VTI;
   type AllIteratorType = rayon::iter::Chain<Ind1::AllIteratorType, Ind2::AllIteratorType>;

   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      self.ind1.c_iter_all().chain(self.ind2.c_iter_all())
   }
}