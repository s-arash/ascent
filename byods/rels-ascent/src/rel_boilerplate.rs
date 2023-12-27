use std::marker::PhantomData;

use ascent::internal::{RelIndexMerge, RelIndexWrite};

pub struct NoopRelIndexWrite<K, V>(PhantomData<(K, V)>);

impl<K, V> Default for NoopRelIndexWrite<K, V> {
   #[inline(always)]
   fn default() -> Self { Self(PhantomData) }
}

impl<K, V> RelIndexMerge for NoopRelIndexWrite<K, V> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { }
}

impl <K, V> RelIndexWrite for NoopRelIndexWrite<K, V> {
   type Key = K;
   type Value = V;
   #[inline(always)]
   fn index_insert(&mut self, _key: Self::Key, _value: Self::Value) { }
}
