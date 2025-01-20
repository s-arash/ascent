use std::marker::PhantomData;
use std::ops::Index;

pub struct FakeVec<T> {
   _phantom: PhantomData<T>,
}

impl<T> Default for FakeVec<T> {
   fn default() -> Self { Self { _phantom: PhantomData } }
}

impl<T> FakeVec<T> {
   #[inline(always)]
   pub fn push(&self, _: T) {}

   pub fn is_empty(&self) -> bool { self.len() == 0 }

   pub fn len(&self) -> usize { 0 }

   pub fn iter(&self) -> std::iter::Empty<&T> { std::iter::empty() }
}

impl<T> Index<usize> for FakeVec<T> {
   type Output = T;

   fn index(&self, _index: usize) -> &Self::Output { panic!("FakeVec is empty!") }
}
