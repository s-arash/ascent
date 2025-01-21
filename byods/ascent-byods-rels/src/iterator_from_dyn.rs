use std::rc::Rc;

pub struct IteratorFromDyn<'a, T> {
   iter: Box<dyn Iterator<Item = T> + 'a>,
   producer: Rc<dyn Fn() -> Box<dyn Iterator<Item = T> + 'a> + 'a>,
}

impl<'a, T> IteratorFromDyn<'a, T> {
   pub fn from_box_clo<F: Fn() -> Box<dyn Iterator<Item = T> + 'a> + 'a>(producer: F) -> Self {
      let iter = producer();
      Self { iter, producer: Rc::new(producer) as _ }
   }

   pub fn new<F: Fn() -> Iter + 'a, Iter: Iterator<Item = T> + 'a>(producer: F) -> Self {
      Self::from_box_clo(move || Box::new(producer()))
   }
}
impl<T> Iterator for IteratorFromDyn<'_, T> {
   type Item = T;

   #[inline(always)]
   fn next(&mut self) -> Option<Self::Item> { self.iter.next() }
}

impl<T> Clone for IteratorFromDyn<'_, T> {
   fn clone(&self) -> Self { Self { iter: (self.producer)(), producer: self.producer.clone() } }
}
