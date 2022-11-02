
use std::rc::Rc;
use std::sync::Arc;

pub trait Convert<TSource> {
   fn convert(source: TSource) -> Self;
}

impl<T> Convert<T> for T {
   #[inline(always)]
   fn convert(source: T) -> T {source}
}

impl<T> Convert<&T> for T where T: Clone {
   #[inline(always)]
   fn convert(source: &T) -> T {source.clone()}
}

impl Convert<&str> for String {
   fn convert(source: &str) -> Self {
      source.to_string()
   }
}

impl<T: Clone> Convert<&Rc<T>> for T {
   fn convert(source: &Rc<T>) -> Self {
      source.as_ref().clone()
   }
}

impl<T: Clone> Convert<&Arc<T>> for T {
   fn convert(source: &Arc<T>) -> Self {
      source.as_ref().clone()
   }
}
