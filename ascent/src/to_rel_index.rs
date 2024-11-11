pub trait ToRelIndex0<Rel> {
   type RelIndex<'a>
   where
      Self: 'a,
      Rel: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a>;

   type RelIndexWrite<'a>
   where
      Self: 'a,
      Rel: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a>;

   type CRelIndexWrite<'a>
   where
      Self: 'a,
      Rel: 'a;
   fn to_c_rel_index_write<'a>(&'a self, rel: &'a Rel) -> Self::CRelIndexWrite<'a>;
}

pub trait ToRelIndex<Rel> {
   type RelIndex<'a>
   where
      Self: 'a,
      Rel: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a>;

   type RelIndexWrite<'a>
   where
      Self: 'a,
      Rel: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a>;
}

impl<T, Rel> ToRelIndex0<Rel> for T
where T: ToRelIndex<Rel>
{
   type RelIndex<'a>
      = T::RelIndex<'a>
   where
      Self: 'a,
      Rel: 'a;

   #[inline(always)]
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { self.to_rel_index(rel) }

   type RelIndexWrite<'a>
      = T::RelIndexWrite<'a>
   where
      Self: 'a,
      Rel: 'a;
   #[inline(always)]
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a> { self.to_rel_index_write(rel) }

   type CRelIndexWrite<'a>
      = T::RelIndex<'a>
   where
      Self: 'a,
      Rel: 'a;
   #[inline(always)]
   fn to_c_rel_index_write<'a>(&'a self, rel: &'a Rel) -> Self::CRelIndexWrite<'a> { self.to_rel_index(rel) }
}
