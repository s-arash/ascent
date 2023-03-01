
pub trait ToRelIndex<Rel> {
   
   type RelIndex<'a> where Self: 'a, Rel: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a>;
   
   type RelIndexWrite<'a> where Self: 'a, Rel: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a>;
}
