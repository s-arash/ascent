use paste::paste;

pub trait TupleOfBorrowed {
   type Tuple;
   fn tuple_of_borrowed(self) -> Self::Tuple;
}

impl<'a, T1> TupleOfBorrowed for &'a (T1,) {
   type Tuple = (&'a T1,);

   #[inline(always)]
   fn tuple_of_borrowed(self) -> Self::Tuple { (&self.0,) }
}

impl<'a, T1> TupleOfBorrowed for (&'a T1,) {
   type Tuple = Self;

   #[inline(always)]
   fn tuple_of_borrowed(self) -> Self::Tuple { self }
}

macro_rules! impl_tuple_of_borrowed {
   ($($i: literal),*) => { paste!{
        impl<'a, $([<T $i>]),*> TupleOfBorrowed for &'a ($([<T $i>]),*) {
            type Tuple = ($(&'a [<T $i>]),*);
            #[allow(clippy::unused_unit)]
            #[inline(always)]
            fn tuple_of_borrowed(self) -> Self::Tuple {
                ($(&self.$i),*)
            }
        }

        impl<'a, $([<T $i>]),*> TupleOfBorrowed for ($(&'a [<T $i>]),*) {
            type Tuple = Self;
            #[inline(always)]
            fn tuple_of_borrowed(self) -> Self::Tuple {
                self
            }
        }
    }};
}

impl_tuple_of_borrowed!();
// impl_tuple_of_borrowed!(0);
impl_tuple_of_borrowed!(0, 1);
impl_tuple_of_borrowed!(0, 1, 2);
impl_tuple_of_borrowed!(0, 1, 2, 3);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4, 5);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4, 5, 6);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4, 5, 6, 7);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4, 5, 6, 7, 8);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
impl_tuple_of_borrowed!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
