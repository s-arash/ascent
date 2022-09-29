use std::fmt::Display;

use syn::token::In;

use crate::ascent_hir2::IrIndexShape;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Clone, Debug)]
enum Type {
   Map(Vec<usize>, Box<Type>),
   Target
}

impl Type {
   #[allow(dead_code)]
   fn map(from: Vec<usize>, to: Type) -> Self { Self::Map(from, Box::new(to)) }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Unit;

impl Display for Unit {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "") }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum RefKind {
   Owned,
   Borrowed
}

impl Display for RefKind {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         RefKind::Owned => write!(f, ""),
         RefKind::Borrowed => write!(f, "&"),
      }
   }
}

pub(crate) fn get_index_transformation(actual: &IrIndexShape, virt: &[usize], is_unique: bool) -> (IndexTransformation, NestedTuple<usize, RefKind>) {
   fn index_shape_to_type(shape: &[Vec<usize>]) -> Type {
      if shape.len() == 0 {
         Type::Target
      } else {
         Type::Map(shape[0].clone(), Box::new(index_shape_to_type(&shape[1..])))
      }
   }
   get_index_transformation_inner(IndexTransformation::PhysicalIndex{ is_unique }, index_shape_to_type(actual), is_unique, virt, None)
}

fn get_index_transformation_inner(source_val: IndexTransformation, actual: Type, is_unique: bool, virt: &[usize], shape_prefix: Option<NestedTuple<usize, RefKind>>) 
-> (IndexTransformation, NestedTuple<usize, RefKind>) 
{
   fn slice_prefix_of<'a>(a: &[usize], b: &'a [usize]) -> Option<&'a [usize]> {
      if a.len() > b.len() {return None}
      if a == &b[0..a.len()] {
         Some(&b[a.len() ..])
      } else {
         None
      }
   }
   fn with_shape_prefix(shape_prefix: Option<NestedTuple<usize, RefKind>>, shape: NestedTuple<usize, RefKind>, ref_kind: RefKind) -> NestedTuple<usize, RefKind> {
      match shape_prefix {
         Some(prefix) => NestedTuple::Tuple(vec![prefix, shape], ref_kind),
         None => shape,
      }
   }
   use Type::*;
   match &actual {
      Map(actual_from, actual_to) => {
         if let Some(rest) = slice_prefix_of(actual_from, virt) {
            match actual_to.as_ref() {
               Map(actual_to_from, actual_to_to) => {
                  if rest.len() > 0 && actual_to_from[0] == rest[0] {
                     let actual_shape = NestedTuple::flat_shape2(actual_from.clone(), &RefKind::Owned, RefKind::Borrowed);
                     let new_shape_prefix = with_shape_prefix(shape_prefix, actual_shape, RefKind::Owned);
                     let inner_unique = match actual_to_to.as_ref() {
                        Map(_, _) => true,
                        Target => is_unique,
                     };
                     let new_val = IndexTransformation::Uncurried { sub: Box::new(source_val), inner_unique };
                     get_index_transformation_inner(new_val, actual_to.as_ref().clone(), is_unique, &rest, Some(new_shape_prefix))
                  } else {
                     assert!(virt.len() == actual_from.len());
                     // fn arrow_count(typ: &Type) -> usize {
                     //    match typ {
                     //       Map(_from, to) => 1 + arrow_count(to),
                     //       Target => 0,
                     //    }
                     // }
                     // let mut res = source_val;
                     // for _ in 0..arrow_count(&actual_to) {
                     //    res = IndexTransformation::SkipMiddle(Box::new(res));
                     // }

                     let mut res = source_val;
                     let mut typ = actual_to.as_ref();
                     let mut outer_multi = false;
                     loop {
                        match typ {
                           Map(_, to) => {
                              let inner_multi = match to.as_ref() {
                                 Map(_, _) => false,
                                 Target => !is_unique,
                              };
                              res = IndexTransformation::SkipMiddle{ sub: Box::new(res), outer_unique: !outer_multi, inner_unique: !inner_multi };
                              typ = to.as_ref();
                              outer_multi = true;
                           },
                           Target => break,
                        }
                     }
                     (res, with_shape_prefix(shape_prefix, NestedTuple::flat_shape2(virt.to_vec(), &RefKind::Owned, RefKind::Borrowed), RefKind::Owned))
                  }
               },
               Target => {
                  assert_eq!(&actual_from, &virt);
                  (source_val, with_shape_prefix(shape_prefix, NestedTuple::flat_shape2(virt.to_vec(), &RefKind::Owned, RefKind::Borrowed), RefKind::Owned))
               },
            }
         } else {
            panic!("no match between actual ind {:?} and virt ind {:?}", actual, virt);
         }
      },
      Target => {
         assert!(virt.is_empty(), "no match between actual ind {:?} and virt ind {:?}", actual, virt);
         (source_val, NestedTuple::Tuple(vec![], RefKind::Owned))
      },
   }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum IndexTransformation {
   // TODO: is_unique is wrong. remove it.
   PhysicalIndex { is_unique: bool },
   Uncurried{ sub: Box<IndexTransformation>, inner_unique: bool },
   SkipMiddle{ sub: Box<IndexTransformation>, outer_unique: bool, inner_unique: bool },
}

impl IndexTransformation {
   pub fn is_physical_index_unique(&self) -> bool {
      match self {
         IndexTransformation::PhysicalIndex { is_unique } => *is_unique,
         IndexTransformation::Uncurried{ sub, .. } => sub.is_physical_index_unique(),
         IndexTransformation::SkipMiddle{ sub, .. } => sub.is_physical_index_unique(),
      }
   }
}

pub fn index_transformation_for_full_index_shape(shape: &[Vec<usize>]) -> IndexTransformation {
   assert!(shape.len() > 0);
   let mut res = IndexTransformation::PhysicalIndex { is_unique: true };
   for _ in 0..shape.len() - 1 {
      res = IndexTransformation::Uncurried{ sub: Box::new(res), inner_unique: true }
   }
   res
}

#[test]
fn test_map_index() {
   use Type::*;
   // TODO add expected values to test cases
   let test_cases = vec![
      (Type::map(vec![1, 2], Type::map(vec![3], Target)), vec![1, 2, 3]),
      (Type::map(vec![1, 2], Type::map(vec![3], Target)), vec![1, 2]),
      (Type::map(vec![1, 2], Type::map(vec![3, 4], Target)), vec![1, 2, 3, 4]),
      (Type::map(vec![1, 2], Type::map(vec![3], Type::map(vec![4], Target))), vec![1, 2, 3, 4]),
      (Type::map(vec![1, 2], Type::map(vec![3], Type::map(vec![4], Target))), vec![1, 2, 3]),
      (Type::map(vec![1], Type::map(vec![2], Type::map(vec![3, 4], Type::map(vec![5], Type::map(vec![6], Target))))), vec![1, 2, 3, 4]),
      (Type::map(vec![1, 2], Type::map(vec![3], Type::map(vec![4, 5], Type::map(vec![6], Type::map(vec![7], Target))))), vec![1, 2, 3, 4, 5])
   ];

   for (actual_ind, virt) in test_cases.into_iter() {
      let (mapped, mapped_shape) = get_index_transformation_inner(
         IndexTransformation::PhysicalIndex { is_unique: false },
         actual_ind.clone(), false, &virt, None
      );
      println!("actual: {:?}", actual_ind);
      println!("virt: {:?}", virt);
      println!("mapped: {:?}", mapped);
      println!("mapped_shape: {}", mapped_shape);
      println!("================================\n");
   }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum NestedTuple<B, MD = ()> {
   Base(B, MD),
   Tuple(Vec<NestedTuple<B, MD>>, MD),
}

impl<B, MD> NestedTuple<B, MD> {

   pub fn flatten(&self) -> Vec<B> where B: Clone {
      fn inner<T: Clone, MD>(nested: &NestedTuple<T, MD>, res: &mut Vec<T>) {
         match nested {
            NestedTuple::Base(b, _) => res.push(b.clone()),
            NestedTuple::Tuple(items, _) => {
               for item in items.iter() {
                  inner(item, res);
               }
            },
         }
      }
      let mut res = Vec::new();
      inner(self, &mut res);
      res
   }

   pub fn pure_shape(&self) -> NestedTuple<(), &MD> {
      self.as_ref().map(|_| ())
   }

   pub fn map_md<MD2>(self, f: impl Fn(MD) -> MD2 + Clone) -> NestedTuple<B, MD2> {
      match self {
         NestedTuple::Base(x, md) => NestedTuple::Base(x, f(md)),
         NestedTuple::Tuple(xs, md) => NestedTuple::Tuple(xs.into_iter().map(|t| t.map_md(f.clone())).collect(), f(md)),
      }
   }

   pub fn with_top_level_md(self, md: MD) -> Self {
      match self {
         NestedTuple::Base(x, _) => NestedTuple::Base(x, md),
         NestedTuple::Tuple(x, _) => NestedTuple::Tuple(x, md),
      }
   }

   pub fn map<T>(self, f: impl Fn(B) -> T + Clone) -> NestedTuple<T, MD> {
      match self {
         NestedTuple::Base(b, md) => NestedTuple::Base(f(b), md),
         NestedTuple::Tuple(items, md) => NestedTuple::Tuple(items.into_iter().map(|item| item.map(f.clone())).collect(), md),
      }
   }

   pub fn as_ref(&self) -> NestedTuple<&B, &MD> {
      match self {
         NestedTuple::Base(b, md) => NestedTuple::Base(b, md),
         NestedTuple::Tuple(items, md) => NestedTuple::Tuple(items.iter().map(|item| item.as_ref()).collect(), md),
      }
   }   

   pub fn with_locations(self) -> NestedTuple<(B, Vec<usize>), MD> {
      let shape = self.pure_shape().map_md(|_| ());
      self.zip(shape.to_locations())
   }

   pub fn zip<T>(self, other: NestedTuple<T>) -> NestedTuple<(B, T), MD> {
      match (self, other) {
         (NestedTuple::Base(b, md), NestedTuple::Base(t, _)) => NestedTuple::Base((b, t), md),
         (NestedTuple::Tuple(items1, md), NestedTuple::Tuple(items2, _)) => {
            assert_eq!(items1.len(), items2.len());
            NestedTuple::Tuple(items1.into_iter().zip(items2).map(|(item1, item2)| item1.zip(item2)).collect(), md)
         },
         _ => panic!("NestedTuple::zip: nested tuples have incompatible shapes")
      }
   }

   pub fn flat_shape2(items: Vec<B>, base_md: &MD, tuple_md: MD) -> Self  where MD: Clone{
      NestedTuple::Tuple(items.into_iter().map(|x| NestedTuple::Base(x, base_md.clone())).collect(), tuple_md)
   }
}

impl<'a, B, MD: Clone> NestedTuple<B, &'a MD> {
   pub fn md_cloned(self) -> NestedTuple<B, MD> {
      self.map_md(|x| x.clone())
   }
}


impl<B> NestedTuple<B, ()> {
   pub fn flat_shape(items: Vec<B>) -> NestedTuple<B> {
      NestedTuple::Tuple(items.into_iter().map(|x| NestedTuple::Base(x, ())).collect(), ())
   }

   pub fn with_shape<MD>(tuple: &[B], shape: NestedTuple<(), MD>) -> NestedTuple<B, MD> where B: Clone {
      let (res, consumed) = tuple_to_nested(tuple, shape);
      assert_eq!(consumed, tuple.len());
      res
   }
}

impl<MD> NestedTuple<(), MD> {
   pub fn to_locations(&self) -> NestedTuple<Vec<usize>> {
      fn inner<MD>(shape: &NestedTuple<(), MD>, prefix: Vec<usize>) -> NestedTuple<Vec<usize>> {
         match shape {
            NestedTuple::Base(_, _) => NestedTuple::Base(prefix, ()),
            NestedTuple::Tuple(items, _) => {
               let new_items = items.into_iter().enumerate().map(|(i, item)| {
                  let mut new_prefix = prefix.clone();
                  new_prefix.push(i);
                  inner(item, new_prefix)
               });
               NestedTuple::Tuple(new_items.collect(), ())
            },
         }
      }
      inner(self, vec![])
   }
}


impl<B: Display, MD: Display> Display for NestedTuple<B, MD> {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         NestedTuple::Base(b, md) => write!(f, "{}{}", md, b),
         NestedTuple::Tuple(sub_tuples, md) => {
            write!(f, "{}(", md)?;
            if sub_tuples.len() == 1 {
               write!(f, "{},", sub_tuples[0])?;
            } else {
               for (i, sub_tuple) in sub_tuples.iter().enumerate() {
                  if i == sub_tuples.len() - 1 {
                     write!(f, "{}", sub_tuple)?;
                  } else {
                     write!(f, "{}, ", sub_tuple)?;
                  }
               }
            }
            write!(f, ")")
         },
      }
   }
}

/// Returns a pair: (nested_tuple, consumed_items_count)
fn tuple_to_nested<T: Clone, MD>(tuple: &[T], shape: NestedTuple<(), MD>) -> (NestedTuple<T, MD>, usize) {
   match shape {
      NestedTuple::Base(_, md) => {
         (NestedTuple::Base(tuple[0].clone(), md), 1)
      },
      NestedTuple::Tuple(sub_tuples, md) => {
         let mut ind = 0;
         let mut res = Vec::new();
         for sub_tuple in sub_tuples.into_iter() {
            let (sub_res, consumed) = tuple_to_nested(&tuple[ind..], sub_tuple);
            res.push(sub_res);
            ind += consumed;
         }
         (NestedTuple::Tuple(res, md), ind)
      },
   }
}

#[test]
fn test_tuple_to_nested() {
   let tuple = [1, 2, 3, 4, 5];
   use NestedTuple::*;
   let shape = Tuple(vec![Base((), ()), Tuple(vec![Base((), ()), Base((), ())], ()), Tuple(vec![Base((), ()), Base((), ())], ())], ());
   let nested = tuple_to_nested(&tuple, shape);
   println!("nested: {}", nested.0.as_ref().map_md(|_| Unit));
   let flattened = nested.0.flatten();
   assert_eq!(flattened, tuple);
}

#[test]
fn test_to_locations() {
   use NestedTuple::*;
   let shape = Tuple(vec![Base((), ()), Tuple(vec![Base((), ()), Base((), ())], ()), Tuple(vec![Base((), ()), Base((), ())], ())], ());
   let expected_locations = Tuple(vec![Base(vec![0], ()), Tuple(vec![Base(vec![1, 0], ()), Base(vec![1, 1], ())], ()), Tuple(vec![Base(vec![2, 0], ()), Base(vec![2,1], ())], ())], ());
   let locations = shape.to_locations();
   println!("locations: {:?}", locations);
   assert_eq!(locations, expected_locations);
}
