use std::fmt::Display;

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
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "_") }
}

#[allow(dead_code)]
fn map_index(source_val: String, actual: Type, virt: &[usize]) -> (String, NestedTuple<usize>) {
   map_index_inner(source_val, actual, virt, None)
}

fn map_index_inner(source_val: String, actual: Type, virt: &[usize], shape_prefix: Option<NestedTuple<usize>>) 
-> (String, NestedTuple<usize>) 
{
   fn slice_prefix_of<'a>(a: &[usize], b: &'a [usize]) -> Option<&'a [usize]> {
      if a.len() > b.len() {return None}
      if a == &b[0..a.len()] {
         Some(&b[a.len() ..])
      } else {
         None
      }
   }
   fn with_shape_prefix(shape_prefix: Option<NestedTuple<usize>>, shape: NestedTuple<usize>) -> NestedTuple<usize> {
      match shape_prefix {
         Some(prefix) => NestedTuple::Tuple(vec![prefix, shape]),
         None => shape,
      }
   }
   use Type::*;
   match &actual {
      Map(actual_from, actual_to) => {
         if let Some(rest) = slice_prefix_of(actual_from, virt) {
            match actual_to.as_ref() {
               Map(actual_to_from, _actual_to_to) => {
                  if rest.len() > 0 && actual_to_from[0] == rest[0] {
                     let actual_shape = NestedTuple::flat_shape(actual_from.clone());
                     let new_shape_prefix = with_shape_prefix(shape_prefix, actual_shape);
                     let new_val = format!("DictOfDictUncurried({})", source_val);
                     map_index_inner(new_val, actual_to.as_ref().clone(), &rest, Some(new_shape_prefix))
                  } else {
                     assert!(virt.len() == actual_from.len());
                     fn arrow_count(typ: &Type) -> usize {
                        match typ {
                           Map(_from, to) => 1 + arrow_count(to),
                           Target => 0,
                        }
                     }
                     let mut res = source_val;
                     for _ in 0..arrow_count(&actual_to) {
                        res = format!("DictOfDictAsMultiDict({})", res);
                     }
                     (res, with_shape_prefix(shape_prefix, NestedTuple::flat_shape(virt.to_vec())))
                  }
               },
               Target => {
                  assert_eq!(&actual_from, &virt);
                  (source_val, with_shape_prefix(shape_prefix, NestedTuple::flat_shape(virt.to_vec())))
               },
            }
         } else {
            panic!("no match between actual ind {:?} and virt ind {:?}", actual, virt);
         }
      },
      Target => {
         assert!(virt.is_empty(), "no match between actual ind {:?} and virt ind {:?}", actual, virt);
         (source_val, NestedTuple::Tuple(vec![]))
      },
   }
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
      let (mapped, mapped_shape) = map_index("val".into(), actual_ind.clone(), &virt);
      println!("actual: {:?}", actual_ind);
      println!("virt: {:?}", virt);
      println!("mapped: {}", mapped);
      println!("mapped_shape: {}", mapped_shape);
      println!("================================\n");
   }
}

#[derive(Clone)]
enum NestedTuple<B> {
   Base(B),
   Tuple(Vec<NestedTuple<B>>),
}

impl<B> NestedTuple<B> {
   pub fn flat_shape(items: Vec<B>) -> Self {
      NestedTuple::Tuple(items.into_iter().map(NestedTuple::Base).collect())
   }
}

impl<B: Display> Display for NestedTuple<B> {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         NestedTuple::Base(b) => b.fmt(f),
         NestedTuple::Tuple(sub_tuples) => {
            write!(f, "(")?;
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

#[allow(dead_code)]
fn tuple_to_nested<T: Clone>(tuple: &[T], shape: &NestedTuple<()>) -> (NestedTuple<T>, usize) {
   match shape {
      NestedTuple::Base(_) => {
         (NestedTuple::Base(tuple[0].clone()), 1)
      },
      NestedTuple::Tuple(sub_tuples) => {
         let mut ind = 0;
         let mut res = Vec::new();
         for sub_tuple in sub_tuples.iter() {
            let (sub_res, consumed) = tuple_to_nested(&tuple[ind..], sub_tuple);
            res.push(sub_res);
            ind += consumed;
         }
         (NestedTuple::Tuple(res), ind)
      },
   }
}

#[allow(dead_code)]
fn flatten_nested_tuple<T: Clone>(nested: &NestedTuple<T>) -> Vec<T> {
   fn inner<T: Clone>(nested: &NestedTuple<T>, res: &mut Vec<T>) {
      match nested {
         NestedTuple::Base(b) => res.push(b.clone()),
         NestedTuple::Tuple(items) => {
            for item in items.iter() {
               inner(item, res);
            }
         },
      }
   }
   let mut res = Vec::new();
   inner(nested, &mut res);
   res
}

#[test]
fn test_tuple_to_nested() {
   let tuple = [1, 2, 3, 4, 5];
   use NestedTuple::*;
   let shape = Tuple(vec![Base(()), Tuple(vec![Base(()), Base(())]), Tuple(vec![Base(()), Base(())])]);
   let nested = tuple_to_nested(&tuple, &shape);
   println!("nested: {}", nested.0);
   let flattened = flatten_nested_tuple(&nested.0);
   assert_eq!(flattened, tuple);
}
