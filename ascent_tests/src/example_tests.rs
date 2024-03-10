use ascent::ascent_run;
use ascent::ascent;
use std::rc::Rc;
use ascent::aggregators::mean;
use crate::ascent_m_par;
use crate::ascent_run_m_par;
use crate::assert_rels_eq;
use crate::utils::rels_equal;
use std::hash::Hash;

#[test]
fn test_generators_conditions_example() {
   let res = ascent_run! {
      relation node(i32, Rc<Vec<i32>>);
      relation edge(i32, i32);
      
      node(1, Rc::new(vec![2, 3]));
      node(2, Rc::new(vec![3, 4]));

      edge(x, y) <--
         node(x, neighbors),
         for &y in neighbors.iter();
   };
   println!("edges: {:?}", res.edge);
   assert!(rels_equal(&res.edge, &[(1, 2), (1, 3), (2, 3), (2, 4)]));
}

#[test]
fn test_agg_example() {
   type Student = u32;
   type Course = u32;
   type Grade = u16;
   ascent_m_par! {
      relation student(Student);
      relation course_grade(Student, Course, Grade);
      relation avg_grade(Student, Grade);

      avg_grade(s, avg as Grade) <--
         student(s),
         agg avg = mean(g) in course_grade(s, _, g);
   }
   let mut prog = AscentProgram::default();
   prog.student = FromIterator::from_iter([(1, ), (2, )]);
   prog.course_grade = FromIterator::from_iter([(1, 600, 60), (1, 602, 80), (2, 602, 70), (2, 605, 90)]);
   prog.run();
   println!("avg grade: {:?}", prog.avg_grade);
   assert_rels_eq!(&prog.avg_grade, &[(1, 70), (2, 80)]);
}

#[test]
fn test_tc_example() {
   fn tc(r: Vec<(i32, i32)>, reflexive: bool) -> Vec<(i32, i32)> {
      ascent_run!{
         relation r(i32, i32) = r;
         relation tc(i32, i32);
         tc(x, y) <-- r(x, y);
         tc(x, z) <-- r(x, y), tc(y, z);
         tc(x, x), tc(y, y) <-- if reflexive, r(x, y);
      }.tc
   }
   let r = vec![(1, 2), (2, 4), (3, 1)];
   println!("tc: {:?}", tc(r.clone(), true));
   println!("reflexive tc: {:?}", tc(r.clone(), true));
   assert_rels_eq!(tc(r.clone(), true), 
                   vec![(1,1), (2,2), (3,3), (4,4), (1, 2), (1, 4), (2, 4), (3, 1), (3, 2), (3, 4)]);
}


#[test]
fn test_generic_tc_example() {
   fn tc<N>(r: Vec<(N, N)>, reflexive: bool) -> Vec<(N, N)> where N: Clone + Hash + Eq{
      ascent_run!{
         struct TC<N: Clone + Hash + Eq>;
         relation r(N, N) = r;
         relation tc(N, N);
         tc(x, y) <-- r(x, y);
         tc(x, z) <-- r(x, y), tc(y, z);
         tc(x, x), tc(y, y) <-- if reflexive, r(x, y);
      }.tc
   }
   let r = vec![(1, 2), (2, 4), (3, 1)];
   println!("tc: {:?}", tc(r.clone(), true));
   println!("reflexive tc: {:?}", tc(r.clone(), true));
   assert_rels_eq!(tc(r.clone(), true), 
                   vec![(1,1), (2,2), (3,3), (4,4), (1, 2), (1, 4), (2, 4), (3, 1), (3, 2), (3, 4)]);
}

#[test]
fn test_generic_ty() {
   ascent! {
      struct AscentProgram<T> where T: Clone + Hash + Eq;
      relation dummy(T);
   }

   struct Container<T>(AscentProgram<T>) where T: Clone + Hash + Eq;

   impl<T> Container<T>
   where
      T: Clone + Hash + Eq
   {
      fn run(&mut self) {
         self.0.run();
      }
   }

   let mut container: Container<bool> = Container(AscentProgram::default());
   container.run();
}

#[test]
fn test_generic_ty_with_divergent_impl_generics() {
   ascent! {
      struct AscentProgram<T>;
      impl<T> AscentProgram<T> where T: Clone + Hash + Eq;
      relation dummy(T);
   }

   struct Container<T>(AscentProgram<T>);

   impl<T> Container<T>
   where
      T: Clone + Hash + Eq
   {
      fn run(&mut self) {
         self.0.run();
      }
   }

   let mut container: Container<bool> = Container(AscentProgram::default());
   container.run();
}

#[test]
fn test_borrowed_strings() {
   ascent_m_par! {
      struct Ancestry<'a>;
      relation parent(&'a str, &'a str);
      relation ancestor(&'a str,&'a str);

      ancestor(p, c) <-- parent(p, c);

      ancestor(p, gc) <--
         parent(p, c), ancestor(c, gc);
   }

   let james = "James".to_string();
   let harry = "Harry".to_string();
   let albus = "Albus".to_string();

   let parent_rel = vec![(james.clone(), harry.clone()), (harry.clone(), albus.clone())];

   let mut prog = Ancestry::default();
   // prog.parent = vec![(&zal[..], &rostam[..]), (&rostam[..], &sohrab[..])];
   prog.parent = parent_rel.iter().map(|(p, c)| (&p[..], &c[..])).collect();
   prog.run();
   println!("ancestors: {:?}", prog.ancestor);
   assert_eq!(prog.ancestor.len(), 3);
}

#[test]
fn test_borrowed_strings_2() {

   fn ancestry_fn<'a>(parent_rel: impl Iterator<Item = (&'a str, &'a str)>) -> Vec<(&'a str, &'a str)> {
      ascent_run_m_par! {
         struct Ancestry<'a>;
         relation parent(&'a str, &'a str) = parent_rel.collect();
         relation ancestor(&'a str,&'a str);

         ancestor(p, c) <-- parent(p, c);

         ancestor(p, gc) <--
            parent(p, c), ancestor(c, gc);
      }.ancestor.into_iter().collect()
   }

   let james = "James".to_string();
   let harry = "Harry".to_string();
   let albus = "Albus".to_string();

   let parent_rel = vec![(james.clone(), harry.clone()), (harry.clone(), albus.clone())];
   let ancestor = ancestry_fn(parent_rel.iter().map(|(x, y)| (&x[..], &y[..])));
   println!("ancestors: {:?}", ancestor);
   assert_eq!(ancestor.len(), 3);
}
