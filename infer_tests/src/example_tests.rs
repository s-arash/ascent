use infer::infer_run;
use infer::infer;
use std::rc::Rc;
use infer::aggregators::mean;
use crate::utils::rels_equal;
use std::hash::Hash;

#[test]
fn test_generators_conditions_example() {
   let res = infer_run! {
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
   infer! {
      relation student(Student);
      relation course_grade(Student, Course, Grade);
      relation avg_grade(Student, Grade);

      avg_grade(s, avg as Grade) <--
         student(s),
         agg avg = mean(g) in course_grade(s, _, g);
   }
   let mut prog = InferProgram::default();
   prog.student = vec![(1, ), (2, )];
   prog.course_grade = vec![(1, 600, 60), (1, 602, 80), (2, 602, 70), (2, 605, 90)];
   prog.run();
   println!("avg grade: {:?}", prog.avg_grade);
   assert!(rels_equal(&prog.avg_grade, &[(1, 70), (2, 80)]));
}

#[test]
fn test_tc_example() {
   fn tc(r: Vec<(i32, i32)>, reflexive: bool) -> Vec<(i32, i32)> {
      infer_run!{
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
   assert!(rels_equal(tc(r.clone(), true), 
          vec![(1,1), (2,2), (3,3), (4,4), (1, 2), (1, 4), (2, 4), (3, 1), (3, 2), (3, 4)]));
}


#[test]
fn test_generic_tc_example() {
   fn tc<N>(r: Vec<(N, N)>, reflexive: bool) -> Vec<(N, N)> where N: Clone + Hash + Eq{
      infer_run!{
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
   assert!(rels_equal(tc(r.clone(), true), 
          vec![(1,1), (2,2), (3,3), (4,4), (1, 2), (1, 4), (2, 4), (3, 1), (3, 2), (3, 4)]));
}
