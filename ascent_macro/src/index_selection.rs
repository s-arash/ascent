use std::cmp::Ordering;
use std::collections::{HashSet, HashMap};
use std::ops::RangeBounds;

use itertools::Itertools;
use petgraph::matrix_graph::NodeIndex;
use petgraph::stable_graph::{StableDiGraph, StableGraph};
use petgraph::visit::{IntoEdges, IntoEdgeReferences, EdgeRef, Visitable};
use petgraph::{Graph, Directed};
use petgraph::prelude::{DiGraphMap, UnGraph};
use std::hash::Hash;


// TODO adhoc algorithm, does not produce optimal indices
/// returns a list of pairs, each pair is (actual_index, list of requested indices covered)
pub fn select_concrete_indices_old(requested_indices: &[HashSet<usize>]) -> Vec<(Vec<Vec<usize>>, Vec<usize>)> {

   let mut graph_pre_tred = Vec::new();
   for i in 0..requested_indices.len() {
      for j in 0..requested_indices.len() {
         if i == j {continue}
         if requested_indices[i].is_subset(&requested_indices[j]) {
               graph_pre_tred.push((i, j));
         }
      }
   }
   
   // println!("graph_pre_tred: {:?}", graph_pre_tred);
   
   let mut graph_pre_tred = DiGraphMap::<_,()>::from_edges(&graph_pre_tred);
   for i in 0..requested_indices.len() {graph_pre_tred.add_node(i);}

   // println!("graph_pre_tred: {:?}", graph_pre_tred);

   // let mut graph_pre_tred_dbg: Graph<usize, (), Directed, usize> = graph_pre_tred.clone().into_graph::<usize>();
   
   let toposorted = petgraph::algo::toposort(&graph_pre_tred, None).unwrap();
   let (toposorted_adj_list, rev_map) = petgraph::algo::tred::dag_to_toposorted_adjacency_list(&graph_pre_tred, &toposorted);
   let transitive_reduced = petgraph::algo::tred::dag_transitive_reduction_closure::<_, usize>(&toposorted_adj_list).0;
   
   fn index_of<T: Eq>(arr: &[T], elem: &T) -> usize { arr.iter().find_position(|&x| x == elem).unwrap().0 }
   let mut graph = DiGraphMap::<_,()>::from_edges(
      transitive_reduced.edge_references().map(|er| (index_of(&rev_map, &er.source()), index_of(&rev_map, &er.target())))
   );
   for i in 0..requested_indices.len() {graph.add_node(i);}

   let mut graph: Graph<usize, (), Directed, usize> = graph.into_graph::<usize>();
   

   
   let mut graph = StableDiGraph::from(graph);

   fn maximal_path(graph: &mut StableGraph<usize, (), Directed, usize>) -> Option<Vec<usize>>{
      let source_node = graph.externals(petgraph::Direction::Incoming).next()?;
      let mut res = Vec::new();
      let mut to_be_deleted = Vec::new();

      let mut node = source_node;
      res.push(graph[node]);
      to_be_deleted.push(node);
      while let Some(next_node) = graph.neighbors(node).next() {
         node = next_node;
         res.push(graph[next_node]);
         to_be_deleted.push(next_node);
      }
      for &node in to_be_deleted.iter() {
         graph.remove_node(node);
      }
      Some(res)
   }

   let mut res = Vec::new();
   while let Some(path) = maximal_path(&mut graph){
      let mut actual_index = Vec::new();
      let mut included_columns = HashSet::new();
      for &requested_ind in path.iter() {
         let requested_ind = &requested_indices[requested_ind];
         let mut tuple = Vec::new();
         for &i in requested_ind.iter().sorted_by(|a, b| a.cmp(b)) { 
            if included_columns.insert(i) {
               tuple.push(i)
            }
         }
         actual_index.push(tuple)
      }
      res.push((actual_index, path))
   }
   res
}

/// returns a list of pairs, each pair is (actual_index, list of requested indices covered)
pub fn select_concrete_indices(requested_indices: &[HashSet<usize>]) -> Vec<(Vec<Vec<usize>>, Vec<usize>)> 
{
   #[derive(Clone, Copy, PartialEq, Eq, Hash)]
   enum Side { Left, Right }

   let mut graph: UnGraph<_, ()> = UnGraph::new_undirected();
   let mut left_nodes  = Vec::with_capacity(requested_indices.len());
   let mut right_nodes = Vec::with_capacity(requested_indices.len());

   for i in 0..requested_indices.len() {
      left_nodes.push(graph.add_node((i, Side::Left)));
      right_nodes.push(graph.add_node((i, Side::Right)));
   }
   for (i, ind1) in requested_indices.iter().enumerate() {
      for (j, ind2) in requested_indices.iter().enumerate() {
         if i != j && ind1.is_subset(ind2) {
            graph.add_edge(left_nodes[i], right_nodes[j], ());
         }
      }
   } 
   
   let matching = petgraph::algo::maximum_matching(&graph);

   let mut remaining_indices = (0..requested_indices.len()).collect::<HashSet<_>>();
   let mut res: Vec<HashSet<usize>> = vec![];
   for (n1, n2) in matching.edges() {
      let (n1, n2) = (graph[n1], graph[n2]);
      let (n1, n2) = if n1.1 == Side::Left {(n1, n2)} else {(n2, n1)};
      remaining_indices.remove(&n1.0);
      remaining_indices.remove(&n2.0);
      let s1 = res.iter().enumerate().find(|(i, s)| s.contains(&n1.0)).map(|s| s.0);
      let s2 = res.iter().enumerate().find(|(i, s)| s.contains(&n2.0)).map(|s| s.0);
      match (s1, s2) {
         (None, None) => res.push(HashSet::from([n1.0, n2.0])),
         (None, Some(s)) => { res[s].insert(n1.0); },
         (Some(s), None) => { res[s].insert(n2.0); },
         (Some(s1), Some(s2)) => {
            let (s1, s2) = (s1.min(s2), s1.max(s2));
            let s2 = res.remove(s2);
            for x in s2 { res[s1].insert(x); }
         }
      }
   }
   for i in remaining_indices {
      res.push(HashSet::from([i]));
   }

   let mut complete_res = Vec::new();
   for s in res {
      let path = s.iter().sorted_by(|&&i, &&j| 
         if requested_indices[i].is_subset(&requested_indices[j]) {Ordering::Less} 
         else if &requested_indices[i] == &requested_indices[j] {Ordering::Equal} 
         else {Ordering::Greater}
      ).cloned().collect_vec();

      let mut actual_index = Vec::new();
      let mut included_columns = HashSet::new();
      for &requested_ind in path.iter() {
         let requested_ind = &requested_indices[requested_ind];
         let mut tuple = Vec::new();
         for &i in requested_ind.iter().sorted_by(|a, b| a.cmp(b)) { 
            if included_columns.insert(i) {
               tuple.push(i)
            }
         }
         actual_index.push(tuple)
      }
      complete_res.push((actual_index, path))
   }
   complete_res
}

#[test]
fn test_concrete_indices() {
   let requested_indices = vec![
      vec![2].into_iter().collect::<HashSet<_>>(),
      vec![1, 2].into_iter().collect::<HashSet<_>>(),
      vec![1, 2, 3].into_iter().collect::<HashSet<_>>(),
      vec![1, 2, 4, 5, 6].into_iter().collect::<HashSet<_>>(),
      vec![2, 3, 4].into_iter().collect::<HashSet<_>>(),
      vec![1, 2, 3, 4, 5, 6].into_iter().collect::<HashSet<_>>(),
   ];

   let concrete_indices = select_concrete_indices(&requested_indices);

   for (concrete_index, covered) in concrete_indices.into_iter() {
      println!("concrete index: {:?}", concrete_index);
      for covered_ind in covered {
         let requested_ind = &requested_indices[covered_ind];
         println!("covers {:?}", requested_ind);
         assert!(HashSet::from_iter(concrete_index.iter().flatten().cloned()).is_superset(requested_ind))
      }
   }
}

#[test]
fn test_concrete_indices2() {
   let test_cases = vec![
      (vec![
         vec![0],
         vec![1, 0],
         vec![1]
      ], 2),
      (vec![
         vec![0, 2],
         vec![1, 2],
         vec![0],
         vec![2],
         vec![0, 1, 2]
      ], 2)
   ];

   for (requested_indices, expected_count) in test_cases {
      println!("=======================");
      let requested_indices = requested_indices.into_iter().map(|x| x.into_iter().collect::<HashSet<_>>()).collect_vec();

      let concrete_indices = select_concrete_indices(&requested_indices);
      
      for (concrete_index, covered) in concrete_indices.iter() {
         println!("concrete index: {:?}", concrete_index);
         for &covered_ind in covered {
            let requested_ind = &requested_indices[covered_ind];
            println!("covers {:?}", requested_ind);
            assert!(HashSet::from_iter(concrete_index.iter().flatten().cloned()).is_superset(requested_ind))
         }
      }
      assert_eq!(concrete_indices.len(), expected_count);
   }
}
