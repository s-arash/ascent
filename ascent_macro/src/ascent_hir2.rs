use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use itertools::Itertools;
use proc_macro2::Ident;
use syn::Expr;

use crate::ascent_hir::{IrBodyClauseGeneric, IrBodyItemGeneric, IrRuleGeneric, IrAggClauseGeneric, AscentIr, RelationMetadata, IrRule, AscentConfig};
use crate::ascent_syntax::{RelationIdentity, Declaration};
use crate::index_selection::select_concrete_indices;
use crate::index_shape::{IndexTransformation, NestedTuple, get_index_transformation, index_transformation_for_full_index_shape, RefKind};

pub(crate) struct AscentIr2 {
   pub relations_metadata: HashMap<RelationIdentity, RelationMetadata>,
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrPhysicalRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrPhysicalRelation>,
   pub lattices_full_indices: HashMap<RelationIdentity, IrVirtualRelation>,
   pub physical_relations_covered_indices : HashMap<IrPhysicalRelation, Vec<HashSet<usize>>>,
   pub rules: Vec<Ir2Rule>,
   pub declaration: Declaration,
   pub config: AscentConfig,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub(crate) struct IrVirtualRelation {
   pub physical_relation: IrPhysicalRelation,
   pub index_transformation: IndexTransformation,
   pub tuple_shape: NestedTuple<usize, RefKind>,
}

impl IrVirtualRelation {
   pub fn relation(&self) -> &RelationIdentity {
      &self.physical_relation.relation
   }

   pub fn is_full_index(&self) -> bool {
      self.tuple_shape.flatten().len() == self.relation().field_types.len()
   }

   // pub fn ir_name(&self) -> Ident {
   //    // self.
   //    todo!()
   // }
   pub fn display_name(&self) -> String {
      format!("{}({})", self.physical_relation.ir_name(), self.tuple_shape.flatten().into_iter().join("-"))
   }
}

pub type IrIndexShape = Vec<Vec<usize>>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub(crate) struct IrPhysicalRelation {
   pub relation: RelationIdentity,
   pub index_shape: IrIndexShape,
}

impl IrPhysicalRelation {
   pub fn is_full_index(&self) -> bool {
      self.index_shape.iter().flatten().count() == self.relation.field_types.len()
   }

   pub fn ir_name(&self) -> Ident {
      let name = self.index_shape.iter().map(|tuple| if tuple.is_empty() {format!("none")} else {tuple.iter().join("_")}).join("_to_");
      let name = format!("{}_{}", self.relation.name, name);
      Ident::new(&name, self.relation.name.span())
   }

   // pub fn index_shape_nested_tuple(&self) -> NestedTuple<usize> {
   //    if self.index_shape.len() == 1 {
   //       NestedTuple::flat_shape(self.index_shape[0].clone())
   //    } else {
   //       NestedTuple::Tuple(self.index_shape.iter().map(|tuple| {
   //          NestedTuple::flat_shape(tuple.clone())
   //       }).collect_vec())
   //    }
   // }

   pub fn index_shape_nested_tuple(&self) -> NestedTuple<usize, RefKind> {

      // let index_transformation = index_transformation_for_full_index_shape(&self.index_shape);

      let virt = self.index_shape.iter().flatten().cloned().collect_vec();
      let (_index_transformation, nested_tuple) = get_index_transformation(&self.index_shape, &virt, self.is_full_index());

      nested_tuple

      
   }

}

impl Ir2BodyClause {
   pub fn selected_args(&self) -> Vec<Expr> {
      self.rel.tuple_shape.flatten().iter().map(|&i| self.args[i].clone()).collect()
   }
}
pub(crate) type Ir2BodyClause = IrBodyClauseGeneric<IrVirtualRelation>;
pub(crate) type Ir2BodyItem = IrBodyItemGeneric<IrVirtualRelation>;
pub(crate) type Ir2Rule = IrRuleGeneric<IrVirtualRelation>;
pub(crate) type Ir2AggClause = IrAggClauseGeneric<IrVirtualRelation>;


/// return value of 0 means the requested index is not covered, any positive value means the index is covered,
/// with higher values indicating a better match
fn requested_index_covered_by_physical_index(requested_index: &HashSet<usize>, physical_index: &Vec<Vec<usize>>) -> usize {
   let mut remaining_columns = requested_index.clone();
   let mut took_segments = 0;
   for seg in physical_index.iter() {
      if remaining_columns.is_empty() {
         break;
      }
      took_segments += 1;
      let seg = seg.iter().cloned().collect::<HashSet<_>>();
      if seg.is_subset(&remaining_columns) {
         for x in seg {
            remaining_columns.remove(&x);
         }
      } else {
         return 0;
      }
   }
   if remaining_columns.is_empty() && (!requested_index.is_empty() || physical_index.len() > 0 && physical_index[0].is_empty()) {
      10000 - took_segments * 2 - physical_index.len() 
   } else {0}
}

pub(crate) fn compile_hir_to_hir2(hir: AscentIr) -> AscentIr2 {
   let mut physical_relations = vec![];
   let mut ir_relation_to_ir2_relation = HashMap::new();
   let mut relations_physical_relations = HashMap::new();
   let mut relations_full_indices = HashMap::new();
   let mut lattices_full_indices = HashMap::new();
   let mut physical_relations_covered_indices = HashMap::new();

   for (rel, rel_indices) in hir.relations_ir_relations.iter().sorted_by_key(|(r,_)| r.name.to_string()) {
      let requested_indices = rel_indices.iter().sorted_by_key(|r| r.ir_name().to_string())
         .map(|ir_rel| ir_rel.indices.iter().cloned().collect::<HashSet<_>>()).collect_vec();
      let demanded_physical_indices = hir.relations_metadata[rel].demanded_indices.as_ref();
      let mut rel_physical_indices_for_demanded_indices = HashMap::new();
      let mut remaining_requested_indices = vec![];
      for (i, requested_index) in requested_indices.iter().enumerate() {
         if let Some((covering_demanded_index, score)) = demanded_physical_indices.iter()
            .map(|phys_ind| (phys_ind, requested_index_covered_by_physical_index(requested_index, phys_ind)))
            .max_by_key(|x| x.1)
            .filter(|x| x.1 > 0)
         {
            rel_physical_indices_for_demanded_indices.entry(covering_demanded_index).or_insert(vec![]).push(i);
         } else {
            remaining_requested_indices.push(i);
         }
      }
      let mut rel_physical_indices = 
         select_concrete_indices(&remaining_requested_indices.iter().map(|&i| requested_indices[i].clone()).collect_vec())
         .into_iter()
         .map(|(phys_ind, covered)| (phys_ind, covered.into_iter().map(|covered| remaining_requested_indices[covered]).collect_vec()))
         .collect::<HashMap<_,_>>();
      
      for (k, v) in rel_physical_indices_for_demanded_indices.into_iter() {
         rel_physical_indices.entry(k.clone()).or_default().extend(v);
      }

      let find_physical_index = |columns: &[usize]| {
         let columns = columns.iter().cloned().collect::<HashSet<_>>();
         rel_physical_indices.iter().find_map(|(phys, covered)| {
            for &i in covered {
               if requested_indices[i] == columns {
                  return Some(phys)
               }
            }
            None
         })
      };
      let get_physical_relation = |physical_index: &IrIndexShape| {
         IrPhysicalRelation {
            relation: rel.clone(),
            index_shape: physical_index.clone(),
         }
      };
      let mut rel_physical_relations = HashSet::new();
      for (physical_index, covered_requested_indices) in rel_physical_indices.iter() {
         let is_complete = physical_index.iter().flatten().count() == rel.field_types.len();

         // println!("physical_index: {:?}", physical_index);
         // println!("covers:");
         
         // for &covered_index in covered_requested_indices {
         //    println!(" {:?}", &requested_indices[covered_index]);
         // }
         // println!("--------");


         let physical_index_flattened = physical_index.iter().flatten().cloned().collect_vec();
         let physical_relation = get_physical_relation(physical_index);
         physical_relations.push(physical_relation.clone());
         physical_relations_covered_indices.insert(physical_relation.clone(), 
            covered_requested_indices.iter().map(|&i| requested_indices[i].clone()).collect_vec());
         rel_physical_relations.insert(physical_relation.clone());
         for &covered_index in covered_requested_indices {
            let covered_index_set = &requested_indices[covered_index];
            let covered_index = physical_index_flattened.iter().filter(|i| covered_index_set.contains(i)).cloned().collect_vec();
            let matching_ir_relation = rel_indices.iter().find(|&ir_rel| &ir_rel.indices.iter().cloned().collect::<HashSet<_>>() == covered_index_set).unwrap();
            let (index_transformation, tuple_shape) = get_index_transformation(physical_index, &covered_index, is_complete);
            let virtual_relation = IrVirtualRelation {
               physical_relation: physical_relation.clone(),
               index_transformation,
               tuple_shape,
            };
            ir_relation_to_ir2_relation.insert(matching_ir_relation.clone(), virtual_relation);
         }
      }
      relations_physical_relations.insert(rel.clone(), rel_physical_relations);
      
      let relation_full_index = &hir.relations_full_indices[rel];
      relations_full_indices.insert(rel.clone(), get_physical_relation(find_physical_index(&relation_full_index.indices).unwrap()));
      
      if rel.is_lattice {
         let lattice_full_index = &hir.lattices_full_indices[rel];
         let matching_ir_relation = ir_relation_to_ir2_relation[lattice_full_index].clone();
         // lattices_full_indices.insert(rel.clone(), get_physical_relation(find_physical_index(&lattice_full_index.indices).unwrap()));
         lattices_full_indices.insert(rel.clone(), matching_ir_relation);
      }
   }

   AscentIr2 {
      relations_metadata: hir.relations_metadata,
      rules: hir.rules.into_iter().map(|r| map_ir_rule(r, |rel| ir_relation_to_ir2_relation[&rel].clone())).collect_vec(),
      declaration: hir.declaration,
      config: hir.config,
      relations_ir_relations: relations_physical_relations,
      relations_full_indices,
      lattices_full_indices,
      physical_relations_covered_indices
   }
}

fn map_ir_rule<Rel1, Rel2>(rule: IrRuleGeneric<Rel1>, f: impl Fn(Rel1) -> Rel2) -> IrRuleGeneric<Rel2> {
   IrRuleGeneric { 
      head_clauses: rule.head_clauses, 
      body_items: rule.body_items.into_iter().map(|bi| map_ir_body_item(bi, &f)).collect_vec(), 
      is_simple_join: rule.is_simple_join
   }
}

fn map_ir_body_item<Rel1, Rel2>(bi: IrBodyItemGeneric<Rel1>, f: impl Fn(Rel1) -> Rel2) -> IrBodyItemGeneric<Rel2> {
   match bi {
      IrBodyItemGeneric::Clause(cl) => IrBodyItemGeneric::Clause(map_ir_clause(cl, f)),
      IrBodyItemGeneric::Generator(g) => IrBodyItemGeneric::Generator(g),
      IrBodyItemGeneric::Cond(c) => IrBodyItemGeneric::Cond(c),
      IrBodyItemGeneric::Agg(agg) => IrBodyItemGeneric::Agg(map_ir_agg_clause(agg, f)),
   }
}

fn map_ir_clause<Rel1, Rel2>(cl: IrBodyClauseGeneric<Rel1>, f: impl Fn(Rel1) -> Rel2) -> IrBodyClauseGeneric<Rel2> {
   IrBodyClauseGeneric { 
      rel: f(cl.rel), args: cl.args, rel_args_span: cl.rel_args_span, 
      args_span: cl.args_span, cond_clauses: cl.cond_clauses 
   }
}

fn map_ir_agg_clause<Rel1, Rel2>(agg: IrAggClauseGeneric<Rel1>, f: impl Fn(Rel1) -> Rel2) -> IrAggClauseGeneric<Rel2> {
   IrAggClauseGeneric { 
      span: agg.span, pat: agg.pat, aggregator: agg.aggregator, 
      bound_args: agg.bound_args, rel: f(agg.rel), rel_args: agg.rel_args 
   }
}
