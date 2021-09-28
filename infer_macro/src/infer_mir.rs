use std::{collections::{HashMap, HashSet}, fmt::Display, iter::FromIterator};

use itertools::Itertools;
use petgraph::{algo::{condensation, kosaraju_scc}, dot::{Config, Dot}, graphmap::DiGraphMap};
use proc_macro2::Ident;
use quote::ToTokens;
use syn::{Expr, Type, parse2};
use crate::{infer_hir::IrBodyItem, infer_mir::MirRelationVersion::*, utils::{exp_cloned, tuple, tuple_type}};
use crate::infer_syntax::{CondClause, GeneratorNode, RelationIdentity};
use crate::{infer_hir::{IrBodyClause, IrHeadClause, IrRelation, IrRule, InferIr}};

pub(crate) struct InferMir {
   pub sccs: Vec<MirScc>,
   pub deps: HashMap<usize, HashSet<usize>>,
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>
}

pub(crate) struct MirScc {
   pub rules: Vec<MirRule>,
   pub dynamic_relation_indices: HashSet<IrRelation>,
   pub dynamic_relations: HashMap<RelationIdentity, HashSet<IrRelation>> 
}

pub(crate) struct MirRule {
   pub head_clause: IrHeadClause,
   pub body_items: Vec<MirBodyItem>,
}

#[derive(Clone)]
pub(crate) enum MirBodyItem {
   Clause(MirBodyClause),
   Generator(GeneratorNode)
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct MirBodyClause {
   pub rel: MirRelation,
   pub args: Vec<Expr>,
   pub cond_clauses : Vec<CondClause>
}
impl MirBodyClause {
   pub fn selected_args(&self) -> Vec<Expr> {
      self.rel.indices.iter().map(|&i| self.args[i].clone()).collect()
   }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct MirRelation {
   pub relation: RelationIdentity,
   pub indices: Vec<usize>,
   pub ir_name: Ident,
   pub version: MirRelationVersion,
}

pub(crate) fn ir_relation_version_var_name(ir_name: &Ident, version : MirRelationVersion) -> Ident{
   let name = format!("{}_{}", ir_name, version.to_string());
   Ident::new(&name, ir_name.span())
}

impl MirRelation {
   pub fn var_name(&self) -> Ident {
      ir_relation_version_var_name(&self.ir_name, self.version)
   }

   // TODO this copying is not ideal
   pub fn key_type(&self) -> Type {
      let index_types : Vec<_> = self.indices.iter().map(|&i| self.relation.field_types[i].clone()).collect();
      tuple_type(&index_types)
   }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum MirRelationVersion {
   Total,
   Delta,
   New,
}

impl MirRelationVersion {
   pub fn to_string(&self) -> &'static str{
      match self { Self::Delta => "delta", Self::Total => "total", Self::New => "new" }
   }
}

fn get_hir_dep_graph(hir: &InferIr) -> Vec<(usize,usize)> {
   let mut relations_to_rules_in_head : HashMap<&RelationIdentity, HashSet<usize>> = HashMap::new();
   for (i, rule) in hir.rules.iter().enumerate(){
      let rule = &hir.rules[i];
      let head_rel = &rule.head_clause.rel;
      relations_to_rules_in_head.entry(head_rel).or_default().insert(i);
   }
   
   let mut edges = vec![];
   for (i, rule) in hir.rules.iter().enumerate() {
      for bitem in rule.body_items.iter() {
         if let IrBodyItem::Clause(bcl) = bitem {
            let body_rel = &bcl.rel.relation;
            if let Some(set) = relations_to_rules_in_head.get(body_rel){
               for &rule_with_rel_in_head in set.iter(){
                  edges.push((rule_with_rel_in_head, i));
               }
            }
         }
      }
   }
   edges
}

pub(crate) fn compile_hir_to_mir(hir: &InferIr) -> InferMir{

   let dep_graph = get_hir_dep_graph(hir);
   let mut dep_graph = DiGraphMap::<_,()>::from_edges(&dep_graph);
   for i in 0..hir.rules.len() {dep_graph.add_node(i);}
   let dep_graph = dep_graph.into_graph::<usize>();
   // println!("{:?}", Dot::with_config(&dep_graph, &[Config::EdgeNoLabel]));
   let sccs = kosaraju_scc(&dep_graph);
   let mut sccs = condensation(dep_graph, true);

   let mut mir_sccs = vec![];
   for scc in sccs.node_weights().collect_vec().iter().rev(){
      let mut dynamic_relation_indices = HashSet::new();
      let mut dynamic_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::new();
      let mut dynamic_relations_set = HashSet::new();
      for &rule_ind in scc.iter(){
         let rule_ind = rule_ind as usize;
         for bitem in hir.rules[rule_ind].body_items.iter() {
            if let IrBodyItem::Clause(bcl) = bitem {
               let rel = &bcl.rel;
               dynamic_relations_set.insert(rel.relation.clone());
               dynamic_relation_indices.insert(rel.clone());
               dynamic_relations.entry(rel.relation.clone()).or_default().insert(rel.clone());
            }
         }

         let hcl = &hir.rules[rule_ind].head_clause;
         
         dynamic_relations_set.insert(hcl.rel.clone());
         dynamic_relations.entry(hcl.rel.clone()).or_default();
         for rel_ind in &hir.relations_ir_relations[&hcl.rel]{
            dynamic_relation_indices.insert(rel_ind.clone());
            dynamic_relations.get_mut(&hcl.rel).unwrap().insert(rel_ind.clone());
         }

      }
      
      let rules = scc.iter()
                  .flat_map(|&ind| compile_hir_rule_to_mir_rules(&hir.rules[ind as usize], &dynamic_relations_set))
                  .collect_vec(); 

      let mir_scc = MirScc{
         rules,
         dynamic_relations,
         dynamic_relation_indices
      };
      mir_sccs.push(mir_scc);
   }

   let mut sccs_dep_graph = HashMap::new();
   sccs.reverse();
   let sccs_nodes_count = sccs.node_indices().count();
   for n in sccs.node_indices() {
      //the nodes in the sccs graph is in reverse topological order, so we do this 
      sccs_dep_graph.insert(sccs_nodes_count - n.index() - 1, sccs.neighbors(n).map(|n| sccs_nodes_count - n.index() - 1).collect());
   }

   InferMir {
      sccs: mir_sccs,
      deps: sccs_dep_graph,// HashMap::from_iter([(0, HashSet::new())]),
      relations_ir_relations: hir.relations_ir_relations.clone(),
      relations_full_indices: hir.relations_full_indices.clone(),
   }
}

fn compile_hir_rule_to_mir_rules(rule: &IrRule, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirRule>{
   
   fn hir_body_items_to_mir_body_items_vec(mir_body_clauses: &[IrBodyItem], dynamic_relations: &HashSet<RelationIdentity>) -> Vec<Vec<MirBodyItem>> {
      if mir_body_clauses.len() == 0 { return vec![vec![]];}
      let mut pre_res = hir_body_items_to_mir_body_items_vec(&mir_body_clauses[0..(mir_body_clauses.len() - 1)], dynamic_relations);
      let hir_bcls_for_mir_bcl = hir_body_clause_to_mir_body_clauses(&mir_body_clauses[mir_body_clauses.len() - 1], dynamic_relations);
      for i in 0..pre_res.len() {
         if hir_bcls_for_mir_bcl.len() == 1 {
            pre_res[i].push(hir_bcls_for_mir_bcl[0].clone());
         } else {
            for j in 1 .. hir_bcls_for_mir_bcl.len() {
               let mut copy = pre_res[i].clone();
               copy.push(hir_bcls_for_mir_bcl[j].clone());
               pre_res.push(copy);
            }
            pre_res[i].push(hir_bcls_for_mir_bcl[0].clone());
         }
      }
      pre_res
   }
   fn hir_body_clause_to_mir_body_clauses(hir_bitem : &IrBodyItem, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirBodyItem>{
      match hir_bitem{
         IrBodyItem::Clause(hir_bcl) => {
            let mut res = vec![];
            let versions = if dynamic_relations.contains(&hir_bcl.rel.relation) {vec![Total, Delta]} else {vec![Total]};
            for ver in versions.into_iter(){
               let mir_relation = MirRelation {
                  relation : hir_bcl.rel.relation.clone(),
                  indices : hir_bcl.rel.indices.clone(),
                  ir_name: hir_bcl.rel.ir_name.clone(),
                  version: ver,
               };
               let mir_bcl = MirBodyClause{
                  rel: mir_relation,
                  args : hir_bcl.args.clone(),
                  cond_clauses: hir_bcl.cond_clauses.clone()
               };
               res.push(MirBodyItem::Clause(mir_bcl));
            }
            res
         },
         IrBodyItem::Generator(gen) => {
            vec![MirBodyItem::Generator(gen.clone())]
         }
      }
      
   }
   let mir_body_items = hir_body_items_to_mir_body_items_vec(&rule.body_items, dynamic_relations);

   // let mir_body_clauses = mir_body_clauses.into_iter()
   //    .filter(|bcls| {
   //       let mut dynamic_rel_clauses = bcls.iter().filter(|bcl| dynamic_relations.contains(&bcl.rel.relation)).peekable();
   //       // if body clauses contain dynamic relations, they can't all be total
   //       dynamic_rel_clauses.peek().is_none() || !dynamic_rel_clauses.all(|bcl| bcl.rel.version == Total)
   //    } );
   let mir_body_items = mir_body_items.into_iter().filter(|bitems|{
      let mut bcls = bitems.iter().filter_map(|bi| match bi {MirBodyItem::Clause(bcl) => Some(bcl), _ => None}).peekable(); 
      bcls.peek().is_none() || !bcls.all(|bcl| bcl.rel.version == Total)
   });
   
   mir_body_items.into_iter()
      .map(|bcls| MirRule {
         body_items: bcls,
         head_clause: rule.head_clause.clone(),
      }).collect()
}
