#![deny(warnings)]
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use itertools::Itertools;
use petgraph::{algo::condensation, graphmap::DiGraphMap};
use proc_macro2::{Ident, Span};
use syn::{Expr, Type};
use crate::{ascent_mir::MirRelationVersion::*, ascent_syntax::Signatures, syn_utils::pattern_get_vars};
use crate::utils::{expr_to_ident, pat_to_ident, tuple_type, intersects};
use crate::ascent_syntax::{CondClause, GeneratorNode, RelationIdentity};
use crate::ascent_hir::{AscentConfig, AscentIr, IndexValType, IrAggClause, IrBodyClause, IrBodyItem, IrHeadClause, IrRelation, IrRule, RelationMetadata};

pub(crate) struct AscentMir {
   pub sccs: Vec<MirScc>,
   #[allow(unused)]
   pub deps: HashMap<usize, HashSet<usize>>,
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub relations_metadata: HashMap<RelationIdentity, RelationMetadata>,
   pub lattices_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub signatures: Signatures,
   pub config: AscentConfig,
   pub is_parallel: bool,
}

pub(crate) struct MirScc {
   pub rules: Vec<MirRule>,
   pub dynamic_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub body_only_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub is_looping: bool
}


pub(crate) fn mir_summary(mir: &AscentMir) -> String {
   let mut res = String::new();
   for (i, scc) in mir.sccs.iter().enumerate() {
      writeln!(&mut res, "scc {}, is_looping: {}:", i, scc.is_looping).unwrap();
      for r in scc.rules.iter() {
         writeln!(&mut res, "  {}", mir_rule_summary(r)).unwrap();
      }
      write!(&mut res, "  dynamic relations: ").unwrap();
      writeln!(&mut res, "{}", scc.dynamic_relations.keys().map(|r| r.name.to_string()).join(", ")).unwrap();
   }
   res
}

#[derive(Clone)]
pub(crate) struct MirRule {
   // TODO rename to head_clauses
   pub head_clause: Vec<IrHeadClause>,
   pub body_items: Vec<MirBodyItem>,
   pub simple_join_start_index: Option<usize>,
   pub reorderable: bool
}

pub(crate) fn mir_rule_summary(rule: &MirRule) -> String {
   fn bitem_to_str(bitem: &MirBodyItem) -> String {
      match bitem {
         MirBodyItem::Clause(bcl) => format!("{}_{}", bcl.rel.ir_name, bcl.rel.version.to_string()),
         MirBodyItem::Generator(gen) => format!("for_{}", pat_to_ident(&gen.pattern).map(|x| x.to_string()).unwrap_or_default()),
         MirBodyItem::Cond(CondClause::If(..)) => format!("if ⋯"),
         MirBodyItem::Cond(CondClause::IfLet(..)) => format!("if let ⋯"),
         MirBodyItem::Cond(CondClause::Let(..)) => format!("let ⋯"),
         MirBodyItem::Agg(agg) => format!("agg {}", agg.rel.ir_name()),
      }
   }
   format!("{} <-- {}{simple_join}{reorderable}",
            rule.head_clause.iter().map(|hcl| hcl.rel.name.to_string()).join(", "),
            rule.body_items.iter().map(bitem_to_str).join(", "),
            simple_join = if rule.simple_join_start_index.is_some() {" [SIMPLE JOIN]"} else {""},
            reorderable = if rule.simple_join_start_index.is_some() && !rule.reorderable {" [NOT REORDERABLE]"} else {""})
}

#[derive(Clone)]
pub(crate) enum MirBodyItem {
   Clause(MirBodyClause),
   Generator(GeneratorNode),
   Cond(CondClause),
   Agg(IrAggClause)
}

impl MirBodyItem {
   pub fn unwrap_clause(&self) -> &MirBodyClause {
      match self {
         MirBodyItem::Clause(cl) => cl,
         _ => panic!("MirBodyItem: unwrap_clause called on non_clause")
      }
   }

   pub fn bound_vars(&self) -> Vec<Ident> {
      match self {
         MirBodyItem::Clause(cl) => {
            let cl_vars = cl.args.iter().filter_map(expr_to_ident);
            let cond_cl_vars = cl.cond_clauses.iter().flat_map(|cc| cc.bound_vars());
            cl_vars.chain(cond_cl_vars).collect()
         },
         MirBodyItem::Generator(gen) => pattern_get_vars(&gen.pattern),
         MirBodyItem::Cond(cond) => cond.bound_vars(),
         MirBodyItem::Agg(agg) => pattern_get_vars(&agg.pat),
      }
   }
}

#[derive(Clone)]
pub(crate) struct MirBodyClause {
   pub rel: MirRelation,
   pub args: Vec<Expr>,
   pub rel_args_span: Span,
   pub args_span: Span,
   pub cond_clauses : Vec<CondClause>
}
impl MirBodyClause {
   pub fn selected_args(&self) -> Vec<Expr> {
      self.rel.indices.iter().map(|&i| self.args[i].clone()).collect()
   }

   /// returns a vec of (var_ind, var) of all the variables in the clause
   pub fn vars(&self) -> Vec<(usize, Ident)> {
      self.args.iter().enumerate()
         .filter_map(|(i,v)| expr_to_ident(v).map(|v| (i, v)))
         .collect::<Vec<_>>()
   }

   #[allow(dead_code)]
   pub fn from(ir_body_clause: IrBodyClause, rel: MirRelation) -> MirBodyClause{
      MirBodyClause {
         rel,
         args: ir_body_clause.args,
         rel_args_span: ir_body_clause.rel_args_span,
         args_span: ir_body_clause.args_span,
         cond_clauses: ir_body_clause.cond_clauses,
      }
   }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct MirRelation {
   pub relation: RelationIdentity,
   pub indices: Vec<usize>,
   pub ir_name: Ident,
   pub version: MirRelationVersion,
   pub is_full_index: bool,
   pub is_no_index: bool,
   pub val_type: IndexValType,
}

pub(crate) fn ir_relation_version_var_name(ir_name: &Ident, version : MirRelationVersion) -> Ident{
   let name = format!("{}_{}", ir_name, version.to_string());
   Ident::new(&name, ir_name.span())
}

impl MirRelation {
   pub fn var_name(&self) -> Ident {
      ir_relation_version_var_name(&self.ir_name, self.version)
   }

   #[allow(dead_code)]
   pub fn key_type(&self) -> Type {
      let index_types : Vec<_> = self.indices.iter().map(|&i| self.relation.field_types[i].clone()).collect();
      tuple_type(&index_types)
   }

   pub fn from(ir_relation : IrRelation, version : MirRelationVersion) -> MirRelation {
      MirRelation {
         ir_name: ir_relation.ir_name(),
         is_full_index: ir_relation.is_full_index(),
         is_no_index: ir_relation.is_no_index(),
         relation: ir_relation.relation,
         indices: ir_relation.indices,
         version,
         val_type: ir_relation.val_type
      }
   }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum MirRelationVersion {
   TotalDelta,
   Total,
   Delta,
   New,
}

impl MirRelationVersion {
   pub fn to_string(self) -> &'static str{
      use MirRelationVersion::*;
      match self { TotalDelta => "total+delta", Delta => "delta", Total => "total", New => "new" }
   }
}

fn get_hir_dep_graph(hir: &AscentIr) -> Vec<(usize,usize)> {
   let mut relations_to_rules_in_head : HashMap<&RelationIdentity, HashSet<usize>> = HashMap::with_capacity(hir.rules.len());
   for (i, rule) in hir.rules.iter().enumerate(){
      for head_rel in rule.head_clauses.iter().map(|hcl| &hcl.rel){
         relations_to_rules_in_head.entry(head_rel).or_default().insert(i);
      }
   }
   
   let mut edges = vec![];
   for (i, rule) in hir.rules.iter().enumerate() {
      for bitem in rule.body_items.iter() {
         if let Some(body_rel) = bitem.rel() {
            let body_rel_identity = &body_rel.relation;
            if let Some(set) = relations_to_rules_in_head.get(body_rel_identity){
               for &rule_with_rel_in_head in set.iter(){
                  edges.push((rule_with_rel_in_head, i));
               }
            }
         }
      }
   }
   edges
}

pub(crate) fn compile_hir_to_mir(hir: &AscentIr) -> syn::Result<AscentMir>{

   let dep_graph = get_hir_dep_graph(hir);
   let mut dep_graph = DiGraphMap::<_,()>::from_edges(&dep_graph);
   for i in 0..hir.rules.len() {dep_graph.add_node(i);}
   let dep_graph = dep_graph.into_graph::<usize>();
   // println!("{:?}", Dot::with_config(&dep_graph, &[Config::EdgeNoLabel]));
   let mut sccs = condensation(dep_graph, true);

   let mut mir_sccs = vec![];
   for scc in sccs.node_weights().collect_vec().iter().rev(){
      let mut dynamic_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::new();
      let mut body_only_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::new();

      let mut dynamic_relations_set = HashSet::new();
      for &rule_ind in scc.iter(){
         let rule = &hir.rules[rule_ind];
         for bitem in rule.body_items.iter() {
            if let Some(rel) = bitem.rel(){
               body_only_relations.entry(rel.relation.clone()).or_default().insert(rel.clone());
            }
         }

         for hcl in hir.rules[rule_ind].head_clauses.iter() {
         
            dynamic_relations_set.insert(hcl.rel.clone());
            dynamic_relations.entry(hcl.rel.clone()).or_default();
            // TODO why this?
            // ... we can add only indices used in bodies in the scc, that requires the codegen to be updated.
            for rel_ind in &hir.relations_ir_relations[&hcl.rel]{
               dynamic_relations.get_mut(&hcl.rel).unwrap().insert(rel_ind.clone());
            }
         }

      }

      let mut is_looping = false;
      for rel in dynamic_relations.keys().cloned().collect_vec() {
         if let Some(indices) = body_only_relations.remove(&rel){
            is_looping = true;
            for ind in indices {
               dynamic_relations.entry(rel.clone()).or_default().insert(ind);
            }
         }
      }
      
      let rules = scc.iter()
                  .flat_map(|&ind| compile_hir_rule_to_mir_rules(&hir.rules[ind], &dynamic_relations_set))
                  .collect_vec(); 

      for rule in rules.iter() {
         for bi in rule.body_items.iter() {
            if let MirBodyItem::Agg(agg) = bi {
               if dynamic_relations.contains_key(&agg.rel.relation) {
                  return Err(syn::Error::new(agg.span, 
                     format!("use of aggregated relation {} cannot be stratified", &agg.rel.relation.name)));
               }
            }
         }
      }
      let mir_scc = MirScc{
         rules,
         dynamic_relations,
         body_only_relations,
         is_looping
      };
      mir_sccs.push(mir_scc);
   }

   sccs.reverse();
   let sccs_nodes_count = sccs.node_indices().count();
   let mut sccs_dep_graph = HashMap::with_capacity(sccs_nodes_count);
   for n in sccs.node_indices() {
      //the nodes in the sccs graph is in reverse topological order, so we do this 
      sccs_dep_graph.insert(sccs_nodes_count - n.index() - 1, sccs.neighbors(n).map(|n| sccs_nodes_count - n.index() - 1).collect());
   }

   Ok(AscentMir {
      sccs: mir_sccs,
      deps: sccs_dep_graph,
      relations_ir_relations: hir.relations_ir_relations.clone(),
      relations_full_indices: hir.relations_full_indices.clone(),
      lattices_full_indices: hir.lattices_full_indices.clone(),
      // relations_no_indices: hir.relations_no_indices.clone(),
      relations_metadata: hir.relations_metadata.clone(),
      signatures: hir.signatures.clone(),
      config: hir.config.clone(),
      is_parallel: hir.is_parallel,
   })
}

fn compile_hir_rule_to_mir_rules(rule: &IrRule, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirRule> {
   
   fn versions_base(count: usize) -> Vec<Vec<MirRelationVersion>> {
      if count == 0 {
         vec![]
      } else {
         let mut res = versions_base(count - 1);
         for v in &mut res {
            v.push(MirRelationVersion::TotalDelta);
         }
         let mut new_combination = vec![MirRelationVersion::Total; count];
         new_combination[count - 1] = MirRelationVersion::Delta;
         res.push(new_combination);
         res
      }
   }

   // TODO is it worth it?
   fn versions(dynamic_cls: &[usize], simple_join_start_index: Option<usize>) -> Vec<Vec<MirRelationVersion>> {
      fn remove_total_delta_at_index(ind: usize, res: &mut Vec<Vec<MirRelationVersion>>) {
         let mut i = 0;
         while i < res.len() {
            if res[i].get(ind) == Some(&TotalDelta) {
               res.insert(i + 1, res[i].clone());
               res[i][ind] = Total;
               res[i + 1][ind] = Delta;
            }
            i += 1;
         }
      }
      
      let count = dynamic_cls.len();
      let mut res = versions_base(count);
      let no_total_delta_at_beginning = false;
      if no_total_delta_at_beginning {
         if let Some(ind) = simple_join_start_index {
            remove_total_delta_at_index(ind, &mut res);
            remove_total_delta_at_index(ind + 1, &mut res);
         } else if dynamic_cls.get(0) == Some(&0) {
            remove_total_delta_at_index(0, &mut res);
         }
      }
      res
   }

   fn hir_body_item_to_mir_body_item(hir_bitem : &IrBodyItem, version: Option<MirRelationVersion>) -> MirBodyItem{
      match hir_bitem {
        IrBodyItem::Clause(_) => { },
        _ => assert!(version.is_none())
      }
      match hir_bitem{
         IrBodyItem::Clause(hir_bcl) => {
            let ver = version.unwrap_or(MirRelationVersion::Total);
            let mir_relation = MirRelation::from(hir_bcl.rel.clone(), ver);
            let mir_bcl = MirBodyClause{
               rel: mir_relation,
               args : hir_bcl.args.clone(),
               rel_args_span: hir_bcl.rel_args_span,
               args_span: hir_bcl.args_span,
               cond_clauses: hir_bcl.cond_clauses.clone()
            };
            MirBodyItem::Clause(mir_bcl)
         },
         IrBodyItem::Cond(cl) => MirBodyItem::Cond(cl.clone()),
         IrBodyItem::Generator(gen) => MirBodyItem::Generator(gen.clone()),
         IrBodyItem::Agg(agg) => MirBodyItem::Agg(agg.clone())
      }
   }

   
   let dynamic_cls = rule.body_items.iter().enumerate().filter_map(|(i, cl)| match cl {
      IrBodyItem::Clause(cl) if dynamic_relations.contains(&cl.rel.relation) => Some(i),
      _ => None,
   }).collect_vec();

   let version_combinations = if dynamic_cls.is_empty() {vec![vec![]]} else {versions(&dynamic_cls[..], rule.simple_join_start_index)};

   let mut mir_body_items = Vec::with_capacity(version_combinations.len());

   for version_combination in version_combinations {
      let versions = dynamic_cls.iter().zip(version_combination)
         .fold(vec![None; rule.body_items.len()], |mut acc, (i, v)| {acc[*i] = Some(v); acc});
      let mir_bodys = rule.body_items.iter().zip(versions).map(|(bi, v)| hir_body_item_to_mir_body_item(bi, v))
         .collect_vec();
      mir_body_items.push(mir_bodys)
   }

   mir_body_items.into_iter().map(|bcls| {

      // rule is reorderable if it is a simple join and the second clause does not depend on items 
      // before the first clause (e.g., let z = &1, foo(x, y), bar(y, z) is not reorderable)
      let reorderable = rule.simple_join_start_index.map_or(false, |ind| {
         let pre_first_clause_vars = bcls.iter().take(ind).flat_map(MirBodyItem::bound_vars);
         !intersects(pre_first_clause_vars, bcls[ind + 1].bound_vars())
      });
      MirRule {
         body_items: bcls,
         head_clause: rule.head_clauses.clone(),
         simple_join_start_index: rule.simple_join_start_index,
         reorderable
      }
   }).collect()
}
