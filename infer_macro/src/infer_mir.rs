use std::{collections::{HashMap, HashSet}, fmt::Display, iter::FromIterator, rc::Rc};
use std::fmt::Write;
use itertools::Itertools;
use petgraph::{algo::{condensation, kosaraju_scc}, dot::{Config, Dot}, graphmap::DiGraphMap};
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{Expr, Type, parse2};
use crate::{infer_hir::{InferConfig, IrBodyItem, get_indices_given_grounded_variables, ir_name_for_rel_indices}, infer_mir::MirRelationVersion::*, infer_syntax::Declaration, utils::{exp_cloned, expr_to_ident, pat_to_ident, tuple, tuple_type}};
use crate::infer_syntax::{CondClause, GeneratorNode, RelationIdentity};
use crate::{infer_hir::{IrBodyClause, IrHeadClause, IrRelation, IrRule, InferIr}};

pub(crate) struct InferMir {
   pub sccs: Vec<MirScc>,
   pub deps: HashMap<usize, HashSet<usize>>,
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   // pub relations_no_indices: HashMap<RelationIdentity, IrRelation>,
   pub relations_initializations: HashMap<RelationIdentity, Rc<Expr>>,
   pub lattices_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub declaration: Declaration,
   pub config: InferConfig
}

pub(crate) struct MirScc {
   pub rules: Vec<MirRule>,
   pub dynamic_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub body_only_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub is_looping: bool
}


pub(crate) fn mir_summary(mir: &InferMir) -> String{
   let mut res = String::new();
   for (i, scc) in mir.sccs.iter().enumerate() {
      writeln!(&mut res, "scc {}, is_looping: {}:", i, scc.is_looping);
      for r in scc.rules.iter() {
         writeln!(&mut res, "  {}", mir_rule_summary(r));
      }
      write!(&mut res, "  dynamic relations: ");
      writeln!(&mut res, "{}", scc.dynamic_relations.keys().map(|r| r.name.to_string()).join(", "));
   }
   res
}

#[derive(Clone)]
pub(crate) struct MirRule {
   pub head_clause: Vec<IrHeadClause>,
   pub body_items: Vec<MirBodyItem>,
   pub has_simple_join: bool,
   pub reorderable: bool
}

pub(crate) fn mir_rule_summary(rule: &MirRule) -> String {
   fn bitem_to_str(bitem: &MirBodyItem) -> String {
      match bitem {
         MirBodyItem::Clause(bcl) => format!("{}_{}", bcl.rel.ir_name, bcl.rel.version.to_string()),
         MirBodyItem::Generator(gen) => format!("for_{}", pat_to_ident(&gen.pattern).map(|x| x.to_string()).unwrap_or_default()),
        MirBodyItem::Cond(cl) => format!("if_"),
      }
   }
   format!("{} <-- {}{}",
            rule.head_clause.iter().map(|hcl| hcl.rel.name.to_string()).join(", "),
            rule.body_items.iter().map(bitem_to_str).join(", "),
            if rule.has_simple_join {" [SIMPLE JOIN]"} else {""})
}

#[derive(Clone)]
pub(crate) enum MirBodyItem {
   Clause(MirBodyClause),
   Generator(GeneratorNode),
   Cond(CondClause)
}

impl MirBodyItem {
   pub fn unwrap_clause(&self) -> &MirBodyClause {
      match self {
         MirBodyItem::Clause(cl) => cl,
         _ => panic!("MirBodyItem: unwrap_clause called on non_clause")
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

   pub fn from(ir_relation : IrRelation, version : MirRelationVersion) -> MirRelation {
      MirRelation {
         ir_name: ir_relation.ir_name(),
         is_full_index: ir_relation.is_full_index(),
         is_no_index: ir_relation.is_no_index(),
         relation: ir_relation.relation,
         indices: ir_relation.indices,
         version,
      }
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
      for head_rel in rule.head_clauses.iter().map(|hcl| &hcl.rel){
         relations_to_rules_in_head.entry(head_rel).or_default().insert(i);
      }
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
   let mut sccs = condensation(dep_graph, true);

   let mut mir_sccs = vec![];
   for scc in sccs.node_weights().collect_vec().iter().rev(){
      let mut dynamic_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::new();
      let mut body_only_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::new();

      let mut dynamic_relations_set = HashSet::new();
      for &rule_ind in scc.iter(){
         let rule = &hir.rules[rule_ind];
         for bitem in rule.body_items.iter() {
            if let IrBodyItem::Clause(bcl) = bitem {
               let rel = &bcl.rel;
               body_only_relations.entry(rel.relation.clone()).or_default().insert(rel.clone());
               if rule.is_simple_join {
                  // body_only_relations.entry(rel.relation.clone()).or_default().insert(hir.relations_full_indices[&rel.relation].clone());
                  // TODO we can use only no_index, for that codegen needs to be updated.
                  // body_only_relations.entry(rel.relation.clone()).or_default().insert(hir.relations_no_indices[&rel.relation].clone());
               }
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
                  .flat_map(|&ind| compile_hir_rule_to_mir_rules(&hir.rules[ind as usize], &dynamic_relations_set))
                  .collect_vec(); 

      let mir_scc = MirScc{
         rules,
         dynamic_relations,
         body_only_relations,
         is_looping
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
      deps: sccs_dep_graph,
      relations_ir_relations: hir.relations_ir_relations.clone(),
      relations_full_indices: hir.relations_full_indices.clone(),
      lattices_full_indices: hir.lattices_full_indices.clone(),
      // relations_no_indices: hir.relations_no_indices.clone(),
      relations_initializations: hir.relations_initializations.clone(),
      declaration: hir.declaration.clone(),
      config: hir.config.clone()
   }
}

// TODO redundant?
fn compile_hir_simple_join_rule_to_mir_rules(rule: &IrRule, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirRule> {
   if let (IrBodyItem::Clause(cl1), IrBodyItem::Clause(cl2)) = (&rule.body_items[0], &rule.body_items[1]) {
      if !(dynamic_relations.contains(&cl1.rel.relation) && dynamic_relations.contains(&cl2.rel.relation)){
         return compile_hir_arbitrary_rule_to_mir_rules(rule, dynamic_relations);
      }

      let rest_body_items = rule.body_items[2..].iter().map(|item|
         match item {
            IrBodyItem::Clause(_) => unreachable!(),
            IrBodyItem::Generator(gen) => MirBodyItem::Generator(gen.clone()),
            IrBodyItem::Cond(cond) => MirBodyItem::Cond(cond.clone()),
         }).collect_vec();

      let cl2_variables = cl2.args.iter().filter_map(expr_to_ident).collect_vec();
      let cl1_indices = get_indices_given_grounded_variables(&cl1.args, &cl2_variables);
      
      println!("simple join: {} <-- {}, {}", rule.head_clauses[0].rel.name.to_string(), cl1.rel.ir_name().to_string(), cl2.rel.ir_name().to_string());
      // println!("cl1 indices: {:?}", cl1_indices);

      let rule_cl2_delta_cl1_total = {
         let cl1_second_relation = MirRelation::from(IrRelation {
            relation: cl1.rel.relation.clone(),
            indices: cl1_indices,
         }, Total);

         let cl2_first_relation = MirRelation::from(IrRelation {
            relation: cl2.rel.relation.clone(),
            indices: vec![]
         }, Delta);
         
         MirRule{
            body_items: vec![MirBodyItem::Clause(MirBodyClause{rel: cl2_first_relation, args:cl2.args.clone(), rel_args_span: cl2.rel_args_span, args_span: cl2.args_span, cond_clauses: cl2.cond_clauses.clone() }),
                             MirBodyItem::Clause(MirBodyClause{rel: cl1_second_relation, args:cl1.args.clone(), rel_args_span: cl1.rel_args_span, args_span: cl1.args_span, cond_clauses: cl1.cond_clauses.clone() })]
                           .into_iter().chain(rest_body_items.clone()).collect_vec(),
            head_clause: rule.head_clauses.clone(),
            has_simple_join: true, reorderable: true
         }
      };
      let rule_cl1_delta_cl2_total = MirRule{
         body_items: vec![MirBodyItem::Clause(MirBodyClause::from(cl1.clone(), MirRelation::from(cl1.rel.clone(), Delta))),
                          MirBodyItem::Clause(MirBodyClause::from(cl2.clone(), MirRelation::from(cl2.rel.clone(), Total)))]
                        .into_iter().chain(rest_body_items.clone()).collect_vec(),
         head_clause: rule.head_clauses.clone(),
        has_simple_join: true, reorderable: true
      };
      let rule_cl1_delta_cl2_delta = MirRule{
         body_items: vec![MirBodyItem::Clause(MirBodyClause::from(cl1.clone(), MirRelation::from(cl1.rel.clone(), Delta))),
                          MirBodyItem::Clause(MirBodyClause::from(cl2.clone(), MirRelation::from(cl2.rel.clone(), Delta)))]
                        .into_iter().chain(rest_body_items.clone()).collect_vec(),
         head_clause: rule.head_clauses.clone(),
        has_simple_join: true, reorderable: true
      };
      vec![rule_cl2_delta_cl1_total, rule_cl1_delta_cl2_total, rule_cl1_delta_cl2_delta]
   } else {
      panic!("bad simple join rule")
   }
}

fn compile_hir_arbitrary_rule_to_mir_rules(rule: &IrRule, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirRule>{
   
   fn hir_body_items_to_mir_body_items_vec(mir_body_clauses: &[IrBodyItem], dynamic_relations: &HashSet<RelationIdentity>) -> Vec<Vec<MirBodyItem>> {
      if mir_body_clauses.len() == 0 { return vec![vec![]];}
      let mut pre_res = hir_body_items_to_mir_body_items_vec(&mir_body_clauses[0..(mir_body_clauses.len() - 1)], dynamic_relations);
      let hir_bcls_for_mir_bcl = hir_body_item_to_mir_body_items(&mir_body_clauses[mir_body_clauses.len() - 1], dynamic_relations);
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
   fn hir_body_item_to_mir_body_items(hir_bitem : &IrBodyItem, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirBodyItem>{
      match hir_bitem{
         IrBodyItem::Clause(hir_bcl) => {
            let mut res = vec![];
            let versions = if dynamic_relations.contains(&hir_bcl.rel.relation) {vec![Total, Delta]} else {vec![Total]};
            for ver in versions.into_iter(){
               let mir_relation = MirRelation::from(hir_bcl.rel.clone(), ver);
               let mir_bcl = MirBodyClause{
                  rel: mir_relation,
                  args : hir_bcl.args.clone(),
                  rel_args_span: hir_bcl.rel_args_span,
                  args_span: hir_bcl.args_span,
                  cond_clauses: hir_bcl.cond_clauses.clone()
               };
               res.push(MirBodyItem::Clause(mir_bcl));
            }
            res
         },
         IrBodyItem::Cond(cl) => {
            vec![MirBodyItem::Cond(cl.clone())]
         }
         IrBodyItem::Generator(gen) => {
            vec![MirBodyItem::Generator(gen.clone())]
         }
      }
      
   }
   let mir_body_items = hir_body_items_to_mir_body_items_vec(&rule.body_items, dynamic_relations);

   let mir_body_items = mir_body_items.into_iter().filter(|bitems|{
      // if body clauses contain dynamic relations, they can't all be total
      let mut bcls = bitems.iter().filter_map(|bi| match bi {MirBodyItem::Clause(bcl) => Some(bcl), _ => None}); 
      let mut dynamic_bcls = bcls.filter(|bcl| dynamic_relations.contains(&bcl.rel.relation)).peekable();
      dynamic_bcls.peek().is_none() || !dynamic_bcls.all(|bcl| bcl.rel.version == Total)
   });
   
   mir_body_items.into_iter()
      .map(|bcls| MirRule {
         body_items: bcls,
         head_clause: rule.head_clauses.clone(),
         has_simple_join: rule.is_simple_join,
         reorderable: rule.is_simple_join
      }).collect()
}

fn compile_hir_rule_to_mir_rules(rule: &IrRule, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirRule>{
   if false && rule.is_simple_join {
      compile_hir_simple_join_rule_to_mir_rules(rule, dynamic_relations)
   } else {
      compile_hir_arbitrary_rule_to_mir_rules(rule, dynamic_relations)
   }
}