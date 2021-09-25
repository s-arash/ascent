use std::{collections::{HashMap, HashSet}, ops::Index};

use itertools::Itertools;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::{Expr, Type};

use crate::{BodyItemNode, CondClause, GeneratorNode, InferProgram, RelationIdentity, RuleNode, expr_to_ident, ir_name_for_rel_indices, pat_to_ident, utils::{into_set, tuple, tuple_type}};


pub(crate) struct InferIr {
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub rules: Vec<IrRule>
}

pub(crate) struct IrRule {
   pub head_clause: IrHeadClause,
   pub body_items: Vec<IrBodyItem>,
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct IrHeadClause{
   pub rel : RelationIdentity,
   pub args : Vec<Expr>,
}

pub(crate) enum IrBodyItem {
   Clause(IrBodyClause),
   Generator(GeneratorNode)
}

pub(crate) struct IrBodyClause {
   pub rel : IrRelation,
   pub args : Vec<Expr>,
   pub cond_clauses : Vec<CondClause>
}

impl IrBodyClause {
   pub fn selected_args(&self) -> Vec<Expr> {
      self.rel.indices.iter().map(|&i| self.args[i].clone()).collect()
   }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub(crate) struct IrRelation {
   pub relation: RelationIdentity,
   pub indices: Vec<usize>,
   pub ir_name: Ident,
}

impl IrRelation {
   pub fn key_type(&self) -> Type {
      let index_types : Vec<_> = self.indices.iter().map(|&i| self.relation.field_types[i].clone()).collect();
      tuple_type(&index_types)
   }
}

pub(crate) fn compile_infer_program_to_hir(prog: &InferProgram) -> InferIr{
   let ir_rules : Vec<IrRule> = prog.rules.iter().map(|r| compile_rule_to_ir_rule(r, prog)).collect();
   let mut relations_ir_relations = HashMap::new();
   let mut relations_full_indices = HashMap::new();
   for rel in prog.relations.iter(){
      let rel_identity = RelationIdentity::from(rel);

      let full_indices = (0 .. rel_identity.field_types.len()).collect_vec();
      let ir_name = ir_name_for_rel_indices(&rel_identity.name, &full_indices);
      let rel_full_index = IrRelation{
         relation: rel_identity.clone(),
         indices: full_indices,
         ir_name: ir_name
      };
      relations_ir_relations.insert(rel_identity.clone(), into_set([rel_full_index.clone()]));

      relations_full_indices.insert(rel_identity, rel_full_index);
   }
   for ir_rule in ir_rules.iter(){
      for bcl in ir_rule.body_items.iter(){
         if let IrBodyItem::Clause(ref bcl) = bcl {
            let relation = &bcl.rel.relation;
            
            let entry = relations_ir_relations.entry(relation.clone()).or_insert_with(||HashSet::new());
            entry.insert(bcl.rel.clone());
         }
      }
   }
   InferIr{
      rules: ir_rules,
      relations_ir_relations,
      relations_full_indices
   }
}

fn compile_rule_to_ir_rule(rule: &RuleNode, prog: &InferProgram) -> IrRule {
   let mut body_items = vec![];
   let mut grounded_vars = vec![];
   for bitem in rule.body_items.iter() {
      match bitem {
         BodyItemNode::Clause(ref bcl) => {
            let mut indices = vec![];
            for (i,arg) in bcl.args.iter().enumerate() {
               if let Some(var) = expr_to_ident(arg) {
                  if grounded_vars.contains(&var){
                     // TODO how is this not a bug?
                     indices.push(i);
                  } else {
                     grounded_vars.push(var);
                  }
               } else {
                  indices.push(i);
               }
            }
            let ir_name = ir_name_for_rel_indices(&bcl.rel, &indices);
            // TODO compile error if the relation does not exist
            let relation = prog.relations.iter().filter(|r| r.name.to_string() == bcl.rel.to_string()).next().unwrap();

            
            let ir_rel = IrRelation{
               relation: relation.into(),
               indices,
               ir_name
            };
            let ir_bcl = IrBodyClause {
               args: bcl.args.iter().cloned().collect(),
               rel: ir_rel,
               cond_clauses: bcl.cond_clauses.clone()
            };
            body_items.push(IrBodyItem::Clause(ir_bcl));
         },
         BodyItemNode::Generator(ref gen) => {
            if let Some(ident) = pat_to_ident(&gen.pattern) {
               grounded_vars.push(ident);
            }
            body_items.push(IrBodyItem::Generator(gen.clone()));
         }
      }
      
   }
   let rel = prog.relations.iter().filter(|r| r.name.to_string() == rule.head_clause.rel.to_string()).next();
   if rel.is_none() {
      // TODO do better error handling
      panic!("relation {} not defined!", rule.head_clause.rel);
   }
   let rel = RelationIdentity::from(rel.unwrap());
   let head_clause = IrHeadClause {
      rel,
      args : rule.head_clause.args.iter().cloned().collect()
   };
   IrRule {
      head_clause, body_items
   }
}
