use std::{collections::{HashMap, HashSet}, ops::Index};

use itertools::Itertools;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::{Error, Expr, Type};

use crate::{InferProgram, utils::{expr_to_ident, into_set, pattern_get_vars, tuple, tuple_type}};
use crate::infer_syntax::{BodyClauseArg, BodyItemNode, CondClause, GeneratorNode, IfLetClause, RelationIdentity, RuleNode};

pub(crate) struct InferIr {
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub rules: Vec<IrRule>
}

pub(crate) struct IrRule {
   pub head_clauses: Vec<IrHeadClause>,
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

pub(crate) fn compile_infer_program_to_hir(prog: &InferProgram) -> syn::Result<InferIr>{
   let ir_rules : Vec<IrRule> = prog.rules.iter().map(|r| compile_rule_to_ir_rule(r, prog)).try_collect()?;
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
   Ok(InferIr{
      rules: ir_rules,
      relations_ir_relations,
      relations_full_indices
   })
}

fn compile_rule_to_ir_rule(rule: &RuleNode, prog: &InferProgram) -> syn::Result<IrRule> {
   let mut body_items = vec![];
   let mut grounded_vars = vec![];
   for bitem in rule.body_items.iter() {
      match bitem {
         BodyItemNode::Clause(ref bcl) => {
            let mut indices = vec![];
            for (i,arg) in bcl.args.iter().enumerate() {
               if let Some(var) = expr_to_ident(arg.unwrap_expr_ref()) {
                  if grounded_vars.contains(&var){
                     indices.push(i);
                  } else {
                     grounded_vars.push(var);
                  }
               } else {
                  indices.push(i);
               }
            }
            let ir_name = ir_name_for_rel_indices(&bcl.rel, &indices);
            let relation = prog.relations.iter().filter(|r| r.name.to_string() == bcl.rel.to_string()).next();

            let relation = match relation {
                Some(rel) => rel,
                None => return Err(Error::new(bcl.rel.span(), format!("relation {} not defined", bcl.rel))),
            };

            for cond_clause in bcl.cond_clauses.iter() {
               if let CondClause::IfLet(if_let_cl) = &cond_clause {
                  let pat_vars = pattern_get_vars(&if_let_cl.pattern);
                  grounded_vars.extend(pat_vars);
               }
            }
            
            let ir_rel = IrRelation{
               relation: relation.into(),
               indices,
               ir_name
            };
            let ir_bcl = IrBodyClause {
               args: bcl.args.iter().cloned().map(BodyClauseArg::unwrap_expr).collect(),
               rel: ir_rel,
               cond_clauses: bcl.cond_clauses.clone()
            };
            body_items.push(IrBodyItem::Clause(ir_bcl));
         },
         BodyItemNode::Generator(ref gen) => {
            for ident in pattern_get_vars(&gen.pattern) {
               grounded_vars.push(ident);
            }
            body_items.push(IrBodyItem::Generator(gen.clone()));
         }
         _ => panic!("unrecognized body item")
      }
      
   }
   let mut head_clauses = vec![];
   for hcl_node in rule.head_clauses.iter(){
      let rel = prog.relations.iter().filter(|r| r.name.to_string() == hcl_node.rel.to_string()).next();
      let rel = match rel {
         Some(rel) => rel,
         None => return Err(Error::new(hcl_node.rel.span(), format!("relation {} not defined", hcl_node.rel))),
      };
      
      let rel = RelationIdentity::from(rel);
      let head_clause = IrHeadClause {
         rel,
         args : hcl_node.args.iter().cloned().collect()
      };
      head_clauses.push(head_clause);
   }

   Ok(IrRule {
      head_clauses: head_clauses, body_items
   })
}

fn ir_name_for_rel_indices(rel: &Ident, indices: &[usize]) -> Ident {
   let name = format!("{}_indices_{}", rel, indices.iter().join("_"));
   Ident::new(&name, rel.span())
}