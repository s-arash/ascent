use std::{collections::{HashMap, HashSet}, ops::Index};

use itertools::Itertools;
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{Error, Expr, Type, parse2, spanned::Spanned};

use crate::{InferProgram, infer_syntax::Declaration, utils::{expr_to_ident, into_set, pattern_get_vars, tuple, tuple_type}};
use crate::infer_syntax::{BodyClauseArg, BodyItemNode, CondClause, GeneratorNode, IfLetClause, RelationIdentity, RuleNode};

pub(crate) struct InferIr {
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub lattices_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub rules: Vec<IrRule>,
   pub declaration: Declaration,
}

pub(crate) struct IrRule {
   pub head_clauses: Vec<IrHeadClause>,
   pub body_items: Vec<IrBodyItem>,
   pub is_simple_join: bool,
}

#[derive(Clone)]
pub(crate) struct IrHeadClause{
   pub rel : RelationIdentity,
   pub args : Vec<Expr>,
   pub span: Span,
   pub args_span: Span,
}

pub(crate) enum IrBodyItem {
   Clause(IrBodyClause),
   Generator(GeneratorNode),
   Cond(CondClause)
}

#[derive(Clone)]
pub(crate) struct IrBodyClause {
   pub rel : IrRelation,
   pub args : Vec<Expr>,
   pub rel_args_span: Span,
   pub args_span: Span,
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
}

impl IrRelation {
   pub fn key_type(&self) -> Type {
      let index_types : Vec<_> = self.indices.iter().map(|&i| self.relation.field_types[i].clone()).collect();
      tuple_type(&index_types)
   }
   pub fn ir_name(&self) -> Ident {
      ir_name_for_rel_indices(&self.relation.name, &self.indices)
   }
}

pub(crate) fn compile_infer_program_to_hir(prog: &InferProgram) -> syn::Result<InferIr>{
   let ir_rules : Vec<(IrRule, Vec<IrRelation>)> = prog.rules.iter().map(|r| compile_rule_to_ir_rule(r, prog)).try_collect()?;
   let mut relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::new();
   let mut relations_full_indices = HashMap::new();
   let mut lattices_full_indices = HashMap::new();
   for rel in prog.relations.iter(){
      let rel_identity = RelationIdentity::from(rel);

      if rel.is_lattice {
         let indices = (0 .. rel_identity.field_types.len() - 1).collect_vec();
         let ir_name = ir_name_for_rel_indices(&rel_identity.name, &indices);
         let lat_full_index = IrRelation{
            relation: rel_identity.clone(),
            indices: indices,
         };
         relations_ir_relations.entry(rel_identity.clone()).or_default().insert(lat_full_index.clone());
         lattices_full_indices.insert(rel_identity.clone(), lat_full_index);
      }

      let full_indices = (0 .. rel_identity.field_types.len()).collect_vec();
      let ir_name = ir_name_for_rel_indices(&rel_identity.name, &full_indices);
      let rel_full_index = IrRelation{
         relation: rel_identity.clone(),
         indices: full_indices,
      };
      relations_ir_relations.entry(rel_identity.clone()).or_default().insert(rel_full_index.clone());
      relations_full_indices.insert(rel_identity, rel_full_index);
   }
   for (ir_rule, extra_relations) in ir_rules.iter(){
      for bcl in ir_rule.body_items.iter(){
         if let IrBodyItem::Clause(ref bcl) = bcl {
            let relation = &bcl.rel.relation;
            relations_ir_relations.entry(relation.clone()).or_default().insert(bcl.rel.clone());
         }
      }
      for extra_rel in extra_relations.iter(){
         relations_ir_relations.entry(extra_rel.relation.clone()).or_default().insert(extra_rel.clone());
      }
   }
   let declaration = prog.declaration.clone().unwrap_or(parse2(quote! {pub struct InferProgram;}).unwrap());
   Ok(InferIr{
      rules: ir_rules.into_iter().map(|(rule, extra_rels)| rule).collect_vec(),
      relations_ir_relations,
      relations_full_indices,
      lattices_full_indices,
      declaration
   })
}

fn compile_rule_to_ir_rule(rule: &RuleNode, prog: &InferProgram) -> syn::Result<(IrRule, Vec<IrRelation>)> {
   let mut body_items = vec![];
   let mut grounded_vars = vec![];
   let mut all_args_vars = true;
   let mut all_items_simple_clauses = true;
   for (bitem_ind, bitem) in rule.body_items.iter().enumerate() {
      match bitem {
         BodyItemNode::Clause(ref bcl) => {
            if (bcl.cond_clauses.len() > 0 && bitem_ind > 0) || 
               bitem_ind > 1 
            {
               all_items_simple_clauses = false;
            }
            let mut indices = vec![];
            for (i,arg) in bcl.args.iter().enumerate() {
               if let Some(var) = expr_to_ident(arg.unwrap_expr_ref()) {
                  if grounded_vars.contains(&var){
                     indices.push(i);
                  } else {
                     grounded_vars.push(var);
                  }
               } else {
                  all_args_vars = false;
                  indices.push(i);
               }
            }
            let ir_name = ir_name_for_rel_indices(&bcl.rel, &indices);
            let relation = prog.relations.iter().filter(|r| r.name.to_string() == bcl.rel.to_string()).next();
            
            let relation = match relation {
                Some(rel) => rel,
                None => return Err(Error::new(bcl.rel.span(), format!("relation {} not defined", bcl.rel))),
            };
            if relation.is_lattice {
               all_items_simple_clauses = false;
            }

            for cond_clause in bcl.cond_clauses.iter() {
               if let CondClause::IfLet(if_let_cl) = &cond_clause {
                  grounded_vars.extend(pattern_get_vars(&if_let_cl.pattern));
               }
            }
            
            let ir_rel = IrRelation{
               relation: relation.into(),
               indices,
            };
            let ir_bcl = IrBodyClause {
               rel: ir_rel,
               args: bcl.args.iter().cloned().map(BodyClauseArg::unwrap_expr).collect(),
               rel_args_span: bcl.rel.span().join(bcl.args.span()).unwrap_or(bcl.rel.span()),
               args_span: bcl.args.span(),
               cond_clauses: bcl.cond_clauses.clone()
            };
            body_items.push(IrBodyItem::Clause(ir_bcl));
         },
         BodyItemNode::Generator(ref gen) => {
            all_items_simple_clauses = false;
            grounded_vars.extend(pattern_get_vars(&gen.pattern));
            body_items.push(IrBodyItem::Generator(gen.clone()));
         },
         &BodyItemNode::Cond(ref cl) => {
            body_items.push(IrBodyItem::Cond(cl.clone()));
            if bitem_ind <= 1 {
               all_items_simple_clauses = false;
            }
            if let CondClause::IfLet(if_let_cl) = cl {
               grounded_vars.extend(pattern_get_vars(&if_let_cl.pattern));
            }
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
         args : hcl_node.args.iter().cloned().collect(),
         span: hcl_node.span(),
         args_span: hcl_node.args.span()
      };
      head_clauses.push(head_clause);
   }
   let is_simple_join = all_args_vars && all_items_simple_clauses && body_items.len() >= 2; 
   let extra_ir_relations = if is_simple_join {
      let (bcl1, bcl2) = match (&body_items[0], &body_items[1]) {
         (IrBodyItem::Clause(bcl1), IrBodyItem::Clause(bcl2)) => (bcl1, bcl2),
          _ => unreachable!()
      };  
      let bcl2_vars = bcl2.args.iter().filter_map(expr_to_ident).collect_vec();
      let indices = get_indices_given_grounded_variables(&bcl1.args, &bcl2_vars);
      let new_cl1_ir_relation = IrRelation{
         // ir_name: ir_name_for_rel_indices(&bcl1.rel.relation.name, &indices),
         indices,
         relation: bcl1.rel.relation.clone(),
      };
      let new_cl2_ir_relation = IrRelation {
         // ir_name: ir_name_for_rel_indices(&bcl2.rel.relation.name, &[]),
         indices: vec![],
         relation: bcl2.rel.relation.clone(),
      };
      vec![new_cl1_ir_relation, new_cl2_ir_relation]
   } else {vec![]};
   Ok((IrRule {
      is_simple_join,
      head_clauses, 
      body_items, 
   }, extra_ir_relations))
}

pub fn ir_name_for_rel_indices(rel: &Ident, indices: &[usize]) -> Ident {
   let name = format!("{}_indices_{}", rel, indices.iter().join("_"));
   Ident::new(&name, rel.span())
}

/// for a clause with args, returns the indices assuming vars are grounded.
pub fn get_indices_given_grounded_variables(args: &[Expr], vars: &[Ident]) -> Vec<usize>{
   let mut res = vec![];
   for (i, arg) in args.iter().enumerate(){
      if let Some(arg_var) = expr_to_ident(arg){
         if vars.contains(&arg_var) {
            res.push(i);
         }
      } else {
         res.push(i);
      }  
   }
   res
}