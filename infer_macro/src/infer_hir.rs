use std::{collections::{HashMap, HashSet}, ops::Index};

use itertools::Itertools;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::{Expr, Type};

use crate::{RelationIdentity, RuleNode, InferProgram, expr_to_ident, ir_name_for_rel_indices, utils::{into_set, tuple, tuple_type}};


pub(crate) struct InferIr {
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub rules: Vec<IrRule>
}

pub(crate) struct IrRule {
   pub head_clause: IrHeadClause,
   pub body_clauses: Vec<IrBodyClause>,
   pub when_clause: Option<Expr>,
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct IrHeadClause{
   pub rel : RelationIdentity,
   pub args : Vec<Expr>,
}

pub(crate) struct IrBodyClause {
   pub rel : IrRelation,
   pub args : Vec<Expr>,
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
      for bcl in ir_rule.body_clauses.iter(){
         let relation = &bcl.rel.relation;

         let entry = relations_ir_relations.entry(relation.clone()).or_insert_with(||HashSet::new());
         entry.insert(bcl.rel.clone());
      }
   }
   InferIr{
      rules: ir_rules,
      relations_ir_relations,
      relations_full_indices
   }
}

fn compile_rule_to_ir_rule(rule: &RuleNode, prog: &InferProgram) -> IrRule {
   let mut body_clauses = vec![];
   let mut grounded_vars = vec![];
   for bcl in rule.body_clauses.iter() {

      let mut indices = vec![];
      for (i,arg) in bcl.args.iter().enumerate() {
         if let Some(var) = expr_to_ident(arg) {
            if grounded_vars.contains(&var){
               indices.push(i);
            } else {
               grounded_vars.push(var);
            }
         }
      }
      let ir_name = ir_name_for_rel_indices(&bcl.rel, &indices);
      let relation = prog.relations.iter().filter(|r| r.name.to_string() == bcl.rel.to_string()).next().unwrap();
      
      let ir_rel = IrRelation{
         relation: relation.into(),
         indices,
         ir_name
      };
      let ir_bcl = IrBodyClause {
         args: bcl.args.iter().cloned().collect(),
         rel: ir_rel
      };
      body_clauses.push(ir_bcl);
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
      head_clause, body_clauses, when_clause: rule.when_clause.clone()
   }
}


pub(crate) fn compile_ir_rule(prog: &InferIr, rule: &IrRule, clause_ind: usize) -> proc_macro2::TokenStream {

   if false && clause_ind == 0 {
      quote! {
         // TODO
      }
   } else if clause_ind < rule.body_clauses.len() {

      let bclause = &rule.body_clauses[clause_ind];
      let bclause_rel = &bclause.rel;
      let bclause_rel_sel = &bclause_rel.ir_name;
      let bclause_rel_name = &bclause.rel.relation.name;
      let selected_args = &bclause.selected_args();

      let pre_clause_vars = rule.body_clauses.iter().take(clause_ind)
                                 .flat_map(|cl| cl.args.iter().filter_map(expr_to_ident))
                                 .collect::<Vec<_>>();
      
      let clause_vars = bclause.args.iter().enumerate()
                           .filter_map(|(i,v)| expr_to_ident(v).map(|v| (i, v)))
                           .collect::<Vec<_>>();
      
      let common_vars = clause_vars.iter().filter(|(i,v)| pre_clause_vars.contains(v)).collect::<Vec<_>>();
      let common_vars_no_indices = common_vars.iter().map(|(i,v)| v.clone()).collect::<Vec<_>>();
      
      let mut new_vars_assignmnets = vec![];
      for (i,var) in clause_vars.iter(){
         if common_vars_no_indices.contains(var) {continue;}
         let i_ind = syn::Index::from(*i);
         new_vars_assignmnets.push(quote! {let #var = row.#i_ind;});
      }

      let next_loop = compile_ir_rule(prog, rule, clause_ind + 1);
      let selected_args_tuple = tuple(&selected_args);
      quote! {
         let matching = self.#bclause_rel_sel.get( &#selected_args_tuple );
         if matching.is_some() {
            let matching = matching.unwrap();
            for &ind in matching.iter() {
               let row = &self.#bclause_rel_name[ind];
               #(#new_vars_assignmnets)*
               #next_loop
            }
         }
      }
   } else {
      let head_rel_name = &rule.head_clause.rel.name;
      let mut assignments = vec![];
      for (i, expr) in rule.head_clause.args.iter().enumerate(){
         let i_ind = syn::Index::from(i);
         assignments.push(quote! {
            #expr
         });
      }
      println!("rule head cl relation: {}", rule.head_clause.rel.name.to_string());
      let head_relation = &rule.head_clause.rel;
      let row_type = tuple_type(&head_relation.field_types);
      let cond = rule.when_clause.as_ref().map(|x| x.to_token_stream()).unwrap_or( quote! {true}).clone();

      let add_row = quote! {
         let new_row: #row_type = (#(#assignments),*) ;
         new_db.#head_rel_name.push(new_row);
      };
      if let Some(ref when_clause) = rule.when_clause{
         quote! {
            if #when_clause {
               #add_row
            }
         }
      } else {
         add_row
      }
   }

}


pub(crate) fn dl_impl_hir_to_code(input: proc_macro2::TokenStream) -> syn::Result<proc_macro2::TokenStream>{

   let prog: InferProgram = syn::parse2(input)?;
   // let prog = InferProgram::parse(input.into());// parse_macro_input!(input as InferProgram);
   
   println!("prog relations: {}", prog.relations.len());
   let ir = compile_infer_program_to_hir(&prog);
   println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());

   println!("relations: {}", ir.relations_ir_relations.keys().map(|r| &r.name).join(", "));
   let mut relation_fields = vec![];
   
   for (rel, rel_indices) in ir.relations_ir_relations.iter(){
      let name = &rel.name;
      let field_types = tuple_type(&rel.field_types);
      relation_fields.push(quote! {
         pub #name : Vec<#field_types>,
      });
      for ind in rel_indices.iter(){
         let name = ir_name_for_rel_indices(&rel.name, &ind.indices);
         let index_type: Vec<Type> = ind.indices.iter().map(|&i| rel.field_types[i].clone()).collect();
         let index_type = tuple_type(&index_type);
         relation_fields.push(quote!{
            pub #name: std::collections::HashMap< #index_type , std::collections::HashSet<usize>>,
         });
      }
   }
   let mut compiled_rules = vec![];
   for rule in ir.rules.iter() {
      compiled_rules.push(compile_ir_rule(&ir, rule, 0));
   }

   let mut merge_rels = vec![];
   for rel in prog.relations.iter(){
      let name = &rel.name;
      merge_rels.push(quote! {
         Self::extend_no_dup(&mut self.#name, & other.#name);
      });
   }
   let res = quote! {
      #[derive(Default)]
      struct DLProgram {
         #(#relation_fields)*
      }

      impl DLProgram {
         pub fn run(&mut self) {
            loop {
               let mut new_db = DLProgram::default();
               #(#compiled_rules)*
               if !self.merge(&new_db) {break;}
            }
         }

         fn merge(&mut self, other: &Self) -> bool{
            let changed = false;
            #(#merge_rels)*
            changed
         }

         fn extend_no_dup<T : Clone + Eq>(vec1 : &mut Vec<T>, vec2: & Vec<T>) -> bool {
            let mut res = false;
            for item in vec2.iter(){
               if !vec1.contains(item) {
                  vec1.push(item.clone());
                  res = true;
               }
            }
            res
         }
      }
   };
   println!("res:\n {}", res);

   Ok(res)
   // TokenStream::from(res)
}
