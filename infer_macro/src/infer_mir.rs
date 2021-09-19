use std::{collections::{HashMap, HashSet}, fmt::Display, iter::FromIterator};

use itertools::Itertools;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::{Expr, Type, parse2};
use crate::{expr_to_ident, utils::{exp_cloned, tuple, tuple_type}, infer_mir::MirRelationVersion::*};

use crate::{RelationIdentity, infer_hir::{IrBodyClause, IrHeadClause, IrRelation, IrRule, InferIr}};

pub(crate) struct InferMir {
   sccs: Vec<MirScc>,
   deps: HashMap<usize, HashSet<usize>>,
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>
}

pub(crate) struct MirScc {
   rules: Vec<MirRule>,
   dynamic_relation_indices: HashSet<IrRelation>,
   dynamic_relations: HashMap<RelationIdentity, HashSet<IrRelation>> 
}

struct MirRule {
   head_clause: IrHeadClause,
   body_clauses: Vec<MirBodyClause>,
   when_clause: Option<Expr>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct MirBodyClause {
   rel: MirRelation,
   args: Vec<Expr>,
}
impl MirBodyClause {
   fn selected_args(&self) -> Vec<Expr> {
      self.rel.indices.iter().map(|&i| self.args[i].clone()).collect()
   }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct MirRelation {
   relation: RelationIdentity,
   indices: Vec<usize>,
   ir_name: Ident,
   version: MirRelationVersion,
}

fn ir_relation_version_var_name(ir_name: &Ident, version : MirRelationVersion) -> Ident{
   let name = format!("{}_{}", ir_name, version.to_string());
   Ident::new(&name, ir_name.span())
}

impl MirRelation {
   fn var_name(&self) -> Ident {
      ir_relation_version_var_name(&self.ir_name, self.version)
   }

   // TODO this copying is not ideal
   fn key_type(&self) -> Type {
      let index_types : Vec<_> = self.indices.iter().map(|&i| self.relation.field_types[i].clone()).collect();
      tuple_type(&index_types)
   }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum MirRelationVersion {
   Total,
   Delta,
   New,
}

impl MirRelationVersion {
   fn to_string(&self) -> &'static str{
      match self { Self::Delta => "delta", Self::Total => "total", Self::New => "new" }
   }
}

pub(crate) fn compile_hir_to_mir(hir: &InferIr) -> InferMir{

   // TODO actually compute the dependency graph and the sccs
   let mut dynamic_relation_indices = HashSet::new();
   let mut dynamic_relations = HashMap::new();
   let mut dynamic_relations_set = HashSet::new();
   for (rel, indices) in hir.relations_ir_relations.iter() {
      let indices_set: HashSet<_> = indices.iter().cloned().collect();
      for rel_ind in indices {
         dynamic_relation_indices.insert(rel_ind.clone());
      }
      dynamic_relations.insert(rel.clone(), indices_set);
      dynamic_relations_set.insert(rel.clone());
   }
   let rules: Vec<_> = hir.rules.iter().flat_map(|r| compile_hir_rule_to_mir_rules(r, &dynamic_relations_set)).collect();
   let scc = MirScc{
      rules,
      dynamic_relations,
      dynamic_relation_indices
   };
   InferMir {
      sccs: vec![scc],
      deps: HashMap::from_iter([(0, HashSet::new())]),
      relations_ir_relations: hir.relations_ir_relations.clone(),
      relations_full_indices: hir.relations_full_indices.clone(),
   }
}

fn compile_hir_rule_to_mir_rules(rule: &IrRule, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirRule>{
   
   fn hir_body_clauses_to_mir_body_clauses_vec(mir_body_clauses: &[IrBodyClause], dynamic_relations: &HashSet<RelationIdentity>) -> Vec<Vec<MirBodyClause>> {
      if mir_body_clauses.len() == 0 { return vec![vec![]];}
      let mut pre_res = hir_body_clauses_to_mir_body_clauses_vec(&mir_body_clauses[0..(mir_body_clauses.len() - 1)], dynamic_relations);
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
   fn hir_body_clause_to_mir_body_clauses(hir_bcl : &IrBodyClause, dynamic_relations: &HashSet<RelationIdentity>) -> Vec<MirBodyClause>{
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
            args : hir_bcl.args.clone()
         };
         res.push(mir_bcl);
      }
      res
   }
   let mir_body_clauses = hir_body_clauses_to_mir_body_clauses_vec(&rule.body_clauses, dynamic_relations);

   mir_body_clauses.into_iter()
      .map(|bcls| MirRule {
         body_clauses: bcls,
         head_clause: rule.head_clause.clone(),
         when_clause: rule.when_clause.clone(),
      }).collect()
}

pub(crate) fn compile_mir(mir: &InferMir) -> proc_macro2::TokenStream {
   
   let mut relation_fields = vec![];

   for (rel, rel_indices) in mir.relations_ir_relations.iter(){
      let name = &rel.name;
      let field_types = tuple_type(&rel.field_types);
      relation_fields.push(quote! {
         pub #name : Vec<#field_types>,
      });
      for ind in rel_indices.iter(){
         let name = &ind.ir_name;
         let index_type: Vec<Type> = ind.indices.iter().map(|&i| rel.field_types[i].clone()).collect();
         let index_type = tuple_type(&index_type);
         relation_fields.push(quote!{
            pub #name: std::collections::HashMap< #index_type , std::collections::HashSet<usize>>,
         });
      }
   }
   
   let sccs_ordered = &mir.sccs;
   let mut sccs_compiled = vec![];
   for scc in sccs_ordered {
      sccs_compiled.push(compile_mir_scc(scc, mir));
   }

   let update_indices_body = compile_update_indices_function_body(mir);
   quote! {
      use std::collections::{HashMap, HashSet};
      fn move_index_contents<K, V>(hm1: &mut HashMap<K, HashSet<V>>, hm2: &mut HashMap<K, HashSet<V>>) 
         where K : Eq + std::hash::Hash, V: Eq + std::hash::Hash {
         for (k,v) in hm1.drain(){
            let set = hm2.entry(k).or_default();
            set.extend(v);
         }
      }
      fn comment(_: &str){}
      #[derive(Default)]
      pub struct DLProgram {
         #(#relation_fields)*
      }

      impl DLProgram {
         pub fn run(&mut self) {
            #(#sccs_compiled)*
         }
         pub fn update_indices(&mut self) {
            #update_indices_body
         }
      }
   }
}

fn compile_mir_scc(scc: &MirScc, mir: &InferMir) -> proc_macro2::TokenStream {

   let mut move_total_to_delta = vec![];
   let mut shift_delta_to_total_new_to_delta = vec![];
   let mut move_total_to_field = vec![];

   for rel in scc.dynamic_relation_indices.iter() {
      let delta_var_name = ir_relation_version_var_name(&rel.ir_name, MirRelationVersion::Delta);
      let total_var_name = ir_relation_version_var_name(&rel.ir_name, MirRelationVersion::Total);
      let new_var_name = ir_relation_version_var_name(&rel.ir_name, MirRelationVersion::New);
      let total_field = &rel.ir_name;
      let ty = rel_index_type(&rel.key_type());
      move_total_to_delta.push(quote! {
         let #delta_var_name : &mut #ty = &mut self.#total_field;
         let mut #total_var_name : #ty = HashMap::default();
         let mut #new_var_name : #ty = HashMap::default();
      });

      shift_delta_to_total_new_to_delta.push(quote!{
         move_index_contents(#delta_var_name, &mut #total_var_name);
         #delta_var_name.clear();
         std::mem::swap(&mut #new_var_name, #delta_var_name);
      });

      move_total_to_field.push(quote!{
         self.#total_field = #total_var_name;
      });
   }
   
   let mut evaluate_rules = vec![];
   for rule in scc.rules.iter() {
      let comment = format!("{} <-- {}",
                             rule.head_clause.rel.name.to_string(),
                             rule.body_clauses.iter().map(|bcl| format!("{}_{}", bcl.rel.ir_name, bcl.rel.version.to_string())).join(", "));
      evaluate_rules.push(quote! {
         comment(#comment);
      });
      evaluate_rules.push(compile_mir_rule(rule, scc, mir, 0));
   }

   quote! {
      // define variables for delta and new versions of dynamic relations in the scc
      // move total versions of dynamic indices to delta
      #(#move_total_to_delta)*

      loop {
         let mut changed = false;
         // evaluate rules
         #(#evaluate_rules)*

         // for dynamic indices in the scc:
         //    append delta to total.
         //    move new to delta
         #(#shift_delta_to_total_new_to_delta)*
         if !changed {break;}
      }

      #(#move_total_to_field)*
   }
}

fn compile_update_indices_function_body(mir: &InferMir) -> proc_macro2::TokenStream {
   
   let mut res = vec![];
   for (r,indices_set) in mir.relations_ir_relations.iter(){

      let mut update_indices = vec![];
      for ind in indices_set.iter(){
         let ind_name = &ind.ir_name;
         let selection_tuple : Vec<Expr> = ind.indices.iter().map(|&i| {
            let ind = syn::Index::from(i); 
            parse2(quote! { tuple.#ind.clone()}).unwrap()
         }).collect_vec();
         let selection_tuple = tuple(&selection_tuple);
         update_indices.push(quote! {
            let selection_tuple = #selection_tuple;
            self.#ind_name.entry(selection_tuple).or_default().insert(i);
         });

      }
      let rel_name = &r.name;
      res.push(quote! {
         for (i, tuple) in self.#rel_name.iter().enumerate() {
            #(#update_indices)*
         }
      });
   }

   quote! {#(#res)*}
}

fn compile_mir_rule(rule: &MirRule, scc: &MirScc, mir: &InferMir, clause_ind: usize) -> proc_macro2::TokenStream {

   if clause_ind < rule.body_clauses.len(){

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
         new_vars_assignmnets.push(quote! {let #var = &row.#i_ind;});
      }

      let next_loop = compile_mir_rule(rule, scc, mir, clause_ind + 1);
      let selected_args_cloned = selected_args.iter().map(exp_cloned).collect_vec();
      let selected_args_tuple = tuple(&selected_args_cloned);
      let rel_version_var_name = bclause.rel.var_name();
      quote! {
         let matching = #rel_version_var_name.get( &#selected_args_tuple );
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
      let new_row_tuple = tuple(&rule.head_clause.args);
      
      println!("rule head cl relation: {}", rule.head_clause.rel.name.to_string());
      let head_relation = &rule.head_clause.rel;
      // let head_relation = prog.relations.keys().filter(|v| v.name.to_string() == rule.head_clause.rel.to_string()).next().unwrap();
      let row_type = tuple_type(&head_relation.field_types);

      let mut update_indices = vec![];
      let rel_indices = scc.dynamic_relations.get(head_relation);
      if let Some(rel_indices) = rel_indices {
         for rel_ind in rel_indices.iter(){
            let var_name = ir_relation_version_var_name(&rel_ind.ir_name, New);
            // TODO args must be cloned here
            let args_tuple : Vec<Expr> = rel_ind.indices.iter().map(|&i| {
               let i_ind = syn::Index::from(i);
               syn::parse2(quote!{ new_row.#i_ind.clone()}).unwrap()
            }).collect();
            // let args_tuple : Vec<_> = rel_ind.indices.iter().map(|&i| rule.head_clause.args[i].clone()).collect();
            let args_tuple = tuple(&args_tuple);
            update_indices.push(quote! {
               #var_name.entry(#args_tuple).or_default().insert(new_row_ind);
            });
         }
      }

      let head_rel_full_index = &mir.relations_full_indices[head_relation];
      let head_rel_full_index_var_name_new = ir_relation_version_var_name(&head_rel_full_index.ir_name, New);
      let head_rel_full_index_var_name_delta = ir_relation_version_var_name(&head_rel_full_index.ir_name, Delta);
      let head_rel_full_index_var_name_total = ir_relation_version_var_name(&head_rel_full_index.ir_name, Total);

      let add_row = quote! {
         // comment("check if the tuple already exists");
         let new_row: #row_type = #new_row_tuple;
         if #head_rel_full_index_var_name_new.contains_key(&new_row) ||
            #head_rel_full_index_var_name_delta.contains_key(&new_row) ||
            #head_rel_full_index_var_name_total.contains_key(&new_row)
            {continue;}
         let new_row_ind = self.#head_rel_name.len();
         #(#update_indices)*
         self.#head_rel_name.push(new_row);
         changed = true;
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


fn rel_index_type(key_type: &Type) -> Type {
   let ty = quote! {
      HashMap<#key_type, HashSet<usize>>
   };
   syn::parse2(ty).unwrap()
}

struct Foo{
   a : Vec<String>,
   b : Vec<usize>,
}
fn test() -> Foo{

   let mut hm1 = HashMap::<usize, Vec<usize>>::new();
   let mut hm2 = HashMap::<usize, Vec<usize>>::new();

   hm1.clear();

   std::mem::swap(&mut hm1, &mut hm2);

   let mut foo = Foo{a: vec![], b: vec![]};
   let aprime = foo.a;
   let bprime = foo.b;
   foo.a = aprime;
   foo.b = bprime;
   foo
}