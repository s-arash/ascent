use std::fmt::Display;

use itertools::{Iterate, Itertools};
use proc_macro2::Ident;
use syn::{Expr, Type, parse2};

use crate::{expr_to_ident, infer_mir::{InferMir, MirBodyItem, MirRelationVersion, MirRule, MirScc, ir_relation_version_var_name}, pat_to_ident, utils::{exp_cloned, tuple, tuple_type}};
use crate::infer_mir::MirRelationVersion::*;

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
   for (i, scc) in sccs_ordered.iter().enumerate() {
      let msg = format!("scc {}", i);
      sccs_compiled.push(quote!{
         comment(#msg);
      });
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
      #[allow(non_snake_case)]
      impl DLProgram {
         #[allow(non_snake_case)]
         pub fn run(&mut self) {
            #(#sccs_compiled)*
         }
         #[allow(non_snake_case)]
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
   fn bitem_to_str(bitem: &MirBodyItem) -> String {
      match bitem {
         MirBodyItem::Clause(bcl) => format!("{}_{}", bcl.rel.ir_name, bcl.rel.version.to_string()),
         MirBodyItem::Generator(gen) => format!("for_{}", pat_to_ident(&gen.pattern).map(|x| x.to_string()).unwrap_or_default()),
      }
   }
   for rule in scc.rules.iter() {
      let comment = format!("{} <-- {}",
                             rule.head_clause.rel.name.to_string(),
                             rule.body_items.iter().map(bitem_to_str).join(", "));
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

   if clause_ind < rule.body_items.len(){
      let bitem = &rule.body_items[clause_ind];
      let next_loop = compile_mir_rule(rule, scc, mir, clause_ind + 1);

      match bitem {
         MirBodyItem::Clause(bclause) => {
            let bclause_rel = &bclause.rel;
            let bclause_rel_sel = &bclause_rel.ir_name;
            let bclause_rel_name = &bclause.rel.relation.name;
            let selected_args = &bclause.selected_args();

            fn bitem_vars(bitem : &MirBodyItem) -> Vec<Ident> {
               match bitem {
                  MirBodyItem::Clause(cl) => cl.args.iter().filter_map(expr_to_ident).collect(),
                  MirBodyItem::Generator(gen) => pat_to_ident(&gen.pattern).into_iter().collect()
               }
            }
            let pre_clause_vars = rule.body_items.iter().take(clause_ind)
                                       .flat_map(bitem_vars)
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

            let selected_args_cloned = selected_args.iter().map(exp_cloned).collect_vec();
            let selected_args_tuple = tuple(&selected_args_cloned);
            let rel_version_var_name = bclause.rel.var_name();
            
            let mut conds_then_next_loop = next_loop;
            for cond in bclause.cond_clauses.iter().rev() {
               match cond {
                  crate::CondClause::IfLet(if_let_clause) => {
                     let pat = &if_let_clause.pattern;
                     let expr = &if_let_clause.exp;
                     conds_then_next_loop = quote! {
                       if let #pat = #expr {
                          #conds_then_next_loop
                       }
                     }
                  }
                  crate::CondClause::If(if_clause) => {
                     let cond = &if_clause.cond;
                     conds_then_next_loop = quote! {
                        if #cond {
                           #conds_then_next_loop
                        }
                     }
                  }
               }
            }

            quote! {
               if let Some(matching) = #rel_version_var_name.get( &#selected_args_tuple) {
                  for &ind in matching.iter() {
                     // TODO we may be doing excessive cloning
                     let row = &self.#bclause_rel_name[ind].clone();
                     #(#new_vars_assignmnets)*
                     #conds_then_next_loop
                  }
               }
            }
         },
         MirBodyItem::Generator(gen) => {
            let pat = &gen.pattern;
            let expr = &gen.expr;
            quote! {
               for #pat in #expr {
                  #next_loop
               }
            }
         }
      }
      
   } else {
      let head_rel_name = &rule.head_clause.rel.name;
      let new_row_tuple = tuple(&rule.head_clause.args);
      
      // println!("rule head cl relation: {}", rule.head_clause.rel.name.to_string());
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
         if !(#head_rel_full_index_var_name_new.contains_key(&new_row) ||
            #head_rel_full_index_var_name_delta.contains_key(&new_row) ||
            #head_rel_full_index_var_name_total.contains_key(&new_row))
         {
            let new_row_ind = self.#head_rel_name.len();
            #(#update_indices)*
            self.#head_rel_name.push(new_row);
            changed = true;
         }
      };
      add_row
   }
}


fn rel_index_type(key_type: &Type) -> Type {
   let ty = quote! {
      HashMap<#key_type, HashSet<usize>>
   };
   syn::parse2(ty).unwrap()
}

