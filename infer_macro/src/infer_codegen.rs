use std::{collections::{BTreeSet, HashSet}, fmt::Display};

use itertools::{Iterate, Itertools};
use proc_macro2::{Ident, Span};
use syn::{Expr, Type, parse2, spanned::Spanned};

use crate::{infer_mir::{InferMir, MirBodyItem, MirRelationVersion, MirRule, MirScc, ir_relation_version_var_name, mir_summary}, infer_syntax::CondClause, utils::{exp_cloned, expr_to_ident, pat_to_ident, tuple, tuple_spanned, tuple_type}};
use crate::infer_mir::MirRelationVersion::*;

pub(crate) fn compile_mir(mir: &InferMir, is_infer_run: bool) -> proc_macro2::TokenStream {
   
   let mut relation_fields = vec![];
   let mut field_defaults = vec![];

   for (rel, rel_indices) in mir.relations_ir_relations.iter(){
      let name = &rel.name;
      let field_types = tuple_type(&rel.field_types);
      relation_fields.push(quote! {
         pub #name : Vec<#field_types>,
      });
      field_defaults.push(quote! {#name : Default::default(),});
      for ind in rel_indices.iter(){
         let name = &ind.ir_name();
         let index_type: Vec<Type> = ind.indices.iter().map(|&i| rel.field_types[i].clone()).collect();
         let index_type = tuple_type(&index_type);
         let rel_index_type = rel_index_type(&index_type, ind.relation.is_lattice);
         relation_fields.push(quote!{
            #[allow(non_snake_case)]
            pub #name: #rel_index_type,
         });
         field_defaults.push(quote! {#name : Default::default(),});
      }
   }

   let sccs_ordered = &mir.sccs;
   let mut scc_time_fields = vec![];
   let mut scc_time_field_defaults = vec![];
   for i in 0..mir.sccs.len(){
      let name = scc_time_field_name(i);
      scc_time_fields.push(quote!{
         pub #name: std::time::Duration,
      });
      scc_time_field_defaults.push(quote!{
         #name: std::time::Duration::ZERO,
      });
   }
   
   let mut sccs_compiled = vec![];
   for (i, scc) in sccs_ordered.iter().enumerate() {
      let msg = format!("scc {}", i);
      let scc_compiled = compile_mir_scc(scc, mir);
      let scc_time_field_name = scc_time_field_name(i);
      sccs_compiled.push(quote!{
         comment(#msg);
         let _scc_start_time = ::std::time::Instant::now();
         #scc_compiled
         _self.#scc_time_field_name += _scc_start_time.elapsed();
         // TODO remove this:
         // eprintln!("scc {} done.", #i);
      });
   }

   let update_indices_body = compile_update_indices_function_body(mir);
   let relation_sizes_body = compile_relation_sizes_body(mir);
   let scc_times_summary_body = compile_scc_times_summary_body(mir);

   let mut type_constaints = vec![];
   let mut field_type_names = HashSet::<String>::new();
   let mut lat_field_type_names = HashSet::<String>::new();

   
   /// for reference: 
   struct TypeConstraints where i32: ::core::clone::Clone + ::core::cmp::Eq + ::core::hash::Hash {}
   for relation in mir.relations_ir_relations.keys() {
      use crate::quote::ToTokens;
      for (i,field_type) in relation.field_types.iter().enumerate() {
         let is_lat = relation.is_lattice && i == relation.field_types.len() - 1;
         let add = if let Type::Path(path) = field_type {
            let container = if is_lat {&mut lat_field_type_names} else {&mut field_type_names};
            container.insert(path.path.clone().into_token_stream().to_string())
         } else {true}; 
         if add {
            let type_constaints_type = 
               if is_lat {quote_spanned!(field_type.span()=>LatTypeConstraints)} 
               else {quote_spanned!(field_type.span()=>TypeConstraints)}; 
            type_constaints.push(quote_spanned!{field_type.span()=>
               let _type_constraints : infer::internal::#type_constaints_type<#field_type>;
            });
         }
      }
   }

   let run_func = if is_infer_run {quote!{}} else {
      quote! {
         #[allow(unused_imports)]
         pub fn run(&mut self) {
            use core::cmp::PartialEq;
            let _self = self;
            #(#sccs_compiled)*
         }
      }
   };
   let run_code = if !is_infer_run {quote!{}} else {
      quote! {
         use core::cmp::PartialEq;
         let _self = &mut res;
         #(#sccs_compiled)*
      }
   };
   let summary = mir_summary(mir);
   let (impl_generics, ty_generics, where_clause) = mir.declaration.generics.split_for_impl();
   let vis = &mir.declaration.visibility;
   let struct_name = &mir.declaration.ident;
   let generics = &mir.declaration.generics;
   let summary_fn = if is_infer_run {
      quote! {
         pub fn summary(&self) -> &'static str {#summary}
      }
   } else { quote! {
      pub fn summary() -> &'static str {#summary}
   }};
   let res = quote! {
      use std::collections::{HashMap, HashSet};
      fn comment(_: &str){}
      #vis struct #struct_name #generics {
         #(#relation_fields)*
         #(#scc_time_fields)*
      }
      impl #impl_generics #struct_name #ty_generics #where_clause {
         #run_func
         pub fn update_indices(&mut self) {
            #update_indices_body
         }
         #[allow(unused_imports)]
         fn type_constaints() {
            #(#type_constaints)*
         }
         #summary_fn
         
         pub fn relation_sizes_summary(&self) -> String {
            #relation_sizes_body
         }
         pub fn scc_times_summary(&self) -> String {
            #scc_times_summary_body
         }
      }
      impl #impl_generics Default for #struct_name #ty_generics #where_clause {
         fn default() -> Self {
            #struct_name {
               #(#field_defaults)*
               #(#scc_time_field_defaults)*
            }
         }
      }
   };
   if !is_infer_run {res} else {
      quote! {
         {
            #res
            let mut res: #struct_name #ty_generics = #struct_name::default();
            #[allow(unused_imports)]
            {
               comment("running...");
               #run_code
            }
            res
         }
      }
   }
}

fn rel_index_type(key_type: &Type, is_lattice: bool) -> Type {
   let ty = if is_lattice {quote! {
      infer::internal::LatticeIndexType<#key_type>
   }} else { quote! {
      infer::internal::RelIndexType<#key_type>
   }};
   syn::parse2(ty).unwrap()
}

fn scc_time_field_name(i: usize) -> Ident {
   Ident::new(&format!("scc{}_duration", i), Span::call_site())
}

fn compile_mir_scc(scc: &MirScc, mir: &InferMir) -> proc_macro2::TokenStream {

   let mut move_total_to_delta = vec![];
   let mut shift_delta_to_total_new_to_delta = vec![];
   let mut move_total_to_field = vec![];

   for rel in scc.dynamic_relations.iter().flat_map(|(rel, indices)| indices.iter()) {
      let ir_name = rel.ir_name();
      let delta_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Delta);
      let total_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Total);
      let new_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::New);
      let total_field = &rel.ir_name();
      let ty = rel_index_type(&rel.key_type(), rel.relation.is_lattice);
      move_total_to_delta.push(quote! {
         #[allow(non_snake_case)]
         let #delta_var_name : &mut #ty = &mut _self.#total_field;
         #[allow(non_snake_case)]
         let mut #total_var_name : #ty = HashMap::default();
         #[allow(non_snake_case)]
         let mut #new_var_name : #ty = HashMap::default();
      });

      shift_delta_to_total_new_to_delta.push(quote_spanned!{rel.relation.name.span()=>
         infer::internal::RelIndexTrait::move_index_contents(#delta_var_name, &mut #total_var_name);
         #delta_var_name.clear();
         std::mem::swap(&mut #new_var_name, #delta_var_name);
      });

      move_total_to_field.push(quote_spanned!{rel.relation.name.span()=>
         _self.#total_field = #total_var_name;
      });
   }
   for rel in scc.body_only_relations.iter().flat_map(|(rel, indices)| indices.iter()) {
      let total_var_name = ir_relation_version_var_name(&rel.ir_name(), MirRelationVersion::Total);
      let ty = rel_index_type(&rel.key_type(), rel.relation.is_lattice);
      let total_field = &rel.ir_name();

      move_total_to_delta.push(quote! {
         #[allow(non_snake_case)]
         let #total_var_name : &mut #ty = &mut _self.#total_field;
      });
   }
   
   let mut evaluate_rules = vec![];
   fn bitem_to_str(bitem: &MirBodyItem) -> String {
      match bitem {
         MirBodyItem::Clause(bcl) => format!("{}_{}", bcl.rel.ir_name, bcl.rel.version.to_string()),
         MirBodyItem::Generator(gen) => format!("for_{}", pat_to_ident(&gen.pattern).map(|x| x.to_string()).unwrap_or_default()),
         MirBodyItem::Cond(_cl) => format!("if_")
      }
   }
   for rule in scc.rules.iter() {
      let msg = format!("{} <-- {}",
                             rule.head_clause.iter().map(|hcl| hcl.rel.name.to_string()).join(", "),
                             rule.body_items.iter().map(bitem_to_str).join(", "));
      evaluate_rules.push(quote! {
         comment(#msg);
      });
      evaluate_rules.push(compile_mir_rule(rule, scc, mir, 0));
   }

   let evaluate_rules_loop = if scc.is_looping { quote! {
      #[allow(unused_assignments, unused_variables)]
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
   }} else {quote! {
      #[allow(unused_assignments, unused_variables)]
      {
         let mut changed = false;
         #(#evaluate_rules)*
         #(#shift_delta_to_total_new_to_delta)*
         #(#shift_delta_to_total_new_to_delta)*
      }
   }};
   quote! {
      // define variables for delta and new versions of dynamic relations in the scc
      // move total versions of dynamic indices to delta
      #(#move_total_to_delta)*

      #evaluate_rules_loop

      #(#move_total_to_field)*
   }
}


fn compile_relation_sizes_body(mir: &InferMir) -> proc_macro2::TokenStream {
   let mut write_sizes = vec![];
   for r in mir.relations_ir_relations.keys() {
      let rel_name = &r.name;
      let rel_name_str = r.name.to_string();
      write_sizes.push(quote! {
         writeln!(&mut res, "{} size: {}", #rel_name_str, self.#rel_name.len()).unwrap();
      });
   }
   quote! {
      use std::fmt::Write;
      let mut res = String::new();
      #(#write_sizes)*
      res
   }
}

fn compile_scc_times_summary_body(mir: &InferMir) -> proc_macro2::TokenStream {
   let mut res = vec![];
   for i in 0..mir.sccs.len(){
      let i_str = format!("{}", i);
      let scc_time_field_name = scc_time_field_name(i);
      res.push(quote!{
         writeln!(&mut res, "scc {} time: {:?}", #i_str, self.#scc_time_field_name).unwrap();
      });
   }
   quote!{
      use std::fmt::Write;
      let mut res = String::new();
      #(#res)*
      res
   }
}

fn compile_update_indices_function_body(mir: &InferMir) -> proc_macro2::TokenStream {
   
   let mut res = vec![];
   for (r,indices_set) in mir.relations_ir_relations.iter(){

      let mut update_indices = vec![];
      for ind in indices_set.iter(){
         let ind_name = &ind.ir_name();
         let selection_tuple : Vec<Expr> = ind.indices.iter().map(|&i| {
            let ind = syn::Index::from(i); 
            parse2(quote_spanned! {r.name.span()=> tuple.#ind.clone()}).unwrap()
         }).collect_vec();
         let selection_tuple = tuple_spanned(&selection_tuple, r.name.span());
         update_indices.push(quote_spanned! {r.name.span()=>
            let selection_tuple = #selection_tuple;
            infer::internal::RelIndexTrait::index_insert(&mut self.#ind_name, selection_tuple, i);
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

fn compile_cond_clause(cond: &CondClause, body: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
   match cond {
      CondClause::IfLet(if_let_clause) => {
         let pat = &if_let_clause.pattern;
         let expr = &if_let_clause.exp;
         quote_spanned! {if_let_clause.if_keyword.span()=>
            if let #pat = #expr {
               #body
            }
         }
      }
      CondClause::If(if_clause) => {
         let cond = &if_clause.cond;
         quote_spanned! {if_clause.if_keyword.span()=>
            if #cond {
               #body
            }
         }
      }
   }
}

fn compile_mir_rule(rule: &MirRule, scc: &MirScc, mir: &InferMir, clause_ind: usize) -> proc_macro2::TokenStream {

   if clause_ind < rule.body_items.len(){
      let bitem = &rule.body_items[clause_ind];
      let next_loop = compile_mir_rule(rule, scc, mir, clause_ind + 1);

      match bitem {
         MirBodyItem::Clause(bclause) => {
            let bclause_rel_name = &bclause.rel.relation.name;
            let selected_args = &bclause.selected_args();

            fn bitem_vars(bitem : &MirBodyItem) -> Vec<Ident> {
               match bitem {
                  MirBodyItem::Clause(cl) => cl.args.iter().filter_map(expr_to_ident).collect(),
                  MirBodyItem::Generator(gen) => pat_to_ident(&gen.pattern).into_iter().collect(),
                  MirBodyItem::Cond(CondClause::IfLet(if_let)) => pat_to_ident(&if_let.pattern).into_iter().collect(),
                  MirBodyItem::Cond(CondClause::If(_)) => vec![]
               }
            }
            let pre_clause_vars = rule.body_items.iter().take(clause_ind)
                                       .flat_map(bitem_vars)
                                       .collect::<Vec<_>>();

            let clause_vars = bclause.args.iter().enumerate()
                                 .filter_map(|(i,v)| expr_to_ident(v).map(|v| (i, v)))
                                 .collect::<Vec<_>>();
            let common_vars = clause_vars.iter().filter(|(_i,v)| pre_clause_vars.contains(v)).collect::<Vec<_>>();
            let common_vars_no_indices = common_vars.iter().map(|(_i,v)| v.clone()).collect::<Vec<_>>();

            let mut new_vars_assignmnets = vec![];
            for (i,var) in clause_vars.iter(){
               if common_vars_no_indices.contains(var) {continue;}
               let i_ind = syn::Index{index: *i as u32, span: var.span()};
               new_vars_assignmnets.push(quote_spanned! {var.span()=> let #var = &row.#i_ind;});
            }

            let selected_args_cloned = selected_args.iter().map(exp_cloned).collect_vec();
            let selected_args_tuple = tuple_spanned(&selected_args_cloned, bclause.args_span);
            let rel_version_var_name = bclause.rel.var_name();
            
            let mut conds_then_next_loop = next_loop;
            for cond in bclause.cond_clauses.iter().rev() {
               conds_then_next_loop = compile_cond_clause(cond, conds_then_next_loop);
            }

            quote_spanned! {bclause.rel_args_span=>
               if let Some(matching) = #rel_version_var_name.get( &#selected_args_tuple) {
                  for &ind in matching.iter() {
                     // TODO we may be doing excessive cloning
                     let row = &_self.#bclause_rel_name[ind].clone();
                     #(#new_vars_assignmnets)*
                     #conds_then_next_loop
                  }
               }
            }
         },
         MirBodyItem::Generator(gen) => {
            let pat = &gen.pattern;
            let expr = &gen.expr;
            quote_spanned! {gen.for_keyword.span()=>
               for #pat in #expr {
                  #next_loop
               }
            }
         }
        MirBodyItem::Cond(cond) => compile_cond_clause(cond, next_loop),
      }
      
   } else {
      let mut add_rows = vec![];

      for hcl in rule.head_clause.iter() {

         let head_rel_name = &hcl.rel.name;
         let new_row_tuple = tuple_spanned(&hcl.args, hcl.args_span);
         
         let head_relation = &hcl.rel;
         let row_type = tuple_type(&head_relation.field_types);

         let mut update_indices = vec![];
         let rel_indices = scc.dynamic_relations.get(head_relation);
         if let Some(rel_indices) = rel_indices {
            for rel_ind in rel_indices.iter(){
               let var_name = ir_relation_version_var_name(&rel_ind.ir_name(), New);
               let args_tuple : Vec<Expr> = rel_ind.indices.iter().map(|&i| {
                  let i_ind = syn::Index::from(i);
                  syn::parse2(quote_spanned!{head_rel_name.span()=> new_row.#i_ind.clone()}).unwrap()
               }).collect();
               let args_tuple = tuple(&args_tuple);
               update_indices.push(quote_spanned! {hcl.span=>
                  infer::internal::RelIndexTrait::index_insert(&mut #var_name, #args_tuple, new_row_ind);
               });
            }
         }

         let head_rel_full_index = &mir.relations_full_indices[head_relation];
         let head_rel_full_index_var_name_new = ir_relation_version_var_name(&head_rel_full_index.ir_name(), New);
         let head_rel_full_index_var_name_delta = ir_relation_version_var_name(&head_rel_full_index.ir_name(), Delta);
         let head_rel_full_index_var_name_total = ir_relation_version_var_name(&head_rel_full_index.ir_name(), Total);

         if !hcl.rel.is_lattice { 
            let head_rel_name_string = head_rel_name.to_string();
            let add_row = quote_spanned!{hcl.span=>
               // comment("check if the tuple already exists");
               let new_row: #row_type = #new_row_tuple;
               if !(#head_rel_full_index_var_name_new.contains_key(&new_row) ||
                  #head_rel_full_index_var_name_delta.contains_key(&new_row) ||
                  #head_rel_full_index_var_name_total.contains_key(&new_row))
               {
                  let new_row_ind = _self.#head_rel_name.len();
                  #(#update_indices)*
                  _self.#head_rel_name.push(new_row);
                  changed = true;
                  // TODO remove
                  // if new_row_ind % 100000 == 0 && new_row_ind > 0 {
                  //    eprintln!("{} size: {}", #head_rel_name_string, new_row_ind);
                  // }
               }
            };
            add_rows.push(add_row);
         } else { 
            let head_lat_full_index = &mir.lattices_full_indices[head_relation];
            let head_lat_full_index_var_name_new = ir_relation_version_var_name(&head_lat_full_index.ir_name(), New);
            let head_lat_full_index_var_name_delta = ir_relation_version_var_name(&head_lat_full_index.ir_name(), Delta);
            let head_lat_full_index_var_name_full = ir_relation_version_var_name(&head_lat_full_index.ir_name(), Total);
            let tuple_lat_index = syn::Index::from(hcl.rel.field_types.len() - 1);
            let lattice_key_args = hcl.args.iter().take(hcl.args.len() - 1).map(exp_cloned).collect_vec();
            let lattice_key_tuple = tuple(&lattice_key_args);

            let add_row = quote! {
               let new_row: #row_type = #new_row_tuple;
               let lattice_key = #lattice_key_tuple;
               if let Some(existing_ind) = #head_lat_full_index_var_name_new.get(&lattice_key)
                  .or_else(|| #head_lat_full_index_var_name_delta.get(&lattice_key))
                  .or_else(|| #head_lat_full_index_var_name_full.get(&lattice_key)) 
               {
                  let existing_ind = *existing_ind.iter().next().unwrap();
                  // TODO possible excessive cloning here?
                  let lat_changed = ::infer::Lattice::join_mut(&mut _self.#head_rel_name[existing_ind].#tuple_lat_index, new_row.#tuple_lat_index.clone());
                  if lat_changed {
                     let new_row_ind = existing_ind;
                     #(#update_indices)*
                     changed = true;
                  }
               } else {
                  let new_row_ind = _self.#head_rel_name.len();
                  #(#update_indices)*
                  _self.#head_rel_name.push(new_row);
                  changed = true;
               }
            };
            add_rows.push(add_row);
         }
      }
      quote!{
         #(#add_rows)*
      }
   }
}
