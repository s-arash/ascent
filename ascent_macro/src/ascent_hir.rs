use std::{collections::{HashMap, HashSet}, ops::Index, rc::Rc};

use itertools::Itertools;
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{Attribute, Error, Expr, Pat, Type, parse2, spanned::Spanned, parse_quote};

use crate::{AscentProgram, ascent_syntax::{Declaration, RelationNode, rule_node_summary}, utils::{expr_to_ident, into_set, is_wild_card, tuple, tuple_type}, syn_utils::{expr_get_vars, pattern_get_vars}};
use crate::ascent_syntax::{BodyClauseArg, BodyItemNode, CondClause, GeneratorNode, IfLetClause, RelationIdentity, RuleNode};

#[derive(Clone)]
pub(crate) struct AscentConfig {
   pub attrs: Vec<Attribute>,
   pub include_rule_times: bool,
   pub generate_run_partial: bool,
   pub inter_rule_parallelism: bool,
}

impl AscentConfig {
   const MEASURE_RULE_TIMES_ATTR: &'static str = "measure_rule_times";
   const GENERATE_RUN_TIMEOUT_ATTR: &'static str = "generate_run_timeout";
   const INTER_RULE_PARALLELISM_ATTR: &'static str = "inter_rule_parallelism";

   pub fn new(attrs: Vec<Attribute>, is_parallel: bool) -> syn::Result<AscentConfig> {
      let include_rule_times = attrs.iter().any(|attr| attr.path.is_ident(Self::MEASURE_RULE_TIMES_ATTR));
      let generate_run_partial = attrs.iter().any(|attr| attr.path.is_ident(Self::GENERATE_RUN_TIMEOUT_ATTR));
      let inter_rule_parallelism = attrs.iter().filter(|attr| attr.path.is_ident(Self::INTER_RULE_PARALLELISM_ATTR)).next();

      let recognized_attrs = [Self::MEASURE_RULE_TIMES_ATTR, Self::GENERATE_RUN_TIMEOUT_ATTR, Self::INTER_RULE_PARALLELISM_ATTR];
      for attr in attrs.iter() {
         if !recognized_attrs.iter().any(|recognized_attr| attr.path.is_ident(recognized_attr)) {
            return Err(Error::new_spanned(attr, 
                       format!("unrecognized attribute. recognized attributes are: {}",
                               recognized_attrs.join(", "))));
         }
      }
      if let Some(inter_rule_parallelism_attr) = inter_rule_parallelism {
         if !is_parallel {
            return Err(Error::new_spanned(inter_rule_parallelism, "attribute only allowed in parallel Ascent"));
         }
      }
      Ok(AscentConfig {
         inter_rule_parallelism: inter_rule_parallelism.is_some(),
         attrs,
         include_rule_times,
         generate_run_partial,
      })
   }
}

pub(crate) struct AscentIr {
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub lattices_full_indices: HashMap<RelationIdentity, IrRelation>,
   // pub relations_no_indices: HashMap<RelationIdentity, IrRelation>,
   pub relations_metadata: HashMap<RelationIdentity, RelationMetadata>,
   pub rules: Vec<IrRule>,
   pub declaration: Declaration,
   pub config: AscentConfig,
   pub is_parallel: bool,
}

#[derive(Clone, Default)]
pub(crate) struct RelationMetadata{
   pub initialization: Option<Rc<Expr>>,
   pub attributes: Rc<Vec<Attribute>>,
}

pub(crate) struct IrRule {
   pub head_clauses: Vec<IrHeadClause>,
   pub body_items: Vec<IrBodyItem>,
   pub simple_join_start_index: Option<usize>
}

pub(crate) fn ir_rule_summary(rule: &IrRule) -> String {
   fn bitem_to_str(bi: &IrBodyItem) -> String {
      match bi {
         IrBodyItem::Clause(cl) => cl.rel.ir_name().to_string(),
         IrBodyItem::Generator(_) => "for ⋯".into(),
         IrBodyItem::Cond(CondClause::If(..)) => format!("if ⋯"),
         IrBodyItem::Cond(CondClause::IfLet(..)) => format!("if let ⋯"),
         IrBodyItem::Cond(CondClause::Let(..)) => format!("let ⋯"),
         IrBodyItem::Agg(agg) => format!("agg {}", agg.rel.ir_name()),
      }
   }
   format!("{} <-- {}",
            rule.head_clauses.iter().map(|hcl| hcl.rel.name.to_string()).join(", "),
            rule.body_items.iter().map(bitem_to_str).join(", "))
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
   Cond(CondClause),
   Agg(IrAggClause)
}

impl IrBodyItem {
   pub(crate) fn rel(&self) -> Option<&IrRelation> {
      match self {
         IrBodyItem::Clause(bcl) => Some(&bcl.rel),
         IrBodyItem::Agg(agg) => Some(&agg.rel),
         IrBodyItem::Generator(_) |
         IrBodyItem::Cond(_) => None,
      }
   }
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

#[derive(Clone)]
pub(crate) struct IrAggClause {
   pub span: Span,
   pub pat: Pat,
   pub aggregator: Expr,
   pub bound_args: Vec<Ident>,
   pub rel: IrRelation,
   pub rel_args: Vec<Expr>
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub(crate) struct IrRelation {
   pub relation: RelationIdentity,
   pub indices: Vec<usize>,
   pub val_type: IndexValType,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum IndexValType {
   Reference,
   Direct(Vec<usize>)
}

impl IrRelation {
   pub fn new(relation: RelationIdentity, indices: Vec<usize>) -> Self {
      // TODO this is not the right place for this
      let val_type = if relation.is_lattice //|| indices.len() == relation.field_types.len() 
      {
         IndexValType::Reference
      } else {
         IndexValType::Direct((0..relation.field_types.len()).filter(|i| !indices.contains(&i)).collect_vec())
      };
      IrRelation { relation, indices, val_type }
   }

   pub fn key_type(&self) -> Type {
      let index_types : Vec<_> = self.indices.iter().map(|&i| self.relation.field_types[i].clone()).collect();
      tuple_type(&index_types)
   }
   pub fn ir_name(&self) -> Ident {
      ir_name_for_rel_indices(&self.relation.name, &self.indices)
   }
   pub fn is_full_index(&self) -> bool {
      self.relation.field_types.len() == self.indices.len()
   }
   pub fn is_no_index(&self) -> bool {
      self.indices.is_empty()
   }

   pub fn value_type(&self) -> Type {
      match &self.val_type {
         IndexValType::Reference => parse_quote!{usize},
         IndexValType::Direct(cols) => {
            let index_types : Vec<_> = cols.iter().map(|&i| self.relation.field_types[i].clone()).collect();
            tuple_type(&index_types)
         },
      }
   }
}

pub(crate) fn compile_ascent_program_to_hir(prog: &AscentProgram, is_parallel: bool) -> syn::Result<AscentIr>{
   let ir_rules : Vec<(IrRule, Vec<IrRelation>)> = prog.rules.iter().map(|r| compile_rule_to_ir_rule(r, prog)).try_collect()?;
   let num_relations = prog.relations.len();
   let mut relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::with_capacity(num_relations);
   let mut relations_full_indices = HashMap::with_capacity(num_relations);
   let mut relations_initializations = HashMap::new();
   let mut relations_metadata = HashMap::with_capacity(num_relations);
   // let mut relations_no_indices = HashMap::new();
   let mut lattices_full_indices = HashMap::new();
   for rel in prog.relations.iter(){
      let rel_identity = RelationIdentity::from(rel);

      if rel.is_lattice {
         let indices = (0 .. rel_identity.field_types.len() - 1).collect_vec();
         let ir_name = ir_name_for_rel_indices(&rel_identity.name, &indices);
         let lat_full_index = IrRelation::new(rel_identity.clone(), indices);
         relations_ir_relations.entry(rel_identity.clone()).or_default().insert(lat_full_index.clone());
         lattices_full_indices.insert(rel_identity.clone(), lat_full_index);
      }

      let full_indices = (0 .. rel_identity.field_types.len()).collect_vec();
      let ir_name = ir_name_for_rel_indices(&rel_identity.name, &full_indices);
      let rel_full_index = IrRelation::new(rel_identity.clone(),full_indices);
      let rel_no_index = IrRelation::new(rel_identity.clone(), vec![]);

      relations_ir_relations.entry(rel_identity.clone()).or_default().insert(rel_full_index.clone());
      // relations_ir_relations.entry(rel_identity.clone()).or_default().insert(rel_no_index.clone());
      relations_full_indices.insert(rel_identity.clone(), rel_full_index);
      if let Some(init_expr) = &rel.initialization {
         relations_initializations.insert(rel_identity.clone(), Rc::new(init_expr.clone()));
      }
      relations_metadata.insert(
         rel_identity.clone(),
         RelationMetadata {
            initialization: rel.initialization.clone().map(|i| Rc::new(i)),
            attributes: Rc::new(rel.attrs.clone())
         }
      );
      // relations_no_indices.insert(rel_identity, rel_no_index);
   }
   for (ir_rule, extra_relations) in ir_rules.iter(){
      for bitem in ir_rule.body_items.iter(){
         let rel = match bitem {
            IrBodyItem::Clause(bcl) => Some(&bcl.rel),
            IrBodyItem::Agg(agg) => Some(&agg.rel),
            _ => None
         };
         if let Some(rel) = rel {
            let relation = &rel.relation;
            relations_ir_relations.entry(relation.clone()).or_default().insert(rel.clone());
         }
      }
      for extra_rel in extra_relations.iter(){
         relations_ir_relations.entry(extra_rel.relation.clone()).or_default().insert(extra_rel.clone());
      }
   }
   let declaration = prog.declaration.clone().unwrap_or_else(|| parse2(quote! {pub struct AscentProgram;}).unwrap());
   Ok(AscentIr {
      rules: ir_rules.into_iter().map(|(rule, extra_rels)| rule).collect_vec(),
      relations_ir_relations,
      relations_full_indices,
      lattices_full_indices,
      relations_metadata: relations_metadata,
      // relations_no_indices,
      declaration,
      config: AscentConfig::new(prog.attributes.clone(), is_parallel)?,
      is_parallel
   })
}

fn compile_rule_to_ir_rule(rule: &RuleNode, prog: &AscentProgram) -> syn::Result<(IrRule, Vec<IrRelation>)> {
   let mut body_items = vec![];
   let mut grounded_vars = vec![];
   fn extend_grounded_vars(grounded_vars: &mut Vec<Ident>, new_vars: impl IntoIterator<Item = Ident>) -> syn::Result<()> {
      for v in new_vars.into_iter() {
         if grounded_vars.contains(&v) {
            // TODO may someday this will work
            let other_var = grounded_vars.iter().find(|&x| x == &v).unwrap();
            let other_err = Error::new(other_var.span(), "variable being shadowed");
            let mut err = Error::new(v.span(), format!("'{}' shadows another variable with the same name", v));
            err.combine(other_err);
            return Err(err);
         }
         grounded_vars.push(v);
      }
      Ok(())
   }

   let first_clause_ind = rule.body_items.iter().enumerate().find(|(_, bi)| matches!(bi, BodyItemNode::Clause(..))).map(|(i, _)| i);
   let mut first_two_clauses_simple = first_clause_ind.is_some() &&
      matches!(rule.body_items.get(first_clause_ind.unwrap() + 1), Some(BodyItemNode::Clause(..)));
   for (bitem_ind, bitem) in rule.body_items.iter().enumerate() {
      match bitem {
         BodyItemNode::Clause(ref bcl) => {
            if first_clause_ind == Some(bitem_ind) && bcl.cond_clauses.iter().any(|c| matches!(c, &CondClause::IfLet(_)))
            {
               first_two_clauses_simple = false;
            }

            if first_clause_ind.map(|x| x + 1) == Some(bitem_ind) && first_two_clauses_simple{
               let mut self_vars = HashSet::new();
               for var in bcl.args.iter().filter_map(|arg| expr_to_ident(arg.unwrap_expr_ref())) {
                  if !self_vars.insert(var) {
                     first_two_clauses_simple = false;
                  }
               }
               for cond_cl in bcl.cond_clauses.iter(){
                  let cond_expr = cond_cl.expr();
                  let expr_idents = expr_get_vars(&cond_expr);
                  if !expr_idents.iter().all(|v| self_vars.contains(v)){
                     first_two_clauses_simple = false;
                     break;
                  }
                  self_vars.extend(cond_cl.bound_vars());
               }
            }
            let mut indices = vec![];
            for (i,arg) in bcl.args.iter().enumerate() {
               if let Some(var) = expr_to_ident(arg.unwrap_expr_ref()) {
                  if grounded_vars.contains(&var){
                     indices.push(i);
                     if first_clause_ind == Some(bitem_ind) {
                        first_two_clauses_simple = false;
                     }
                  } else {
                     grounded_vars.push(var);
                  }
               } else {
                  indices.push(i);
                  if bitem_ind < 2 + first_clause_ind.unwrap_or(0) {
                     first_two_clauses_simple = false;
                  }
               }
            }
            let ir_name = ir_name_for_rel_indices(&bcl.rel, &indices);
            let relation = prog_get_relation(prog, &bcl.rel, bcl.args.len())?;
            // if relation.is_lattice {
            //    first_two_items_simple_clauses = false;
            // }

            for cond_clause in bcl.cond_clauses.iter() {
               extend_grounded_vars(&mut grounded_vars, cond_clause.bound_vars())?;
            }
            
            let ir_rel = IrRelation::new(relation.into(), indices);
            let ir_bcl = IrBodyClause {
               rel: ir_rel,
               args: bcl.args.iter().cloned().map(BodyClauseArg::unwrap_expr).collect(),
               rel_args_span: bcl.rel.span().join(bcl.args.span()).unwrap_or_else(|| bcl.rel.span()),
               args_span: bcl.args.span(),
               cond_clauses: bcl.cond_clauses.clone()
            };
            body_items.push(IrBodyItem::Clause(ir_bcl));
         },
         BodyItemNode::Generator(ref gen) => {
            extend_grounded_vars(&mut grounded_vars, pattern_get_vars(&gen.pattern))?;
            body_items.push(IrBodyItem::Generator(gen.clone()));
         },
         BodyItemNode::Cond(ref cl) => {
            body_items.push(IrBodyItem::Cond(cl.clone()));
            extend_grounded_vars(&mut grounded_vars, cl.bound_vars())?;
         },
         BodyItemNode::Agg(ref agg) => {
            extend_grounded_vars(&mut grounded_vars, pattern_get_vars(&agg.pat))?;
            let indices = agg.rel_args.iter().enumerate().filter(|(i, expr)| {
               if is_wild_card(expr) {
                  return false;
               } else if let Some(ident) = expr_to_ident(expr) {
                  if agg.bound_args.iter().contains(&ident) {
                     return false;
                  }
               }
               true
            }).map(|(i, expr)| i).collect_vec();
            let relation = prog_get_relation(prog, &agg.rel, agg.rel_args.len())?;
            
            let ir_rel = IrRelation::new(relation.into(), indices);
            let ir_agg_clause = IrAggClause {
               span: agg.agg_kw.span,
               pat: agg.pat.clone(),
               aggregator: agg.aggregator.get_expr(),
               bound_args: agg.bound_args.iter().cloned().collect_vec(),
               rel: ir_rel,
               rel_args: agg.rel_args.iter().cloned().collect_vec(),
            };
            body_items.push(IrBodyItem::Agg(ir_agg_clause));
         },
         _ => panic!("unrecognized body item")
      }
      
   }
   let mut head_clauses = vec![];
   for hcl_node in rule.head_clauses.iter(){
      let hcl_node = hcl_node.clause();
      let rel = prog.relations.iter().filter(|r| hcl_node.rel == r.name.to_string()).next();
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
   
   let is_simple_join = first_two_clauses_simple && body_items.len() >= 2;
   let simple_join_start_index = if is_simple_join {first_clause_ind} else {None};

   let simple_join_ir_relations = if let Some(start_ind) = simple_join_start_index {
      let (bcl1, bcl2) = match &body_items[start_ind..start_ind + 2] {
         [IrBodyItem::Clause(bcl1), IrBodyItem::Clause(bcl2)] => (bcl1, bcl2),
          _ => panic!("incorrect simple join handling in ascent_hir")
      };  
      let bcl2_vars = bcl2.args.iter().filter_map(expr_to_ident).collect_vec();
      let indices = get_indices_given_grounded_variables(&bcl1.args, &bcl2_vars);
      let new_cl1_ir_relation = IrRelation::new(bcl1.rel.relation.clone(), indices);
      vec![new_cl1_ir_relation]
   } else {vec![]};

   if let Some(start_ind) = simple_join_start_index {
      if let IrBodyItem::Clause(cl1) = &mut body_items[start_ind] {
         cl1.rel = simple_join_ir_relations[0].clone();
      }
   }

   Ok((IrRule {
      simple_join_start_index,
      head_clauses, 
      body_items, 
   }, vec![]))
}

pub fn ir_name_for_rel_indices(rel: &Ident, indices: &[usize]) -> Ident {
   let indices_str = if indices.len() == 0 {format!("none")} else {indices.iter().join("_")};
   let name = format!("{}_indices_{}", rel, indices_str);
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

pub(crate) fn prog_get_relation<'a>(prog: &'a AscentProgram, name: &Ident, arity: usize) -> syn::Result<&'a RelationNode> {
   let relation = prog.relations.iter().filter(|r| *name == r.name.to_string()).next();
   match relation {
      Some(rel) => {
         if rel.field_types.len() != arity {
            Err(Error::new(name.span(), format!("Wrong arity for relation {}. Actual arity: {}", name, rel.field_types.len())))
         } else {
            Ok(rel)
         }
      },
      None => Err(Error::new(name.span(), format!("Relation {} not defined", name))),
   }
}