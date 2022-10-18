use std::{collections::{HashMap, HashSet}, ops::Index, rc::Rc};

use itertools::Itertools;
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{Attribute, Error, Expr, Pat, Type, parse2, spanned::Spanned};

use crate::{AscentProgram, ascent_syntax::{Declaration, RelationNode, rule_node_summary, IndexSpecNode}, utils::{expr_to_ident, into_set, is_wild_card, tuple, tuple_type}, syn_utils::{expr_get_vars, pattern_get_vars}};
use crate::ascent_syntax::{BodyClauseArg, BodyItemNode, CondClause, GeneratorNode, IfLetClause, RelationIdentity, RuleNode};

#[derive(Clone)]
pub(crate) struct AscentConfig {
   pub attrs: Vec<Attribute>,
   pub include_rule_times: bool,
   pub generate_run_partial: bool,
}

impl AscentConfig {
   const MEASURE_RULE_TIMES_ATTR: &'static str = "measure_rule_times";
   const GENERATE_RUN_TIMEOUT_ATTR: &'static str = "generate_run_timeout";
   pub fn new(attrs: Vec<Attribute>) -> syn::Result<AscentConfig> {
      let include_rule_times = attrs.iter().any(|attr| attr.path.is_ident(Self::MEASURE_RULE_TIMES_ATTR));
      let generate_run_partial = attrs.iter().any(|attr| attr.path.is_ident(Self::GENERATE_RUN_TIMEOUT_ATTR));

      let recognized_attrs = [Self::MEASURE_RULE_TIMES_ATTR, Self::GENERATE_RUN_TIMEOUT_ATTR];
      for attr in attrs.iter() {
         if !recognized_attrs.iter().any(|recognized_attr| attr.path.is_ident(recognized_attr)) {
            return Err(Error::new_spanned(attr, 
                       format!("unrecognized attribute. recognized attributes are: {}",
                               recognized_attrs.join(", "))));
         }
      }
      Ok(AscentConfig {
         attrs,
         include_rule_times,
         generate_run_partial
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
}

#[derive(Clone, Default)]
pub(crate) struct RelationMetadata{
   pub inititialization: Option<Rc<Expr>>,
   pub attributes: Rc<Vec<Attribute>>,
   pub demanded_indices: Rc<HashSet<Vec<Vec<usize>>>>,
}

pub trait IrRelationTrait {
   fn ir_name(&self) -> String;
}

pub(crate) struct IrRuleGeneric<Rel> {
   pub head_clauses: Vec<IrHeadClause>,
   pub body_items: Vec<IrBodyItemGeneric<Rel>>,
   pub is_simple_join: bool,
}
pub(crate) type IrRule = IrRuleGeneric<IrRelation>;

pub(crate) fn ir_rule_summary<Rel: IrRelationTrait>(rule: &IrRuleGeneric<Rel>) -> String {
   fn bitem_to_str<Rel: IrRelationTrait>(bi: &IrBodyItemGeneric<Rel>) -> String {
      match bi {
         IrBodyItemGeneric::Clause(cl) => cl.rel.ir_name().to_string(),
         IrBodyItemGeneric::Generator(_) => "for ⋯".into(),
         IrBodyItemGeneric::Cond(CondClause::If(..)) => format!("if ⋯"),
         IrBodyItemGeneric::Cond(CondClause::IfLet(..)) => format!("if let ⋯"),
         IrBodyItemGeneric::Cond(CondClause::Let(..)) => format!("let ⋯"),
         IrBodyItemGeneric::Agg(agg) => format!("agg {}", agg.rel.ir_name()),
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

pub(crate) enum IrBodyItemGeneric<Rel> {
   Clause(IrBodyClauseGeneric<Rel>),
   Generator(GeneratorNode),
   Cond(CondClause),
   Agg(IrAggClauseGeneric<Rel>)
}
pub(crate) type IrBodyItem = IrBodyItemGeneric<IrRelation>;

impl<Rel> IrBodyItemGeneric<Rel> {
   pub(crate) fn rel(&self) -> Option<&Rel> {
      match self {
         IrBodyItemGeneric::Clause(bcl) => Some(&bcl.rel),
         IrBodyItemGeneric::Agg(agg) => Some(&agg.rel),
         IrBodyItemGeneric::Generator(_) |
         IrBodyItemGeneric::Cond(_) => None,
      }
   }
}

#[derive(Clone)]
pub(crate) struct IrBodyClauseGeneric<Rel> {
   pub rel : Rel,
   pub args : Vec<Expr>,
   pub rel_args_span: Span,
   pub args_span: Span,
   pub cond_clauses : Vec<CondClause>
}

pub(crate) type IrBodyClause = IrBodyClauseGeneric<IrRelation>;

impl IrBodyClause {
   pub fn selected_args(&self) -> Vec<Expr> {
      self.rel.indices.iter().map(|&i| self.args[i].clone()).collect()
   }
}

#[derive(Clone)]
pub(crate) struct IrAggClauseGeneric<Rel> {
   pub span: Span,
   pub pat: Pat,
   pub aggregator: Expr,
   pub bound_args: Vec<Ident>,
   pub rel: Rel,
   pub rel_args: Vec<Expr>
}
pub(crate) type IrAggClause = IrAggClauseGeneric<IrRelation>;

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
   pub fn is_full_index(&self) -> bool {
      self.relation.field_types.len() == self.indices.len()
   }
   pub fn is_no_index(&self) -> bool {
      self.indices.is_empty()
   }
}

pub(crate) fn compile_ascent_program_to_hir(prog: &AscentProgram) -> syn::Result<AscentIr>{
   let ir_rules : Vec<(IrRule, Vec<IrRelation>)> = prog.rules.iter().map(|r| compile_rule_to_ir_rule(r, prog)).try_collect()?;
   let mut relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>> = HashMap::new();
   let mut relations_full_indices = HashMap::new();
   let mut relations_initializations = HashMap::new();
   let mut relations_metadata = HashMap::new();
   // let mut relations_no_indices = HashMap::new();
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
      // relations_ir_relations.entry(rel_identity.clone()).or_default().insert(rel_no_index.clone());
      relations_full_indices.insert(rel_identity.clone(), rel_full_index);
      if let Some(init_expr) = &rel.initialization {
         relations_initializations.insert(rel_identity.clone(), Rc::new(init_expr.clone()));
      }
      let mut demanded_indices = HashSet::new();
      let mut attributes = vec![];
      for attr in rel.attrs.iter() {
         if attr.path.is_ident(&Ident::new("index", Span::call_site())){
            let spec = syn::parse2::<IndexSpecNode>(attr.tokens.clone())?;
            let index = spec.to_index_spec(rel.field_types.len())?;
            demanded_indices.insert(index);
         } else {
            attributes.push(attr.clone());
         }
      }
      relations_metadata.insert(
         rel_identity.clone(),
         RelationMetadata {
            inititialization: rel.initialization.clone().map(|i| Rc::new(i)),
            attributes: Rc::new(attributes),
            demanded_indices: Rc::new(demanded_indices)
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
   let declaration = prog.declaration.clone().unwrap_or(parse2(quote! {pub struct AscentProgram;}).unwrap());
   Ok(AscentIr {
      rules: ir_rules.into_iter().map(|(rule, extra_rels)| rule).collect_vec(),
      relations_ir_relations,
      relations_full_indices,
      lattices_full_indices,
      relations_metadata: relations_metadata,
      // relations_no_indices,
      declaration,
      config: AscentConfig::new(prog.attributes.clone())?
   })
}

fn compile_rule_to_ir_rule(rule: &RuleNode, prog: &AscentProgram) -> syn::Result<(IrRule, Vec<IrRelation>)> {
   let mut body_items = vec![];
   let mut grounded_vars = vec![];
   fn extend_grounded_vars(grounded_vars: &mut Vec<Ident>, new_vars: impl IntoIterator<Item = Ident>) -> syn::Result<()> {
      for v in new_vars.into_iter() {
         if grounded_vars.contains(&v) {
            // TODO may someday this will work
            // let other_var = grounded_vars.iter().find(|&x| x == &v).unwrap();
            // let other_err = Error::new(other_var.span(), "");
            let err = Error::new(v.span(), format!("'{}' shadows another variable with the same name", v));
            // err.combine(other_err);
            return Err(err);
         }
         grounded_vars.push(v);
      }
      Ok(())
   }
   let mut first_two_items_all_args_vars = true;
   let mut first_two_items_simple_clauses = true;
   for (bitem_ind, bitem) in rule.body_items.iter().enumerate() {
      match bitem {
         BodyItemNode::Clause(ref bcl) => {
            if bitem_ind == 0 && bcl.cond_clauses.iter().any(|c| matches!(c, &CondClause::IfLet(_)))
            {
               first_two_items_simple_clauses = false;
            }

            if bitem_ind == 1 && first_two_items_simple_clauses{
               let mut self_vars = HashSet::new();
               for var in bcl.args.iter().filter_map(|arg| expr_to_ident(arg.unwrap_expr_ref())) {
                  if !self_vars.insert(var) {
                     first_two_items_simple_clauses = false;
                  }
               }
               for cond_cl in bcl.cond_clauses.iter(){
                  let cond_expr = cond_cl.expr();
                  let expr_idents = expr_get_vars(&cond_expr);
                  if !expr_idents.iter().all(|v| self_vars.contains(v)){
                     first_two_items_simple_clauses = false;
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
                  } else {
                     grounded_vars.push(var);
                  }
               } else {
                  indices.push(i);
                  if bitem_ind < 2 {
                     first_two_items_all_args_vars = false;
                  }
               }
            }
            let ir_name = ir_name_for_rel_indices(&bcl.rel, &indices);
            let relation = prog_get_relation(prog, &bcl.rel, bcl.args.len())?;
            if relation.is_lattice {
               first_two_items_simple_clauses = false;
            }

            for cond_clause in bcl.cond_clauses.iter() {
               extend_grounded_vars(&mut grounded_vars, cond_clause.bound_vars())?;
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
            if bitem_ind <= 1 {
               first_two_items_simple_clauses = false;
            }
            extend_grounded_vars(&mut grounded_vars, pattern_get_vars(&gen.pattern))?;
            body_items.push(IrBodyItem::Generator(gen.clone()));
         },
         BodyItemNode::Cond(ref cl) => {
            body_items.push(IrBodyItem::Cond(cl.clone()));
            if bitem_ind <= 1 {
               first_two_items_simple_clauses = false;
            }
            extend_grounded_vars(&mut grounded_vars, cl.bound_vars())?;
         },
         BodyItemNode::Agg(ref agg) => {
            if bitem_ind <= 1 {
               first_two_items_simple_clauses = false;
            }
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
            
            let ir_rel = IrRelation {
               relation: relation.into(),
               indices,
            };
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
   let is_simple_join = first_two_items_all_args_vars && first_two_items_simple_clauses && body_items.len() >= 2; 
   let simple_join_ir_relations = if is_simple_join {
      let (bcl1, bcl2) = match (&body_items[0], &body_items[1]) {
         (IrBodyItem::Clause(bcl1), IrBodyItem::Clause(bcl2)) => (bcl1, bcl2),
          _ => panic!("incorrect simple join handling in ascent_hir")
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
   if is_simple_join {
      if let [IrBodyItem::Clause(cl1), IrBodyItem::Clause(cl2)] = &mut body_items[0..=1] {
         cl1.rel = simple_join_ir_relations[0].clone();
      }
   }
   // println!("is_simple_join is {} for rule {}", is_simple_join, rule_node_summary(rule));
   Ok((IrRule {
      is_simple_join,
      head_clauses, 
      body_items, 
   }, vec![]))
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

pub(crate) fn prog_get_relation<'a>(prog: &'a AscentProgram, name: &Ident, arity: usize) -> syn::Result<&'a RelationNode> {
   let relation = prog.relations.iter().filter(|r| r.name.to_string() == name.to_string()).next();
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