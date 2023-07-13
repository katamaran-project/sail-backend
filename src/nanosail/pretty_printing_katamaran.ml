open List
open PPrint
open Ast

let opt_list_notations = ref false

(******************************************************************************)
(* Utility definitions *)

let indent = nest 2
let small_step = twice hardline
let big_step = twice small_step

let nys = string "NOT_YET_SUPPORTED "
let ic = string " IMPOSSIBLE_CASE "

let list_pp l = match l with
  | [] -> brackets empty
  | _  -> soft_surround 2 0 lbracket (separate (semi ^^ break 1) l) rbracket

let prod_pp v1 v2 = soft_surround 1 0 lparen (v1 ^^ comma ^^ break 1 ^^ v2)
  rparen

let simple_app argv = indent (flow (break 1) argv)
let parens_app argv = parens (simple_app argv)

(******************************************************************************)
(* Heading pretty printing *)

let require_import src names = prefix 5 1
  (string ("From " ^ src ^ " Require Import"))
  (separate_map hardline string names)
  ^^ dot

let import names = string "Import "
  ^^ align (separate_map hardline string names) ^^ dot


let heading list_not = separate small_step [
  require_import "Coq" (if list_not
    then ["Lists.List"; "Strings.String"; "ZArith.BinInt"]
    else ["Strings.String"; "ZArith.BinInt"]);
  require_import "Katamaran" ["Semantics.Registers"; "Program"];
  import (if list_not
    then ["ctx.notations"; "ctx.resolution"; "ListNotations"]
    else ["ctx.notations"; "ctx.resolution"]);
  string "Local Open Scope string_scope.";
  string "Local Open Scope list_scope.";
]

(******************************************************************************)
(* Base pretty printing *)

let defaultBase = string "Import DefaultBase."


(******************************************************************************)
(* FunDeclKit pretty printing *)

let ty_id_pp = function
  | Unit   -> string "ty.unit"
  | Bool   -> string "ty.bool"
  | Int    -> string "ty.int"
  | String -> string "ty.string"
  | List   -> string "ty.list"
  | Prod   -> string "ty.prod"
  | Id_nys -> !^"TY_ID_" ^^ nys

let rec ty_pp = function
  | Ty_id (ty_id)       -> ty_id_pp ty_id
  | Ty_app (ty_id, tys) -> parens_app ((ty_id_pp ty_id) :: (map ty_pp tys))
  | Ty_nys              -> !^"TY_" ^^ nys

let bind_pp (arg, t) =
  utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ ty_pp t

let funDecl_pp funDef = indent (simple_app [
  string ("| " ^ funDef.name ^ " : Fun");
  list_pp (map bind_pp funDef.funType.arg_types);
  ty_pp funDef.funType.ret_type
])

let funDeclKit_pp funDefList =
  indent (separate small_step [
    string "Section FunDeclKit.";
    string "Inductive Fun : PCtx -> Ty -> Set :=" ^^ hardline
    ^^ separate_map hardline funDecl_pp funDefList ^^ dot;
    separate_map hardline utf8string [
      "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
      "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
      "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
    ]
  ]) ^^ small_step ^^ string "End FunDeclKit."

(******************************************************************************)
(* Value pretty printing *)

let int_pp i =
  let i_pp = string (Big_int.to_string i ^ "%Z") in 
  if i < Z.zero then parens i_pp else i_pp

let rec value_pp = function
  | Val_unit           -> string "tt"
  | Val_bool b         -> string (string_of_bool b)
  | Val_int i          -> int_pp i
  | Val_string s       -> dquotes (string s)
  | Val_prod (v1, v2)  -> prod_pp (value_pp v1) (value_pp v2) 
  | Val_nys            -> !^"VAL_" ^^ nys

(******************************************************************************)
(* Expression pretty printing *)

let infix_binOp_pp = function
  | Plus  -> plus
  | Times -> star
  | Minus -> minus
  | And   -> twice ampersand
  | Or    -> twice bar
  | Eq    -> equals
  | Neq   -> bang ^^ equals
  | Le    -> langle ^^ equals
  | Lt    -> langle
  | Ge    -> rangle ^^ equals
  | Gt    -> rangle

  | _     -> ic

let rec expression_pp e =

  let rec exp_list_pp = function
    | []      -> string "nil"
    | x :: xs -> parens_app [string "cons"; par_expression_pp x;
        exp_list_pp xs]

  in let exp_val_pp = function
    | Val_bool true  -> string "exp_true"
    | Val_bool false -> string "exp_false"
    | Val_int n      -> simple_app [string "exp_int"; int_pp n]
    | Val_string s   -> simple_app [string "exp_string"; dquotes (string s)]
    | v              -> simple_app [string "exp_val"; ty_pp (ty_val v);
        value_pp v]
  
  in let exp_binop_pp bo e1 e2 = match bo with
    | Pair   -> simple_app [!^"exp_binop"; !^"bop.pair"; par_expression_pp e1;
        par_expression_pp e2]
    | Cons   -> simple_app [!^"exp_binop"; !^"bop.cons"; par_expression_pp e1;
        par_expression_pp e2]
    | Append -> simple_app [!^"exp_binop"; !^"bop.append"; par_expression_pp e1;
        par_expression_pp e2]
    | _      -> infix 2 1 (infix_binOp_pp bo) (par_expression_pp e1)
        (par_expression_pp e2)

  in match e with 
  | Exp_var v  -> simple_app [string "exp_var"; dquotes (string v)]
  | Exp_val v  -> exp_val_pp v
  | Exp_neg e  -> string "- " ^^ par_expression_pp e
  | Exp_not e  -> simple_app [string "exp_not"; par_expression_pp e]
  | Exp_list l -> simple_app [string "exp_list"; if !opt_list_notations
      then list_pp (map expression_pp l)
      else exp_list_pp l]
  | Exp_binop (bo, e1, e2) -> exp_binop_pp bo e1 e2
  | Exp_nys -> !^"EXP_" ^^ nys

and par_expression_pp e = parens (expression_pp e)


(******************************************************************************)
(* Statement pretty printing *)

let rec statement_pp = function
  | Stm_val v -> simple_app [(string "stm_val"); ty_pp (ty_val v); value_pp v]
  | Stm_exp e -> simple_app [(string "stm_exp"); par_expression_pp e]
  | Stm_match_list m -> simple_app [(string "stm_match_list");
      par_statement_pp m.s;
      par_statement_pp m.alt_nil;
      dquotes (string m.xh);
      dquotes (string m.xt);
      par_statement_pp m.alt_cons;
    ]
  | Stm_call (f, arg_list) -> simple_app
      (!^"call" :: !^f :: (map par_expression_pp arg_list))
  | Stm_let (v, s1, s2) -> simple_app [!^("let: \"" ^ v ^ "\" :=");
      statement_pp s1; !^"in"; statement_pp s2]
  | Stm_nys -> !^"STM_" ^^ nys
and par_statement_pp s = parens (statement_pp s)

(******************************************************************************)
(* FunDefKit pretty printing *)

let funDef_pp funDef =
  let body = funDef.funBody in
  indent (simple_app [
    string ("Definition fun_" ^ funDef.name ^ " : Stm");
    list_pp (map bind_pp funDef.funType.arg_types);
    ty_pp funDef.funType.ret_type
  ] ^^ !^" :=" ^^ hardline ^^ statement_pp body ^^ dot)

let funDefKit_pp funDefList =
  let name_binding_pp funDef = prefix 4 1 (string ("| " ^ funDef.name ^ " =>"))
    (string ("fun_" ^ funDef.name)) in
  indent (separate small_step [
    string "Section FunDefKit.";  
    separate_map small_step funDef_pp funDefList;
    indent (separate  hardline [
      utf8string "Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=";
      utf8string "match f in Fun Δ τ return Stm Δ τ with";
      separate_map hardline name_binding_pp funDefList;
      string "end."
    ]);
  ]) ^^ small_step ^^ string "End FunDefKit."
   
(******************************************************************************)
(* FunDefKit pretty printing *)

let foreignKit_pp =
  indent (separate_map hardline string [
    "Section ForeignKit.";
    "Definition Memory : Set := unit.";
    "Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)";
    "  (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop := False.";
    "Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :";
    "  exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.";
    "Proof. destruct f. Qed."
  ]) ^^ hardline ^^ string "End ForeignKit."

(******************************************************************************)
(* Program pretty printing *)

let program_module_pp program_name base_name funDefList =
  indent (separate small_step [
    string ("Module Import " ^ program_name ^ "Program <: Program " ^ base_name
      ^ "Base.");
    funDeclKit_pp funDefList;
    string ("Include FunDeclMixin " ^ base_name ^ "Base.");
    funDefKit_pp funDefList;
    string ("Include DefaultRegStoreKit " ^ base_name ^ "Base.");
    foreignKit_pp;
    string ("Include ProgramMixin " ^ base_name ^ "Base.");
  ]) ^^ small_step ^^ string ("End " ^ program_name ^ "Program.")

(******************************************************************************)

let fromIR_pp ir =
  let base =
    separate small_step [
      string "(*** TYPES ***)";
      defaultBase
    ]
  in let program = 
    separate small_step [
      string "(*** PROGRAM ***)";
      program_module_pp ir.program_name "Default" ir.funDefList
    ]
  in separate big_step [
    heading !opt_list_notations;
    base;
    program
  ]

let pretty_print len out doc =
  ToChannel.pretty 1. len out (doc ^^ small_step)
