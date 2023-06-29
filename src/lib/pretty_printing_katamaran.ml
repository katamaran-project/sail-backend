open List
open PPrint
open IR

(******************************************************************************)
(* Utility definitions *)

let indent = nest 2
let small_step = twice hardline
let big_step = twice small_step

(* let nyp = string "(* NOT YET PROCESSED *)" *)

let list_pp l = match l with
  | [] -> brackets empty
  | _  -> soft_surround 2 1 lbracket (separate (semi ^^ break 1) l) rbracket

let prod_pp v1 v2 = soft_surround 1 0 lparen (v1 ^^ comma ^^ break 1 ^^ v2)
  rparen

let simple_app argv = indent (flow (break 1) argv)

(******************************************************************************)
(* Heading pretty printing *)

let require_import src names = prefix 5 1 
  (string ("From " ^ src ^ " Require Import"))
  (separate_map hardline string names)
  ^^ dot

let import names = string "Import "
  ^^ align (separate_map hardline string names) ^^ dot


let heading = separate small_step [
  require_import "Coq" ["Lists.List"; "Strings.String"; "ZArith.BinInt"];
  require_import "Katamaran" ["Semantics.Registers"; "Program"];
  import ["ctx.notations"; "ctx.resolution"; "ListNotations"];
  string "Local Open Scope string_scope.";
  string "Local Open Scope list_scope.";
]

(******************************************************************************)
(* Base pretty printing *)

let defaultBase = string "Import DefaultBase."


(******************************************************************************)
(* FunDeclKit pretty printing *)

let rec ty_pp = function
  | Int           -> string "ty.int"
  | List t        -> parens (simple_app [!^"ty.list"; ty_pp t])
  | Bool          -> string "ty.bool"
  | Unit          -> string "ty.unit"
  | String        -> string "ty.string"
  | Prod (t1, t2) -> parens (simple_app [!^"ty.prod"; ty_pp t1; ty_pp t2])
  (*
  | Sum (t1, t2) -> parens_app !^"ty.sum" [ty_pp t1; ty_pp t2]
  *)
  | Undecide      -> underscore


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

let rec value_pp = function
  | Val_bool b        -> string (string_of_bool b)
  | Val_int i         -> string (string_of_int i ^ "%Z")
  | Val_unit          -> string "()"
  | Val_list l        -> list_pp (map value_pp l)
  | Val_string s      -> dquotes (string s)
  | Val_prod (v1, v2) -> prod_pp (value_pp v1) (value_pp v2) 
  (*
  | Val_Sum v         -> 
  *)

(******************************************************************************)
(* Expression pretty printing *)

let expression_pp = function
  | Exp_var v -> simple_app [(string "exp_var"); dquotes (string v)]
  | Exp_val v -> simple_app [(string "exp_val"); ty_pp (ty_val v); value_pp v]

(******************************************************************************)
(* Statement pretty printing *)

let rec statement_pp = function
  | Stm_val v -> simple_app [(string "stm_val"); ty_pp (ty_val v); value_pp v]
  | Stm_exp e -> simple_app [(string "stm_exp"); parens (expression_pp e)]
  | Stm_match_list m -> simple_app [(string "stm_match_list");
      parens (statement_pp m.s);
      parens (statement_pp m.alt_nil);
      dquotes (string m.xh);
      dquotes (string m.xt);
      parens (statement_pp m.alt_cons);
    ]

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
    heading;
    base;
    program
  ]

let pretty_print len out doc =
  ToChannel.pretty 1. len out (doc ^^ small_step)
