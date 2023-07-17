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

let list_pp = function
  | [] -> brackets empty
  | l  -> soft_surround 2 0 lbracket (separate (semi ^^ break 1) l) rbracket

let prod_pp v1 v2 = soft_surround 1 0 lparen (v1 ^^ comma ^^ break 1 ^^ v2)
  rparen

let simple_app argv = indent (flow (break 1) argv)
let parens_app argv = parens (simple_app argv)


(******************************************************************************)
(* Heading pretty printing *)

let require_import_pp src names = prefix 5 1
  (string ("From " ^ src ^ " Require Import"))
  (separate_map hardline string names)
  ^^ dot

let import_pp names = string "Import "
  ^^ align (separate_map hardline string names) ^^ dot

let open_scope_pp scope = string ("Local Open Scope " ^ scope ^ ".")


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
  string ("| " ^ funDef.funName ^ " : Fun");
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
  | Val_unit          -> string "tt"
  | Val_bool b        -> string (string_of_bool b)
  | Val_int i         -> int_pp i
  | Val_string s      -> dquotes (string s)
  | Val_prod (v1, v2) -> prod_pp (value_pp v1) (value_pp v2) 
  | Val_nys           -> !^"VAL_" ^^ nys


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

let rec ty_of_val = function
  | Val_unit          -> Ty_id Unit
  | Val_bool _        -> Ty_id Bool
  | Val_int _         -> Ty_id Int
  | Val_string _      -> Ty_id String
  | Val_prod (v1, v2) -> Ty_app (Prod, [ty_of_val v1; ty_of_val v2])
  | Val_nys           -> Ty_nys   

let rec expression_pp e = 

  let rec exp_list_pp = function
    | []      -> string "nil"
    | x :: xs -> parens_app [!^"cons"; par_expression_pp x; exp_list_pp xs] in

  let exp_val_pp = function
    | Val_bool true  -> string "exp_true"
    | Val_bool false -> string "exp_false"
    | Val_int n      -> simple_app [string "exp_int"; int_pp n]
    | Val_string s   -> simple_app [string "exp_string"; dquotes (string s)]
    | v              -> simple_app [string "exp_val"; ty_pp (ty_of_val v);
        value_pp v] in
  
  let exp_binop_pp bo e1 e2 =
    match bo with
    | Pair   -> simple_app [!^"exp_binop";
        !^"bop.pair";
        par_expression_pp e1;
        par_expression_pp e2]
    | Cons   -> simple_app [!^"exp_binop";
        !^"bop.cons";
        par_expression_pp e1;
        par_expression_pp e2]
    | Append -> simple_app [!^"exp_binop";
        !^"bop.append";
        par_expression_pp e1;
        par_expression_pp e2]
    | _      -> infix 2 1 (infix_binOp_pp bo)
        (par_expression_pp e1)
        (par_expression_pp e2) in

  match e with 
  | Exp_var v  -> simple_app [string "exp_var"; dquotes (string v)]
  | Exp_val v  -> exp_val_pp v
  | Exp_neg e  -> string "- " ^^ par_expression_pp e
  | Exp_not e  -> simple_app [string "exp_not"; par_expression_pp e]
  | Exp_list l ->
      let l_pp = if !opt_list_notations
        then list_pp (map expression_pp l)
        else exp_list_pp l in
      simple_app [string "exp_list"; l_pp]
  | Exp_binop (bo, e1, e2) -> exp_binop_pp bo e1 e2
  | Exp_nys -> !^"EXP_" ^^ nys

and par_expression_pp e = parens (expression_pp e)


(******************************************************************************)
(* Statement pretty printing *)

let rec statement_pp = function
  | Stm_exp e -> simple_app [(string "stm_exp"); par_expression_pp e]
  | Stm_match_list m -> simple_app [(string "stm_match_list");
      par_statement_pp m.s;
      par_statement_pp m.alt_nil;
      dquotes (string m.xh);
      dquotes (string m.xt);
      par_statement_pp m.alt_cons]
  | Stm_match_prod m -> simple_app [(string "stm_match_prod");
      par_statement_pp m.s;
      dquotes (string m.xl);
      dquotes (string m.xr);
      par_statement_pp m.rhs]
  | Stm_call (f, arg_list) -> simple_app (string "call" :: !^f ::
      (map par_expression_pp arg_list))
  | Stm_let (v, s1, s2) -> simple_app [string ("let: \"" ^ v ^ "\" :=");
      statement_pp s1;
      string "in";
      statement_pp s2;]
  | Stm_if (s, s1, s2) -> simple_app [(string "stm_if");
      par_statement_pp s;
      par_statement_pp s1;
      par_statement_pp s2;]
  | Stm_nys -> !^"STM_" ^^ nys

and par_statement_pp s = parens (statement_pp s)


(******************************************************************************)
(* FunDefKit pretty printing *)

let funDef_pp funDef =
  indent (simple_app [string ("Definition fun_" ^ funDef.funName ^ " : Stm");
    list_pp (map bind_pp funDef.funType.arg_types);
    ty_pp funDef.funType.ret_type
  ] ^^ !^" :=" ^^ hardline ^^ statement_pp funDef.funBody ^^ dot)

let funDefKit_pp funDefList =
  let name_binding_pp funDef = prefix 4 1
    (string ("| " ^ funDef.funName ^ " =>"))
    (string ("fun_" ^ funDef.funName)) in
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
(* Full pretty printing *)

let coq_lib_modules = ref [
  "Strings.String";
  "ZArith.BinInt";
]

let katamaran_lib_modules = ref [
  "Semantics.Registers";
  "Program";
]

let more_modules = ref [
  "ctx.notations";
  "ctx.resolution";
]

let scopes = ref [
  "string_scope";
  "list_scope"
]

let fromIR_pp ir =
  if !opt_list_notations then (
    coq_lib_modules := "Lists.List" :: !coq_lib_modules;
    more_modules := append !more_modules ["ListNotations"]
  );
  let heading =
    separate small_step [
      require_import_pp "Coq" !coq_lib_modules;
      require_import_pp "Katamaran" !katamaran_lib_modules;
      import_pp !more_modules;
      separate_map hardline open_scope_pp !scopes
    ] in
  let base =
    separate small_step [
      string "(*** TYPES ***)";
      defaultBase
    ] in
  let program = 
    separate small_step [
      string "(*** PROGRAM ***)";
      program_module_pp ir.program_name "Default" ir.funDefList
    ] in
  separate big_step [
    heading;
    base;
    program;
  ]

let pretty_print len out doc = ToChannel.pretty 1. len out (doc ^^ small_step)
