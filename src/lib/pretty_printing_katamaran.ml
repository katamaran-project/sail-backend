open List
open PPrint
open IR

(******************************************************************************)
(* Utility definitions *)

let small_indent = nest 2
let big_indent = nest 4
let small_step = twice hardline
let big_step = twice small_step

let nyp = string "(* NOT YET PROCESSED *)"

(*  Create a block surrounded by begin_str and end_str with nice alignment
  for a list of document separated by set_str.
    with_spc set to true will add a breakable space after begin_str and before 
  end_str  *)
let align_block with_spc begin_str end_str sep_str doc_list = 
  let spc = if with_spc then 1 else 0 in
  group (align (
    string begin_str ^^ blank spc ^^ align (
      separate (string sep_str ^^ break 1) doc_list
    ) ^^ blank spc ^^ string end_str
  ))

(*  Create a block surrounded by begin_str and end_str with small indentation
  for a list of document separated by set_str.
    with_spc set to true will add a breakable space after begin_str and before 
  end_str  *)
let simple_block with_spc begin_str end_str sep_str doc_list =
  let spc = if with_spc then 1 else 0 in
    group (small_indent (
      string begin_str ^^ break spc ^^
      separate (string sep_str ^^ break 1) doc_list
      ) ^^ break spc ^^ string end_str
    )

let parens_doc doc = simple_block false "(" ")" "" [doc]

let simple_app doc_list =
  group (small_indent (
    separate (break 1) doc_list
  ))

(******************************************************************************)
(* Heading pretty printing *)

let require_import src names = nest 5 (
  group (
    string ("From " ^ src ^ " Require Import") ^^
    break 1 ^^
    separate_map hardline string names ^^
    dot
  )
)

let import names = nest 7 (
  group (
    string "Import " ^^
    separate_map hardline string names ^^
    dot
  )
)

let heading = separate small_step [
  require_import "Coq" ["Lists.List"; "Strings.String"];
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
  | Int      -> string "ty.int"
  | List t   -> parens_doc (string "ty.list " ^^ ty_pp t)
  | Bool     -> string "ty.bool"
  | Unit     -> string "ty.unit"
  | Undecide -> nyp


let bind_pp (arg, t) =
  utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ ty_pp t

let funDecl_pp funDef = group (big_indent (
  string ("| " ^ funDef.name ^ " :") ^^ break 1 ^^
  group (
    string "Fun " ^^ align_block true "[" "]" ";" 
        (map bind_pp funDef.funType.arg_types)
    ^^ group (break 1 ^^ ty_pp funDef.funType.ret_type)
  )
))

let funDeclKit_pp funDefList =
  small_indent (
    separate hardline [
      string "Section FunDeclKit.";
      string "Inductive Fun : PCtx -> Ty -> Set :=";
      separate_map hardline funDecl_pp funDefList ^^ dot;
      empty;
      utf8string "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
      utf8string "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
      utf8string "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set."
    ]
  ) ^^ hardline ^^ string "End FunDeclKit." 

(******************************************************************************)
(* Value pretty printing *)

let rec value_pp = function
  | Val_bool b -> string (string_of_bool b)
  | Val_int i  -> string (string_of_int i)
  | Val_unit   -> string "()"
  | Val_list l -> align_block false "[" "]" ";" (map value_pp l)

(******************************************************************************)
(* Expression pretty printing *)

let expression_pp = function
  | Exp_var v -> simple_app [string "exp_var"; dquotes (string v)]
  | Exp_val v -> simple_app [string "exp_val";
                             ty_pp (ty_val v);
                             value_pp v]

(******************************************************************************)
(* Statement pretty printing *)

let rec statement_pp = function
  | Stm_val v -> simple_app [
      string "stm_val";
      ty_pp (ty_val v);
      value_pp v]
  | Stm_exp e -> simple_app [
      string "stm_exp";
      parens_doc (expression_pp e)]
  | Stm_match_list m -> simple_app [ 
      string "stm_match_list";
      parens_doc (statement_pp m.s);
      parens_doc (statement_pp m.alt_nil);
      dquotes (string m.xh);
      dquotes (string m.xt);
      parens_doc (statement_pp m.alt_cons);
    ]

(******************************************************************************)
(* FunDefKit pretty printing *)

let funDef_pp funDef =
  let body = funDef.funBody in
  group (big_indent (
    string ("Definition fun_" ^ funDef.name ^ " :") ^^ break 1 ^^
    group (
      string "Stm " ^^ align_block true "[" "]" ";" 
          (map bind_pp funDef.funType.arg_types)
      ^^ group (break 1 ^^ ty_pp funDef.funType.ret_type)
    )
  )) ^^ string " :=" ^^ small_indent (
    hardline ^^ statement_pp body ^^ dot
  )

let funDefKit_pp funDefList =
  let name_binding_pp funDef = group (big_indent (
    string ("| " ^ funDef.name ^ " =>") ^^ break 1 ^^
    string ("fun_" ^ funDef.name)
  )) in
  small_indent (separate small_step [
    string "Section FunDefKit.";
    separate_map small_step funDef_pp funDefList;
    small_indent (separate  hardline [
      utf8string "Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=";
      utf8string "match f in Fun Δ τ return Stm Δ τ with";
      separate_map hardline name_binding_pp funDefList;
      string "end."
    ])
  ]) ^^ small_step ^^ string "End FunDefKit." 
   
(******************************************************************************)
(* FunDefKit pretty printing *)

let foreignKit_pp =
  small_indent (separate_map hardline string [
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
  small_indent (
    separate small_step [
      string ("Module Import " ^ program_name ^
              "Program <: Program " ^ base_name ^ "Base.");
      funDeclKit_pp funDefList;
      string ("Include FunDeclMixin " ^ base_name ^ "Base.");
      funDefKit_pp funDefList;
      string ("Include DefaultRegStoreKit " ^ base_name ^ "Base.");
      foreignKit_pp;
      string ("Include ProgramMixin " ^ base_name ^ "Base.");
    ]
  ) ^^ small_step ^^ string ("End " ^ program_name ^ "Program.")

(******************************************************************************)

let fromIR_pp ir =
  let types =
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
    types;
    program
  ]

let pretty_print len out doc =
  ToChannel.pretty 1. len out (doc ^^ small_step)
