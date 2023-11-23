open List
open PPrint
open Ast
open Util
open Pputil

let opt_list_notations = ref false

let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)

(******************************************************************************)
(* Type definition pretty printing *)

let pp_numeric_expression (numeric_expression : numeric_expression) =
  let protect =
    enclose lparen rparen
  in
  let rec pp level numexp =
    let protect_if lvl doc =
      if level <= lvl
      then doc
      else protect doc
    in    
    match numexp with
    | NE_constant z   -> string (Big_int.to_string z)
    | NE_add (x, y)   -> protect_if 0 (concat [ pp 0 x; space; plus; space; pp 0 y ])
    | NE_minus (x, y) -> protect_if 0 (concat [ pp 0 x; space; minus; space; pp 0 y ])
    | NE_times (x, y) -> protect_if 1 (concat [ pp 1 x; space; star; space; pp 1 y ])
    | NE_neg x        -> protect_if 2 (concat [ minus; pp 3 x ])
  in
  pp 0 numeric_expression

(******************************************************************************)
(* Heading pretty printing *)

let require_import_pp src names = prefix 5 1
  (string ("From " ^ src ^ " Require Import"))
  (separate_map hardline string names)
  ^^ pp_eol

let import_pp names = string "Import "
  ^^ align (separate_map hardline string names) ^^ pp_eol

let open_scope_pp scope =
  concat [
      string "Local Open Scope";
      space;
      string scope;
      pp_eol
    ]


(******************************************************************************)
(* Base pretty printing *)

let defaultBase = string "Import DefaultBase."


(******************************************************************************)
(* FunDeclKit pretty printing *)

let ty_id_pp = function
  | Unit      -> string "ty.unit"
  | Bool      -> string "ty.bool"
  | Int       -> string "ty.int"
  | String    -> string "ty.string"
  | List      -> string "ty.list"
  | Prod      -> string "ty.prod"
  | Bitvector -> string "ty.bvec" 
  | Id_nys    -> string "TY_ID_" ^^ nys

let rec ty_pp = function
  | Ty_id (ty_id)         -> ty_id_pp ty_id
  | Ty_app (ty_id, targs) -> parens_app ((ty_id_pp ty_id) :: (map type_argument_pp targs))
  | Ty_nys                -> !^"TY_" ^^ nys
and type_argument_pp (type_argument : type_argument) =
  match type_argument with
  | TA_type t   -> ty_pp t
  | TA_numexp e -> pp_numeric_expression e

let bind_pp (arg, t) =
  utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ ty_pp t

let funDecl_pp funDef = indent (simple_app [
  string ("| " ^ funDef.funName ^ " : Fun");
  list_pp (map bind_pp funDef.funType.arg_types);
  ty_pp funDef.funType.ret_type
])

let funDeclKit_pp funDefList =
  let contents = 
    separate small_step [
        string "Inductive Fun : PCtx -> Ty -> Set :=" ^^ hardline ^^ separate_map hardline funDecl_pp funDefList ^^ pp_eol;
        separate_map hardline utf8string [
            "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
            "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
            "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
          ]
      ]
  in
  pp_coq_section "FunDeclKit" contents

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
  | Val_prod (v1, v2) -> Ty_app (Prod, [TA_type (ty_of_val v1); TA_type (ty_of_val v2)])
  | Val_nys           -> Ty_nys   

let rec expression_pp e = 
  let rec exp_list_pp = function
    | []      -> string "nil"
    | x :: xs -> parens_app [!^"cons"; par_expression_pp x; exp_list_pp xs]
  in
  let exp_val_pp = function
    | Val_bool true  -> string "exp_true"
    | Val_bool false -> string "exp_false"
    | Val_int n      -> simple_app [string "exp_int"; int_pp n]
    | Val_string s   -> simple_app [string "exp_string"; dquotes (string s)]
    | v -> simple_app [
               string "exp_val";
               ty_pp (ty_of_val v);
               value_pp v
             ]
  in
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
  | Stm_match_list m ->
     simple_app [
         (string "stm_match_list");
         par_statement_pp m.s;
         par_statement_pp m.alt_nil;
         dquotes (string m.xh);
         dquotes (string m.xt);
         par_statement_pp m.alt_cons
       ]
  | Stm_match_prod m ->
     simple_app [
         (string "stm_match_prod");
         par_statement_pp m.s;
         dquotes (string m.xl);
         dquotes (string m.xr);
         par_statement_pp m.rhs
       ]
  | Stm_call (f, arg_list) ->
     simple_app (string "call" :: !^f :: (map par_expression_pp arg_list))
  | Stm_let (v, s1, s2) ->
     simple_app [
         string ("let: \"" ^ v ^ "\" :=");
         statement_pp s1;
         string "in";
         statement_pp s2;
       ]
  | Stm_if (s, s1, s2) ->
     simple_app [
         (string "stm_if");
         par_statement_pp s;
         par_statement_pp s1;
         par_statement_pp s2;
       ]
  | Stm_nys -> !^"STM_" ^^ nys

and par_statement_pp s = parens (statement_pp s)


(******************************************************************************)
(* FunDefKit pretty printing *)

let funDef_pp funDef =
  indent (simple_app [string ("Definition fun_" ^ funDef.funName ^ " : Stm");
    list_pp (map bind_pp funDef.funType.arg_types);
    ty_pp funDef.funType.ret_type
  ] ^^ !^" :=" ^^ hardline ^^ statement_pp funDef.funBody ^^ pp_eol)

let funDefKit_pp funDefList =
  let name_binding_pp funDef = prefix 4 1
    (string ("| " ^ funDef.funName ^ " =>"))
    (string ("fun_" ^ funDef.funName)) in
  indent (separate small_step [
    string "Section FunDefKit.";  
    separate_map small_step funDef_pp funDefList;
    indent (separate hardline [
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
(* Type definition pretty printing *)

let pp_multiline_comment comment =
  string "(*" ^^ twice hardline ^^ indent' comment ^^ string "\n*)"

let annotate_with_original_definition show_original original translation =
  if
    show_original
  then
    pp_multiline_comment (pp_sail_definition original) ^^ hardline ^^ translation
  else
    translation

let type_module_pp show_original type_definitions =
  let type_definition_pp (original : sail_definition) (type_definition : type_definition) : document =
    let document =
      match type_definition with
      | TD_abbreviation (identifier, TA_numeric_expression numexpr) ->
        concat [
          string "Definition";
          space;
          string identifier;
          space;
          string ":=";
          space;
          pp_numeric_expression numexpr;
          pp_eol
        ]
    in
    annotate_with_original_definition show_original original document
  in
  List.map (uncurry type_definition_pp) type_definitions


(******************************************************************************)
(* Register definition pretty printing *)

let register_module_pp _show_original (register_definitions : (sail_definition * register_definition) list) : document =
  let pp_register_definition ({ identifier; typ } : register_definition) =
    concat [
        string "|";
        space;
        string identifier;
        space; colon; space;
        string "Reg";
        space;
        ty_pp typ
      ]
  in
  let register_lines =
    List.map (compose_functions pp_register_definition snd) register_definitions
  in
  let lines = List.flatten
                [
                  [ string "Inductive Reg : Ty -> Set :=" ];
                  register_lines;
                  [ pp_eol ]
                ]
  in
  separate hardline lines
  

(******************************************************************************)
(* Untranslated definition pretty printing *)

let untranslated_module_pp untranslated_definitions =
  let untranslated_definition_pp (original : sail_definition) (untranslated_definition : untranslated_definition) =
    let { filename; line_number; sail_location; message } = untranslated_definition in
    let ocaml_location_string = Printf.sprintf "OCaml location: %s line %d" filename line_number in
    let sail_location_string = Printf.sprintf "Sail location: %s" (string_of_location sail_location) in
    let message_string =
      match message with
      | Some message -> Printf.sprintf "Message: %s" message
      | None         -> Printf.sprintf "No message"
    in
    concat [
        pp_sail_definition original;
        string ocaml_location_string;
        hardline;
        string sail_location_string;
        hardline;
        string message_string
      ]
  in
  pp_multiline_comment (separate small_step (List.map (uncurry untranslated_definition_pp) untranslated_definitions))


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

let pp_module_header title =
  string (Printf.sprintf "(*** %s ***)" title)

let fromIR_pp ?(show_original=false) ?(show_untranslated=false) ir =
  if !opt_list_notations then (
    coq_lib_modules := "Lists.List" :: !coq_lib_modules;
    more_modules := append !more_modules ["ListNotations"]
  );
  let generate_section segments =
    [ separate small_step segments ]
  in
  let heading =
    let segments =
      [
        require_import_pp "Coq" !coq_lib_modules;
        require_import_pp "Katamaran" !katamaran_lib_modules;
        import_pp !more_modules;
        separate_map hardline open_scope_pp !scopes
      ]
    in
    generate_section segments
  in
  let base =
    let segments =
      List.concat [
          [ pp_module_header "TYPES" ];
          [ defaultBase ];
          type_module_pp show_original ir.type_definitions;
        ]
    in
    generate_section segments
  in
  let program =
    let segments =
      [
        pp_module_header "PROGRAM";
        program_module_pp ir.program_name "Default" ir.function_definitions
      ]
    in
    generate_section segments
  in
  let registers : document list =
    if
      List.is_empty ir.register_definitions
    then
      []
    else
      let segments =
        [
          pp_module_header "REGISTERS";
          register_module_pp show_original ir.register_definitions
        ]
      in
      generate_section segments
  in
  let untranslated =
    if
      show_untranslated
    then
      let segments =
        [
           pp_module_header "UNTRANSLATED";
           untranslated_module_pp ir.untranslated_definitions
        ]
      in
      generate_section segments
    else
      []
  in
  let sections =
    List.flatten [
        heading;
        base;
        program;
        registers;
        untranslated
      ]
  in
  separate big_step sections

let pretty_print len out doc = ToChannel.pretty 1. len out (doc ^^ small_step)
