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

let pp_require_import src names = prefix 5 1
  (string ("From " ^ src ^ " Require Import"))
  (separate_map hardline string names)
  ^^ pp_eol

let pp_import names = string "Import "
  ^^ align (separate_map hardline string names) ^^ pp_eol

let pp_open_scope scope =
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

let pp_ty_id = function
  | Unit      -> string "ty.unit"
  | Bool      -> string "ty.bool"
  | Int       -> string "ty.int"
  | String    -> string "ty.string"
  | List      -> string "ty.list"
  | Prod      -> string "ty.prod"
  | Bitvector -> string "ty.bvec" 
  | Id_nys    -> string "TY_ID_" ^^ nys

let rec pp_ty = function
  | Ty_id (ty_id)         -> pp_ty_id ty_id
  | Ty_app (ty_id, targs) -> parens_app ((pp_ty_id ty_id) :: (map pp_type_argument targs))
  | Ty_nys                -> !^"TY_" ^^ nys
and pp_type_argument (type_argument : type_argument) =
  match type_argument with
  | TA_type t   -> pp_ty t
  | TA_numexp e -> pp_numeric_expression e

let pp_bind (arg, t) =
  utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ pp_ty t

let pp_function_declaration funDef = indent (simple_app [
  string ("| " ^ funDef.funName ^ " : Fun");
  pp_list (map pp_bind funDef.funType.arg_types);
  pp_ty funDef.funType.ret_type
])

let pp_funDeclKit funDefList =
  let contents = 
    separate small_step [
        string "Inductive Fun : PCtx -> Ty -> Set :=" ^^ hardline ^^ separate_map hardline pp_function_declaration funDefList ^^ pp_eol;
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

let pp_int i =
  let pp_i = string (Big_int.to_string i ^ "%Z") in 
  if i < Z.zero then parens pp_i else pp_i

let rec pp_value = function
  | Val_unit          -> string "tt"
  | Val_bool b        -> string (string_of_bool b)
  | Val_int i         -> pp_int i
  | Val_string s      -> dquotes (string s)
  | Val_prod (v1, v2) -> pp_prod (pp_value v1) (pp_value v2) 
  | Val_nys           -> !^"VAL_" ^^ nys


(******************************************************************************)
(* Expression pretty printing *)

let pp_infix_binOp = function
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

let rec pp_expression e = 
  let rec pp_exp_list = function
    | []      -> string "nil"
    | x :: xs -> parens_app [!^"cons"; pp_par_expression x; pp_exp_list xs]
  in
  let pp_exp_val = function
    | Val_bool true  -> string "exp_true"
    | Val_bool false -> string "exp_false"
    | Val_int n      -> simple_app [string "exp_int"; pp_int n]
    | Val_string s   -> simple_app [string "exp_string"; dquotes (string s)]
    | v -> simple_app [
               string "exp_val";
               pp_ty (ty_of_val v);
               pp_value v
             ]
  in
  let pp_exp_binop bo e1 e2 =
    match bo with
    | Pair ->
       simple_app [
           string "exp_binop";
           string "bop.pair";
           pp_par_expression e1;
           pp_par_expression e2
         ]
    | Cons ->
       simple_app [
           string "exp_binop";
           string "bop.cons";
           pp_par_expression e1;
           pp_par_expression e2
         ]
    | Append ->
       simple_app [
           string "exp_binop";
           string "bop.append";
           pp_par_expression e1;
           pp_par_expression e2
         ]
    | _  ->
       infix 2 1 (pp_infix_binOp bo) (pp_par_expression e1) (pp_par_expression e2)
  in
  match e with 
  | Exp_var v  -> simple_app [string "exp_var"; dquotes (string v)]
  | Exp_val v  -> pp_exp_val v
  | Exp_neg e  -> string "- " ^^ pp_par_expression e
  | Exp_not e  -> simple_app [string "exp_not"; pp_par_expression e]
  | Exp_list l ->
      let pp_l = if !opt_list_notations
        then pp_list (map pp_expression l)
        else pp_exp_list l in
      simple_app [string "exp_list"; pp_l]
  | Exp_binop (bo, e1, e2) -> pp_exp_binop bo e1 e2
  | Exp_nys -> !^"EXP_" ^^ nys

and pp_par_expression e = parens (pp_expression e)


(******************************************************************************)
(* Statement pretty printing *)

let rec pp_statement = function
  | Stm_exp e -> simple_app [(string "stm_exp"); pp_par_expression e]
  | Stm_match_list m ->
     simple_app [
         (string "stm_match_list");
         pp_par_statement m.s;
         pp_par_statement m.alt_nil;
         dquotes (string m.xh);
         dquotes (string m.xt);
         pp_par_statement m.alt_cons
       ]
  | Stm_match_prod m ->
     simple_app [
         (string "stm_match_prod");
         pp_par_statement m.s;
         dquotes (string m.xl);
         dquotes (string m.xr);
         pp_par_statement m.rhs
       ]
  | Stm_call (f, arg_list) ->
     simple_app (string "call" :: !^f :: (map pp_par_expression arg_list))
  | Stm_let (v, s1, s2) ->
     simple_app [
         string ("let: \"" ^ v ^ "\" :=");
         pp_statement s1;
         string "in";
         pp_statement s2;
       ]
  | Stm_if (s, s1, s2) ->
     simple_app [
         (string "stm_if");
         pp_par_statement s;
         pp_par_statement s1;
         pp_par_statement s2;
       ]
  | Stm_nys -> !^"STM_" ^^ nys

and pp_par_statement s = parens (pp_statement s)


(******************************************************************************)
(* FunDefKit pretty printing *)

let pp_funDef funDef =
  indent (simple_app [string ("Definition fun_" ^ funDef.funName ^ " : Stm");
    pp_list (map pp_bind funDef.funType.arg_types);
    pp_ty funDef.funType.ret_type
  ] ^^ !^" :=" ^^ hardline ^^ pp_statement funDef.funBody ^^ pp_eol)

let pp_funDefKit funDefList =
  let pp_name_binding funDef = prefix 4 1
    (string ("| " ^ funDef.funName ^ " =>"))
    (string ("fun_" ^ funDef.funName))
  in
  let contents =
    separate small_step [
        separate_map small_step pp_funDef funDefList;
        indent (separate hardline [
                    utf8string "Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=";
                    utf8string "match f in Fun Δ τ return Stm Δ τ with";
                    separate_map hardline pp_name_binding funDefList;
                    string "end."
          ]);
      ]
  in
  pp_coq_section "FunDefKit" contents


(******************************************************************************)
(* FunDefKit pretty printing *)

let pp_foreignKit =
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

let pp_program_module program_name base_name funDefList =
  indent (separate small_step [
    string ("Module Import " ^ program_name ^ "Program <: Program " ^ base_name
      ^ "Base.");
    pp_funDeclKit funDefList;
    string ("Include FunDeclMixin " ^ base_name ^ "Base.");
    pp_funDefKit funDefList;
    string ("Include DefaultRegStoreKit " ^ base_name ^ "Base.");
    pp_foreignKit;
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

let pp_type_module show_original type_definitions =
  let pp_type_definition (original : sail_definition) (type_definition : type_definition) : document =
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
  List.map (uncurry pp_type_definition) type_definitions


(******************************************************************************)
(* Register definition pretty printing *)

let pp_register_module _show_original (register_definitions : (sail_definition * register_definition) list) : document =
  let pp_register_definition ({ identifier; typ } : register_definition) =
    concat [
        string "|";
        space;
        string identifier;
        space; colon; space;
        string "Reg";
        space;
        pp_ty typ
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

let pp_untranslated_module untranslated_definitions =
  let pp_untranslated_definition (original : sail_definition) (untranslated_definition : untranslated_definition) =
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
  pp_multiline_comment (separate small_step (List.map (uncurry pp_untranslated_definition) untranslated_definitions))


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
        pp_require_import "Coq" !coq_lib_modules;
        pp_require_import "Katamaran" !katamaran_lib_modules;
        pp_import !more_modules;
        separate_map hardline pp_open_scope !scopes
      ]
    in
    generate_section segments
  in
  let base =
    let segments =
      List.concat [
          [ pp_module_header "TYPES" ];
          [ defaultBase ];
          pp_type_module show_original ir.type_definitions;
        ]
    in
    generate_section segments
  in
  let program =
    let segments =
      [
        pp_module_header "PROGRAM";
        pp_program_module ir.program_name "Default" ir.function_definitions
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
          pp_register_module show_original ir.register_definitions
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
           pp_untranslated_module ir.untranslated_definitions
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
