open PPrint
open Ast
open Auxlib
open Util

module FunDeclKit = Fundeclkit

let opt_list_notations = ref false

let opt_include_untranslated = ref false

(******************************************************************************)
(* Base pretty printing *)

let defaultBase = string "Import DefaultBase."



(******************************************************************************)
(* Value pretty printing *)

let rec pp_value = function
  | Val_unit          -> string "tt"
  | Val_bool b        -> string (string_of_bool b)
  | Val_int i         -> Coq.integer i
  | Val_string s      -> dquotes (string s)
  | Val_prod (v1, v2) -> Coq.product (pp_value v1) (pp_value v2)
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
    | Val_int n      -> simple_app [string "exp_int"; Coq.integer n]
    | Val_string s   -> simple_app [string "exp_string"; dquotes (string s)]
    | v -> simple_app [
               string "exp_val";
               Sail.pp_ty (ty_of_val v);
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
        then Coq.list (List.map pp_expression l)
        else pp_exp_list l in
      simple_app [string "exp_list"; pp_l]
  | Exp_binop (bo, e1, e2) -> pp_exp_binop bo e1 e2
  | Exp_nys -> !^"EXP_" ^^ nys

and pp_par_expression e = parens (pp_expression e)


(******************************************************************************)
(* Statement pretty printing *)

let rec pp_statement statement =
  match statement with
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
     simple_app (string "call" :: !^f :: (List.map pp_par_expression arg_list))
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

let pp_function_definition original_sail_code function_definition =
  let identifier =
    PP.string ("fun_" ^ function_definition.funName)
  in
  let parameters =
    empty
  in
  let return_type =
    pp_hanging_list (PP.string "Stm") [
      Coq.list (List.map Sail.pp_bind function_definition.funType.arg_types);
      Sail.pp_ty function_definition.funType.ret_type
    ]
  in
  let body =
    pp_statement function_definition.funBody
  in
  Coq.annotate_with_original_definition original_sail_code (
    Coq.definition identifier parameters return_type body
  )

let pp_function_definitions function_definitions =
  List.map (uncurry pp_function_definition) function_definitions

let pp_funDefKit function_definitions =
  let fundef =
    let identifier = string "FunDef"
    and parameters = utf8string "{Δ τ} (f : Fun Δ τ)"
    and return_type = utf8string "Stm Δ τ"
    and body =
      let matched_expression =
        utf8string "f in Fun Δ τ return Stm Δ τ"
      and cases =
        let case_of_function_definition function_definition =
          (
            string function_definition.funName,
            string (Printf.sprintf "fun_%s" function_definition.funName)
          )
        in
        List.map case_of_function_definition (List.map snd function_definitions)
      in
      Coq.match' matched_expression cases
    in
    Coq.definition identifier parameters return_type body
  in
  let contents =
    separate small_step (
        build_list (fun { add; addall } ->
            addall (pp_function_definitions function_definitions);
            add fundef
          )
      )
  in
  Coq.section "FunDefKit" contents


(******************************************************************************)
(* ForeignDefKit pretty printing *)

let pp_foreignKit =
  let title = "ForeignKit"
  and contents = 
    separate_map hardline utf8string [
      "Definition Memory : Set := unit.";
      "Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)";
      "  (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop := False.";
      "Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :";
      "  exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.";
      "Proof. destruct f. Qed."
    ]
  in
  Coq.section title contents


(******************************************************************************)
(* Program pretty printing *)

let pp_program_module program_name base_name function_definitions =
  indent (separate small_step [
    string ("Module Import " ^ program_name ^ "Program <: Program " ^ base_name ^ "Base.");
    FunDeclKit.generate (List.map snd function_definitions);
    string ("Include FunDeclMixin " ^ base_name ^ "Base.");
    pp_funDefKit function_definitions;
    string ("Include DefaultRegStoreKit " ^ base_name ^ "Base.");
    pp_foreignKit;
    string ("Include ProgramMixin " ^ base_name ^ "Base.");
  ]) ^^ small_step ^^ string ("End " ^ program_name ^ "Program.")


(******************************************************************************)
(* Type definition pretty printing *)

let pp_type_module type_definitions =
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
          Sail.pp_numeric_expression numexpr;
          Coq.eol
        ]
    in
    Coq.annotate_with_original_definition original document
  in
  List.map (uncurry pp_type_definition) type_definitions


(******************************************************************************)
(* Full pretty printing *)

let imports () = [
    ("Coq",
     build_list (fun { add; _ } ->
         if !opt_list_notations then add "Lists.List";
         add "Strings.String";
         add "ZArith.BinInt"
    ));
    ("Katamaran",
     [
       "Semantics.Registers";
       "Program"
    ]);
    ("stdpp",
     [
       "finite"
    ]);
    ("Equations",
     [
       "Equations"
    ]);
  ]

let more_modules = [
  "ctx.notations";
  "ctx.resolution";
]

let scopes = [
  "string_scope";
  "list_scope"
]

let pp_module_header title =
  string (Printf.sprintf "(*** %s ***)" title)

let fromIR_pp ir =
  let more_modules =
    if !opt_list_notations
    then List.append more_modules ["ListNotations"]
    else more_modules
  in
  let generate_section title contents =
    string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents
  in
  let heading =
    let require_imports =
      List.map (uncurry Coq.require_imports) (imports ())
    in
    let imports =
      [
        Coq.imports more_modules
      ]
    in
    let scopes =
      [
        Coq.open_scopes scopes
      ]
    in
    let parts =
      build_list (fun { addall; _ } ->
          addall require_imports;
          addall imports;
          addall scopes
        )
    in
    separate small_step parts
  in
  let base =
    let segments =
      build_list (fun { add; addall } ->
          add (pp_module_header "TYPES");
          add defaultBase;
          addall (pp_type_module ir.type_definitions);
          addall (Enums.generate ir.enum_definitions)
        )
    in
    separate small_step segments
  in
  let program =
    generate_section
      "PROGRAM"
      (pp_program_module ir.program_name "Default" ir.function_definitions)
  in
  let registers =
    if
      List.is_empty ir.register_definitions
    then
      empty
    else
      generate_section
        "REGISTERS"
        (Registers.generate ir.register_definitions)
  in
  let untranslated =
    if
      !opt_include_untranslated
    then
      generate_section
        "UNTRANSLATED"
        (Untranslated.generate ir.untranslated_definitions)
    else
      empty
  in
  let sections =
    [
      heading;
      base;
      program;
      registers;
      untranslated
    ]
  in
  Util.separate_nonempty big_step sections

let pretty_print len out doc = ToChannel.pretty 1. len out (doc ^^ small_step)
