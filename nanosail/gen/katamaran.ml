open PPrint
open Ast
open Auxlib
open Util
open Monad

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
    | []      -> generate (string "nil")
    | x :: xs ->
       let* x' = pp_par_expression x in
       let* xs' = pp_exp_list xs
       in
       generate (
           parens_app [string "cons"; x'; xs']
         )
  in
  let pp_exp_val = function
    | Val_bool true  -> generate (string "exp_true")
    | Val_bool false -> generate (string "exp_false")
    | Val_int n      -> generate (simple_app [string "exp_int"; Coq.integer n])
    | Val_string s   -> generate (simple_app [string "exp_string"; dquotes (string s)])
    | v ->
       let* v_type = Sail.pp_ty (ty_of_val v);
       in
       generate (simple_app [
                     string "exp_val";
                     v_type;
                     pp_value v
                   ]
         )
  in
  let pp_exp_binop bo e1 e2 =
    let* e1' = pp_par_expression e1 in
    let* e2' = pp_par_expression e2
    in
    match bo with
    | Pair ->
       generate (
           simple_app [
               string "exp_binop";
               string "bop.pair";
               e1';
               e2'
             ]
         )
    | Cons ->
       generate (
           simple_app [
               string "exp_binop";
               string "bop.cons";
               e1';
               e2'
             ]
         )
    | Append ->
       generate (
           simple_app [
               string "exp_binop";
               string "bop.append";
               e1';
               e2'
             ]
         )
    | _  ->
       generate (
           infix 2 1 (pp_infix_binOp bo) e1' e2'
         )
  in
  match e with
  | Exp_var v  -> generate (simple_app [string "exp_var"; dquotes (string v)])
  | Exp_val v  -> pp_exp_val v
  | Exp_neg e  -> let* e' = pp_par_expression e in
                  generate (string "- " ^^ e')
  | Exp_not e  -> let* e' = pp_par_expression e in
                  generate (simple_app [string "exp_not"; e'])
  | Exp_list lst ->
     let* lst' = if !opt_list_notations
                 then
                   (
                     let* expressions = map pp_expression lst in
                     generate (Coq.list expressions)
                   )
                 else pp_exp_list lst
     in
     generate (simple_app [string "exp_list"; lst'])
  | Exp_binop (bo, e1, e2) -> pp_exp_binop bo e1 e2
  | Exp_nys -> not_yet_implemented __POS__

and pp_par_expression e =
  let* e' = pp_expression e
  in
  generate (parens e')


(******************************************************************************)
(* Statement pretty printing *)

let rec pp_statement statement =
  match statement with
  | Stm_exp e ->
     let* e' = pp_par_expression e
     in
     generate (
         simple_app [(string "stm_exp"); e']
       )
  | Stm_match_list m ->
     let* m_s' = pp_par_statement m.s in
     let* m_alt_nil' = pp_par_statement m.alt_nil in
     let* m_alt_cons' = pp_par_statement m.alt_cons
     in
     generate (
         simple_app [
             (string "stm_match_list");
             m_s';
             m_alt_nil';
             dquotes (string m.xh);
             dquotes (string m.xt);
             m_alt_cons'
           ]
       )
  | Stm_match_prod m ->
     let* m_s' = pp_par_statement m.s in
     let* m_rhs' = pp_par_statement m.rhs
     in
     generate (
         simple_app [
             (string "stm_match_prod");
             m_s';
             dquotes (string m.xl);
             dquotes (string m.xr);
             m_rhs'
           ]
       )
  | Stm_call (f, arg_list) ->
     let* arg_list' =
       map pp_par_expression arg_list
     in
     generate (
         simple_app (string "call" :: !^f :: arg_list')
       )
  | Stm_let (v, s1, s2) ->
     let* s1' = pp_statement s1 in
     let* s2' = pp_statement s2
     in
     generate (
         simple_app [
             string ("let: \"" ^ v ^ "\" :=");
             s1';
             string "in";
             s2'
           ]
       )
  | Stm_if (s, s1, s2) ->
     let* s' = pp_par_statement s in
     let* s1' = pp_par_statement s1 in
     let* s2' = pp_par_statement s2
     in
     generate (
         simple_app [
             (string "stm_if");
             s';
             s1';
             s2'
           ]
       )
  | Stm_nys -> not_yet_implemented __POS__

and pp_par_statement s = lift parens (pp_statement s)


(******************************************************************************)
(* FunDefKit pretty printing *)

let pp_function_definition
      ((sail_function_definition : sail_definition), (function_definition : function_definition))
      type_constraint =
  let identifier =
    PP.string ("fun_" ^ function_definition.funName)
  in
  let parameters =
    empty
  in
  let coq_definition =
    let* return_type =
      let* bindings =
        let* docs = map Sail.pp_bind function_definition.funType.arg_types
        in
        generate (Coq.list docs)
      in
      let* return_type =
        Sail.pp_ty function_definition.funType.ret_type
      in
      generate (
          pp_hanging_list (PP.string "Stm") [
              bindings;
              return_type
            ]
        )
    in
    let* body =
      pp_statement function_definition.funBody
    in
    generate (Coq.definition identifier parameters return_type body)
  in
  let original_sail_code =
    build_list (fun { add; _ } ->
        (
          match type_constraint with
          | Some (sail_type_constraint, _) -> add sail_type_constraint
          | None                           -> ()
        );
        add sail_function_definition;
      )
  in
  Coq.annotate_with_original_definitions
    original_sail_code
    (Coq.annotate coq_definition)

let pp_function_definitions
      (function_definitions : (sail_definition * function_definition) list)
      top_level_type_constraint_definitions =
  let type_and_function_pairs =
    let find_type_constraint function_name =
      match List.filter (fun (_, type_constraint) -> type_constraint.identifier = function_name) top_level_type_constraint_definitions with
      | [x] -> Some x
      | []  -> None
      | _   -> None
    in
    List.map (fun ((_sail_definition, function_definition) as fdef) ->
        (fdef, find_type_constraint function_definition.funName))
      function_definitions
  in
  List.map (uncurry pp_function_definition) type_and_function_pairs

let pp_funDefKit
      function_definitions
      top_level_type_constraint_definitions =
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
            addall (pp_function_definitions function_definitions top_level_type_constraint_definitions);
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

let pp_program_module
      program_name
      base_name
      function_definitions
      top_level_type_constraint_definitions =
  indent (
      separate small_step [
          string ("Module Import " ^ program_name ^ "Program <: Program " ^ base_name ^ "Base.");
          FunDeclKit.generate (List.map snd function_definitions);
          string ("Include FunDeclMixin " ^ base_name ^ "Base.");
          pp_funDefKit function_definitions top_level_type_constraint_definitions;
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
      | TD_abbreviation (identifier, type_abbreviation) ->
         (
           match type_abbreviation with
           | TA_numeric_expression numexpr ->
              let* numexpr' = Sail.pp_numeric_expression numexpr
              in
              generate (
                  separate space [
                      string "Definition";
                      string identifier;
                      string ":=";
                      numexpr'
                    ] ^^ Coq.eol
                )
           | TA_numeric_constraint numconstraint ->
              let* numconstraint' = Sail.pp_numeric_constraint numconstraint
              in
              generate (
                  separate space [
                      string "Definition";
                      string identifier;
                      string ":=";
                      numconstraint'
                    ] ^^ Coq.eol
                )
         )
    in
    Coq.annotate_with_original_definition original (Coq.annotate document)
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

let _generate_module_header title =
  generate (string (Printf.sprintf "(*** %s ***)" title))

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
      (
        pp_program_module
          ir.program_name
          "Default"
          ir.function_definitions
          ir.top_level_type_constraint_definitions
      )
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
