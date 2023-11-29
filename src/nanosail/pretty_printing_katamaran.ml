open PPrint
open Ast
open Util
open Pputil

module PP = struct
  include PPrint
  
  module Coq = Coq_generation

  module Katamaran = struct
    module Registers = Pp_registers
  end
end

module S = struct
  include Sail_util
end


let opt_list_notations = ref false

let include_original_sail_code = ref false

let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)

let annotate_with_original_definition original translation =
  if
    !include_original_sail_code
  then
    concat [
      PP.Coq.comment (pp_sail_definition original);
      hardline;
      translation
    ]
  else
    translation


(******************************************************************************)
(* Base pretty printing *)

let defaultBase = string "Import DefaultBase."


(******************************************************************************)
(* FunDeclKit pretty printing *)

let pp_funDeclKit function_definitions =
  let pp_function_declaration function_definition =
    let name = string function_definition.funName
    and function_type =
      let parameter_types = PP.Coq.list (List.map S.pp_bind function_definition.funType.arg_types)
      and return_type = S.pp_ty function_definition.funType.ret_type
      in
      concat [
        string "Fun";
        space;
        align (
          group (
            concat [
              parameter_types;
              break 1;
              return_type
            ]
          )
        )
      ]
    in
    (name, function_type)
  in
  let inductive_type_declaration =
    let name = string "Fun"
    and typ = string "PCtx -> Ty -> Set"
    in
    PP.Coq.build_inductive_type name typ (fun add_constructor ->
        List.iter
          (fun function_definition ->
            let name, typ = pp_function_declaration function_definition
            in
            add_constructor ~typ:typ name
          )
          function_definitions
      )
  in
  let contents =
    separate small_step [
        inductive_type_declaration;
        separate_map hardline utf8string [
            "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
            "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
            "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
          ]
      ]
  in
  PP.Coq.section "FunDeclKit" contents

(******************************************************************************)
(* Value pretty printing *)

let rec pp_value = function
  | Val_unit          -> string "tt"
  | Val_bool b        -> string (string_of_bool b)
  | Val_int i         -> PP.Coq.integer i
  | Val_string s      -> dquotes (string s)
  | Val_prod (v1, v2) -> PP.Coq.product (pp_value v1) (pp_value v2)
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
    | Val_int n      -> simple_app [string "exp_int"; PP.Coq.integer n]
    | Val_string s   -> simple_app [string "exp_string"; dquotes (string s)]
    | v -> simple_app [
               string "exp_val";
               S.pp_ty (ty_of_val v);
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
        then PP.Coq.list (List.map pp_expression l)
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
      PP.Coq.list (List.map S.pp_bind function_definition.funType.arg_types);
      S.pp_ty function_definition.funType.ret_type
    ]
  in
  let body =
    pp_statement function_definition.funBody
  in
  annotate_with_original_definition original_sail_code (
    PP.Coq.definition identifier parameters return_type body
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
      PP.Coq.match' matched_expression cases
    in
    PP.Coq.definition identifier parameters return_type body
  in
  let contents =
    separate small_step (
        build_list (fun { add; addall } ->
            addall (pp_function_definitions function_definitions);
            add fundef
          )
      )
  in
  PP.Coq.section "FunDefKit" contents


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
  PP.Coq.section title contents


(******************************************************************************)
(* Program pretty printing *)

let pp_program_module program_name base_name function_definitions =
  indent (separate small_step [
    string ("Module Import " ^ program_name ^ "Program <: Program " ^ base_name ^ "Base.");
    pp_funDeclKit (List.map snd function_definitions);
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
          S.pp_numeric_expression numexpr;
          PP.Coq.eol
        ]
    in
    annotate_with_original_definition original document
  in
  List.map (uncurry pp_type_definition) type_definitions


(******************************************************************************)
(* Enums pretty printing *)

let pp_enums (enum_definitions : (sail_definition * enum_definition) list) =
  let pp_enum sail_definition enum_definition =
    let coq_translation =
      let identifier = string enum_definition.enum_identifier
      and typ = string "Set"
      in
      PP.Coq.build_inductive_type identifier typ (fun add_constructor ->
          List.iter
            (fun (case : string) ->
              add_constructor (string case)
            )
            enum_definition.enum_cases
        )
    in
    annotate_with_original_definition sail_definition coq_translation
  in
  List.map (uncurry pp_enum) enum_definitions

(******************************************************************************)
(* Untranslated definition pretty printing *)

let pp_untranslated_module untranslated_definitions =
  let pp_sail_location (location : Libsail.Parse_ast.l) =
    match location with
    | Libsail.Parse_ast.Range (start, stop) ->
       if start.pos_fname = stop.pos_fname
       then (
         if start.pos_lnum = stop.pos_lnum
         then
           Printf.sprintf "%s line %d chars %d-%d"
             start.pos_fname
             start.pos_lnum
             (start.pos_cnum - start.pos_bol)
             (stop.pos_cnum - stop.pos_bol)
         else
           Printf.sprintf "%s from line %d:%d to line %d:%d"
             start.pos_fname
             start.pos_lnum
             (start.pos_cnum - start.pos_bol)
             stop.pos_lnum
             (stop.pos_cnum - stop.pos_bol)
       )
       else S.string_of_location location
    | _ -> S.string_of_location location
  in
  let pp_untranslated_definition (original : sail_definition) (untranslated_definition : untranslated_definition) =
    let { filename; line_number; sail_location; message } = untranslated_definition in
    let ocaml_location_string = Printf.sprintf "OCaml location: %s line %d" filename line_number in
    let sail_location_string = Printf.sprintf "Sail location: %s" (pp_sail_location sail_location) in
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
  PP.Coq.comment (
      separate small_step (
          List.map (uncurry pp_untranslated_definition) untranslated_definitions
    ))


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

let fromIR_pp ?(show_untranslated=false) ir =
  let more_modules =
    if !opt_list_notations
    then List.append more_modules ["ListNotations"]
    else more_modules
  in
  let generate_section segments =
    [ separate small_step segments ]
  in
  let heading =
    let require_imports =
      List.map (uncurry PP.Coq.require_imports) (imports ())
    in
    let imports =
      [
        PP.Coq.imports more_modules
      ]
    in
    let scopes =
      [
        PP.Coq.open_scopes scopes
      ]
    in
    let parts =
      build_list (fun { addall; _ } ->
          addall require_imports;
          addall imports;
          addall scopes
        )
    in
    generate_section parts
  in
  let base =
    let segments =
      build_list (fun { add; addall } ->
          add (pp_module_header "TYPES");
          add defaultBase;
          addall (pp_type_module ir.type_definitions);
          addall (pp_enums ir.enum_definitions)
        )
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
          PP.Katamaran.Registers.pp_register_module ir.register_definitions
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
