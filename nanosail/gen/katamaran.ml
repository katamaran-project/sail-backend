open PPrint
open Ast
open Auxlib
open Util
open Monad

module FunDeclKit = Fundeclkit


let defaultBase = string "Import DefaultBase."



(* Having a separate aux function is completely redundant, but it allows us to easily modify pp_value's name *)
let pp_value =
  let rec aux = function
    | Val_unit          -> string "tt"
    | Val_bool b        -> string (string_of_bool b)
    | Val_int i         -> Coq.integer i
    | Val_string s      -> dquotes (string s)
    | Val_prod (v1, v2) -> Coq.product (aux v1) (aux v2)
  in
  aux


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

(* Having a separate aux function is completely redundant, but it allows us to easily modify ty_of_val's name *)
let ty_of_val =
  let rec aux = function
    | Val_unit          -> Ty_unit
    | Val_bool _        -> Ty_bool
    | Val_int _         -> Ty_int
    | Val_string _      -> Ty_string
    | Val_prod (v1, v2) -> Ty_tuple [aux v1; aux v2]
  in
  aux

let rec pp_expression e =
  let rec pp_exp_list = function
    | []      -> generate @@ string "nil"
    | x :: xs ->
       let* x'  = pp_par_expression x
       and* xs' = pp_exp_list xs
       in
       generate @@ parens @@ simple_app [string "cons"; x'; xs']
  in
  let pp_exp_val = function
    | Val_bool true        -> generate @@ string "exp_true"
    | Val_bool false       -> generate @@ string "exp_false"
    | Val_int n            -> generate @@ simple_app [string "exp_int"; Coq.integer n]
    | Val_string s         -> generate @@ simple_app [string "exp_string"; dquotes (string s)]
    | Val_unit             -> generate @@ simple_app [string "exp_val"; string "ty.unit"; string "tt"]
    | Val_prod (_, _) as v -> begin
        let* tuple_type' = Sail.pp_nanotype (ty_of_val v)
        in
        let value' = pp_value v
        in
        generate @@ simple_app [
          string "exp_val";
          tuple_type';
          value'
        ]
      end
  in
  let pp_exp_binop bo e1 e2 =
    let* e1' = pp_par_expression e1
    and* e2' = pp_par_expression e2
    in
    match bo with
    | Pair ->
       generate @@ simple_app [
         string "exp_binop";
         string "bop.pair";
         e1';
         e2'
       ]
    | Cons ->
       generate @@ simple_app [
         string "exp_binop";
         string "bop.cons";
         e1';
         e2'
       ]
    | Append ->
       generate @@ simple_app [
         string "exp_binop";
         string "bop.append";
         e1';
         e2'
       ]
    | _  ->
       generate @@ infix 2 1 (pp_infix_binOp bo) e1' e2'
  in
  match e with
  | Exp_var v              -> generate @@ simple_app [string "exp_var"; dquotes (string v)]
  | Exp_val v              -> pp_exp_val v
  | Exp_neg e              -> let* e' = pp_par_expression e in generate @@ string "- " ^^ e'
  | Exp_not e              -> let* e' = pp_par_expression e in generate @@ simple_app [string "exp_not"; e']
  | Exp_binop (bo, e1, e2) -> pp_exp_binop bo e1 e2
  | Exp_list lst           ->
    begin
      let* lst' =
        if
          Configuration.(get use_list_notations)
        then
          let* expressions = map pp_expression lst
          in
          generate @@ Coq.list expressions
        else
          pp_exp_list lst
      in
      generate @@ simple_app [string "exp_list"; lst']
    end

and pp_par_expression e =
  let* e' = pp_expression e
  in
  generate @@ parens e'


(******************************************************************************)
(* Statement pretty printing *)

let rec pp_statement statement =
  match statement with
  | Stm_exp e ->
     let* e' = pp_par_expression e
     in
     generate @@ simple_app [string "stm_exp"; e']

  | Stm_match_list m ->
     let* m_s' = pp_par_statement m.s
     and* m_alt_nil' = pp_par_statement m.alt_nil
     and* m_alt_cons' = pp_par_statement m.alt_cons
     in
     generate @@
         simple_app [
             string "stm_match_list";
             m_s';
             m_alt_nil';
             dquotes (string m.xh);
             dquotes (string m.xt);
             m_alt_cons'
           ]

  | Stm_match_prod m ->
     let* m_s'   = pp_par_statement m.s
     and* m_rhs' = pp_par_statement m.rhs
     in
     generate @@
         simple_app [
             string "stm_match_prod";
             m_s';
             dquotes (string m.xl);
             dquotes (string m.xr);
             m_rhs'
           ]

  | Stm_call (f, arg_list) ->
     let* arg_list' = map pp_par_expression arg_list
     in
     generate @@ simple_app @@ string "call" :: string f :: arg_list'

  | Stm_let (v, s1, s2) ->
     let* s1' = pp_statement s1
     and* s2' = pp_statement s2
     in
     generate @@
         simple_app [
             string ("let: \"" ^ v ^ "\" :=");
             s1';
             string "in";
             s2'
           ]

  | Stm_if (s, s1, s2) ->
     let* s'  = pp_par_statement s
     and* s1' = pp_par_statement s1
     and* s2' = pp_par_statement s2
     in
     generate @@
         simple_app [
             string "stm_if";
             s';
             s1';
             s2'
           ]

  | Stm_seq (s1, s2) ->
     let* s1' = pp_par_statement s1
     and* s2' = pp_par_statement s2
     in
     generate @@ simple_app [ string "stm_seq"; s1'; s2' ]

  | Stm_nys -> not_yet_implemented __POS__

and pp_par_statement s = lift parens (pp_statement s)


(******************************************************************************)
(* FunDefKit pretty printing *)

let pp_function_definition
      ((sail_function_definition : sail_definition), (function_definition : function_definition))
      type_constraint =
  let identifier = Sail.pp_identifier @@ "fun_" ^ function_definition.funName in
  let parameters = [] in
  let coq_definition =
    let* result_type =
      let* bindings =
        let* docs = map Sail.pp_bind function_definition.funType.arg_types
        in
        generate @@ Coq.list docs
      in
      let* result_type =
        Sail.pp_nanotype function_definition.funType.ret_type
      in
      generate @@ Some (
                      pp_hanging_list (PP.string "Stm") [
                          bindings;
                          result_type
                        ]
                    )
    in
    let* body =
      pp_statement function_definition.funBody
    in
    generate @@ Coq.definition ~identifier ~parameters ~result_type ~body
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
      match
        List.filter
          (fun (_, type_constraint) -> type_constraint.identifier = function_name)
          top_level_type_constraint_definitions
      with
      | [x] -> Some x
      | []  -> None
      | _   -> None
    in
    List.map (fun ((_sail_definition, function_definition) as fdef) ->
        (fdef, find_type_constraint function_definition.funName))
      function_definitions
  in
  List.map (uncurry pp_function_definition) type_and_function_pairs

let pp_function_definition_kit
      function_definitions
      top_level_type_constraint_definitions =
  let fundef =
    let identifier = Sail.pp_identifier "FunDef"
    and parameters = [
        utf8string "{Δ τ}";
        utf8string "(f : Fun Δ τ)"
      ]
    and result_type = Some (utf8string "Stm Δ τ")
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
    Coq.definition ~identifier ~parameters ~result_type ~body
  in
  let contents =
    separate small_step (
        build_list (fun { add; addall } ->
            addall @@ pp_function_definitions function_definitions top_level_type_constraint_definitions;
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
  let flag            = Coq.Import
  and identifier      = program_name ^ "Program"
  and base_identifier = base_name ^ "Base" in
  let includes        = [ "Program"; base_identifier ]
  and contents =
    separate (twice hardline) [
      FunDeclKit.generate @@ List.map snd function_definitions;
      Coq.line @@ string @@ "Include FunDeclMixin " ^ base_identifier;
      pp_function_definition_kit function_definitions top_level_type_constraint_definitions;
      Coq.line @@ string @@"Include DefaultRegStoreKit " ^ base_identifier;
      pp_foreignKit;
      Coq.line @@ string @@ "Include ProgramMixin " ^ base_identifier;
    ]
  in
  Coq.module'
    ~flag:flag
    ~includes:includes
    identifier
    contents



(******************************************************************************)
(* Full pretty printing *)

let pp_module_header title =
  string (Printf.sprintf "(*** %s ***)" title)

let _generate_module_header title =
  generate @@ string @@ Printf.sprintf "(*** %s ***)" title

let fromIR_pp ir =
  let prelude =
    Prelude.generate ()
  in
  let generate_section title contents =
    string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents
  in
  let base =
    let translated_types_enums_and_variants =
      let translate_type_enum_or_variant
            (sail_definition : sail_definition)
            (definition      : definition     ) =
        match definition with
        | TypeDefinition def                 -> Some (Types.pp_type_definition sail_definition def)
        | VariantDefinition def              -> Some (Variants.generate_inductive_type sail_definition def)
        | EnumDefinition def                 -> Some (Enums.generate_inductive_type sail_definition def)
        | TopLevelTypeConstraintDefinition _ -> None
        | FunctionDefinition _               -> None
        | RegisterDefinition _               -> None
        | UntranslatedDefinition _           -> None
        | IgnoredDefinition                  -> None
      in
      List.filter_map (uncurry translate_type_enum_or_variant) ir.definitions
    in
    let translated_enum_definitions =
      let enum_definitions = select Extract.enum_definition ir.definitions
      in
      build_list @@ fun { add; addall } -> begin
                        addall @@ List.map (uncurry Enums.generate_constructors_inductive_type) enum_definitions;
                        if not (List.is_empty enum_definitions)
                        then begin
                            add @@ Enums.generate_enum_of_enums enum_definitions;
                            add @@ Enums.generate_eqdecs enum_definitions;
                            add @@ Enums.generate_no_confusions enum_definitions;
                          end
                     end
    and translated_variant_definitions =
      List.map
        (uncurry Variants.generate_inductive_type)
        (select Extract.variant_definition ir.definitions)
    in
    let segments =
      build_list (fun { add; addall } ->
          add    @@ pp_module_header "TYPES";
          add    @@ defaultBase;
          addall @@ translated_types_enums_and_variants;
          addall @@ translated_enum_definitions;
          addall @@ translated_variant_definitions;
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
          (select Extract.function_definition ir.definitions)
          (select Extract.top_level_type_constraint_definition ir.definitions)
      )
  in
  let registers =
    if
      List.is_empty @@ select Extract.register_definition ir.definitions
    then
      empty
    else
      generate_section "REGISTERS" @@ Registers.generate @@ select Extract.register_definition ir.definitions
  in
  let untranslated =
    if
      Configuration.(get include_untranslated_definitions)
    then
      generate_section "UNTRANSLATED" @@ Untranslated.generate @@ select Extract.untranslated_definition ir.definitions
    else
      empty
  in
  let ignored =
    if
      Configuration.(get include_ignored_definitions)
    then
      generate_section "IGNORED" @@ Ignored.generate @@ List.map fst @@ select Extract.ignored_definition ir.definitions
    else
      empty
  in
  let sections =
    [
      prelude;
      base;
      program;
      registers;
      untranslated;
      ignored
    ]
  in
  Util.separate_nonempty big_step sections


let pretty_print len out doc = ToChannel.pretty 1. len out (doc ^^ small_step)
