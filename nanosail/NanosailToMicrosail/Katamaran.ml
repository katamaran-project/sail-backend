open Base
open PPrint
open Ast
open Auxlib
open Util
open Monads.Notations.Star(AnnotationMonad)


module AC = AnnotationMonad

let defaultBase = string "Import DefaultBase."



(* Having a separate aux function is completely redundant, but it allows us to easily modify pp_value's name *)
let pp_value =
  let rec aux = function
    | Val_unit          -> string "tt"
    | Val_bool b        -> string (Bool.to_string b)
    | Val_int i         -> Coq.integer i
    | Val_string s      -> dquotes (string s)
    | Val_prod (v1, v2) -> Coq.product (aux v1) (aux v2)
  in
  aux


(******************************************************************************)
(* Expression pretty printing *)

let pp_infix_binOp (binary_operator : binary_operator) =
  match binary_operator with
  | Plus   -> AC.return @@ plus
  | Times  -> AC.return @@ star
  | Minus  -> AC.return @@ minus
  | And    -> AC.return @@ twice ampersand
  | Or     -> AC.return @@ twice bar
  | Eq     -> AC.return @@ equals
  | Neq    -> AC.return @@ bang ^^ equals
  | Le     -> AC.return @@ langle ^^ equals
  | Lt     -> AC.return @@ langle
  | Ge     -> AC.return @@ rangle ^^ equals
  | Gt     -> AC.return @@ rangle
  | Pair   -> AC.not_yet_implemented [%here] (* Should not occur *)
  | Cons   -> AC.not_yet_implemented [%here] (* Should not occur *)
  | Append -> AC.not_yet_implemented [%here] (* Should not occur *)


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
    | []      -> AC.return @@ string "nil"
    | x :: xs ->
       let* x'  = pp_par_expression x
       and* xs' = pp_exp_list xs
       in
       AC.return @@ parens @@ simple_app [string "cons"; x'; xs']
  in
  let pp_exp_val = function
    | Val_bool true        -> AC.return @@ string "exp_true"
    | Val_bool false       -> AC.return @@ string "exp_false"
    | Val_int n            -> AC.return @@ simple_app [string "exp_int"; Coq.integer n]
    | Val_string s         -> AC.return @@ simple_app [string "exp_string"; dquotes (string s)]
    | Val_unit             -> AC.return @@ simple_app [string "exp_val"; string "ty.unit"; string "tt"]
    | Val_prod (_, _) as v -> begin
        let* tuple_type' = Sail.pp_nanotype (ty_of_val v)
        in
        let value' = pp_value v
        in
        AC.return @@ simple_app [
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
       AC.return @@ simple_app [
         string "exp_binop";
         string "bop.pair";
         e1';
         e2'
       ]
    | Cons ->
       AC.return @@ simple_app [
         string "exp_binop";
         string "bop.cons";
         e1';
         e2'
       ]
    | Append ->
       AC.return @@ simple_app [
         string "exp_binop";
         string "bop.append";
         e1';
         e2'
       ]
    | _  ->
      let* binop' = pp_infix_binOp bo
      in
      AC.return @@ infix 2 1 binop' e1' e2'
  in
  match e with
  | Exp_var v              -> AC.return @@ simple_app [string "exp_var"; dquotes (string v)]
  | Exp_val v              -> pp_exp_val v
  | Exp_neg e              -> let* e' = pp_par_expression e in AC.return @@ string "- " ^^ e'
  | Exp_not e              -> let* e' = pp_par_expression e in AC.return @@ simple_app [string "exp_not"; e']
  | Exp_binop (bo, e1, e2) -> pp_exp_binop bo e1 e2
  | Exp_list lst           ->
    begin
      let* lst' =
        if
          Configuration.(get use_list_notations)
        then
          let* expressions = AC.map pp_expression lst
          in
          AC.return @@ Coq.list expressions
        else
          pp_exp_list lst
      in
      AC.return @@ simple_app [string "exp_list"; lst']
    end

and pp_par_expression e =
  let* e' = pp_expression e
  in
  AC.return @@ parens e'


(******************************************************************************)
(* Statement pretty printing *)

let rec pp_statement statement =
  match statement with
  | Stm_exp e ->
     let* e' = pp_par_expression e
     in
     AC.return @@ simple_app [string "stm_exp"; e']

  | Stm_match match_pattern -> begin
      match match_pattern with
      | MP_list { matched; when_nil; when_cons } -> begin
          let id_head, id_tail, when_cons_body = when_cons
          in
          let* matched'   = pp_par_statement matched
          and* when_nil'  = pp_par_statement when_nil
          and* when_cons' = pp_par_statement when_cons_body
          in
          AC.return @@ simple_app [
            string "stm_match_list";
            matched';
            when_nil';
            dquotes (string id_head);
            dquotes (string id_tail);
            when_cons';
          ]
        end

      | MP_product { matched; id_fst; id_snd; body } -> begin
          let* matched' = pp_par_statement matched
          and* body'    = pp_par_statement body
          in
          AC.return @@ simple_app [
            string "stm_match_prod";
            matched';
            dquotes (string id_fst);
            dquotes (string id_snd);
            body';
          ]
        end

      | MP_bool { condition; when_true; when_false } -> begin
          let* condition'  = pp_par_statement condition
          and* when_true'  = pp_par_statement when_true
          and* when_false' = pp_par_statement when_false
          in
          AC.return @@ simple_app [
            string "stm_if";
            condition';
            when_true';
            when_false'
          ]
        end

      | MP_enum { matched; cases } -> begin
          let translate_case ~(key:string) ~(data:statement) (acc : document list AC.t) =
            let* acc
            and* pattern = AC.return @@ string key
            and* clause = pp_statement data
            in
            AC.return @@ separate space [
              string "|";
              pattern;
              string " => ";
              clause
            ] :: acc
          in
          let* matched' = pp_par_statement matched
          and* cases' = StringMap.fold cases ~init:(AC.return []) ~f:translate_case
          in
          AC.return @@ separate hardline @@ build_list @@ fun { add; addall } -> begin
            add @@ Coq.comment @@ string "TODO Fix this";
            add @@ separate space [ string "match"; matched'; string "with" ];
            addall cases'
          end
        end
    end

  | Stm_call (f, arg_list) ->
     let* arg_list' = AC.map pp_par_expression arg_list
     in
     AC.return @@ simple_app @@ string "call" :: string f :: arg_list'

  | Stm_let (v, s1, s2) ->
     let* s1' = pp_statement s1
     and* s2' = pp_statement s2
     in
     AC.return @@
         simple_app [
             string ("let: \"" ^ v ^ "\" :=");
             s1';
             string "in";
             s2'
           ]

  | Stm_seq (s1, s2) ->
     let* s1' = pp_par_statement s1
     and* s2' = pp_par_statement s2
     in
     AC.return @@ simple_app [ string "stm_seq"; s1'; s2' ]

and pp_par_statement s =
  let* s' = pp_statement s
  in
  AC.return @@ parens s'


(******************************************************************************)
(* FunDefKit pretty printing *)

let pp_function_definition
      ((sail_function_definition : sail_definition), (function_definition : function_definition))
      type_constraint =
  let identifier = Sail.pp_identifier @@ "fun_" ^ function_definition.function_name in
  let parameters = [] in
  let coq_definition =
    let* result_type =
      let* bindings =
        let* docs = AC.map Sail.pp_bind function_definition.function_type.arg_types
        in
        AC.return @@ Coq.list docs
      in
      let* result_type =
        Sail.pp_nanotype function_definition.function_type.ret_type
      in
      AC.return @@ Some (
                      pp_hanging_list (PP.string "Stm") [
                          bindings;
                          result_type
                        ]
                    )
    in
    let* body =
      pp_statement function_definition.function_body
    in
    AC.return @@ Coq.definition ~identifier ~parameters ~result_type ~body
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
          ~f:(fun (_, type_constraint) -> String.equal type_constraint.identifier function_name)
          top_level_type_constraint_definitions
      with
      | [x] -> Some x
      | []  -> None
      | _   -> None
    in
    List.map ~f:(fun ((_sail_definition, function_definition) as fdef) ->
        (fdef, find_type_constraint function_definition.function_name))
      function_definitions
  in
  List.map ~f:(uncurry pp_function_definition) type_and_function_pairs

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
            string function_definition.function_name,
            string (Printf.sprintf "fun_%s" function_definition.function_name)
          )
        in
        List.map ~f:case_of_function_definition (List.map ~f:snd function_definitions)
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
      FunDeclKit.generate @@ List.map ~f:snd function_definitions;
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
  AC.return @@ string @@ Printf.sprintf "(*** %s ***)" title

let fromIR_pp ir =
  let prelude =
    Prelude.generate ()
  in
  let generate_section title contents =
    string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents
  in
  let base =
    let translated_type_definitions =
      let type_definitions = select Extract.type_definition ir.definitions
      in
      List.map ~f:(uncurry Types.pp_type_definition) type_definitions
    in
    let extra_enum_definitions =
      let enum_definitions = select Extract.enum_definition ir.definitions
      in
      if List.is_empty enum_definitions
      then []
      else [
        Types.Enums.generate_enum_of_enums enum_definitions;
        Types.Enums.generate_eqdecs enum_definitions;
        Types.Enums.generate_no_confusions enum_definitions;
      ]
    in
    let segments =
      build_list (fun { add; addall } ->
          add    @@ pp_module_header "TYPES";
          add    @@ defaultBase;
          addall @@ translated_type_definitions;
          addall @@ extra_enum_definitions;
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
      generate_section "IGNORED" @@ Ignored.generate @@ List.map ~f:fst @@ select Extract.ignored_definition ir.definitions
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
