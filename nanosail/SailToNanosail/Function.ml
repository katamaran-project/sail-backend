module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module N = Ast
module TC = TranslationContext

open Base
open Monads.Notations.Star(TC)
open Identifier
open Nanotype


let type_from_lvar (lvar : S.typ S.Ast_util.lvar) (loc : S.l) : S.typ TC.t =
  match lvar with
  | S.Ast_util.Register t   -> TC.return t
  | S.Ast_util.Enum t       -> TC.return t
  | S.Ast_util.Local (_, t) -> TC.return t
  | S.Ast_util.Unbound _    -> TC.not_yet_implemented [%here] loc


let rec binds_of_pat (S.P_aux (aux, ((location, _annotation) as annotation))) =
  match aux with
  | P_lit (L_aux (lit, _loc)) ->
     begin
       match lit with
       | S.L_unit     -> TC.return @@ [("()", N.Ty_unit)]
       | S.L_zero     -> TC.not_yet_implemented [%here] location
       | S.L_one      -> TC.not_yet_implemented [%here] location
       | S.L_true     -> TC.not_yet_implemented [%here] location
       | S.L_false    -> TC.not_yet_implemented [%here] location
       | S.L_num _    -> TC.not_yet_implemented [%here] location
       | S.L_hex _    -> TC.not_yet_implemented [%here] location
       | S.L_bin _    -> TC.not_yet_implemented [%here] location
       | S.L_string _ -> TC.not_yet_implemented [%here] location
       | S.L_undef    -> TC.not_yet_implemented [%here] location
       | S.L_real _   -> TC.not_yet_implemented [%here] location
     end
  | P_id id -> begin
      let* x  = translate_identifier [%here] id in
      let* ty = nanotype_of_sail_type @@ Libsail.Type_check.typ_of_annot annotation
      in
      TC.return [(x, ty)]
    end
  | P_tuple pats -> begin
      let* pats' = TC.map ~f:binds_of_pat pats
      in
      TC.return @@ List.concat pats'
    end
  | S.P_wild -> begin
      let* typ = nanotype_of_sail_type @@ Libsail.Type_check.typ_of_annot annotation
      and* id  = TC.generate_unique_identifier "_"
      in
      TC.return [(id, typ)]
    end
  | S.P_typ (_typ, pattern)       -> binds_of_pat pattern (* parameter is annotated with type, e.g., function foo(x : int) = { } *)
  | S.P_or (_, _)                 -> TC.not_yet_implemented [%here] location
  | S.P_not _                     -> TC.not_yet_implemented [%here] location
  | S.P_as (_, _)                 -> TC.not_yet_implemented [%here] location
  | S.P_var (_, _)                -> TC.not_yet_implemented [%here] location
  | S.P_app (_, _)                -> TC.not_yet_implemented [%here] location
  | S.P_vector _                  -> TC.not_yet_implemented [%here] location
  | S.P_vector_concat _           -> TC.not_yet_implemented [%here] location
  | S.P_vector_subrange (_, _, _) -> TC.not_yet_implemented [%here] location
  | S.P_list _                    -> TC.not_yet_implemented [%here] location
  | S.P_cons (_, _)               -> TC.not_yet_implemented [%here] location
  | S.P_string_append _           -> TC.not_yet_implemented [%here] location
  | S.P_struct (_, _)             -> TC.not_yet_implemented [%here] location


let binds_of_pexp (pexp : N.type_annotation Libsail.Ast.pexp) =
  let S.Pat_aux (aux, (location, _annotation)) = pexp
  in
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _       -> TC.not_yet_implemented [%here] location


let value_of_literal (S.L_aux (literal, location)) =
  match literal with
  | L_true     -> TC.return @@ N.Val_bool true
  | L_false    -> TC.return @@ N.Val_bool false
  | L_num n    -> TC.return @@ N.Val_int n
  | L_unit     -> TC.return @@ N.Val_unit
  | L_string s -> TC.return @@ N.Val_string s
  | S.L_zero   -> TC.not_yet_implemented [%here] location
  | S.L_one    -> TC.not_yet_implemented [%here] location
  | S.L_hex _  -> TC.not_yet_implemented [%here] location
  | S.L_bin _  -> TC.not_yet_implemented [%here] location
  | S.L_undef  -> TC.not_yet_implemented [%here] location
  | S.L_real _ -> TC.not_yet_implemented [%here] location


let flatten_named_statements
      (named_statements : (string * N.statement) list list) : (string * N.statement) list =
  let flattened = List.concat named_statements
  in
  let statement_names = List.map ~f:fst flattened
  in
  if List.contains_dup statement_names ~compare:String.compare
  then failwith "BUG: two statements bear the same name"
  else flattened


(*
  Sail has only expressions, microSail makes the distinction between statements and expressions.
  Also, microSail statements can evaluate to a value, just like expressions do.

  Some Sail expressions need to be translated into microSail statements (e.g., reading from a register).

  This function returns a pair:

  - The first element, of type N.expression, is the part of the Sail expression that fits in a microSail expression
  - The second element, a list of "named statements" (i.e., pairs of identifiers and statements), are the parts of the Sail expression
  that were translated into microSail statements. Since the evaluation result of a statement can be referred in the resulting microSail expression,
  we also name each statement.

  For example, a result

  (expr, [("a", s1); ("b"; s2)])

  should be interpreted as

    let a = s1 in
    let b = s2 in
    expr
*)
let rec expression_of_aval
          (location : S.l         )
          (value    : S.typ S.aval) : (N.expression * (string * N.statement) list) TC.t
  =
  let expression_of_tuple
        (elements : S.typ S.aval list)
    =
    if List.is_empty elements
    then TC.not_yet_implemented ~message:"Encountered empty tuple; should not occur" [%here] location
    else begin
        let* translation_pairs = TC.map ~f:(expression_of_aval location) elements
        in
        let translation_expressions = List.map ~f:fst translation_pairs
        and translation_statements  = List.map ~f:snd translation_pairs
        and make_pair x y           = N.Exp_binop (Pair, x, y)
        in
        let resulting_expression    = Auxlib.reduce ~f:make_pair translation_expressions
        in
        TC.return (resulting_expression, flatten_named_statements translation_statements)
      end

  and expression_of_literal
        (literal : S.lit)
        (_typ    : S.typ)
    =
    let* lit' = value_of_literal literal
    in
    TC.return @@ (N.Exp_val lit', [])

  and expression_of_identifier
        (identifier : S.id                       )
        (lvar       : S.typ Libsail.Ast_util.lvar)
    =
    let* id' = translate_identifier [%here] identifier
    in
    match lvar with
    | Local (_, _) -> TC.return @@ (N.Exp_var id', [])
    | Register _   -> TC.not_yet_implemented [%here] location (* todo return named statement *)
    | Enum _       -> TC.not_yet_implemented [%here] location
    | Unbound _    -> TC.not_yet_implemented [%here] location

  and expression_of_list
        (list : S.typ S.aval list)
        (_typ : S.typ            )
    =
    let* translation_pairs = TC.map ~f:(expression_of_aval location) list
    in
    let translation_expressions, translation_statements =
      List.unzip translation_pairs
    in
    TC.return @@ (N.Exp_list translation_expressions, flatten_named_statements translation_statements)

  in
  match value with
  | AV_tuple elements     -> expression_of_tuple elements
  | AV_lit (literal, typ) -> expression_of_literal literal typ
  | AV_id (id, lvar)      -> expression_of_identifier id lvar
  | AV_list (list, typ)   -> expression_of_list list typ
  | AV_ref (_, _)         -> TC.not_yet_implemented [%here] location
  | AV_vector (_, _)      -> TC.not_yet_implemented [%here] location
  | AV_record (_, _)      -> TC.not_yet_implemented [%here] location
  | AV_cval (_, _)        -> TC.not_yet_implemented [%here] location


let make_sequence statements location =
  let rec aux statements =
    match statements with
    | []    -> TC.not_yet_implemented ~message:"Should not happen" [%here]  location
    | [x]   -> TC.return x
    | x::xs -> begin
        let* xs' = aux xs
        in
        TC.return @@ N.Stm_seq (x, xs')
      end
  in
  aux statements


(*
  Given a list of named statements (i.e., pairs of strings and statements)
  and a statement, generates nested let bindings for each of the named statements
  and puts statement in the center.

  For example, say
  - named_statements = [("a", s1), ("b", s2)]
  - statement        = stm
  
  This function then returns

    let a = s1 in
    let b = s2 in
    stm
*)
let rec wrap_in_named_statements_context
      (named_statements : (string * N.statement) list)
      (statement        : N.statement                ) : N.statement =
  match named_statements with
  | (name, stm)::rest -> Stm_let (name, stm, wrap_in_named_statements_context rest statement)
  | []                -> statement


let rec statement_of_aexp (expression : S.typ S.aexp) =
  let S.AE_aux (expression, _environment, location) = expression
  in

  let statement_of_match
        (location : S.l                                              )
        (matched  : S.typ S.aval                                     )
        (cases    : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp) list) : N.statement TC.t =
    let rec match_list () =
      let* () =
        let error_message =
          lazy "matching list; expected exactly two cases"
        in
        TC.check [%here] (List.length cases = 2) error_message
      in
      
      let* nil_case, cons_case =
        match cases with
        | [ (AP_aux (AP_nil  _, _, _), _, _) as nil_case;
            (AP_aux (AP_cons _, _, _), _, _) as cons_case ] -> TC.return (nil_case, cons_case)
        | [ (AP_aux (AP_cons _, _, _), _, _) as cons_case;
            (AP_aux (AP_nil  _, _, _), _, _) as nil_case  ] -> TC.return (nil_case, cons_case)
        | _                                                 -> TC.fail [%here] "unrecognized cases; should be nil and cons"
      in

      match nil_case, cons_case with
      | ( (AP_aux (AP_nil _, _, _), _, nil_clause),
          (AP_aux (AP_cons (
                       AP_aux (AP_id (id_h, _), _, _),
                       AP_aux (AP_id (id_t, _), _, _)
                     ), _, _), _, cons_clause) ) -> begin
          let* matched =
            let* expression, named_statements = expression_of_aval location matched
            in
            TC.return @@ wrap_in_named_statements_context named_statements @@ N.Stm_exp expression

          and* when_nil = statement_of_aexp nil_clause

          and* when_cons =
            let* id_head = translate_identifier [%here] id_h
            and* id_tail = translate_identifier [%here] id_t
            and* clause = statement_of_aexp cons_clause
            in
            TC.return (id_head, id_tail, clause)
          in
          let match_pattern =
            N.MP_list {
                matched;
                when_cons;
                when_nil;
              }
          in
          TC.return @@ N.Stm_match match_pattern
        end
      | _ -> TC.fail [%here] "list cases do not have expected structure"

    and match_tuple () =
      let* () =
        let n_cases = List.length cases
        in
        let error_message = lazy (Printf.sprintf "match tuple; expected only one case, got %d" n_cases)
        in
        TC.check [%here] (n_cases = 1) error_message
      in

      match cases with
      | [ (AP_aux (AP_tuple [
                       AP_aux (AP_id (id_l, _), _, _);
                       AP_aux (AP_id (id_r, _), _, _);
                     ], _, _),_ , clause) ] -> begin
          let* (matched, named_statements) =
            let* expr, named_statements = expression_of_aval location matched
            in
            TC.return (N.Stm_exp expr, named_statements)
          and* id_fst = translate_identifier [%here] id_l
          and* id_snd = translate_identifier [%here] id_r
          and* body   = statement_of_aexp clause
          in
          TC.return @@ wrap_in_named_statements_context named_statements @@ N.Stm_match (N.MP_product { matched; id_fst; id_snd; body })
        end
      | _ -> TC.not_yet_implemented [%here] location

    and match_type_by_identifier (type_identifier : S.id) =
      let S.Id_aux (type_identifier, location) = type_identifier
      in

      match type_identifier with
      | S.Id id -> begin
          let* lookup_result = TC.lookup_type Ast.Extract.of_anything id
          in
          match lookup_result with
          | Some (TD_abbreviation def) -> match_abbreviation def
          | Some (TD_variant def)      -> match_variant def
          | Some (TD_enum def)         -> match_enum def
          | Some (TD_record def)       -> match_record def
          | None                       -> TC.fail [%here] @@ Printf.sprintf "Unknown type %s" id
        end
      | S.Operator _ -> TC.not_yet_implemented [%here] location

    and match_enum (enum_definition : N.enum_definition) =
      (* at this point we know that matched is a value of an enum described by the parameter enum_definition *)
      let* () =
        let n_match_cases = List.length cases
        and n_enum_cases = List.length enum_definition.cases
        in
        let error_message = lazy begin
                                let enum_values = String.concat ~sep:", " enum_definition.cases
                                in
                                Printf.sprintf
                                  "expected fewer or as many match cases (%d) as there are enum values (%d: %s)"
                                  n_match_cases
                                  n_enum_cases
                                  enum_values
                              end
        in
        TC.check [%here] (n_match_cases <= n_enum_cases) error_message
      in

      let process_case
            (table      : N.statement StringMap.t                     )
            (match_case : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp)) : N.statement StringMap.t TC.t =
        let (AP_aux (pattern, _, _), _, body) = match_case
        in

        match pattern with
        | S.AP_id (S.Id_aux (id, location), _typ) -> begin
            match id with
            | S.Operator  _   -> TC.not_yet_implemented [%here] location
            | S.Id identifier -> begin
                let* () =
                  let error_message = lazy begin
                                          Printf.sprintf
                                            "encountered unknown case %s while matching an %s value"
                                            identifier
                                            enum_definition.identifier
                                        end
                  in
                  TC.check
                    [%here]
                    (List.mem enum_definition.cases identifier ~equal:String.equal)
                    error_message
                in
                let* body' = statement_of_aexp body
                in
                let result = StringMap.add table ~key:identifier ~data:body'
                in
                match result with
                | `Duplicate -> TC.fail
                                  [%here]
                                  (Printf.sprintf "duplicate case %s in enum match" identifier)
                | `Ok table' -> TC.return table'
              end
          end
        | S.AP_wild S.Typ_aux (_, _loc) -> begin
            let* body' = statement_of_aexp body
            in
            let add_case table case =
              match StringMap.add table ~key:case ~data:body' with
              | `Duplicate -> table   (* wildcard only fills in missing cases, so ignore if there's already an entry for this enum case *)
              | `Ok table' -> table'
            in
            TC.return @@ List.fold_left enum_definition.cases ~init:table ~f:add_case
          end
        | S.AP_tuple _        -> TC.fail [%here] "unexpected case while matching on enum"
        | S.AP_global (_, _)  -> TC.fail [%here] "unexpected case while matching on enum"
        | S.AP_app (_, _, _)  -> TC.fail [%here] "unexpected case while matching on enum"
        | S.AP_cons (_, _)    -> TC.fail [%here] "unexpected case while matching on enum"
        | S.AP_as (_, _, _)   -> TC.fail [%here] "unexpected case while matching on enum"
        | S.AP_struct (_, _)  -> TC.fail [%here] "unexpected case while matching on enum"
        | S.AP_nil _          -> TC.fail [%here] "unexpected case while matching on enum"
      in
      let* (matched, named_statements) =
        let* matched_expression, named_statements = expression_of_aval location matched
        in
        TC.return @@ (N.Stm_exp matched_expression, named_statements)

      and* cases = TC.fold_left ~f:process_case ~init:StringMap.empty cases
      in
      let match_statement = N.Stm_match (N.MP_enum {
                                             matched;
                                             cases
                              })
      in
      TC.return @@ wrap_in_named_statements_context named_statements match_statement

    and match_variant (_variant_definition : N.variant_definition) =
      TC.not_yet_implemented [%here] location

    and match_abbreviation (_type_abbreviation : N.type_abbreviation_definition) =
      TC.not_yet_implemented [%here] location

    and match_record (_record_definition : N.record_definition) =
      (* at this point we know that matched is a value of an enum described by the parameter enum_definition *)
      TC.not_yet_implemented [%here] location

    and match_typed (Typ_aux (type_of_matched, location) : S.typ) =
      match type_of_matched with
      | S.Typ_app (Id_aux (Id "list", _), _) -> match_list ()
      | S.Typ_tuple _                        -> match_tuple ()
      | S.Typ_id id                          -> match_type_by_identifier id
      | S.Typ_internal_unknown               -> TC.not_yet_implemented [%here] location
      | S.Typ_var _                          -> TC.not_yet_implemented [%here] location
      | S.Typ_fn (_, _)                      -> TC.not_yet_implemented [%here] location
      | S.Typ_bidir (_, _)                   -> TC.not_yet_implemented [%here] location
      | S.Typ_app (_, _)                     -> TC.not_yet_implemented [%here] location
      | S.Typ_exist (_, _, _)                -> TC.not_yet_implemented [%here] location

    in
    match matched with
    | S.AV_id (_id, lvar) -> begin
        match lvar with (* todo replace by type_from_lvar *)
        | S.Ast_util.Local (_mut, typ) -> match_typed typ
        | S.Ast_util.Register typ      -> match_typed typ
        | S.Ast_util.Enum typ          -> match_typed typ
        | S.Ast_util.Unbound _         -> TC.not_yet_implemented [%here] location
      end
    | S.AV_lit (_, _)    -> TC.not_yet_implemented [%here] location
    | S.AV_ref (_, _)    -> TC.not_yet_implemented [%here] location
    | S.AV_tuple _       -> TC.not_yet_implemented [%here] location
    | S.AV_list (_, _)   -> TC.not_yet_implemented [%here] location
    | S.AV_vector (_, _) -> TC.not_yet_implemented [%here] location
    | S.AV_record (_, _) -> TC.not_yet_implemented [%here] location
    | S.AV_cval (_, _)   -> TC.not_yet_implemented [%here] location

  and statement_of_field_access
        (location         : S.l         )
        (aval             : S.typ S.aval)
        (field_identifier : S.id        )
        (_field_type      : S.typ       )
    =
    let* field_identifier = translate_identifier [%here] field_identifier
    in
    match aval with
    | S.AV_id (record_identifier, lvar) -> begin
        let* record_identifier = translate_identifier [%here] record_identifier
        and* S.Typ_aux (t, _loc) = type_from_lvar lvar location
        in
        match t with
        | S.Typ_id record_type_identifier -> begin
            let* record_type_identifier = translate_identifier [%here] record_type_identifier
            in
            let* lookup_result = TC.lookup_type N.Extract.of_record record_type_identifier
            in
            match lookup_result with
            | Some record_type_definition -> begin
                let field_identifiers = List.map ~f:fst record_type_definition.fields
                in
                match Auxlib.find_index_of ~f:(String.equal field_identifier) field_identifiers with
                | Some selected_field_index -> begin
                    let* receiving_variables = TC.map ~f:TC.generate_unique_identifier field_identifiers
                    in
                    let expression = N.Exp_record_field_access {
                                         record_identifier;
                                         receiving_variables;
                                         selected_field_index;
                                       }
                    in
                    TC.return @@ N.Stm_exp expression
                  end
                | None -> TC.fail [%here] @@ Printf.sprintf "Record %s should have field named %s" record_type_identifier field_identifier
              end
            | None -> TC.fail [%here] @@ Printf.sprintf "Tried looking up %s; expected to find record type definition" record_type_identifier
          end
        | S.Typ_internal_unknown -> TC.not_yet_implemented [%here] location
        | S.Typ_var _            -> TC.not_yet_implemented [%here] location
        | S.Typ_fn (_, _)        -> TC.not_yet_implemented [%here] location
        | S.Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] location
        | S.Typ_tuple _          -> TC.not_yet_implemented [%here] location
        | S.Typ_app (_, _)       -> TC.not_yet_implemented [%here] location
        | S.Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] location
      end
    | S.AV_lit (_, _)    -> TC.not_yet_implemented [%here] location
    | S.AV_ref (_, _)    -> TC.not_yet_implemented [%here] location
    | S.AV_tuple _       -> TC.not_yet_implemented [%here] location
    | S.AV_list (_, _)   -> TC.not_yet_implemented [%here] location
    | S.AV_vector (_, _) -> TC.not_yet_implemented [%here] location
    | S.AV_record (_, _) -> TC.not_yet_implemented [%here] location
    | S.AV_cval (_, _)   -> TC.not_yet_implemented [%here] location

  and statement_of_value
        (value : S.typ S.aval) =
    let* expression, named_statements = expression_of_aval location value
    in
    TC.return @@ wrap_in_named_statements_context named_statements @@ N.Stm_exp expression

  and statement_of_application
          (receiver_identifier : S.id             )
          (arguments           : S.typ S.aval list)
          (_typ                : S.typ            ) =
    let* id' = translate_identifier [%here] receiver_identifier
    in
    match arguments with
    | [car; cdr] when String.equal id' "sail_cons" -> begin
        let* car', car_named_statements = expression_of_aval location car
        and* cdr', cdr_named_statements = expression_of_aval location cdr
        in
        let named_statements = flatten_named_statements [ car_named_statements; cdr_named_statements ]
        in
        TC.return @@ wrap_in_named_statements_context named_statements @@ N.Stm_exp (Exp_binop (Cons, car', cdr'))
      end
    | _ -> begin
        let* pairs = TC.map ~f:(expression_of_aval location) arguments
        in
        let argument_expressions, named_statements_list = List.unzip pairs
        in
        let named_statements = flatten_named_statements named_statements_list
        in
        TC.return @@ wrap_in_named_statements_context named_statements @@ N.Stm_call (id', argument_expressions)
      end

  and statement_of_let
        (_mutability : Libsail.Ast_util.mut)
        (identifier  : S.id                )
        (_typ1       : S.typ               )
        (expression  : S.typ S.aexp        )
        (body        : S.typ S.aexp        )
        (_typ2       : S.typ               )
    =
    let* id' = translate_identifier [%here] identifier
    and* s1  = statement_of_aexp expression
    and* s2  = statement_of_aexp body
    in
    TC.return @@ N.Stm_let (id', s1, s2)

  and statement_of_if
        (condition   : S.typ S.aval)
        (then_clause : S.typ S.aexp)
        (else_clause : S.typ S.aexp)
        (_typ        : S.typ       )
    =
    let* (condition, condition_named_statements) =
      let* condition_expression, named_statements = expression_of_aval location condition
      in
      TC.return (N.Stm_exp condition_expression, named_statements)
    and* when_true = statement_of_aexp then_clause
    and* when_false = statement_of_aexp else_clause
    in
    TC.return @@ wrap_in_named_statements_context condition_named_statements @@ N.Stm_match (MP_bool { condition; when_true; when_false })

  and statement_of_block
        (statements     : S.typ S.aexp list)
        (last_statement : S.typ S.aexp     )
        (_typ           : S.typ            )
    =
    let* translated_statements = TC.map ~f:statement_of_aexp (statements @ [last_statement])
    in
    make_sequence translated_statements location

  in
  match expression with
  | AE_val value                                                  -> statement_of_value value
  | AE_app (id, avals, typ)                                       -> statement_of_application id avals typ
  | AE_let (mutability, identifier, typ1, expression, body, typ2) -> statement_of_let mutability identifier typ1 expression body typ2
  | AE_if (condition, then_clause, else_clause, typ)              -> statement_of_if condition then_clause else_clause typ
  | AE_match (aval, cases, _)                                     -> statement_of_match location aval cases
  | AE_block (statements, last_statement, typ)                    -> statement_of_block statements last_statement typ
  | AE_field (aval, field_identifier, field_type)                 -> statement_of_field_access location aval field_identifier field_type
  | AE_struct_update (_aval, _bindings, _typ) -> begin
      TC.not_yet_implemented [%here] location
    end
  | S.AE_typ (_, _)                                               -> TC.not_yet_implemented [%here] location
  | S.AE_assign (_, _)                                            -> TC.not_yet_implemented [%here] location
  | S.AE_return (_, _)                                            -> TC.not_yet_implemented [%here] location
  | S.AE_exit (_, _)                                              -> TC.not_yet_implemented [%here] location
  | S.AE_throw (_, _)                                             -> TC.not_yet_implemented [%here] location
  | S.AE_try (_, _, _)                                            -> TC.not_yet_implemented [%here] location
  | S.AE_for (_, _, _, _, _, _)                                   -> TC.not_yet_implemented [%here] location
  | S.AE_loop (_, _, _)                                           -> TC.not_yet_implemented [%here] location
  | S.AE_short_circuit (_, _, _)                                  -> TC.not_yet_implemented [%here] location


let body_of_pexp pexp =
  let S.Pat_aux (aux, (location, _annot)) = pexp
  in
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (S.anf exp)
  | Pat_when _       -> TC.not_yet_implemented [%here] location


let ty_of_pexp (pexp : N.type_annotation Libsail.Ast.pexp) =
  let S.Pat_aux (aux, (location, _annot)) = pexp
  in
  match aux with
  | Pat_exp (_, exp) -> nanotype_of_sail_type (Libsail.Type_check.typ_of exp)
  | Pat_when _       -> TC.not_yet_implemented [%here] location


let translate_function_definition
      (definition_annotation : S.def_annot               )
      (function_definition   : N.type_annotation S.fundef)
  =
  let S.FD_aux ((FD_function (_, _, funcls)), _) = function_definition
  in
  match funcls with
  | [funcl] -> begin
      let S.FCL_aux (S.FCL_funcl (id, pexp), (_def_annot, _type_annotation)) = funcl
      in
      let* function_name = translate_identifier [%here] id
      and* arg_types     = binds_of_pexp pexp
      and* ret_type      = ty_of_pexp pexp
      and* function_body = body_of_pexp pexp
      in
      TC.return @@ N.FunctionDefinition {
        function_name;
        function_type = {
            arg_types;
            ret_type;
          };
        function_body;
      }
    end
  | _ -> TC.not_yet_implemented [%here] definition_annotation.loc
