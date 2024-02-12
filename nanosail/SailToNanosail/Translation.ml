open Base
open Basics

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast

module TC = TranslationContext
open Monads.Notations.Star(TC)
open Identifier
open Numeric


let type_from_lvar (lvar : S.typ S.Ast_util.lvar) (loc : S.l) : S.typ TC.t =
  match lvar with
  | S.Ast_util.Register t   -> TC.return t
  | S.Ast_util.Enum t       -> TC.return t
  | S.Ast_util.Local (_, t) -> TC.return t
  | S.Ast_util.Unbound _    -> TC.not_yet_implemented [%here] loc




let ty_of_pexp (S.Pat_aux (aux, (location, _annot))) =
  match aux with
  | Pat_exp (_, exp) -> nanotype_of_sail_type (Libsail.Type_check.typ_of exp)
  | Pat_when _       -> TC.not_yet_implemented [%here] location


let translate_expression (expression : N.type_annotation S.exp) : N.statement TC.t =
  let S.E_aux (expression, (location, _type_annotation)) = expression
  in
  match expression with
  | S.E_lit literal -> begin
     let S.L_aux (literal, _literal_location) = literal
     in
     match literal with
      | S.L_num n    -> TC.return @@ N.Stm_exp (N.Exp_val (N.Val_int n))
      | S.L_unit     -> TC.not_yet_implemented [%here] location
      | S.L_zero     -> TC.not_yet_implemented [%here] location
      | S.L_one      -> TC.not_yet_implemented [%here] location
      | S.L_true     -> TC.not_yet_implemented [%here] location
      | S.L_false    -> TC.not_yet_implemented [%here] location
      | S.L_hex _    -> TC.not_yet_implemented [%here] location
      | S.L_bin _    -> TC.not_yet_implemented [%here] location
      | S.L_string _ -> TC.not_yet_implemented [%here] location
      | S.L_undef    -> TC.not_yet_implemented [%here] location
      | S.L_real _   -> TC.not_yet_implemented [%here] location
    end
  | S.E_block _                                -> TC.not_yet_implemented [%here] location
  | S.E_id _                                   -> TC.not_yet_implemented [%here] location
  | S.E_typ (_, _)                             -> TC.not_yet_implemented [%here] location
  | S.E_app (_, _)                             -> TC.not_yet_implemented [%here] location
  | S.E_app_infix (_, _, _)                    -> TC.not_yet_implemented [%here] location
  | S.E_tuple _                                -> TC.not_yet_implemented [%here] location
  | S.E_if (_, _, _)                           -> TC.not_yet_implemented [%here] location
  | S.E_loop (_, _, _, _)                      -> TC.not_yet_implemented [%here] location
  | S.E_for (_, _, _, _, _, _)                 -> TC.not_yet_implemented [%here] location
  | S.E_vector _                               -> TC.not_yet_implemented [%here] location
  | S.E_vector_access (_, _)                   -> TC.not_yet_implemented [%here] location
  | S.E_vector_subrange (_, _, _)              -> TC.not_yet_implemented [%here] location
  | S.E_vector_update (_, _, _)                -> TC.not_yet_implemented [%here] location
  | S.E_vector_update_subrange (_, _, _, _)    -> TC.not_yet_implemented [%here] location
  | S.E_vector_append (_, _)                   -> TC.not_yet_implemented [%here] location
  | S.E_list _                                 -> TC.not_yet_implemented [%here] location
  | S.E_cons (_, _)                            -> TC.not_yet_implemented [%here] location
  | S.E_struct _                               -> TC.not_yet_implemented [%here] location
  | S.E_struct_update (_, _)                   -> TC.not_yet_implemented [%here] location
  | S.E_field (_, _)                           -> TC.not_yet_implemented [%here] location
  | S.E_match (_, _)                           -> TC.not_yet_implemented [%here] location
  | S.E_let (_, _)                             -> TC.not_yet_implemented [%here] location
  | S.E_assign (_, _)                          -> TC.not_yet_implemented [%here] location
  | S.E_sizeof _                               -> TC.not_yet_implemented [%here] location
  | S.E_return _                               -> TC.not_yet_implemented [%here] location
  | S.E_exit _                                 -> TC.not_yet_implemented [%here] location
  | S.E_ref _                                  -> TC.not_yet_implemented [%here] location
  | S.E_throw _                                -> TC.not_yet_implemented [%here] location
  | S.E_try (_, _)                             -> TC.not_yet_implemented [%here] location
  | S.E_assert (_, _)                          -> TC.not_yet_implemented [%here] location
  | S.E_var (_, _, _)                          -> TC.not_yet_implemented [%here] location
  | S.E_internal_plet (_, _, _)                -> TC.not_yet_implemented [%here] location
  | S.E_internal_return _                      -> TC.not_yet_implemented [%here] location
  | S.E_internal_value _                       -> TC.not_yet_implemented [%here] location
  | S.E_internal_assume (_, _)                 -> TC.not_yet_implemented [%here] location
  | S.E_constraint _                           -> TC.not_yet_implemented [%here] location


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
  | S.P_typ (_typ, pattern)       -> begin
      (*
         parameter is annotated with type, e.g., function foo(x : int) = { }
      *)
      binds_of_pat pattern
    end


let binds_of_pexp (S.Pat_aux (aux, (location, _annotation))) =
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _ -> TC.not_yet_implemented [%here] location


let value_of_lit (S.L_aux (literal, location)) =
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


let rec expression_of_aval location (value : S.typ S.aval) =
  match value with
  | AV_tuple elts ->
     begin
       match elts with
       | [] -> TC.not_yet_implemented ~message:"Should not occur" [%here] location
       | h::t -> begin
           let* e_h = expression_of_aval location h
           in
           let f e1 aval2 =
             let* e2 = expression_of_aval location aval2
             in
             TC.return @@ N.Exp_binop (Pair, e1, e2) in
           TC.fold_left ~f:f ~init:e_h t
         end
     end
  | AV_lit (lit, _)   -> begin
      let* lit' = value_of_lit lit
      in
      TC.return @@ N.Exp_val lit'
    end
  | AV_id (id, _lvar) -> begin
      let* id' = translate_identifier [%here] id
      in
      TC.return @@ N.Exp_var id'
    end
  | AV_list (lst, _)  -> begin
      let* lst' = TC.map ~f:(expression_of_aval location) lst
      in
      TC.return @@ N.Exp_list lst'
    end
  | AV_ref (_, _)     -> TC.not_yet_implemented [%here] location
  | AV_vector (_, _)  -> TC.not_yet_implemented [%here] location
  | AV_record (_, _)  -> TC.not_yet_implemented [%here] location
  | AV_cval (_, _)    -> TC.not_yet_implemented [%here] location


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


let rec statement_of_aexp (expression : S.typ S.aexp)  =
  let S.AE_aux (aux, _environment, location) =  expression
  in
  match aux with
  | AE_val aval -> begin
      let* aval' = expression_of_aval location aval
      in
      TC.return @@ N.Stm_exp aval'
    end

  | AE_app (id, avals, _) -> begin
      let* id' = translate_identifier [%here] id
      in
      match avals with
      | [aval1; aval2] when String.equal id' "sail_cons" -> begin
          let* e1 = expression_of_aval location aval1
          and* e2 = expression_of_aval location aval2
          in
          TC.return @@ N.Stm_exp (Exp_binop (Cons, e1, e2))
        end
      | _ -> begin
          let* args = TC.map ~f:(expression_of_aval location) avals
          in
          TC.return @@ N.Stm_call (id', args)
        end
    end

  | AE_let (_, id, _, aexp1, aexp2, _) -> begin
      let* id' = translate_identifier [%here] id
      and* s1 = statement_of_aexp aexp1
      and* s2 = statement_of_aexp aexp2
      in
      TC.return @@ N.Stm_let (id', s1, s2)
    end

  | AE_if (aval, aexp1, aexp2, _) -> begin
      let* condition =
        let* e = expression_of_aval location aval  (* use lift *)
        in
        TC.return @@ N.Stm_exp e
      and* when_true = statement_of_aexp aexp1
      and* when_false = statement_of_aexp aexp2
      in
      TC.return @@ N.Stm_match (MP_bool { condition; when_true; when_false })
    end

  | AE_match (aval, cases, _) -> statement_of_match location aval cases

  | S.AE_block (statements, last_statement, _type) -> begin
      let* translated_statements = TC.map ~f:statement_of_aexp (statements @ [last_statement])
      in
      make_sequence translated_statements location
    end

  (*
       record.field

     gets translated into

       let* ["field_1"; "field_2"; ...; "field_n"] = record in field_i
       
  *)
  | S.AE_field (aval, field_identifier, _field_type) -> begin
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
    end

  | S.AE_struct_update (_aval, _bindings, _typ) -> begin
      TC.not_yet_implemented [%here] location
    end

  | S.AE_typ (_, _)              -> TC.not_yet_implemented [%here] location
  | S.AE_assign (_, _)           -> TC.not_yet_implemented [%here] location
  | S.AE_return (_, _)           -> TC.not_yet_implemented [%here] location
  | S.AE_exit (_, _)             -> TC.not_yet_implemented [%here] location
  | S.AE_throw (_, _)            -> TC.not_yet_implemented [%here] location
  | S.AE_try (_, _, _)           -> TC.not_yet_implemented [%here] location
  | S.AE_for (_, _, _, _, _, _)  -> TC.not_yet_implemented [%here] location
  | S.AE_loop (_, _, _)          -> TC.not_yet_implemented [%here] location
  | S.AE_short_circuit (_, _, _) -> TC.not_yet_implemented [%here] location

and statement_of_match (location : S.l                                              )
                       (matched  : S.typ S.aval                                     )
                       (cases    : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp) list) : N.statement TC.t
  =
  let rec match_list () =
    let* () =
      let error_message = lazy "matching list; expected exactly two cases"
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
          let* expr = expression_of_aval location matched
          in
          TC.return @@ N.Stm_exp expr (* use lift *)
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
        let* matched =
          let* expr = expression_of_aval location matched
          in
          TC.return @@ N.Stm_exp expr (* use lift *)
        and* id_fst = translate_identifier [%here] id_l
        and* id_snd = translate_identifier [%here] id_r
        and* body = statement_of_aexp clause
        in
        TC.return @@ N.Stm_match (N.MP_product { matched; id_fst; id_snd; body })
      end
    | _ -> TC.not_yet_implemented [%here] location

  and match_type_by_identifier (S.Id_aux (type_identifier, location) : S.id) =
    match type_identifier with
    | S.Id id -> begin
        let* lookup_result = TC.lookup_type Ast.Extract.of_anything id
        in
        match lookup_result with
        | Some (TD_abbreviation def)  -> match_abbreviation def
        | Some (TD_variant def)       -> match_variant def
        | Some (TD_enum def)          -> match_enum def
        | Some (TD_record def)        -> match_record def
        | None                        -> TC.fail [%here] @@ Printf.sprintf "Unknown type %s" id
      end
    | S.Operator _ -> TC.not_yet_implemented [%here] location

  and match_enum (enum_definition : N.enum_definition) =
    (*
      at this point we know that matched is a value of an enum described by the parameter enum_definition
    *)
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
    let* matched =
      let* expr = expression_of_aval location matched
      in
      TC.return @@ N.Stm_exp expr (* use lift *)
    and* cases = TC.fold_left ~f:process_case ~init:StringMap.empty cases
    in
    TC.return @@ N.Stm_match (N.MP_enum {
        matched;
        cases
      })

  and match_variant (_variant_definition : N.variant_definition) =
    TC.not_yet_implemented [%here] location

  and match_abbreviation (_type_abbreviation : N.type_abbreviation_definition) =
    TC.not_yet_implemented [%here] location

  and match_record (_record_definition : N.record_definition) =
    (*
      at this point we know that matched is a value of an enum described by the parameter enum_definition
    *)
    TC.not_yet_implemented [%here] location

  (*
     called after we determine what the type of matched is
  *)
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


let body_of_pexp pexp =
  let S.Pat_aux (aux, (location, _annot)) = pexp
  in
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (S.anf exp)
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
      and* arg_types = binds_of_pexp pexp
      and* ret_type = ty_of_pexp pexp
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


let translate_kind (S.K_aux (kind, _location)) : Ast.kind TC.t =
  match kind with
  | S.K_type -> TC.return @@ Ast.Kind_type
  | S.K_int  -> TC.return @@ Ast.Kind_int
  | S.K_bool -> TC.return @@ Ast.Kind_bool


let translate_kind_id (S.Kid_aux (Var kind_id, _id_loc)) : string TC.t =
  TC.return @@ kind_id


let translate_type_quantifier_item (S.QI_aux (quantifier_item, location)) =
  match quantifier_item with
  | S.QI_id (KOpt_aux (KOpt_kind (kind, kind_id), _loc)) ->
    let* kind'    = translate_kind kind
    and* kind_id' = translate_kind_id kind_id
    in
    TC.return @@ (kind_id', kind')
  | S.QI_constraint _ -> TC.not_yet_implemented [%here] location


let translate_type_quantifier (S.TypQ_aux (quantifier, _location)) =
  match quantifier with
  | S.TypQ_tq items  -> TC.map ~f:translate_type_quantifier_item items
  | S.TypQ_no_forall -> TC.return @@ []


let translate_type_abbreviation
      _definition_annotation
      _type_annotation
      (identifier : S.id)
      (quantifier : S.typquant)
      (S.A_aux (arg, _arg_location)) : N.type_definition TC.t =
  let* quantifier' = translate_type_quantifier quantifier
  and* identifier' = translate_identifier [%here] identifier
  in
  let* type_abbreviation =
    match arg with
    | A_nexp numeric_expression -> begin
        let* numeric_expression' = translate_numeric_expression numeric_expression
        in
        TC.return @@ N.TA_numeric_expression (quantifier', numeric_expression')
      end
    | A_typ typ -> begin
        let* typ' = nanotype_of_sail_type typ
        in
        TC.return @@ N.TA_alias (quantifier', typ')
      end
    | A_bool numeric_constraint -> begin
        let* numeric_constraint' = translate_numeric_constraint numeric_constraint
        in
        TC.return @@ N.TA_numeric_constraint (quantifier', numeric_constraint')
      end
  in
  TC.return @@ N.TD_abbreviation { identifier = identifier'; abbreviation = type_abbreviation }


let translate_enum
      (_definition_annotation : S.def_annot)
      (_type_annotation       : 'a S.annot )
      (identifier             : S.id       )
      (cases                  : S.id list  ) : N.type_definition TC.t
  =
  let* identifier' = translate_identifier [%here] identifier
  and* cases'      = TC.map ~f:(translate_identifier [%here]) cases
  in
  TC.return @@ N.TD_enum {
      identifier = identifier';
      cases      = cases'     ;
    }


let translate_variant
      (_definition_annotation : S.def_annot              )
      (_type_annotation       : N.type_annotation S.annot)
      (identifier             : S.id                     )
      (type_quantifier        : S.typquant               )
      (constructors           : S.type_union list        )
      (_flag                  : bool                     ) : N.type_definition TC.t
  =
  let* identifier' = translate_identifier [%here] identifier
  and* type_quantifier' = translate_type_quantifier type_quantifier
  and* constructors' =
    let translate_constructor (S.Tu_aux (Tu_ty_id (typ, identifier), _annotation)) =
      let* identifier' = translate_identifier [%here] identifier
      and* typ' = nanotype_of_sail_type typ
      in
      TC.return @@ (identifier', typ')
    in
    TC.map ~f:translate_constructor constructors
  in
  TC.return @@ N.TD_variant {
      identifier      = identifier'     ;
      type_quantifier = type_quantifier';
      constructors    = constructors'   ;
    }


let translate_record
      (_definition_annotation : S.def_annot              )
      (_type_annotation       : N.type_annotation S.annot)
      (identifier             : S.id                     )
      (type_quantifier        : S.typquant               )
      (fields                 : (S.typ * S.id) list      ) : N.type_definition TC.t
  =
  let translate_field (field_type : S.typ) (field_identifier : S.id) =
    let* field_type'       = nanotype_of_sail_type field_type
    and* field_identifier' = translate_identifier [%here] field_identifier
    in
    TC.return @@ (field_identifier', field_type')
  in
  let* identifier      = translate_identifier [%here] identifier
  and* type_quantifier = translate_type_quantifier type_quantifier
  and* fields          = TC.map ~f:(Auxlib.uncurry translate_field) fields
  in
  TC.return @@ N.TD_record {
                   identifier;
                   type_quantifier;
                   fields
                 }


let translate_type_definition
      (definition_annotation     : S.def_annot                 )
      (annotated_type_definition : N.type_annotation S.type_def) : N.definition TC.t
  =
  let S.TD_aux (type_definition, type_annotation) = annotated_type_definition
  in
  let register translation =
    let* result = translation
    in
    let* () = TC.register_type result
    in
    TC.return @@ N.TypeDefinition result
  in
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg)                      -> register @@ translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_variant (identifier, type_quantifier, constructors, flag) -> register @@ translate_variant definition_annotation type_annotation identifier type_quantifier constructors flag
  | TD_enum (identifier, cases, _)                               -> register @@ translate_enum definition_annotation type_annotation identifier cases
  | TD_record (identifier, quantifier, fields, _)                -> register @@ translate_record definition_annotation type_annotation identifier quantifier fields
  | TD_bitfield (_, _, _)                                        -> TC.not_yet_implemented [%here] definition_annotation.loc

let translate_top_level_type_constraint
      (_definition_annotation : S.def_annot)
      (S.VS_aux (value_specification, _vspec_annotation)) : N.definition TC.t =
  let VS_val_spec (
          TypSchm_aux (
              TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)),
              _type_scheme_location),
          identifier, _extern) = value_specification
  in
  let* identifier' = translate_identifier [%here] identifier
  in
  TC.return @@ N.TopLevelTypeConstraintDefinition { identifier = identifier' }

let translate_register
      (_definition_annotation        : S.def_annot                 )
      (annotated_register_definition : N.type_annotation S.dec_spec) : N.definition TC.t
  =
  let (S.DEC_aux (DEC_reg (sail_type, identifier, expression), (_spec_location, _spec_annotation))) = annotated_register_definition
  in
  match expression with
  | None -> begin
      let* identifier' = translate_identifier [%here] identifier
      and* nanotype    = nanotype_of_sail_type sail_type
      in
      TC.return @@ N.RegisterDefinition {
          identifier = identifier';
          typ        = nanotype   ;
        }
    end
  | Some (E_aux (_expr, (location, _annotation))) -> TC.not_yet_implemented [%here] location


let translate_mapping_definition
      (_definition_annotation : S.def_annot)
      (S.MD_aux (_definition, (location, _mapping_annotation))) =
  TC.not_yet_implemented [%here] location

let translate_impl_definition
      (_definition_annotation : S.def_annot)
      (S.FCL_aux (_definition, (annot, _))) =
  TC.not_yet_implemented [%here] annot.loc

let translate_value_definition
      (_definition_annotation : S.def_annot)
      (let_definition : N.type_annotation S.letbind)
  =
  let S.LB_aux (S.LB_val (S.P_aux (pattern, (pattern_location, _)), expression), (_location, _type_annotation)) = let_definition
  in
  match pattern with
  | S.P_id identifier -> begin
      match identifier with
      | S.Id_aux (S.Id identifier, _identifier_location)  -> begin
          let* value = translate_expression expression
          in
          TC.return @@ N.ValueDefinition { identifier; value }
        end
      | S.Id_aux (S.Operator _, _) -> TC.not_yet_implemented [%here] pattern_location
    end
  | S.P_lit _                      -> TC.not_yet_implemented [%here] pattern_location
  | S.P_wild                       -> TC.not_yet_implemented [%here] pattern_location
  | S.P_or (_, _)                  -> TC.not_yet_implemented [%here] pattern_location
  | S.P_not _                      -> TC.not_yet_implemented [%here] pattern_location
  | S.P_as (_, _)                  -> TC.not_yet_implemented [%here] pattern_location
  | S.P_typ (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
  | S.P_var (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
  | S.P_app (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
  | S.P_vector _                   -> TC.not_yet_implemented [%here] pattern_location
  | S.P_vector_concat _            -> TC.not_yet_implemented [%here] pattern_location
  | S.P_vector_subrange (_, _, _)  -> TC.not_yet_implemented [%here] pattern_location
  | S.P_tuple _                    -> TC.not_yet_implemented [%here] pattern_location
  | S.P_list _                     -> TC.not_yet_implemented [%here] pattern_location
  | S.P_cons (_, _)                -> TC.not_yet_implemented [%here] pattern_location
  | S.P_string_append _            -> TC.not_yet_implemented [%here] pattern_location
  | S.P_struct (_, _)              -> TC.not_yet_implemented [%here] pattern_location


let translate_top_level_outcome_definition
      (_definition_annotation : S.def_annot)
      (S.OV_aux (_outcome, location))
      (_definitions : ('a S.def) list) =
  TC.not_yet_implemented [%here] location

let translate_definition (S.DEF_aux (def, annotation) as sail_definition) : (N.sail_definition * N.definition) TC.t =
  if
    Configuration.ignore_definition sail_definition
  then
    TC.return (sail_definition, N.IgnoredDefinition)
  else begin
    let translation =
      let* result =
        match def with
        | DEF_type type_definition                 -> translate_type_definition annotation type_definition
        | DEF_mapdef definition                    -> translate_mapping_definition annotation definition
        | DEF_impl impl_definition                 -> translate_impl_definition annotation impl_definition
        | DEF_let let_definition                   -> translate_value_definition annotation let_definition
        | DEF_val value_specification              -> translate_top_level_type_constraint annotation value_specification
        | DEF_outcome (outcome_spec, definitions)  -> translate_top_level_outcome_definition annotation outcome_spec definitions
        | DEF_register specification               -> translate_register annotation specification
        | DEF_fundef function_definition           -> translate_function_definition annotation function_definition
        | DEF_instantiation (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_fixity (_, _, _)                     -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_overload (_, _)                      -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_default _                            -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_scattered _                          -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_measure (_, _, _)                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_loop_measures (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_internal_mutrec _                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_pragma (pragma, _argument, location) -> TC.not_yet_implemented ~message:("pragma " ^ pragma) [%here] location
      in
      TC.return (sail_definition, result)
    in
    TC.recover translation begin fun error ->
      match error with
      | TC.NotYetImplemented (ocaml_location, sail_location, message) -> begin
          let untranslated_definition = N.UntranslatedDefinition {
              filename = ocaml_location.pos_fname;
              line_number = ocaml_location.pos_lnum;
              sail_location = sail_location;
              message = message
            }
          in
          TC.return (sail_definition, untranslated_definition)
        end
      | TC.AssertionFailure (ocaml_location, message) -> begin
          let location_string =
            Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
          and pretty_printed_sail_code =
            string_of_sail_definition sail_definition
          in
          failwith @@ Printf.sprintf "Assertion error at %s\nMessage: %s\nSail code:\n%s" location_string message pretty_printed_sail_code
        end
    end
  end

let translate (ast : Libsail.Type_check.tannot Libsail.Ast_defs.ast) name : N.program =
  let translate =
    let* () = Prelude.register_types ()
    in
    TC.map ~f:translate_definition ast.defs
  in
  let (result, _context) = TC.run translate
  in
  match result with
  | TC.Success definitions -> { program_name = name; definitions  = definitions }
  | TC.Failure _           -> failwith "Bug: failures should have been recovered from earlier"
