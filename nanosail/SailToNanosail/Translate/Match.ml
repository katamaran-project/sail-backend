open! Base


module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module TC = TranslationContext

open Monads.Notations.Star(TC)


module Pattern = struct
  type t =
    | ListCons of { head_pattern : t; tail_pattern : t }
    | ListNil
    | Tuple    of t list
    | EnumCase of Ast.Identifier.t
    | Variable of Ast.Identifier.t
    | Wildcard

  let rec to_fexpr (pattern : t) : FExpr.t =
    let head id =
      Printf.sprintf "Pattern:%s" id
    in
    match pattern with
    | ListCons { head_pattern; tail_pattern } -> begin
        let keyword =
          [
            ("head", to_fexpr head_pattern);
            ("tail", to_fexpr tail_pattern);
          ]
        in
        FExpr.mk_application ~keyword @@ head "Cons"
      end
    | ListNil -> FExpr.mk_symbol "Nil"
    | Tuple subpatterns -> begin
        let positional =
          List.map ~f:to_fexpr subpatterns
        in
        FExpr.mk_application ~positional @@ head "Tuple"
      end
    | EnumCase identifier -> begin
        let positional =
          [
            Ast.Identifier.to_fexpr identifier
          ]
        in
        FExpr.mk_application ~positional @@ head "EnumCase"
      end
    | Variable identifier -> begin
        let positional =
          [
            Ast.Identifier.to_fexpr identifier
          ]
        in
        FExpr.mk_application ~positional @@ head "Variable"
      end
    | Wildcard -> FExpr.mk_symbol "Wildcard"
end


let rec translate_pattern
    (matched_type : Ast.Type.t  )
    (sail_pattern : S.typ S.apat) : Pattern.t TC.t
  =
  let S.AP_aux (unwrapped_sail_pattern, _type_environment, location) = sail_pattern
  in
  match unwrapped_sail_pattern with
  | AP_cons (head_pattern, tail_pattern) -> begin
      match matched_type with
      | List element_type -> begin
          let* head_pattern = translate_pattern element_type head_pattern
          and* tail_pattern = translate_pattern matched_type tail_pattern
          in
          TC.return @@ Pattern.ListCons { head_pattern; tail_pattern }
        end
      | _ -> TC.fail [%here] "expected list type"
    end
  | AP_nil _typ -> begin
      match matched_type with
      | List _ -> begin
          TC.return @@ Pattern.ListNil
        end
      | _ -> TC.fail [%here] "expected list type"
    end
  | AP_id (identifier, _typ) -> begin
      let* identifier = Identifier.translate_identifier [%here] identifier
      in
      match matched_type with
      | Enum enum_identifier -> begin
          let* enum_definition =
            TC.lookup_type_definition_of_kind Ast.Definition.Select.of_enum enum_identifier
          in
          match enum_definition with
          | Some enum_definition -> begin
              if
                List.mem enum_definition.cases identifier ~equal:Ast.Identifier.equal
              then
                TC.return @@ Pattern.EnumCase identifier
              else
                TC.return @@ Pattern.Variable identifier
            end
          | None -> TC.fail [%here] @@ Printf.sprintf "inconsistency: unknown enum type %s" @@ Ast.Identifier.to_string enum_identifier
        end
      | _ -> TC.return @@ Pattern.Variable identifier
    end
  | AP_wild _typ -> TC.return @@ Pattern.Wildcard
  | AP_tuple subpatterns -> begin
      let aux subpatterns subpattern_types =
        match List.zip subpattern_types subpatterns with
        | Ok pairs -> begin
            let* translated_subpatterns =
              TC.map ~f:(Auxlib.uncurry translate_pattern) pairs
            in
            TC.return @@ Pattern.Tuple translated_subpatterns
          end
        | Unequal_lengths -> TC.fail [%here] "expected as many types as patterns in tuple"
      in
      match matched_type with
      | Tuple subpattern_types -> aux subpatterns subpattern_types
      | Product (t1, t2) -> aux subpatterns [t1; t2]
      | _ -> begin
          let error_message =
            Printf.sprintf "expected tuple or product type, instead got: %s" @@ FExpr.to_string @@ Ast.Type.to_fexpr matched_type
          in
          TC.fail [%here] error_message
        end
    end
  | AP_global (_, _) -> TC.not_yet_implemented [%here] location
  | AP_app (_, _, _) -> TC.not_yet_implemented [%here] location
  | AP_as (_, _, _)  -> TC.not_yet_implemented [%here] location
  | AP_struct (_, _) -> TC.not_yet_implemented [%here] location
  

let translate_case
    (location       : S.l            )
    (matched_type   : Ast.Type.t     )
    (sail_pattern   : S.typ S.apat   )
    (sail_condition : S.typ S.aexp   )
    (body           : Ast.Statement.t) : (Pattern.t * Ast.Statement.t) TC.t
  =
  let* pattern = translate_pattern matched_type sail_pattern
  in
  let S.AE_aux (unwrapped_sail_condition, _) = sail_condition
  in
  (* Check that the condition is simply true; we expect this to be the case if the correct rewrites have been activated *)
  let* () =
    match unwrapped_sail_condition with
    | AE_val value -> begin
        match value with
        | AV_lit (literal, _) -> begin
            let S.L_aux (unwrapped_literal, _) = literal
            in
            match unwrapped_literal with
             | L_unit            -> TC.not_yet_implemented [%here] location
             | L_zero            -> TC.not_yet_implemented [%here] location
             | L_one             -> TC.not_yet_implemented [%here] location
             | L_true            -> TC.return ()
             | L_false           -> TC.not_yet_implemented [%here] location
             | L_num _           -> TC.not_yet_implemented [%here] location
             | L_hex _           -> TC.not_yet_implemented [%here] location
             | L_bin _           -> TC.not_yet_implemented [%here] location
             | L_string _        -> TC.not_yet_implemented [%here] location
             | L_undef           -> TC.not_yet_implemented [%here] location
             | L_real _          -> TC.not_yet_implemented [%here] location
          end
        | AV_id (_, _)           -> TC.not_yet_implemented [%here] location
        | AV_ref (_, _)          -> TC.not_yet_implemented [%here] location
        | AV_tuple _             -> TC.not_yet_implemented [%here] location
        | AV_list (_, _)         -> TC.not_yet_implemented [%here] location
        | AV_vector (_, _)       -> TC.not_yet_implemented [%here] location
        | AV_record (_, _)       -> TC.not_yet_implemented [%here] location
        | AV_cval (_, _)         -> TC.not_yet_implemented [%here] location
      end
    | AE_app (_, _, _)           -> TC.not_yet_implemented [%here] location
    | AE_typ (_, _)              -> TC.not_yet_implemented [%here] location
    | AE_assign (_, _)           -> TC.not_yet_implemented [%here] location
    | AE_let (_, _, _, _, _, _)  -> TC.not_yet_implemented [%here] location
    | AE_block (_, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_return (_, _)           -> TC.not_yet_implemented [%here] location
    | AE_exit (_, _)             -> TC.not_yet_implemented [%here] location
    | AE_throw (_, _)            -> TC.not_yet_implemented [%here] location
    | AE_if (_, _, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_field (_, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_match (_, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_try (_, _, _)           -> TC.not_yet_implemented [%here] location
    | AE_struct_update (_, _, _) -> TC.not_yet_implemented [%here] location
    | AE_for (_, _, _, _, _, _)  -> TC.not_yet_implemented [%here] location
    | AE_loop (_, _, _)          -> TC.not_yet_implemented [%here] location
    | AE_short_circuit (_, _, _) -> TC.not_yet_implemented [%here] location
  in
  Stdio.print_endline @@ FExpr.to_string @@ Pattern.to_fexpr pattern;
  TC.return (pattern, body)


let translate
    (location           : S.l                                              )
    (matched_identifier : Ast.Identifier.t                                 )
    (matched_type       : Ast.Type.t                                       )
    (cases              : (S.typ S.apat * S.typ S.aexp * Ast.Statement.t) list) : unit TC.t
  =
  let* translated_cases =
    let f (pattern, condition, clause) =
      translate_case location matched_type pattern condition clause
    in
    TC.map ~f cases
  in
  TC.return ()
