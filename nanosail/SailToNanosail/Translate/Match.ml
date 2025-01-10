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
    | ListCons of t * t
    | ListNil
    | Tuple    of t list
    | EnumCase of Ast.Identifier.t
    | Variable of Ast.Identifier.t
    | Unit

  
  let rec to_fexpr (pattern : t) : FExpr.t =
    let head id =
      Printf.sprintf "Pattern:%s" id
    in
    match pattern with
    | ListCons (head_pattern, tail_pattern) -> begin
        let keyword =
          [
            ("head", to_fexpr head_pattern);
            ("tail", to_fexpr tail_pattern);
          ]
        in
        FExpr.mk_application ~keyword @@ head "Cons"
      end
    | ListNil -> FExpr.mk_symbol @@ head "Nil"
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
    | Unit -> FExpr.mk_symbol @@ head "Unit"
end


let rec translate_pattern
    (matched_type : Ast.Type.t  )
    (sail_pattern : S.typ S.apat) : Pattern.t TC.t
  =
  let S.AP_aux (unwrapped_sail_pattern, _type_environment, _location) = sail_pattern
  in

  let translate_variable_pattern (sail_identifier : S.id) : Pattern.t TC.t =
    let* identifier = Identifier.translate_identifier [%here] sail_identifier
    in
    TC.return @@ Pattern.Variable identifier
      
  and translate_wildcard_pattern () : Pattern.t TC.t =
    let* fresh_identifier = TC.generate_unique_identifier ~underscore:true ()
    in
    TC.return @@ Pattern.Variable fresh_identifier

  and unexpected_pattern (location : Lexing.position) =
    let error_message =
      Printf.sprintf
        "expected pattern %s while matching type %s"
        (StringOf.Sail.apat sail_pattern)
        (FExpr.to_string @@ Ast.Type.to_fexpr matched_type)
    in
    TC.fail location error_message

  and translate_tuple_pattern
      (subpatterns : S.typ S.apat list)
      (subtypes    : Ast.Type.t list  ) : Pattern.t TC.t
    =
    match List.zip subtypes subpatterns with
    | Ok pairs -> begin
        let* translated_subpatterns =
          TC.map ~f:(Auxlib.uncurry translate_pattern) pairs
        in
        TC.return @@ Pattern.Tuple translated_subpatterns
      end
    | Unequal_lengths -> TC.fail [%here] "expected as many types as patterns in tuple"

  in
  let translate_for_atomic_type () =
    match unwrapped_sail_pattern with
    | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
    | AP_wild _sail_type                  -> translate_wildcard_pattern ()
    | _                                   -> unexpected_pattern [%here]

  in
  match matched_type with
  | List element_type -> begin
      match unwrapped_sail_pattern with
      | AP_cons (head_pattern, tail_pattern) -> begin
          let* head_pattern = translate_pattern element_type head_pattern
          and* tail_pattern = translate_pattern matched_type tail_pattern
          in
          TC.return @@ Pattern.ListCons (head_pattern, tail_pattern)
        end
      | AP_nil _typ                   -> TC.return @@ Pattern.ListNil
      | AP_id (identifier, _sail_typ) -> translate_variable_pattern identifier
      | AP_wild _type                 -> translate_wildcard_pattern ()
      | _                             -> unexpected_pattern [%here]
    end
  | Enum enum_identifier -> begin
      match unwrapped_sail_pattern with
      | AP_id (sail_identifier, _sail_type) -> begin
          let* identifier =
            Identifier.translate_identifier [%here] sail_identifier
          in
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
          | None -> begin
              (* This really should never happen *)
              TC.fail [%here] @@ Printf.sprintf "unknown enum type %s" @@ Ast.Identifier.to_string enum_identifier
            end          
        end
      | AP_wild _type -> translate_wildcard_pattern ()
      | _ -> unexpected_pattern [%here]
    end
  | Unit -> begin
      match unwrapped_sail_pattern with
      | AP_id (_sail_identifier, _sail_type) -> translate_wildcard_pattern ()
      | AP_wild _type                        -> translate_wildcard_pattern ()
      | _                                    -> unexpected_pattern [%here]
    end
  | Tuple subtypes -> begin
      match unwrapped_sail_pattern with
      | AP_tuple sail_subpatterns           -> translate_tuple_pattern sail_subpatterns subtypes
      | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
      | AP_wild _type                       -> translate_wildcard_pattern ()
      | _                                   -> unexpected_pattern [%here]
    end
  | Product (left_subtype, right_subtype) -> begin
      match unwrapped_sail_pattern with
      | AP_tuple sail_subpatterns -> begin
          match sail_subpatterns with
          | [ sail_left_subpattern; sail_right_subpattern ] -> translate_tuple_pattern [sail_left_subpattern; sail_right_subpattern] [left_subtype; right_subtype]
          | _                                               -> unexpected_pattern [%here]
        end
      | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
      | AP_wild _type                       -> translate_wildcard_pattern ()
      | _                                   -> unexpected_pattern [%here]
    end
  | Int                -> translate_for_atomic_type ()
  | Bool               -> translate_for_atomic_type ()
  | String             -> translate_for_atomic_type ()
  | Bit                -> translate_for_atomic_type ()
  | Sum (_, _)         -> translate_for_atomic_type ()
  | Bitvector _        -> translate_for_atomic_type ()
  | Variant _          -> translate_for_atomic_type ()
  | Record _           -> translate_for_atomic_type ()
  | Application (_, _) -> translate_for_atomic_type ()
  | Alias (_, _)       -> translate_for_atomic_type ()
  | Range (_, _)       -> translate_for_atomic_type ()
  

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


(*
   Matching lists currently only supports very specific patterns.

   * Empty and nonempty list:
   
       match list {
         [| |] => ...,
         x :: xs => ...
       }

   * Empty, singleton and 2+ list:

       match list {
         [| |] => ...,
         [| x |] => ...,
         x :: y :: rest => ...
       }

   The match cases can be in any order.
   Subpatterns are not allowed in the current implementation2.
*)
let translate_list_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (element_type       : Ast.Type.t                        )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let translate
      (matched_identifier : Ast.Identifier.t)
      (head_identifier : Ast.Identifier.t)
      (tail_identifier : Ast.Identifier.t)
      (cons_body       : Ast.Statement.t )
      (nil_body        : Ast.Statement.t ) : Ast.Statement.t TC.t
    =
    TC.return begin
      Ast.Statement.Match begin
        Ast.Statement.MatchList {
          matched      = matched_identifier                           ;
          element_type                                                ;
          when_cons    = (head_identifier, tail_identifier, cons_body);
          when_nil     = nil_body                                     ;
        }
      end
    end
  in
  let cases_sorted_by_pattern_depth =
    let rec pattern_depth (pattern : Pattern.t) : int =
      match pattern with
      | ListCons (_, tail) -> 1 + pattern_depth tail
      | ListNil            -> 0
      | Variable _         -> 0
      | _                  -> failwith "should not occur"
    in
    let compare (p1, _) (p2, _) =
      pattern_depth p1 - pattern_depth p2
    in
    List.sort cases ~compare
  in
  match cases_sorted_by_pattern_depth with
  | [ (Pattern.ListNil, nil_body);
      (Pattern.ListCons (Pattern.Variable head_identifier, Pattern.Variable tail_identifier), cons_body) ] -> begin
      translate matched_identifier head_identifier tail_identifier cons_body nil_body
    end
  | [ (Pattern.ListNil, zero_body);
      (Pattern.ListCons (Pattern.Variable first_identifier_1, Pattern.ListNil), one_body);
      (Pattern.ListCons (Pattern.Variable first_identifier_2, Pattern.ListCons (Pattern.Variable second_identifier, Pattern.Variable rest_identifier)), two_or_more_body)] -> begin
      if
        not (Ast.Identifier.equal first_identifier_1 first_identifier_2)
      then
        TC.not_yet_implemented ~message:"differently named first elements in list matching patterns" [%here] location
      else begin
        let first_identifier = first_identifier_1
        in
        let* tail_identifier =
          TC.generate_unique_identifier ()
        in
        let* inner_match =
          translate tail_identifier second_identifier rest_identifier two_or_more_body one_body
        in
        translate matched_identifier first_identifier tail_identifier inner_match zero_body
      end
    end
  | _ -> TC.not_yet_implemented [%here] location


let translate_unit_match
    (_location           : S.l                               )
    (_matched_identifier : Ast.Identifier.t                  )
    (cases               : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  match cases with
  | [ (Pattern.Unit, body) ]       -> TC.return body
  | [ (Pattern.Variable _, body) ] -> TC.return body                                  
  | [ (pattern, _) ]               -> TC.fail [%here] @@ Printf.sprintf "unexpected pattern %s" (FExpr.to_string @@ Pattern.to_fexpr pattern)
  | []                             -> TC.fail [%here] "expected exactly one pattern; got zero"
  | _ :: _ :: _                    -> TC.fail [%here] @@ Printf.sprintf "expected exactly one pattern; got %d" (List.length cases)


let translate_enum_match
    (_location           : S.l                              )
    (matched_identifier : Ast.Identifier.t                  )
    (enum_identifier    : Ast.Identifier.t                  )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  (* Look up enum definition, we need to know which values there are *)
  let* enum_definition = TC.lookup_type_definition_of_kind Ast.Definition.Select.of_enum enum_identifier
  in
  match enum_definition with
  | None -> begin
      TC.fail [%here] @@ Printf.sprintf "unknown enum type %s" (Ast.Identifier.to_string enum_identifier)
    end
  | Some enum_definition -> begin
      (*
         Set up case table: it maps enum values to corresponding bodies
         Note that enum values are represented using identifiers.
      *)
      let* case_table : Ast.Statement.t Ast.Identifier.Map.t =
        let process_case
            (table : Ast.Statement.t Ast.Identifier.Map.t)
            (case  : Pattern.t * Ast.Statement.t         ) : Ast.Statement.t Ast.Identifier.Map.t TC.t
          =
          let pattern, body = case
          in
          match pattern with
          | EnumCase enum_value_identifier -> begin
              (* A pattern matches a specific enum value *)
              match Ast.Identifier.Map.add table ~key:enum_value_identifier ~data:body with
              | `Duplicate -> begin
                  (* This case shouldn't occur. It means that two patterns match the same enum value, making the second one redundant *)
                  TC.fail [%here] "same enum case matched against twice"
                end
              | `Ok updated_table -> begin
                  (* We add the (enum value, clause) association to the table *)
                  TC.return updated_table
                end
            end
          | Variable _ -> begin
              (*
                 The pattern binds the enum value to a variable, meaning
                 it should match all enum values that have hitherto not been processed.
              *)
              let fill_in_missing_case
                  (table                 : Ast.Statement.t Ast.Identifier.Map.t)
                  (enum_value_identifier : Ast.Identifier.t                    ) : Ast.Statement.t Ast.Identifier.Map.t
                =
                match Ast.Identifier.Map.add table ~key:enum_value_identifier ~data:body with
                | `Duplicate        -> begin
                    (*
                       We tried to add an extra (enum value, clause) association to the table, but there
                       already existed one, which is okay. We simply keep the table as is.
                    *)
                    table
                  end
                | `Ok updated_table -> updated_table
              in
              (*
                 We go through all possible enum values and try to add associations for them to the table, i.e.,
                 we only add associations for enum values that are missing from the table.
              *)
              TC.return @@ List.fold enum_definition.cases ~init:table ~f:fill_in_missing_case
            end
          | _ -> TC.fail [%here] @@ Printf.sprintf "unexpected pattern while dealing with enum match: %s" (FExpr.to_string @@ Pattern.to_fexpr pattern)
        in
        TC.fold_left cases ~init:Ast.Identifier.Map.empty ~f:process_case
      in
      (* Check that all enum cases have been handled *)
      let all_enum_cases_handled =
        List.for_all enum_definition.cases ~f:(Ast.Identifier.Map.mem case_table)
      in
      if
        not all_enum_cases_handled
      then
        (*
           Some enum values were not handled by the match.
           For now, we simply fail, but we could instead fill up the gaps with failure statements.
        *)
        TC.fail [%here] "not all enum cases are handled"
      else begin
        let match_pattern =
          Ast.Statement.MatchEnum {
            matched = matched_identifier;
            matched_type = enum_identifier;
            cases = case_table
          }
        in
        TC.return @@ Ast.Statement.Match match_pattern
      end
    end


let translate
    (location           : S.l                                                 )
    (matched_identifier : Ast.Identifier.t                                    )
    (matched_type       : Ast.Type.t                                          )
    (cases              : (S.typ S.apat * S.typ S.aexp * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let* translated_cases =
    let f (pattern, condition, clause) =
      translate_case location matched_type pattern condition clause
    in
    TC.map ~f cases
  in
  match matched_type with
  | List element_type    -> translate_list_match location matched_identifier element_type translated_cases
  | Unit                 -> translate_unit_match location matched_identifier translated_cases
  | Enum enum_identifier -> translate_enum_match location matched_identifier enum_identifier translated_cases
  | Product (_, _)       -> TC.not_yet_implemented [%here] location
  | Int                  -> TC.not_yet_implemented [%here] location
  | Bool                 -> TC.not_yet_implemented [%here] location
  | String               -> TC.not_yet_implemented [%here] location
  | Bit                  -> TC.not_yet_implemented [%here] location
  | Sum (_, _)           -> TC.not_yet_implemented [%here] location
  | Bitvector _          -> TC.not_yet_implemented [%here] location
  | Tuple _              -> TC.not_yet_implemented [%here] location
  | Variant _            -> TC.not_yet_implemented [%here] location
  | Record _             -> TC.not_yet_implemented [%here] location
  | Application (_, _)   -> TC.not_yet_implemented [%here] location
  | Alias (_, _)         -> TC.not_yet_implemented [%here] location
  | Range (_, _)         -> TC.not_yet_implemented [%here] location
