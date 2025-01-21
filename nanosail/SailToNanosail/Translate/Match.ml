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


exception InconsistentBinders of (Ast.Identifier.t list * Ast.Identifier.t list)

module Pattern = struct
  (*
     Wildcards in Sail code are translated into Binder { identifier = gensym; wildcard = true },
     whereas regular binders are translated into Binder { identifier = sail_identifier; wildcard = false }.
     We need to be able to make the distinction when checking for consistent binder names.
     For example,

       match lst {
         [| _ |]   => A,
         [| _, _|] => B,
         _         => C
       }

     should be allowed, whereas

       match lst {
         [| x |]   => A,
         [| y, _|] => B,
         _         => C
       }

     gives different names to the first element of the matched list.
     This is a situation which we do not support but want to detect so as to give an error message.
  *)
  type t =
    | ListCons    of t * t
    | ListNil
    | Tuple       of t list
    | EnumCase    of Ast.Identifier.t
    | VariantCase of Ast.Identifier.t * t
    | Binder      of { identifier : Ast.Identifier.t; wildcard : bool }
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

    | VariantCase (identifier, subpattern) -> begin
        let positional = [
          Ast.Identifier.to_fexpr identifier;
          to_fexpr subpattern
        ]
        in
        FExpr.mk_application ~positional @@ head "VariantCase"
      end

    | Binder { identifier; wildcard } -> begin
        let positional =
          [
            Ast.Identifier.to_fexpr identifier;
          ]
        and keyword =
          [
            ("wildcard", FExpr.mk_bool wildcard)
          ]
        in
        FExpr.mk_application ~positional ~keyword @@ head "Variable"
      end

    | Unit -> FExpr.mk_symbol @@ head "Unit"

  let is_binder (pattern : t) : bool =
    match pattern with
     | Binder _ -> true
     | _        -> false

  let identifier_of_binder (pattern : t) : Ast.Identifier.t =
    match pattern with
    | Binder { identifier; _ } -> identifier
    | _                        -> failwith "bug: should only be called on Binder patterns"
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
    TC.return @@ Pattern.Binder { identifier; wildcard = false }

  and translate_wildcard_pattern () : Pattern.t TC.t =
    let* identifier = TC.generate_unique_identifier ~underscore:true ()
    in
    TC.return @@ Pattern.Binder { identifier; wildcard = true }

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
            TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_enum ~named:enum_identifier ())
          in
          if
            List.mem enum_definition.cases identifier ~equal:Ast.Identifier.equal
          then
            TC.return @@ Pattern.EnumCase identifier
          else
            TC.return @@ Pattern.Binder { identifier; wildcard = false }
        end
      | AP_wild _type -> translate_wildcard_pattern ()
      | _ -> unexpected_pattern [%here]
    end
  | Unit -> begin
      match unwrapped_sail_pattern with
      | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
      | AP_wild _type                       -> translate_wildcard_pattern ()
      | _                                   -> unexpected_pattern [%here]
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
  | Variant variant_identifier -> begin
      match unwrapped_sail_pattern with
      | AP_app (head_sail_identifier, sail_subpattern, _sail_type) -> begin
          let* head_identifier =
            Identifier.translate_identifier [%here] head_sail_identifier
          in
          let* variant_definition =
            TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_variant ~named:variant_identifier ())
          in
          match List.find variant_definition.constructors ~f:(Fn.compose (Ast.Identifier.equal head_identifier) fst) with (* todo create separate function *)
          | Some (constructor_identifier, field_types) -> begin
              let field_type : Ast.Type.t =
                match field_types with
                | []  -> Ast.Type.Unit
                | [t] -> t
                | ts  -> Ast.Type.Tuple ts
              in
              let* subpattern =
                translate_pattern field_type sail_subpattern
              in
              TC.return @@ Pattern.VariantCase (constructor_identifier, subpattern)
            end
          | None -> begin
              (* This really should never occur *)
              let error_message =
                Printf.sprintf
                  "variant %s has no constructor %s"
                  (Ast.Identifier.to_string variant_identifier)
                  (Ast.Identifier.to_string head_identifier)
              in
              TC.fail [%here] error_message
            end
        end
      | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
      | AP_wild _sail_type                  -> translate_wildcard_pattern ()
      | _                                   -> unexpected_pattern [%here]
    end
  | Int                -> translate_for_atomic_type ()
  | Bool               -> translate_for_atomic_type ()
  | String             -> translate_for_atomic_type ()
  | Bit                -> translate_for_atomic_type ()
  | Sum (_, _)         -> translate_for_atomic_type ()
  | Bitvector _        -> translate_for_atomic_type ()
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
   Subpatterns are not allowed in the current implementation.
*)
let translate_list_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (element_type       : Ast.Type.t                        )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let translate
      (matched_identifier : Ast.Identifier.t)
      (head_identifier    : Ast.Identifier.t)
      (tail_identifier    : Ast.Identifier.t)
      (cons_body          : Ast.Statement.t )
      (nil_body           : Ast.Statement.t ) : Ast.Statement.t TC.t
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
      let fail () =
        failwith "unexpected pattern; only patterns applicable on lists should appear"
      in
      match pattern with
      | ListCons (_, tail) -> 1 + pattern_depth tail
      | ListNil            -> 0
      | Binder _           -> 0
      | Tuple _            -> fail ()
      | EnumCase _         -> fail ()
      | VariantCase (_, _) -> fail ()
      | Unit               -> fail ()

    in
    let compare (p1, _) (p2, _) =
      pattern_depth p1 - pattern_depth p2
    in
    List.sort cases ~compare
  in
  match cases_sorted_by_pattern_depth with
  | [ (Pattern.Binder { identifier = binder_identifier; _ }, body) ] -> begin
      (*
         We're dealing with

           match lst {
             xs => body
           }

         which we translate to

           let xs = lst in body
      *)
      let matched_type = Ast.Type.List element_type
      in
      TC.return @@ Ast.Statement.Let {
        variable_identifier = binder_identifier;
        binding_statement_type = matched_type;
        binding_statement = Ast.Statement.Expression (Ast.Expression.Variable (matched_identifier, matched_type));
        body_statement = body
      }
    end
  | [ (Pattern.ListNil, nil_body);
      (Pattern.ListCons (Pattern.Binder { identifier = head_identifier; _ }, Pattern.Binder { identifier = tail_identifier; _ }), cons_body) ] -> begin
      translate matched_identifier head_identifier tail_identifier cons_body nil_body
    end
  | [ (Pattern.ListNil, if_empty_list);
      (Pattern.ListCons (Pattern.Binder { identifier = first_identifier_1; wildcard = wildcard_1 },
                         Pattern.ListNil),
       if_singleton_list);
      (Pattern.ListCons (Pattern.Binder { identifier = first_identifier_2; wildcard = wildcard_2 },
                         Pattern.ListCons (Pattern.Binder { identifier = second_identifier; _ },
                                           Pattern.Binder { identifier = rest_identifier; _ })),
       if_two_or_more_elements) ] -> begin
      (*
         We're dealing with

           match lst {
             [| |] => if_empty_list,
             [| first_identifier_1 |] => if_singleton_list,
             first_identifier_2 :: second_identifier :: rest_identifier => if_two_or_more_elements
           }

         Note that this implementation expects that, in case neither is a wildcard, first_identifier_1 equals first_identifier_2,.
      *)
      let are_first_element_binders_consistent =
        if
          wildcard_1 || wildcard_2
        then
          true
        else
          Ast.Identifier.equal first_identifier_1 first_identifier_2
      in
      if
        not are_first_element_binders_consistent
      then
        TC.not_yet_implemented ~message:"differently named first elements in list matching patterns" [%here] location
      else begin
        let first_identifier = first_identifier_1
        in
        let* tail_identifier =
          TC.generate_unique_identifier ()
        in
        let* inner_match =
          translate tail_identifier second_identifier rest_identifier if_two_or_more_elements if_singleton_list
        in
        translate matched_identifier first_identifier tail_identifier inner_match if_empty_list
      end
    end
  | _ -> TC.not_yet_implemented [%here] location


let translate_unit_match
    (_location           : S.l                               )
    (_matched_identifier : Ast.Identifier.t                  )
    (cases               : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  match cases with
  | [ (Pattern.Binder { identifier = binding_identifier; _ }, body) ] -> begin
      (*
         We're dealing with

           match unit-value {
             x => bla(x)
           }

        I.e., the pattern is a binder (x) and this binder is being used in the body (bla(x)).
        We translate this to

         let x = () in bla(x)
      *)
      TC.return begin
        Ast.Statement.Let {
          variable_identifier    = binding_identifier;
          binding_statement_type = Ast.Type.Unit;
          binding_statement      = Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit);
          body_statement         = body
        }
      end
    end
  | [ (Pattern.Unit, body) ]     -> TC.return body
  | [ (pattern, _) ]             -> TC.fail [%here] @@ Printf.sprintf "unexpected pattern %s" (FExpr.to_string @@ Pattern.to_fexpr pattern)
  | []                           -> TC.fail [%here] "expected exactly one pattern; got zero"
  | _ :: _ :: _                  -> TC.fail [%here] @@ Printf.sprintf "expected exactly one pattern; got %d" (List.length cases)


let translate_enum_match
    (_location          : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (enum_identifier    : Ast.Identifier.t                  )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  (* Look up enum definition, we need to know which values there are *)
  let* enum_definition =
    TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_enum ~named:enum_identifier ())
  in
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
      | Binder { identifier = binder_identifier; _ } -> begin
        (*
           The pattern binds the enum value to a variable, meaning
           it should match all enum values that have hitherto not been processed.
        *)
          let fill_in_missing_case
              (table                 : Ast.Statement.t Ast.Identifier.Map.t)
              (enum_value_identifier : Ast.Identifier.t                    ) : Ast.Statement.t Ast.Identifier.Map.t
            =
            (*
                 match enum_value {
                   binder_identifier => X
                 }

               gets translated to

                 match enum_value {
                   EnumCaseA => let binder_identifier = enum_value in X,
                   EnumCaseB => let binder_identifier = enum_value in X,
                   ...
                 }
            *)
            let extended_body =
              let matched_type = Ast.Type.Enum enum_identifier
              in
              Ast.Statement.Let {
                variable_identifier    = binder_identifier;
                binding_statement_type = matched_type;
                binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (matched_identifier, matched_type));
                body_statement         = body;
              }
            in
            match Ast.Identifier.Map.add table ~key:enum_value_identifier ~data:extended_body with
            | `Duplicate -> begin
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


let translate_variant_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (variant_identifier : Ast.Identifier.t                  )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  (* Look up variant definition, we need to know which constructors there are *)
  let* variant_definition =
    TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant ~named:variant_identifier ())
  in
  (*
     Set up constructor table: it maps constructors to variables to which the fields need to be bound and the clause
  *)
  let* case_table : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t =
    let process_case
        (table : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t)
        (case  : Pattern.t * Ast.Statement.t                                   ) : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t TC.t
      =
      let pattern, body = case
      in
      let add_to_table
          (constructor_identifier : Ast.Identifier.t     )
          (binder_identifiers     : Ast.Identifier.t list) : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t TC.t
        =
        match Ast.Identifier.Map.add table ~key:constructor_identifier ~data:(binder_identifiers, body) with
        | `Duplicate -> begin
            (* This case shouldn't occur given the limitations the current implementation imposes on pattern matching *)
            TC.fail [%here] "same constructor matched against twice"
          end
        | `Ok updated_table -> begin
            (* We add the (constructor, clause) association to the table *)
            TC.return updated_table
          end
      in
      match pattern with
      | VariantCase (constructor_identifier, subpattern) -> begin
          (* Look up information about the current constructor *)
          match Ast.Definition.Type.Variant.find_constructor_field_types variant_definition constructor_identifier with
          | None -> begin
              (* No constructor for the variant we're matching against. Should not occur *)
              let error_message =
                Printf.sprintf
                  "unknown constructor %s for variant %s"
                  (Ast.Identifier.to_string constructor_identifier)
                  (Ast.Identifier.to_string variant_identifier)
              in
              TC.fail [%here] error_message
            end

          | Some field_types -> begin
              match subpattern with
              | Tuple tuple_subpatterns -> begin
                  (*
                     We're dealing with

                       match variant_value {
                         Constructor(subpattern1, subpattern2, ...) => ...
                       }

                     In our current implementation, we expect all subpatterns to be binders, i.e.,
                     we don't support nested patterns.
                  *)
                  if
                    not @@ Int.equal (List.length tuple_subpatterns) (List.length field_types)
                  then
                    TC.fail [%here] "mismatch between number of subpatterns and number of fields"
                  else begin
                    (* For now, we expect the tuple pattern to contain only Variable patterns *)
                    let extract_identifier_from_variable_pattern (pattern : Pattern.t) : Ast.Identifier.t TC.t =
                      match pattern with
                      | Binder { identifier; _ } -> TC.return identifier
                      | _                        -> TC.fail [%here] "only variable subpatterns supported"
                    in
                    let* binders =
                      TC.map ~f:extract_identifier_from_variable_pattern tuple_subpatterns
                    in
                    add_to_table constructor_identifier binders
                  end
                end
              | Binder { identifier; _ } -> begin
                  (*
                     We're dealing with

                       match variant_value {
                         Constructor(x) => ...
                       }

                     Three cases need to be considered:
                     * Constructor has zero fields, in which case x should be bound to unit
                     * Constructor has one field, in which case x should be bound to that field
                     * Constructor has more than one field, in which case x should be bound to a tuple of field values.
                       Our current implementation does not support this.
                  *)
                  match List.length field_types with
                  | 0 -> begin
                      (* We're dealing with a fieldless constructor, which is actually a constructor with one unit-typed field *)
                      add_to_table constructor_identifier [ identifier ]
                    end
                  | 1 -> add_to_table constructor_identifier [ identifier ]
                  | _ -> begin
                      let message =
                        Printf.sprintf
                          "each field needs its own binder; problematic constructor %s"
                          (Ast.Identifier.to_string constructor_identifier)
                      in
                      TC.not_yet_implemented ~message [%here] location
                    end
                end
              | _ -> TC.fail [%here] @@ Printf.sprintf "Unexpected variant subpattern %s" @@ FExpr.to_string @@ Pattern.to_fexpr subpattern
            end
        end

      | Binder { identifier = binder_identifier; _ } -> begin
          (*
             The pattern binds the constructor to a variable, meaning
             it should match all constructors that have hitherto not been processed.
          *)
          let fill_in_missing_case
              (table       : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t)
              (constructor : Ast.Identifier.t * Ast.Type.t list                            ) : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t TC.t
            =
            (*
                 match variant_value {
                   binder_identifier => X
                 }
                gets translated to
                  match variant_value {
                   ConstructorA(gensym, ...) => let binder_identifier = enum_value in X,
                   ConstructorB(gensym, ...) => let binder_identifier = enum_value in X,
                   ...
                 }
            *)
            let extended_body =
              let matched_type = Ast.Type.Variant variant_identifier
              in
              Ast.Statement.Let {
                variable_identifier    = binder_identifier;
                binding_statement_type = matched_type;
                binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (matched_identifier, matched_type));
                body_statement         = body;
              }
            in

            let constructor_identifier, field_types = constructor
            in
            (* Generate new identifiers to be used as binders for each field *)
            let* identifiers : Ast.Identifier.t list =
              let n_identifiers_needed =
                (* If there are zero types, we still need one binder for unit *)
                Int.max 1 @@ List.length field_types
              in
              TC.generate_unique_identifiers n_identifiers_needed
            in
            match Ast.Identifier.Map.add table ~key:constructor_identifier ~data:(identifiers, extended_body) with
            | `Duplicate -> begin
                (*
                   We tried to add an extra (constructor, clause) association to the table, but there
                   already existed one, which is okay. We simply keep the table as is.
                *)
                TC.return table
              end
            | `Ok updated_table -> TC.return updated_table
          in
          (*
             We go through all possible constructors and try to add associations for them to the table, i.e.,
             we only add associations for constructors that are missing from the table.
          *)
          TC.fold_left variant_definition.constructors ~init:table ~f:fill_in_missing_case
        end

      | _ -> TC.fail [%here] @@ Printf.sprintf "unexpected pattern while dealing with enum match: %s" (FExpr.to_string @@ Pattern.to_fexpr pattern)
    in
    TC.fold_left cases ~init:Ast.Identifier.Map.empty ~f:process_case
  in
  (* Check that all enum cases have been handled *)
  let all_constructors_handled =
    let constructor_identifiers =
      List.map ~f:fst variant_definition.constructors
    in
    List.for_all constructor_identifiers ~f:(Ast.Identifier.Map.mem case_table)
  in
  if
    not all_constructors_handled
  then
    (*
       Some enum values were not handled by the match.
       For now, we simply fail, but we could instead fill up the gaps with failure statements.
    *)
    TC.fail [%here] "not all enum cases are handled"
  else begin
    let match_pattern =
      Ast.Statement.MatchVariant {
        matched = matched_identifier;
        matched_type = variant_identifier;
        cases = case_table
      }
    in
    TC.return @@ Ast.Statement.Match match_pattern
  end


(*
   We support a small number of specific matching structures.
*)
let translate_tuple_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (element_types      : Ast.Type.t list                   )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  (*
     This function deals with the special case of having a single match pattern that contains nothing but binders, i.e.,

       match tuple_value {
         (X1, X2, ..., Xn) => ...
       }
  *)
  let translate_tuple_of_binders : Ast.Statement.t TC.t =
    (* Keeps thing lazy *)
    let* () = TC.return ()
    in
    match cases with
    | [ (Pattern.Tuple subpatterns, body) ] when List.for_all subpatterns ~f:Pattern.is_binder -> begin
        let binding_variables =
          List.map subpatterns ~f:Pattern.identifier_of_binder
        in
        match List.zip binding_variables element_types with
        | List.Or_unequal_lengths.Unequal_lengths -> begin
            (* Should never occur *)
            TC.fail [%here] "different number of tuple pattern elements and tuple pattern types"
          end
        | List.Or_unequal_lengths.Ok binder_type_pairs -> begin
            match binder_type_pairs with
            | []  -> TC.fail [%here] "unexpected empty tuple"
            | [_] -> TC.fail [%here] "unexpected singleton tuple"
            | [(id_fst, type_fst); (id_snd, type_snd)] -> begin
                let match_pattern =
                  Ast.Statement.MatchProduct {
                    matched = matched_identifier;
                    type_fst;
                    type_snd;
                    id_fst;
                    id_snd;
                    body
                  }
                in
                TC.return @@ Ast.Statement.Match match_pattern
              end
            | _ -> begin
                let match_pattern =
                  Ast.Statement.MatchTuple {
                    matched = matched_identifier;
                    elements = binder_type_pairs;
                    body
                  }
                in
                TC.return @@ Ast.Statement.Match match_pattern
              end
          end
      end
    | _ -> TC.not_yet_implemented [%here] location

  and translate_pair_of_variants : Ast.Statement.t TC.t =
    (* Keeps things lazy *)
    let* () = TC.return ()
    in
    match element_types with
    | [ (Ast.Type.Variant fst_variant_identifier) as type_fst; (Ast.Type.Variant snd_variant_identifier) as type_snd ] -> begin
        (* todo use these definitions to check for exhaustivity *)
        let* _fst_variant_definition = TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_variant ~named:fst_variant_identifier ())
        and* _snd_variant_definition = TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_variant ~named:snd_variant_identifier ())
        in
        let* table : (Pattern.t * (Pattern.t * Ast.Statement.t) list) Ast.Identifier.Map.t =
          let init = Ast.Identifier.Map.empty
          in
          let add_to_table
              (table : (Pattern.t * (Pattern.t * Ast.Statement.t) list) Ast.Identifier.Map.t)
              (pair  : Pattern.t * Ast.Statement.t                                          ) : (Pattern.t * (Pattern.t * Ast.Statement.t) list) Ast.Identifier.Map.t TC.t
            =
            let pattern, body = pair
            in
            match pattern with
            | Pattern.Tuple [Pattern.VariantCase (constructor_identifier, field_pattern); snd_pattern] -> begin
                let add (previous_data : (Pattern.t * (Pattern.t * Ast.Statement.t) list) option) : Pattern.t * (Pattern.t * Ast.Statement.t) list =
                  match previous_data with
                  | None -> (field_pattern, [(snd_pattern, body)])
                  | Some (previous_field_pattern, previous_pairs) -> begin
                      (* TODO check for equal field patterns, previous_field_pattern should be equal to field_pattern *)
                      (previous_field_pattern, List.append previous_pairs [(snd_pattern, body)])
                    end
                in
                try
                  TC.return @@ Ast.Identifier.Map.update table constructor_identifier ~f:add
                with
                  InconsistentBinders (previous_field_binders, field_binders) -> begin
                    let message =
                      Printf.sprintf
                        "inconsistent binders: %s vs %s"
                        (String.concat ~sep:", " (List.map ~f:Ast.Identifier.to_string previous_field_binders))
                        (String.concat ~sep:", " (List.map ~f:Ast.Identifier.to_string field_binders))
                    in
                    TC.not_yet_implemented ~message [%here] location
                  end
              end
            | _ -> TC.not_yet_implemented [%here] location
          in
          TC.fold_left ~f:add_to_table ~init cases
        in
        (*
           We use pattern matching against the pair, thereby giving names to each value inside it:
              match pair {
               (id_fst, id_snd) => ...
             }
        *)
        let* id_fst = TC.generate_unique_identifier ()
        and* id_snd = TC.generate_unique_identifier ()
        in
        let* snd_match_alist : (Pattern.t * Ast.Statement.t) list =
          let build_match_statement (clauses : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t =
            let* statement =
              translate_variant_match location id_snd snd_variant_identifier clauses
            in
            TC.return statement
          in
          let pairs =
            Ast.Identifier.Map.to_alist table
          in
          TC.map
            pairs
            ~f:(fun (constructor_identifier, (field_pattern, cs)) ->
                let* s = build_match_statement cs
                in
                TC.return (Pattern.VariantCase (constructor_identifier, field_pattern), s)
              )
        in
        let* fst_match =
          translate_variant_match location id_fst fst_variant_identifier snd_match_alist
        in
        let* tuple_match_statement =
          let match_pattern =
            Ast.Statement.MatchProduct {
              matched = matched_identifier;
              type_fst;
              type_snd;
              id_fst;
              id_snd;
              body = fst_match;
            }
          in
          TC.return @@ Ast.Statement.Match match_pattern
        in
        TC.return tuple_match_statement
      end
 | _ -> TC.not_yet_implemented [%here] location

  in
  (* translate_pair_of_variants *)
  TC.try_multiple [ translate_tuple_of_binders; translate_pair_of_variants ]


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
  | List element_type            -> translate_list_match location matched_identifier element_type translated_cases
  | Unit                         -> translate_unit_match location matched_identifier translated_cases
  | Enum enum_identifier         -> translate_enum_match location matched_identifier enum_identifier translated_cases
  | Variant variant_identifier   -> translate_variant_match location matched_identifier variant_identifier translated_cases
  | Tuple element_types          -> translate_tuple_match location matched_identifier element_types translated_cases
  | Product (fst_type, snd_type) -> translate_tuple_match location matched_identifier [ fst_type; snd_type ] translated_cases
  | Int                          -> TC.not_yet_implemented [%here] location
  | Bool                         -> TC.not_yet_implemented [%here] location
  | String                       -> TC.not_yet_implemented [%here] location
  | Bit                          -> TC.not_yet_implemented [%here] location
  | Sum (_, _)                   -> TC.not_yet_implemented [%here] location
  | Bitvector _                  -> TC.not_yet_implemented [%here] location
  | Record _                     -> TC.not_yet_implemented [%here] location
  | Application (_, _)           -> TC.not_yet_implemented [%here] location
  | Alias (_, _)                 -> TC.not_yet_implemented [%here] location
  | Range (_, _)                 -> TC.not_yet_implemented [%here] location
