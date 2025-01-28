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


exception InconsistentBinders of (Pattern.t * Pattern.t)


(*
   Check that both patterns are binders and have the same name, taking into account wildcards.
*)
let consistent_binders
    (pattern_1 : Pattern.t)
    (pattern_2 : Pattern.t) : bool
  =
  match pattern_1 with
   | ListCons (_, _)    -> false
   | ListNil            -> false
   | Tuple _            -> false
   | EnumCase _         -> false
   | VariantCase (_, _) -> false
   | Unit               -> false (* todo might need more nuanced logic *)
   | Binder { identifier = identifier_1; wildcard = wildcard_1 } -> begin
       match pattern_2 with
       | Binder { identifier = identifier_2; wildcard = wildcard_2 } -> begin
           wildcard_1 || wildcard_2 || Ast.Identifier.equal identifier_1 identifier_2
         end
       | _ -> false
     end


(*
   Translates a Sail pattern (type S.typ S.apat) into our own pattern (type Pattern.t).
*)
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
  let translate_pattern_for_atomic_type () =
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
            TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_enum_named enum_identifier)
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
    
  | Variant variant_identifier -> begin
      match unwrapped_sail_pattern with
      | AP_app (head_sail_identifier, sail_subpattern, _sail_type) -> begin
          let* head_identifier =
            Identifier.translate_identifier [%here] head_sail_identifier
          in
          let* variant_definition =
            TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
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
    
  | Int                -> translate_pattern_for_atomic_type ()
  | Bool               -> translate_pattern_for_atomic_type ()
  | String             -> translate_pattern_for_atomic_type ()
  | Bit                -> translate_pattern_for_atomic_type ()
  | Sum (_, _)         -> translate_pattern_for_atomic_type ()
  | Bitvector _        -> translate_pattern_for_atomic_type ()
  | Record _           -> translate_pattern_for_atomic_type ()
  | Application (_, _) -> translate_pattern_for_atomic_type ()
  | Alias (_, _)       -> translate_pattern_for_atomic_type ()
  | Range (_, _)       -> translate_pattern_for_atomic_type ()


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
    TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_enum_named enum_identifier)
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
    TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
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
   Groups together functions that are related to matching against tuples
*)
module TupleMatching = struct
  module PatternNode = struct
    type atomic_data = { identifier : Ast.Identifier.t; wildcard : bool }
    
    type t =
      | Enum       of { enum_identifier : Ast.Identifier.t; table : (Ast.Identifier.t option * t) Ast.Identifier.Map.t; }
      | Variant    of { variant_identifier : Ast.Identifier.t; table : (Ast.Identifier.t list option * t) Ast.Identifier.Map.t }
      | Atomic     of Ast.Type.t * atomic_data option * t
      | Terminal   of Ast.Statement.t option

    
    let rec equal
        (node_1 : t)
        (node_2 : t) : bool
      =
      match node_1 with
      | Enum { enum_identifier = enum_identifier_1; table = table_1 } -> begin
          match node_2 with
          | Enum { enum_identifier = enum_identifier_2; table = table_2 } -> begin
              let table_entry_equality
                  (binder_identifier_1, subtree_1)
                  (binder_identifier_2, subtree_2)
                =
                Option.equal Ast.Identifier.equal
                  binder_identifier_1
                  binder_identifier_2
                &&
                equal
                  subtree_1
                  subtree_2
              in
              Ast.Identifier.equal
                enum_identifier_1
                enum_identifier_2
              &&
              Ast.Identifier.Map.equal table_entry_equality
                table_1
                table_2
            end
          | _ -> false
        end

      | Variant { variant_identifier = variant_identifier_1; table = table_1 } -> begin
          match node_2 with
          | Variant { variant_identifier = variant_identifier_2; table = table_2 } -> begin
              let table_entry_equality
                  ((field_binders_1, subtree_1) : Ast.Identifier.t list option * t)
                  ((field_binders_2, subtree_2) : Ast.Identifier.t list option * t)
                =
                Option.equal (List.equal Ast.Identifier.equal)
                  field_binders_1
                  field_binders_2
                &&
                equal
                  subtree_1
                  subtree_2
              in
              Ast.Identifier.equal
                variant_identifier_1
                variant_identifier_2
              &&
              Ast.Identifier.Map.equal table_entry_equality
                table_1
                table_2
            end
          | _ -> false
        end
        
      | Terminal statement_1 -> begin
          match node_2 with
          | Terminal statement_2 -> begin
              Option.equal Ast.Statement.equal statement_1 statement_2
            end
          | _ -> false
        end

      | Atomic (type_1, data_1, subtree_1) -> begin
          match node_2 with
          | Atomic (type_2, data_2, subtree_2) -> begin
              Ast.Type.equal
                type_1
                type_2
              &&
              Option.equal
                (fun data_1 data_2 -> begin
                     Ast.Identifier.equal
                       data_1.identifier
                       data_2.identifier
                     &&
                     Bool.equal
                       data_1.wildcard
                       data_2.wildcard
                   end)
                data_1
                data_2
              &&
              equal
                subtree_1
                subtree_2
            end
          | _ -> false
        end


    let rec to_fexpr (node : t) : FExpr.t =
      let mk_head (tag : string) =
        Printf.sprintf "PatternNode:%s" tag
      in
      match node with
      | Enum { enum_identifier; table } -> begin
          let keyword =
            [
              (
                "enum_identifier",
                Ast.Identifier.to_fexpr enum_identifier
              );
              (
                "table",
                let fexpr_of_table_entry (binder_identifier, subtree) =
                  FExpr.mk_list [
                    FExpr.mk_option @@ Option.map ~f:Ast.Identifier.to_fexpr binder_identifier;
                    to_fexpr subtree;
                  ]
                in
                Ast.Identifier.Map.to_fexpr fexpr_of_table_entry table
              );
            ]
          in
          FExpr.mk_application ~keyword @@ mk_head "Enum"
        end

      | Variant { variant_identifier; table } -> begin
          let keyword =
            [
              (
                "variant_identifier",
                Ast.Identifier.to_fexpr variant_identifier
              );
              (
                "table",
                let fexpr_of_table_entry (field_binder_identifiers, subtree) =
                  FExpr.mk_list [
                    FExpr.mk_option @@ Option.map field_binder_identifiers ~f:(Fn.compose FExpr.mk_list @@ List.map ~f:Ast.Identifier.to_fexpr);
                    to_fexpr subtree;
                  ]
                in
                Ast.Identifier.Map.to_fexpr fexpr_of_table_entry table
              )
            ]
          in
          FExpr.mk_application ~keyword @@ mk_head "Variant"
        end
        
      | Atomic (typ, data, subtree) -> begin
          let fexpr_of_atomic_data (data : atomic_data) =
            let keyword =
              [
                (
                  "identifier",
                  Ast.Identifier.to_fexpr data.identifier
                );
                (
                  "wildcard",
                  FExpr.mk_bool data.wildcard
                );
              ]
            in
            FExpr.mk_application ~keyword "Data"
          in
          let keyword =
            [
              (
                "type",
                Ast.Type.to_fexpr typ
              );
              (
                "data",
                FExpr.mk_option begin
                  Option.map data ~f:fexpr_of_atomic_data
                end
              );
              (
                "subtree",
                to_fexpr subtree
              )
            ]
          in
          FExpr.mk_application ~keyword @@ mk_head "Atomic"
        end
            
      | Terminal statement -> begin
          let keyword =
            [
              (
                "statement",
                FExpr.mk_option @@ Option.map statement ~f:Ast.Statement.to_fexpr
              );
            ]
          in
          FExpr.mk_application ~keyword @@ mk_head "Terminal"
        end
  end


  let rec build_tuple_pattern_tree
      (location      : S.l            )
      (element_types : Ast.Type.t list) : PatternNode.t TC.t
    =
    let build_enum_node
        (enum_identifier : Ast.Identifier.t)
        (subtree         : PatternNode.t   ) : PatternNode.t TC.t
      =
      let* enum_definition =
        TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_enum_named enum_identifier)
      in
      let table : (Ast.Identifier.t option * PatternNode.t) Ast.Identifier.Map.t =
        let add_to_table
            (table                : (Ast.Identifier.t option * PatternNode.t) Ast.Identifier.Map.t)
            (enum_case_identifier : Ast.Identifier.t                                              ) : (Ast.Identifier.t option * PatternNode.t) Ast.Identifier.Map.t
          =
          Ast.Identifier.Map.add_exn table ~key:enum_case_identifier ~data:(None, subtree)
        in
        List.fold enum_definition.cases ~init:Ast.Identifier.Map.empty ~f:add_to_table
      in
      TC.return begin
        PatternNode.Enum {
          enum_identifier;
          table;
        }
      end

    and build_variant_node
        (variant_identifier : Ast.Identifier.t)
        (subtree            : PatternNode.t   ) : PatternNode.t TC.t
      =
      let* variant_definition =
        TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
      in
      let table : (Ast.Identifier.t list option * PatternNode.t) Ast.Identifier.Map.t =
        let add_to_table
            (table : (Ast.Identifier.t list option * PatternNode.t) Ast.Identifier.Map.t)
            ((constructor_identifier, _field_types) : Ast.Identifier.t * Ast.Type.t list) : (Ast.Identifier.t list option * PatternNode.t) Ast.Identifier.Map.t
          =
          Ast.Identifier.Map.add_exn table ~key:constructor_identifier ~data:(None, subtree)
        in
        List.fold variant_definition.constructors ~init:Ast.Identifier.Map.empty ~f:add_to_table
      in
      TC.return begin
        PatternNode.Variant {
          variant_identifier;
          table
        }
      end            
  
    and build_singleton_node
        (element_type : Ast.Type.t   )
        (subtree      : PatternNode.t) : PatternNode.t TC.t
      =
      TC.return @@ PatternNode.Atomic (element_type, None, subtree)
        
    in
    match element_types with
    | []           -> TC.return @@ PatternNode.Terminal None
    | head :: tail -> begin
        let* tail = build_tuple_pattern_tree location tail
        in 
        match head with
        | Enum enum_identifier       -> build_enum_node enum_identifier tail
        | Int                        -> build_singleton_node Ast.Type.Int tail
        | Variant variant_identifier -> build_variant_node variant_identifier tail
        | Bool                       -> TC.not_yet_implemented [%here] location
        | String                     -> TC.not_yet_implemented [%here] location
        | Bit                        -> TC.not_yet_implemented [%here] location
        | List _                     -> TC.not_yet_implemented [%here] location
        | Sum (_, _)                 -> TC.not_yet_implemented [%here] location
        | Unit                       -> TC.not_yet_implemented [%here] location
        | Bitvector _                -> TC.not_yet_implemented [%here] location
        | Tuple _                    -> TC.not_yet_implemented [%here] location
        | Record _                   -> TC.not_yet_implemented [%here] location
        | Application (_, _)         -> TC.not_yet_implemented [%here] location
        | Alias (_, _)               -> TC.not_yet_implemented [%here] location
        | Range (_, _)               -> TC.not_yet_implemented [%here] location
      end


  let rec contains_gap (pattern_tree : PatternNode.t) : bool =
    match pattern_tree with
    | Enum { table; _ } -> begin
        let values : (Ast.Identifier.t option * PatternNode.t) list =
          Ast.Identifier.Map.data table
        in
        let subtrees : PatternNode.t list =
          List.map ~f:snd values
        in
        List.exists subtrees ~f:contains_gap
      end
      
    | Variant { table; _ } -> begin
        let values : (Ast.Identifier.t list option * PatternNode.t) list =
          Ast.Identifier.Map.data table
        in
        let subtrees : PatternNode.t list =
          List.map ~f:snd values
        in
        List.exists subtrees ~f:contains_gap
      end
      
    | Atomic (_, _, subtree) -> contains_gap subtree
    | Terminal statement     -> Option.is_none statement
    

  let rec categorize_case
      (location          : S.l            )
      (pattern_tree      : PatternNode.t  )
      (tuple_subpatterns : Pattern.t list )
      (body              : Ast.Statement.t)
      (gap_filling       : bool           ) : PatternNode.t TC.t
    =
    let invalid_number_of_subpatterns (location : Lexing.position) =
      TC.fail location "the tree should be as deep as there are tuple subpatterns"
    and invalid_pattern (location : Lexing.position) =
      TC.fail location "pattern is incompatible with type of value being matched"
    in
    match pattern_tree with
    | Enum { enum_identifier; table } -> begin
        match tuple_subpatterns with
        | first_subpattern :: remaining_subpatterns -> begin
            match first_subpattern with
            | EnumCase case_identifier -> begin
                let* updated_table =
                  let binder_identifier, subtree =
                    Ast.Identifier.Map.find_exn table case_identifier
                  in
                  let* updated_subtree =
                    categorize_case location subtree remaining_subpatterns body gap_filling
                  in                  
                  TC.return begin
                    Ast.Identifier.Map.overwrite
                      table
                      ~key:case_identifier
                      ~data:(binder_identifier, updated_subtree)
                  end
                in
                TC.return begin
                  PatternNode.Enum { enum_identifier; table = updated_table }
                end
              end
            | Binder { identifier = pattern_binder_identifier; wildcard = pattern_binder_wildcard } -> begin
                let* enum_definition =
                  TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_enum_named enum_identifier)
                in
                let enum_cases =
                  enum_definition.cases
                in                
                let update_table
                    (table     : (Ast.Identifier.t option * PatternNode.t) Ast.Identifier.Map.t)
                    (enum_case : Ast.Identifier.t                                              ) : (Ast.Identifier.t option * PatternNode.t) Ast.Identifier.Map.t TC.t
                  =
                  let binder_identifier, subtree =
                    Ast.Identifier.Map.find_exn table enum_case
                  in
                  if
                    contains_gap subtree
                  then begin  
                    let* updated_subtree : PatternNode.t =
                      categorize_case
                        location
                        subtree
                        remaining_subpatterns
                        body
                        true
                    in
                    let* updated_binder_identifier : Ast.Identifier.t option =
                      match binder_identifier, pattern_binder_wildcard with
                      | None                  , true  -> TC.return None
                      | None                  , false -> TC.return @@ Some pattern_binder_identifier
                      | Some binder_identifier, true  -> TC.return @@ Some binder_identifier
                      | Some binder_identifier, false -> begin
                          if
                            Ast.Identifier.equal binder_identifier pattern_binder_identifier
                          then
                            TC.return @@ Some binder_identifier
                          else
                          (*
                             The same value was bound to differently named binders.
                             This is not supported.

                             match value_1, value_2 {
                               x, Foo => ...,
                               y, Bar => ...
                             }
                          *)
                            TC.not_yet_implemented ~message:"inconsistent binders" [%here] location
                        end
                    in
                    TC.return @@ Ast.Identifier.Map.overwrite table ~key:enum_case ~data:(updated_binder_identifier, updated_subtree)
                  end
                  else
                    TC.return table
                in
                let* updated_table =
                  TC.fold_left ~f:update_table ~init:table enum_cases
                in
                TC.return begin
                  PatternNode.Enum { enum_identifier; table = updated_table }
                end
              end
            | Unit               -> invalid_pattern [%here]
            | ListCons (_, _)    -> invalid_pattern [%here]
            | ListNil            -> invalid_pattern [%here]
            | Tuple _            -> invalid_pattern [%here]
            | VariantCase (_, _) -> invalid_pattern [%here]
          end
        | [] -> invalid_number_of_subpatterns [%here]
      end

    | Variant { variant_identifier; table } -> begin
        match tuple_subpatterns with
        | first_subpattern :: remaining_subpatterns -> begin
            match first_subpattern with
            | VariantCase (constructor_identifier, field_pattern) -> begin
                let* updated_table =
                  let* variant_definition =
                    TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
                  in
                  let constructor =
                    List.find_exn variant_definition.constructors ~f:(fun (constructor_identifier, _) -> Ast.Identifier.equal constructor_identifier constructor_identifier)
                  in
                  let constructor_field_types =
                    snd constructor
                  in
                  let* pattern_field_binder_identifiers : Ast.Identifier.t list =
                    match List.length constructor_field_types with
                    | 0 -> begin
                        match field_pattern with
                        | ListCons (_, _)    -> invalid_pattern [%here]
                        | ListNil            -> invalid_pattern [%here]
                        | Tuple _            -> invalid_pattern [%here]
                        | EnumCase _         -> invalid_pattern [%here]
                        | VariantCase (_, _) -> invalid_pattern [%here]
                        | Binder _           -> invalid_pattern [%here] (* todo handle this case; binder needs to be bound to unit *)
                        | Unit               -> TC.return []
                      end
                    | 1 -> begin
                        (* Matched variant case has one field *)
                        match field_pattern with
                        | Binder { identifier = binder_identifier; wildcard = _binder_wildcard } -> begin
                            TC.return [binder_identifier]
                          end
                        | ListCons (_, _)    -> invalid_pattern [%here] 
                        | ListNil            -> invalid_pattern [%here] 
                        | Tuple _            -> invalid_pattern [%here] 
                        | EnumCase _         -> invalid_pattern [%here] 
                        | VariantCase (_, _) -> invalid_pattern [%here] 
                        | Unit               -> invalid_pattern [%here] 
                      end
                    | _ -> begin
                        (* Matched variant case has two or more fields *)
                        match field_pattern with
                        | Tuple subpatterns  -> begin
                            (* We expect all subpatterns to be binders *)
                            let extract_identifier_from_binder (pattern : Pattern.t) : Ast.Identifier.t TC.t =
                              match pattern with
                              | Binder { identifier; _ } -> TC.return identifier
                              | _                        -> TC.not_yet_implemented ~message:"only binder patterns supported; no nesting of patterns allowed for now" [%here] location
                            in
                            TC.map subpatterns ~f:extract_identifier_from_binder
                          end
                        | ListCons (_, _)    -> invalid_pattern [%here] 
                        | ListNil            -> invalid_pattern [%here] 
                        | EnumCase _         -> invalid_pattern [%here] 
                        | VariantCase (_, _) -> invalid_pattern [%here] 
                        | Binder _           -> invalid_pattern [%here] 
                        | Unit               -> invalid_pattern [%here] 
                      end
                  in
                  let _existing_field_binder_identifiers, subtree =
                    Ast.Identifier.Map.find_exn table constructor_identifier
                  in
                  (* todo check if existing_field_binder_identifiers are compatible with patern_field_binder_identifiers *)
                  let* updated_subtree =
                    categorize_case location subtree remaining_subpatterns body gap_filling
                  in
                  TC.return begin
                    Ast.Identifier.Map.overwrite
                      table
                      ~key:constructor_identifier
                      ~data:(Some pattern_field_binder_identifiers, updated_subtree)
                  end                  
                in
                TC.return begin
                  PatternNode.Variant {
                    variant_identifier;
                    table = updated_table
                  }
                end
              end
            | Binder { identifier = _pattern_binder_identifier; wildcard = _pattern_binder_wildcard } -> TC.not_yet_implemented [%here] location
            | EnumCase _         -> invalid_pattern [%here]
            | Unit               -> invalid_pattern [%here]
            | ListCons (_, _)    -> invalid_pattern [%here]
            | ListNil            -> invalid_pattern [%here]
            | Tuple _            -> invalid_pattern [%here]
          end
        | [] -> invalid_number_of_subpatterns [%here]
         
      end

    | Atomic (element_type, atomic_data, subtree) -> begin
        match tuple_subpatterns with
        | first_subpattern :: remaining_subpatterns -> begin
            match first_subpattern with
            | Binder { identifier = pattern_identifier; wildcard = pattern_wildcard } -> begin
                let* updated_subtree =
                  categorize_case
                    location
                    subtree
                    remaining_subpatterns
                    body
                    gap_filling
                in
                match atomic_data, pattern_wildcard with
                | None,  _ -> begin
                    TC.return @@ PatternNode.Atomic (element_type, Some { identifier = pattern_identifier; wildcard = pattern_wildcard }, updated_subtree)
                  end
                | Some { identifier = _; wildcard = true }, _ -> begin
                    TC.return @@ PatternNode.Atomic (element_type, Some { identifier = pattern_identifier; wildcard = false }, updated_subtree)
                  end
                | Some { identifier; wildcard = false as wildcard }, true -> begin
                    TC.return @@ PatternNode.Atomic (element_type, Some { identifier; wildcard }, updated_subtree)
                  end
                | Some { identifier; wildcard = false as wildcard }, false -> begin
                    if
                      Ast.Identifier.equal identifier pattern_identifier
                    then
                      TC.return @@ PatternNode.Atomic (element_type, Some { identifier; wildcard }, updated_subtree)
                    else
                      (*
                         The same value was matched against binders with different names.
                         This case is not supported.
                         
                           match (value1, value2) {
                             x, Foo => ...,
                             y, Bar => ...,
                           }
                      *)
                      TC.not_yet_implemented ~message:"inconsistent binder identifiers" [%here] location
                  end
              end
            | _ -> TC.fail [%here] "invalid pattern"
          end
        | [] -> invalid_number_of_subpatterns [%here]
      end
      
    | Terminal statement -> begin
        match tuple_subpatterns with
        | [] -> begin
            match statement with
            | Some _ -> begin
                if
                  gap_filling
                then
                  (* We're in gap-filling mode, but there is no gap, so keep things as they are *)
                  TC.return pattern_tree
                else
                  (*
                     We're not in gap-filling mode, and we expect a gap.
                     However, there is none, which means we're dealing with the same case twice, which should never occur.
                  *)
                  TC.fail [%here] "clashing patterns"
              end
            | None -> TC.return @@ PatternNode.Terminal (Some body)
          end
        | _::_ -> invalid_number_of_subpatterns [%here]
      end


  let rec build_leveled_match_statements
      (tuple_elements : Ast.Identifier.t list)
      (pattern_tree   : PatternNode.t        ) : Ast.Statement.t TC.t
    =
    let invalid_number_of_tuple_elements (location : Lexing.position) =
      TC.fail location "invalid number of tuple elements"
    in
    let fail_due_to_unhandled_cases =
      TC.return @@ Ast.Statement.Fail "incomplete matching"
    in
    match pattern_tree with
    | Enum { enum_identifier; table } -> begin
        match tuple_elements with
        | [] -> invalid_number_of_tuple_elements [%here]
        | first_tuple_element :: remaining_tuple_elements -> begin
            (*
               The decorator adds an extra let if necessary.

                 match enum_value {
                   x => ...
                 }

               becomes

                 match enum_value {
                   Foo => let x = enum_value in ...,
                   Bar => let x = enum_value in ...,
                   ...
                 }
               
            *)
            let decorate_statement
                (binder_identifier : Ast.Identifier.t option)
                (statement         : Ast.Statement.t        ) : Ast.Statement.t =
              match binder_identifier with
              | Some binder_identifier -> begin
                  Ast.Statement.Let {
                    variable_identifier    = binder_identifier;
                    binding_statement_type = Ast.Type.Enum enum_identifier;
                    binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (first_tuple_element, Ast.Type.Enum enum_identifier));
                    body_statement         = statement;
                  }
                end
              | None -> statement
            in
            let* cases : Ast.Statement.t Ast.Identifier.Map.t =
              let table_pairs : (Ast.Identifier.t * (Ast.Identifier.t option * PatternNode.t)) list =
                Ast.Identifier.Map.to_alist table
              in
              let* updated_pairs : (Ast.Identifier.t * Ast.Statement.t) list =
                let update_pair (enum_case, (binder_identifier, subtree)) =
                  let* statement : Ast.Statement.t =
                    let* subtree_statement =
                      build_leveled_match_statements remaining_tuple_elements subtree
                    in
                    TC.return @@ decorate_statement binder_identifier subtree_statement
                  in
                  TC.return (enum_case, statement)
                in
                TC.map ~f:update_pair table_pairs
              in
              TC.return @@ Ast.Identifier.Map.of_alist_exn updated_pairs
            in
            TC.return begin
              Ast.Statement.Match begin
                Ast.Statement.MatchEnum {
                  matched      = first_tuple_element;
                  matched_type = enum_identifier;
                  cases;
                }
              end
            end
          end
      end

    | Variant { variant_identifier; table } -> begin
        match tuple_elements with
        | [] -> invalid_number_of_tuple_elements [%here]
        | first_tuple_element :: remaining_tuple_elements -> begin
            let* variant_definition =
              TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
            in
            let* cases : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t =
              let table_pairs : (Ast.Identifier.t * (Ast.Identifier.t list option * PatternNode.t)) list =
                Ast.Identifier.Map.to_alist table
              in
              let* updated_pairs : (Ast.Identifier.t * (Ast.Identifier.t list * Ast.Statement.t)) list =
                let build_statement_pair
                    (constructor_identifier   : Ast.Identifier.t            )
                    (field_binder_identifiers : Ast.Identifier.t list option)
                    (subtree                  : PatternNode.t               ) : (Ast.Identifier.t * (Ast.Identifier.t list * Ast.Statement.t)) TC.t
                  =
                  let* field_binder_identifiers : Ast.Identifier.t list =
                    match field_binder_identifiers with
                    | Some x -> TC.return x
                    | None   -> begin
                        let constructor =
                          List.find_exn variant_definition.constructors ~f:(fun (id, _) -> Ast.Identifier.equal id constructor_identifier)
                        in
                        let field_count =
                          List.length (snd constructor)
                        in
                        TC.generate_unique_identifiers field_count
                      end
                  in
                  let* statement =
                    build_leveled_match_statements remaining_tuple_elements subtree
                  in
                  TC.return (constructor_identifier, (field_binder_identifiers, statement))
                in
                TC.map table_pairs ~f:(fun (id, (fids, pn)) -> build_statement_pair id fids pn)                      
              in
              TC.return @@ Ast.Identifier.Map.of_alist_exn updated_pairs
            in
            TC.return begin
              Ast.Statement.Match begin
                Ast.Statement.MatchVariant {
                  matched      = first_tuple_element;
                  matched_type = variant_identifier;
                  cases
                }
              end
            end
          end
      end

    | Atomic (element_type, atomic_data, subtree) -> begin
        match tuple_elements with
        | [] -> invalid_number_of_tuple_elements [%here]
        | first_tuple_element :: remaining_tuple_elements -> begin
            match atomic_data with
            | Some { identifier; wildcard = _ } -> begin
                let* continuation =
                  build_leveled_match_statements remaining_tuple_elements subtree
                in
                TC.return begin
                  Ast.Statement.Let {
                    variable_identifier    = identifier;
                    binding_statement_type = element_type;
                    binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (first_tuple_element, element_type));
                    body_statement         = continuation;
                  }
                end
              end
            | None -> fail_due_to_unhandled_cases
          end
      end  
  
    | Terminal statement -> begin
        match tuple_elements with
        | [] -> begin
            match statement with
            | None           -> fail_due_to_unhandled_cases
            | Some statement -> TC.return @@ statement
          end
        | _::_ -> invalid_number_of_tuple_elements [%here]
      end


  let create_tuple_match
      (matched       : Ast.Identifier.t                             )
      (element_types : Ast.Type.t list                              )
      (body_builder  : Ast.Identifier.t list -> Ast.Statement.t TC.t) : Ast.Statement.t TC.t
    =
    let tuple_size =
      List.length element_types
    in
    let* binder_identifiers =
      TC.generate_unique_identifiers tuple_size
    in
    let* body =
      body_builder binder_identifiers
    in
    let binders =
      List.zip_exn binder_identifiers element_types
    in
    match binders with
    | []  -> TC.fail [%here] "empty tuple should not occur"
    | [_] -> TC.fail [%here] "singleton tuple should not occur"
    | [ (id_fst, type_fst); (id_snd, type_snd) ] -> begin
        TC.return begin
          Ast.Statement.Match begin
            Ast.Statement.MatchProduct {
              matched;
              type_fst;
              type_snd;
              id_fst;
              id_snd;
              body;
            }
          end
        end
      end
    | _ -> begin
        TC.return begin
          Ast.Statement.Match begin
            Ast.Statement.MatchTuple {
              matched;
              binders;
              body;
            }
          end
        end
    end
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
  let translate_using_pattern_tree : Ast.Statement.t TC.t =
    (* keeps things lazy *)
    let* () = TC.return ()
    in
    let builder (binder_identifiers : Ast.Identifier.t list) : Ast.Statement.t TC.t =
      let* initial_tree =
        TupleMatching.build_tuple_pattern_tree
          location
          element_types
      in
      let categorize
          (tree      : TupleMatching.PatternNode.t)
          (pattern   : Pattern.t                  )
          (statement : Ast.Statement.t            ) : TupleMatching.PatternNode.t TC.t
        =
        match pattern with
        | Tuple subpatterns -> TupleMatching.categorize_case location tree subpatterns statement false
        | _                 -> TC.fail [%here] "expected tuple pattern"
      in
      let* final_tree =
        TC.fold_left
          ~init:initial_tree
          ~f:(fun tree (pattern, statement) -> categorize tree pattern statement)
          cases
      in
      TupleMatching.build_leveled_match_statements binder_identifiers final_tree
    in
    let* result =
      TupleMatching.create_tuple_match
        matched_identifier
        element_types
        builder
    in
    TC.return result
  
  (*
     This function deals with the special case of having a single match pattern that contains nothing but binders, i.e.,

       match tuple_value {
         (X1, X2, ..., Xn) => ...
       }
  *)
  and translate_tuple_of_binders : Ast.Statement.t TC.t =
    (* Keeps things lazy *)
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
                    binders = binder_type_pairs;
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
        let* _fst_variant_definition = TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_variant_named fst_variant_identifier)
        and* _snd_variant_definition = TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_variant_named snd_variant_identifier)
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
                      if
                        consistent_binders previous_field_pattern field_pattern
                      then
                        (previous_field_pattern, List.append previous_pairs [(snd_pattern, body)])
                      else
                        raise @@ InconsistentBinders (previous_field_pattern, field_pattern)
                    end
                in
                try
                  TC.return @@ Ast.Identifier.Map.update table constructor_identifier ~f:add
                with
                  InconsistentBinders (previous_field_pattern, field_pattern) -> begin
                    let message =
                      Printf.sprintf
                        "inconsistent patterns: %s vs %s"
                        (FExpr.to_string @@ Pattern.to_fexpr previous_field_pattern)
                        (FExpr.to_string @@ Pattern.to_fexpr field_pattern)
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
              matched   = matched_identifier;
              type_fst                      ;
              type_snd                      ;
              id_fst                        ;
              id_snd                        ;
              body     = fst_match          ;
            }
          in
          TC.return @@ Ast.Statement.Match match_pattern
        in
        TC.return tuple_match_statement
      end
    | _ -> TC.not_yet_implemented [%here] location

  in
  TC.try_multiple [
    translate_using_pattern_tree;
    translate_tuple_of_binders;
    translate_pair_of_variants;
  ]


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
