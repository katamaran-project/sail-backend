module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)


let rec value_of_expression (expression : Sail.type_annotation S.exp) : Ast.Value.t TC.t =
  let S.E_aux (unwrapped_expression, (location, type_annotation)) = expression
  in
  match unwrapped_expression with
  | E_lit literal                            -> Literal.value_of_literal literal (Libsail.Type_check.typ_of_annot (location, type_annotation))
  | E_vector bitvector_elements              -> value_of_bitvector_expression ~location ~type_annotation ~bitvector_elements
  | E_block _                                -> TC.not_yet_implemented [%here] location
  | E_id _                                   -> TC.not_yet_implemented [%here] location
  | E_typ (_, _)                             -> TC.not_yet_implemented [%here] location
  | E_app (_, _)                             -> TC.not_yet_implemented [%here] location
  | E_tuple _                                -> TC.not_yet_implemented [%here] location
  | E_if (_, _, _)                           -> TC.not_yet_implemented [%here] location
  | E_loop (_, _, _, _)                      -> TC.not_yet_implemented [%here] location
  | E_for (_, _, _, _, _, _)                 -> TC.not_yet_implemented [%here] location
  | E_vector_append (_, _)                   -> TC.not_yet_implemented [%here] location
  | E_list _                                 -> TC.not_yet_implemented [%here] location
  | E_cons (_, _)                            -> TC.not_yet_implemented [%here] location
  | E_struct (_, _)                          -> TC.not_yet_implemented [%here] location
  | E_struct_update (_, _)                   -> TC.not_yet_implemented [%here] location
  | E_field (_, _)                           -> TC.not_yet_implemented [%here] location
  | E_match (_, _)                           -> TC.not_yet_implemented [%here] location
  | E_let (_, _, _)                          -> TC.not_yet_implemented [%here] location
  | E_assign (_, _)                          -> TC.not_yet_implemented [%here] location
  | E_sizeof _                               -> TC.not_yet_implemented [%here] location
  | E_return _                               -> TC.not_yet_implemented [%here] location
  | E_exit _                                 -> TC.not_yet_implemented [%here] location
  | E_config _                               -> TC.not_yet_implemented [%here] location
  | E_ref _                                  -> TC.not_yet_implemented [%here] location
  | E_throw _                                -> TC.not_yet_implemented [%here] location
  | E_try (_, _)                             -> TC.not_yet_implemented [%here] location
  | E_assert (_, _)                          -> TC.not_yet_implemented [%here] location
  | E_var (_, _, _)                          -> TC.not_yet_implemented [%here] location
  | E_undef                                  -> TC.not_yet_implemented [%here] location
  | E_internal_plet (_, _, _)                -> TC.not_yet_implemented [%here] location
  | E_internal_return _                      -> TC.not_yet_implemented [%here] location
  | E_internal_value _                       -> TC.not_yet_implemented [%here] location
  | E_internal_assume (_, _)                 -> TC.not_yet_implemented [%here] location
  | E_constraint _                           -> TC.not_yet_implemented [%here] location

and value_of_bitvector_expression
      ~(location : S.l)
      ~(type_annotation : 'a)
      ~(bitvector_elements : Sail.type_annotation S.exp list) : Ast.Value.t TC.t
  =
  let _ = type_annotation in
  (*
    We expect the elements of the vector to be bools (bits).
  *)
  let convert_value_to_bool (value : Ast.Value.t) : bool TC.t =
    match value with
     | Bit b       -> TC.return b
     | Unit        -> TC.not_yet_implemented [%here] location
     | Bool _      -> TC.not_yet_implemented [%here] location
     | Int _       -> TC.not_yet_implemented [%here] location
     | String _    -> TC.not_yet_implemented [%here] location
     | Prod (_, _) -> TC.not_yet_implemented [%here] location
     | Bitvector _ -> TC.not_yet_implemented [%here] location
     | Record _    -> TC.not_yet_implemented [%here] location
     | Enum _      -> TC.not_yet_implemented [%here] location
  in
  let* bitvector_values = TC.map ~f:value_of_expression bitvector_elements
  in
  let* bitvector_bits = TC.map ~f:convert_value_to_bool bitvector_values
  in
  TC.return @@ Ast.Value.Bitvector bitvector_bits


(*
   Converts a value computed by Sail's interpreter to a nanosail value.
   The Sail type of the value is needed to find the names of record and enum types.
*)
let rec value_of_sail_value
      (env      : Libsail.Type_check.env)
      (location : S.l                   )
      (typ      : S.typ                 )
      (value    : S.value               ) : Ast.Value.t TC.t
  =
  let S.Typ_aux (unwrapped_type, _) = Libsail.Type_check.Env.expand_synonyms env typ
  in
  match value with
  | V_int n          -> TC.return @@ Ast.Value.Int n
  | V_bool b         -> TC.return @@ Ast.Value.Bool b
  | V_unit           -> TC.return @@ Ast.Value.Unit
  | V_string s       -> TC.return @@ Ast.Value.String s
  | V_bitvector bits -> begin
      let bool_of_bit (bit : Libsail.Extraction.Bit.bit) : bool TC.t =
        match bit with
        | B0 -> TC.return false
        | B1 -> TC.return true
      in
      let* bits = TC.map ~f:bool_of_bit bits
      in
      TC.return @@ Ast.Value.Bitvector bits
    end
  | V_member constructor_identifier -> begin
      match unwrapped_type with
      | Typ_id type_identifier -> begin
          let* type_identifier        = Identifier.translate_identifier [%here] type_identifier
          and* constructor_identifier = Identifier.translate_identifier [%here] constructor_identifier
          in
          TC.return @@ Ast.Value.Enum { type_identifier; constructor_identifier }
        end
      | Typ_internal_unknown
      | Typ_var _
      | Typ_fn (_, _)
      | Typ_bidir (_, _)
      | Typ_tuple _
      | Typ_app (_, _)
      | Typ_exist (_, _, _) -> TC.fail [%here] "enum value whose type is not an enum type"
    end
  | V_record field_values -> begin
      match unwrapped_type with
      | Typ_id sail_type_identifier -> begin
          let* type_identifier = Identifier.translate_identifier [%here] sail_type_identifier
          in
          let _type_quantifier, field_types = Libsail.Type_check.Env.get_record sail_type_identifier env
          in
          (* Take the fields in the order of the record's definition *)
          let field_value ((field_type, field_identifier) : S.typ * S.id) : (Ast.Identifier.t * Ast.Value.t) TC.t =
            let has_identifier ((identifier, _value) : S.id * S.value) : bool =
              Libsail.Ast_compare.Id.compare identifier field_identifier = 0
            in
            match List.find_opt has_identifier field_values with
            | Some (_, field_value) -> begin
                let* field_identifier' = Identifier.translate_identifier [%here] field_identifier
                and* field_value'      = value_of_sail_value env location field_type field_value
                in
                TC.return (field_identifier', field_value')
              end
            | None -> TC.fail [%here] "record value lacks a field"
          in
          let* fields = TC.map ~f:field_value field_types
          in
          TC.return @@ Ast.Value.Record { type_identifier; fields }
        end
      | Typ_app (_, _)        -> TC.not_yet_implemented ~message:"value of a parameterised record type" [%here] location
      | Typ_internal_unknown
      | Typ_var _
      | Typ_fn (_, _)
      | Typ_bidir (_, _)
      | Typ_tuple _
      | Typ_exist (_, _, _)   -> TC.fail [%here] "record value whose type is not a record type"
    end
  | V_tuple _        -> TC.not_yet_implemented ~message:"tuple value"  [%here] location
  | V_vector _       -> TC.not_yet_implemented ~message:"vector value" [%here] location
  | V_list _         -> TC.not_yet_implemented ~message:"list value"   [%here] location
  | V_real _         -> TC.not_yet_implemented ~message:"real value"   [%here] location
  | V_ref _          -> TC.not_yet_implemented ~message:"reference"    [%here] location
  | V_ctor (_, _)    -> TC.not_yet_implemented ~message:"union value"  [%here] location


(*
   The value of a top-level let.

   Literals are translated directly. Other expressions (calls, structs, struct updates,
   references to other top-level lets, ...) are evaluated at translation time by Sail's
   interpreter (see SailToNanosail.translate); the Rocq definition holds the resulting value.
*)
let value_of_top_level_let
      (identifier : S.id                         )
      (expression : Sail.type_annotation S.exp   ) : Ast.Value.t TC.t
  =
  let S.E_aux (unwrapped_expression, (location, _)) = expression
  in
  let is_literal : bool =
    match unwrapped_expression with
    | E_lit _                  -> true
    | E_vector _               -> true
    | E_block _                -> false
    | E_id _                   -> false
    | E_typ (_, _)             -> false
    | E_app (_, _)             -> false
    | E_tuple _                -> false
    | E_if (_, _, _)           -> false
    | E_loop (_, _, _, _)      -> false
    | E_for (_, _, _, _, _, _) -> false
    | E_vector_append (_, _)   -> false
    | E_list _                 -> false
    | E_cons (_, _)            -> false
    | E_struct (_, _)          -> false
    | E_struct_update (_, _)   -> false
    | E_field (_, _)           -> false
    | E_match (_, _)           -> false
    | E_let (_, _, _)          -> false
    | E_assign (_, _)          -> false
    | E_sizeof _               -> false
    | E_return _               -> false
    | E_exit _                 -> false
    | E_config _               -> false
    | E_ref _                  -> false
    | E_throw _                -> false
    | E_try (_, _)             -> false
    | E_assert (_, _)          -> false
    | E_var (_, _, _)          -> false
    | E_undef                  -> false
    | E_internal_plet (_, _, _)-> false
    | E_internal_return _      -> false
    | E_internal_value _       -> false
    | E_internal_assume (_, _) -> false
    | E_constraint _           -> false
  in
  if is_literal
  then value_of_expression expression
  else begin
      let* evaluated_value = TC.lookup_evaluated_top_level_value identifier
      in
      match evaluated_value with
      | Some (Ok value) -> begin
          let env = Libsail.Type_check.env_of expression
          and typ = Libsail.Type_check.typ_of expression
          in
          value_of_sail_value env location typ value
        end
      | Some (Error reason) -> TC.not_yet_implemented ~message:("Sail's interpreter could not evaluate this top-level let: " ^ reason) [%here] location
      | None                -> TC.not_yet_implemented ~message:"Sail's interpreter did not evaluate this top-level let" [%here] location
    end


let translate_value_definition
      (_definition_annotation : Sail.definition_annotation    )
      (pattern                : Sail.type_annotation S.pat    )
      (expression             : Sail.type_annotation S.exp    )
  =
  TC.translation_block [%here] (PP.string "Translating value definition") begin
    let S.P_aux (pattern, (pattern_location, _)) = pattern
    in
    let* identifier, translation =
      match pattern with
      | S.P_id identifier -> begin
          match identifier with
          | S.Id_aux (S.Id identifier, _identifier_location) -> begin
              let sail_identifier = S.Id_aux (S.Id identifier, _identifier_location)
              and identifier      = Ast.Identifier.mk identifier
              in
              let* value = value_of_top_level_let sail_identifier expression
              in
              TC.return (identifier, Ast.Definition.ValueDefinition { identifier; value })
            end
          | S.Id_aux (S.Operator _, _) -> TC.not_yet_implemented [%here] pattern_location
          | S.Id_aux (S.And_bool, _)   -> TC.not_yet_implemented [%here] pattern_location
          | S.Id_aux (S.Or_bool, _)    -> TC.not_yet_implemented [%here] pattern_location
        end
      | S.P_typ (_typ, pattern)         -> begin
          let S.P_aux (unwrapped_pattern, _) = pattern
          in
          match unwrapped_pattern with
          | S.P_id identifier -> begin
              match identifier with
              | S.Id_aux (S.Id identifier, _identifier_location) -> begin
                  let sail_identifier = S.Id_aux (S.Id identifier, _identifier_location)
                  and identifier      = Ast.Identifier.mk identifier
                  in
                  let* value = value_of_top_level_let sail_identifier expression
                  in
                  TC.return @@ (identifier, Ast.Definition.ValueDefinition { identifier; value })
                end
              | S.Id_aux (S.Operator _, _) -> TC.not_yet_implemented [%here] pattern_location
              | S.Id_aux (S.And_bool, _)   -> TC.not_yet_implemented [%here] pattern_location
              | S.Id_aux (S.Or_bool, _)    -> TC.not_yet_implemented [%here] pattern_location
            end
          | S.P_lit _                     -> TC.not_yet_implemented [%here] pattern_location
          | S.P_wild                      -> TC.not_yet_implemented [%here] pattern_location
          | S.P_or (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
          | S.P_not _                     -> TC.not_yet_implemented [%here] pattern_location
          | S.P_as (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
          | S.P_typ (_, _)                -> TC.not_yet_implemented [%here] pattern_location
          | S.P_var (_, _)                -> TC.not_yet_implemented [%here] pattern_location
          | S.P_app (_, _)                -> TC.not_yet_implemented [%here] pattern_location
          | S.P_vector _                  -> TC.not_yet_implemented [%here] pattern_location
          | S.P_vector_concat _           -> TC.not_yet_implemented [%here] pattern_location
          | S.P_vector_subrange (_, _, _) -> TC.not_yet_implemented [%here] pattern_location
          | S.P_tuple _                   -> TC.not_yet_implemented [%here] pattern_location
          | S.P_list _                    -> TC.not_yet_implemented [%here] pattern_location
          | S.P_cons (_, _)               -> TC.not_yet_implemented [%here] pattern_location
          | S.P_string_append _           -> TC.not_yet_implemented [%here] pattern_location
          | S.P_struct (_, _, _)          -> TC.not_yet_implemented [%here] pattern_location
        end
      | S.P_lit _                      -> TC.not_yet_implemented [%here] pattern_location
      | S.P_wild                       -> TC.not_yet_implemented [%here] pattern_location
      | S.P_or (_, _)                  -> TC.not_yet_implemented [%here] pattern_location
      | S.P_not _                      -> TC.not_yet_implemented [%here] pattern_location
      | S.P_as (_, _)                  -> TC.not_yet_implemented [%here] pattern_location
      | S.P_var (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
      | S.P_app (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
      | S.P_vector _                   -> TC.not_yet_implemented [%here] pattern_location
      | S.P_vector_concat _            -> TC.not_yet_implemented [%here] pattern_location
      | S.P_vector_subrange (_, _, _)  -> TC.not_yet_implemented [%here] pattern_location
      | S.P_tuple _                    -> TC.not_yet_implemented [%here] pattern_location
      | S.P_list _                     -> TC.not_yet_implemented [%here] pattern_location
      | S.P_cons (_, _)                -> TC.not_yet_implemented [%here] pattern_location
      | S.P_string_append _            -> TC.not_yet_implemented [%here] pattern_location
      | S.P_struct (_, _, _)           -> TC.not_yet_implemented [%here] pattern_location
    in
    let* () = TC.log [%here] Logging.info @@ lazy (PP.format "Translated value definition %s" @@ Ast.Identifier.to_string identifier)
    in
    TC.return translation
  end
