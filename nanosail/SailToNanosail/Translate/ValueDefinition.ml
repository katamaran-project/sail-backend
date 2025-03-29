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
  | E_lit literal                            -> Literal.value_of_literal literal
  | E_vector bitvector_elements              -> value_of_bitvector_expression ~location ~type_annotation ~bitvector_elements
  | E_block _                                -> TC.not_yet_implemented [%here] location
  | E_id _                                   -> TC.not_yet_implemented [%here] location
  | E_typ (_, _)                             -> TC.not_yet_implemented [%here] location
  | E_app (_, _)                             -> TC.not_yet_implemented [%here] location
  | E_app_infix (_, _, _)                    -> TC.not_yet_implemented [%here] location
  | E_tuple _                                -> TC.not_yet_implemented [%here] location
  | E_if (_, _, _)                           -> TC.not_yet_implemented [%here] location
  | E_loop (_, _, _, _)                      -> TC.not_yet_implemented [%here] location
  | E_for (_, _, _, _, _, _)                 -> TC.not_yet_implemented [%here] location
  | E_vector_access (_, _)                   -> TC.not_yet_implemented [%here] location
  | E_vector_subrange (_, _, _)              -> TC.not_yet_implemented [%here] location
  | E_vector_update (_, _, _)                -> TC.not_yet_implemented [%here] location
  | E_vector_update_subrange (_, _, _, _)    -> TC.not_yet_implemented [%here] location
  | E_vector_append (_, _)                   -> TC.not_yet_implemented [%here] location
  | E_list _                                 -> TC.not_yet_implemented [%here] location
  | E_cons (_, _)                            -> TC.not_yet_implemented [%here] location
  | E_struct _                               -> TC.not_yet_implemented [%here] location
  | E_struct_update (_, _)                   -> TC.not_yet_implemented [%here] location
  | E_field (_, _)                           -> TC.not_yet_implemented [%here] location
  | E_match (_, _)                           -> TC.not_yet_implemented [%here] location
  | E_let (_, _)                             -> TC.not_yet_implemented [%here] location
  | E_assign (_, _)                          -> TC.not_yet_implemented [%here] location
  | E_sizeof _                               -> TC.not_yet_implemented [%here] location
  | E_return _                               -> TC.not_yet_implemented [%here] location
  | E_exit _                                 -> TC.not_yet_implemented [%here] location
  | E_ref _                                  -> TC.not_yet_implemented [%here] location
  | E_throw _                                -> TC.not_yet_implemented [%here] location
  | E_try (_, _)                             -> TC.not_yet_implemented [%here] location
  | E_assert (_, _)                          -> TC.not_yet_implemented [%here] location
  | E_var (_, _, _)                          -> TC.not_yet_implemented [%here] location
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
  in
  let* bitvector_values = TC.map ~f:value_of_expression bitvector_elements
  in
  let* bitvector_bits = TC.map ~f:convert_value_to_bool bitvector_values
  in
  TC.return @@ Ast.Value.Bitvector bitvector_bits


let translate_value_definition
      (_definition_annotation : Sail.definition_annotation    )
      (let_definition         : Sail.type_annotation S.letbind)
  =
  TC.translation_block [%here] (PP.string "Translating value definition") begin
    let S.LB_aux (S.LB_val (S.P_aux (pattern, (pattern_location, _)), expression), (_location, _type_annotation)) = let_definition
    in
    let* identifier, translation =
      match pattern with
      | S.P_id identifier -> begin
          match identifier with
          | S.Id_aux (S.Id identifier, _identifier_location) -> begin
              let identifier = Ast.Identifier.mk identifier
              in
              let* value = value_of_expression expression
              in
              TC.return (identifier, Ast.Definition.ValueDefinition { identifier; value })
            end
          | S.Id_aux (S.Operator _, _) -> TC.not_yet_implemented [%here] pattern_location
        end
      | S.P_typ (_typ, pattern)         -> begin
          let S.P_aux (unwrapped_pattern, _) = pattern
          in
          match unwrapped_pattern with
          | S.P_id identifier -> begin
              match identifier with
              | S.Id_aux (S.Id identifier, _identifier_location) -> begin
                  let identifier = Ast.Identifier.mk identifier
                  in
                  let* value = value_of_expression expression
                  in
                  TC.return @@ (identifier, Ast.Definition.ValueDefinition { identifier; value })
                end
              | S.Id_aux (S.Operator _, _) -> TC.not_yet_implemented [%here] pattern_location
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
          | S.P_struct (_, _)             -> TC.not_yet_implemented [%here] pattern_location
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
      | S.P_struct (_, _)              -> TC.not_yet_implemented [%here] pattern_location
    in
    let* () = TC.log [%here] Logging.info @@ lazy (PP.format "Translated value definition %s" @@ Ast.Identifier.to_string identifier)
    in
    TC.return translation
  end
