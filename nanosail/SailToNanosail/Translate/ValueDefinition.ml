open Base

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


let translate_expression (expression : Sail.type_annotation S.exp) : N.value TC.t =
  let S.E_aux (expression, (location, _type_annotation)) = expression
  in
  match expression with
  | S.E_lit literal -> begin
     let S.L_aux (literal, _literal_location) = literal
     in
     match literal with
      | S.L_num n    -> TC.return @@ N.Val_int n
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


let translate_value_definition
      (_definition_annotation : S.def_annot)
      (let_definition : Sail.type_annotation S.letbind)
  =
  let S.LB_aux (S.LB_val (S.P_aux (pattern, (pattern_location, _)), expression), (_location, _type_annotation)) = let_definition
  in
  match pattern with
  | S.P_id identifier -> begin
      match identifier with
      | S.Id_aux (S.Id identifier, _identifier_location) -> begin
          let identifier = Id.mk identifier
          in
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
