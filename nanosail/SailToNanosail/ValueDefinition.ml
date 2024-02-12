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
open Expression


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
