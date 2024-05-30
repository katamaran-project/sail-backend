open Variant (* must be opened before Base due to name clash *)

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
open TypeAbbreviation
open Enum
open Record


let translate_type_definition
      (definition_annotation     : S.def_annot                    )
      (annotated_type_definition : Sail.type_annotation S.type_def) : N.definition TC.t
  =
  let S.TD_aux (type_definition, type_annotation) = annotated_type_definition
  in
  let register translation =
    let* result = translation
    in
    TC.return @@ N.TypeDefinition result
  in
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg)                      -> register @@ translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_variant (identifier, type_quantifier, constructors, flag) -> register @@ translate_variant definition_annotation type_annotation identifier type_quantifier constructors flag
  | TD_enum (identifier, cases, _)                               -> register @@ translate_enum definition_annotation type_annotation identifier cases
  | TD_record (identifier, quantifier, fields, _)                -> register @@ translate_record definition_annotation type_annotation identifier quantifier fields
  | TD_bitfield (_, _, _)                                        -> TC.not_yet_implemented [%here] definition_annotation.loc
