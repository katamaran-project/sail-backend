module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)


let translate_type_definition
      (definition_annotation     : Sail.definition_annotation     )
      (annotated_type_definition : Sail.type_annotation S.type_def) : Ast.Definition.t TC.t
  =
  TC.translation_block [%here] (PP.string "Translating type definition") begin
    let S.TD_aux (type_definition, type_annotation) = annotated_type_definition
    in
    let location = definition_annotation.loc
    in
    let register translation =
      let* result = translation
      in
      TC.return @@ Ast.Definition.TypeDefinition result
    in
    match type_definition with
    | TD_abbrev (identifier, quantifier, arg)                      -> register @@ TypeAbbreviation.translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
    | TD_variant (identifier, type_quantifier, constructors, flag) -> register @@ Variant.translate_variant definition_annotation type_annotation identifier type_quantifier constructors flag
    | TD_enum (identifier, cases, _)                               -> register @@ Enum.translate_enum definition_annotation type_annotation identifier cases
    | TD_record (identifier, quantifier, fields, _)                -> register @@ Record.translate_record definition_annotation type_annotation identifier quantifier fields
    | TD_bitfield (_, _, _)                                        -> TC.not_yet_implemented [%here] location
    | TD_abstract (_, _)                                           -> TC.not_yet_implemented [%here] location
  end
