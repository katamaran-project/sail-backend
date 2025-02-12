open! ExtBase

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)


let translate_record
      (_definition_annotation : Sail.definition_annotation  )
      (_type_annotation       : Sail.type_annotation S.annot)
      (identifier             : S.id                        )
      (type_quantifier        : S.typquant                  )
      (fields                 : (S.typ * S.id) list         ) : Ast.Definition.Type.t TC.t
  =
  TC.translation_block [%here] (Logging.Message.string "Translating record definition") begin
    let translate_field (field_type : S.typ) (field_identifier : S.id) =
      let* field_type'       = Nanotype.nanotype_of_sail_type field_type
      and* field_identifier' = Identifier.translate_identifier [%here] field_identifier
      in
      TC.return @@ (field_identifier', field_type')
    in
    let* identifier      = Identifier.translate_identifier [%here] identifier
    and* type_quantifier = TypeQuantifier.translate_type_quantifier type_quantifier
    and* fields          = TC.map ~f:(Fn.uncurry translate_field) fields
    in
    TC.return @@ Ast.Definition.Type.Record {
      identifier;
      type_quantifier;
      fields
    }
  end
