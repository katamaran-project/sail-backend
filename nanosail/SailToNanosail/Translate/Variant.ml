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


let translate_variant
      (_definition_annotation : Sail.definition_annotation  )
      (_type_annotation       : Sail.type_annotation S.annot)
      (identifier             : S.id                        )
      (type_quantifier        : S.typquant                  )
      (constructors           : S.type_union list           )
      (_flag                  : bool                        ) : Ast.Definition.Type.t TC.t
  =
  let* identifier'      = Identifier.translate_identifier [%here] identifier
  and* type_quantifier' = TypeQuantifier.translate_type_quantifier type_quantifier
  and* constructors'    =
    let translate_constructor (S.Tu_aux (Tu_ty_id (typ, identifier), _annotation)) =
      let* identifier' = Identifier.translate_identifier [%here] identifier
      and* typ' = Nanotype.nanotype_of_sail_type typ
      in
      let field_nanotypes =
        match typ' with
        | Tuple ts         -> ts
        | Unit             -> []
        | _                -> [ typ' ]
      in
      TC.return @@ (identifier', field_nanotypes)
    in
    TC.map ~f:translate_constructor constructors
  in
  TC.return @@ Ast.Definition.Type.Variant {
      identifier      = identifier'     ;
      type_quantifier = type_quantifier';
      constructors    = constructors'   ;
    }
