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
open Identifier
open Nanotype
open TypeQuantifier
open Enum


let translate_variant
      (_definition_annotation : S.def_annot                 )
      (_type_annotation       : Sail.type_annotation S.annot)
      (identifier             : S.id                        )
      (type_quantifier        : S.typquant                  )
      (constructors           : S.type_union list           )
      (_flag                  : bool                        ) : N.type_definition TC.t
  =
  let* identifier' = translate_identifier [%here] identifier
  and* type_quantifier' = translate_type_quantifier type_quantifier
  and* constructors' =
    let translate_constructor (S.Tu_aux (Tu_ty_id (typ, identifier), _annotation)) =
      let* identifier' = translate_identifier [%here] identifier
      and* typ' = nanotype_of_sail_type typ
      in
      let field_nanotypes = match typ' with
        | N.Ty_tuple ts -> ts
        | _             -> [ typ' ]
      in
      TC.return @@ (identifier', field_nanotypes)
    in
    TC.map ~f:translate_constructor constructors
  in
  TC.return @@ N.TD_variant {
      identifier      = identifier'     ;
      type_quantifier = type_quantifier';
      constructors    = constructors'   ;
    }
