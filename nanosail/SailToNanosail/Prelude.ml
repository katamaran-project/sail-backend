open Ast
open Monads.Notations.Star(TranslationContext)

module TC = TranslationContext


let prelude = [
    TypeDefinition (TD_enum { identifier = Id.mk "unit"; cases = [ Id.mk "()" ] });
  ]


let register_types () =
  TC.iter ~f:TC.register prelude
