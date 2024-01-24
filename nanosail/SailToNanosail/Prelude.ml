open Ast
open Monads.Notations.Star(TranslationContext)

module TC = TranslationContext


let prelude = [
    TypeDefinition (TD_enum { identifier = "unit"; cases = [ "()" ] });
  ]


let register_types () =
  TC.iter ~f:TC.register prelude
