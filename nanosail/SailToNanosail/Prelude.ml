open Monads.Notations.Star(TranslationContext)

module TC = TranslationContext


let prelude = [
    Ast.Definition.TypeDefinition (TD_enum { identifier = Ast.Identifier.mk "unit"; cases = [ Ast.Identifier.mk "()" ] });
  ]


let register_types () =
  TC.iter ~f:TC.register prelude
