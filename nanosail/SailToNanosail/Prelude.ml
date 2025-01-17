open Monads.Notations.Star(TranslationContext)

module TC = TranslationContext


let prelude = [
    Ast.Definition.TypeDefinition (Enum { identifier = Ast.Identifier.mk "unit"; cases = [ Ast.Identifier.mk "()" ] });
  ]


let register_types () =
  TC.iter ~f:TC.store_definition prelude
