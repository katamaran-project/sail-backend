open Base
open Monads.Notations.Star(GenerationContext)



let pp_bind (arg, t) =
  PP.(utf8string ("\"" ^ (Ast.Identifier.string_of arg) ^ "\" ∷ " ) ^^ t)
