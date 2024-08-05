open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


(* todo move this *)
let pp_bind (arg, t) =
  GC.return @@ PP.(utf8string ("\"" ^ (Ast.Identifier.string_of arg) ^ "\" ∷ " ) ^^ t)
