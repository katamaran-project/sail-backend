open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let pp_kind (kind : Ast.Kind.t) : PP.t GC.t =
  match kind with
  | Type -> GC.not_yet_implemented [%here]
  | Int  -> GC.return @@ PP.annotate [%here] @@ PP.string @@ "nat"
  | Bool -> GC.not_yet_implemented [%here]
