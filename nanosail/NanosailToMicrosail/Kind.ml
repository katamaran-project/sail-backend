open! ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let pp_kind (kind : Ast.Kind.t) : PP.document GC.t =
  match kind with
  | Type -> GC.not_yet_implemented [%here]
  | Int  -> GC.return @@ PP.annotate [%here] @@ PP.string @@ "nat"
  | Bool -> GC.not_yet_implemented [%here]
