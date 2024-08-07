open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let pp_type_quantifier quantifier =
  let pp_type_quantifier_item (identifier, kind) =
    let identifier' = Identifier.pp identifier
    in
    let* kind' = Kind.pp_kind kind
    in
    GC.return (identifier', Some kind')
  in
  GC.map ~f:pp_type_quantifier_item quantifier
