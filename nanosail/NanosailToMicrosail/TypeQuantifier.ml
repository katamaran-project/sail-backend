open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_type_quantifier (quantifier : Ast.Definition.TypeQuantifier.t) : (PP.document * PP.document option) list GC.t =
  let pp_type_quantifier_item
        (identifier : Ast.Identifier.t)
        (kind       : Ast.Kind.t      ) : (PP.document * PP.document option) GC.t
    =
    let identifier' =
      PP.annotate [%here] @@ Identifier.pp identifier
    in
    let* kind' =
      GC.pp_annotate [%here] @@ Kind.pp_kind kind
    in
    GC.return (identifier', Some kind')
  in
  let Ast.Definition.TypeQuantifier.TypeQuantifier items =  quantifier
  in
  GC.map ~f:(Auxlib.uncurry pp_type_quantifier_item) items
