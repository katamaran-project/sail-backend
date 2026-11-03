open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


(*
   A union translated to a sum type does not get a muSail definition of its own:
   its values are values of the built-in ty.sum.
   We only leave a comment explaining how its constructors are represented.
*)
let pp_sum_definition (sum_definition : Ast.Definition.Type.Sum.t) : PP.t GC.t =
  GC.generation_block [%here] "Sum" begin
    let { identifier; left; right; type_quantifier = _ } : Ast.Definition.Type.Sum.t = sum_definition
    in
    let comment =
      PP.vertical [
        PP.format "union %s is represented by ty.sum:" (Ast.Identifier.to_string identifier);
        PP.indent begin
          PP.vertical [
            PP.format "%s corresponds to inl" (Ast.Identifier.to_string left.constructor_identifier);
            PP.format "%s corresponds to inr" (Ast.Identifier.to_string right.constructor_identifier);
          ]
        end
      ]
    in
    GC.return @@ PP.annotate [%here] @@ Coq.pp_multiline_comment comment
  end
