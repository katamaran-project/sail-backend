(* TODO find a better home for this orphan; put here as a simple solution to avoid cyclic dependencies between modules *)
let pp_identifier (identifier : Ast.Identifier.t) =
  PP.string @@ Ast.Identifier.string_of identifier
