open Base
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_value (value : Ast.Value.t) : PP.document GC.t =
  match value with
  | Unit        -> GC.return @@ PP.annotate [%here] @@ PP.(string "tt")
  | Int n       -> GC.return @@ PP.annotate [%here] @@ PP.(string @@ Z.to_string n)
  | Bool b      -> GC.return @@ PP.annotate [%here] @@ PP.string @@ if b then "true" else "false"
  | String _    -> GC.not_yet_implemented [%here]
  | Prod (_, _) -> GC.not_yet_implemented [%here]


let pp_value_definition (value_definition : Ast.Definition.value_definition) : PP.document GC.t =
  let { identifier; value } : Ast.Definition.value_definition = value_definition
  in
  let definition =
    let identifier  = PP.annotate [%here] @@ Identifier.pp identifier
    and result_type = None
    in
    let* body = pp_value value
    in
    GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~result_type body
  in
  definition


let generate (definitions : (Sail.sail_definition * Ast.Definition.t) list) : PP.document GC.t =
  let* coq_lines =
    let value_definitions =
      List.map ~f:snd @@ Ast.Definition.Select.(select value_definition definitions)
    in
    GC.map ~f:pp_value_definition value_definitions
  in
  GC.return @@ PP.annotate [%here] @@ PP.vertical coq_lines
