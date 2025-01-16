open Base
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_value (value : Ast.Value.t) : PP.document GC.t =
  match value with
  | Unit         -> GC.return @@ PP.annotate [%here] @@ PP.(string "tt")
  | Int n        -> GC.return @@ PP.annotate [%here] @@ Coq.pp_scope (PP.string "Z") @@ PP.(string @@ Z.to_string n)
  | Bool b       -> GC.return @@ PP.annotate [%here] @@ PP.string @@ if b then "true" else "false"
  | String s     -> GC.return @@ PP.annotate [%here] @@ Coq.pp_string s
  | Prod (_, _)  -> GC.not_yet_implemented [%here]
  | Bit b        -> GC.return @@ PP.annotate [%here] @@ MuSail.Value.pp_bit b
  | Bitvector bv -> GC.return @@ PP.annotate [%here] @@ MuSail.Value.pp_bitvector (List.length bv) (Util.convert_bits_to_z bv)


let pp_value_definition (value_definition : Ast.Definition.Value.t) : PP.document GC.t =
  GC.block begin
      let { identifier; value } : Ast.Definition.Value.t = value_definition
      in
      let definition =
        let identifier = PP.annotate [%here] @@ Identifier.pp identifier
        in
        let* body = pp_value value
        in
        let* result_type =
          GC.lift_option (Some (Nanotype.coq_type_of_nanotype @@ Ast.Value.type_of_value value))
        in
        GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~result_type body
      in
      definition
    end


let generate (definitions : (Sail.sail_definition * Ast.Definition.t) list) : PP.document GC.t =
  let* coq_lines =
    let value_definitions =
      Ast.Definition.Select.(select value_definition @@ drop_sail_definitions definitions)
    in
    GC.map ~f:pp_value_definition value_definitions
  in
  GC.return @@ PP.annotate [%here] @@ PP.vertical coq_lines
