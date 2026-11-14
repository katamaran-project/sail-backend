open ExtBase
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(*
   Rocq term for a value, of type Val σ where σ is the µSail type of the value.

   Records use Rocq's record syntax with the prefixed projection names (see Identifier.record_field_projection_name):

     {| Capability_tag := false; Capability_address := @bv.mk 32 0 I; ... |}

   Enum values are the constructors of the enum's inductive type.
*)
let rec pp_value (value : Ast.Value.t) : PP.t GC.t =
  match value with
  | Unit         -> GC.return @@ PP.annotate [%here] @@ PP.(string "tt")
  | Int n        -> GC.return @@ PP.annotate [%here] @@ Coq.pp_scope ~scope:(PP.string "Z") PP.(string @@ Z.to_string n)
  | Bool b       -> GC.return @@ PP.annotate [%here] @@ PP.string @@ if b then "true" else "false"
  | String s     -> GC.return @@ PP.annotate [%here] @@ Coq.pp_string s
  | Prod (_, _)  -> GC.not_yet_implemented [%here]
  | Bit b        -> GC.return @@ PP.annotate [%here] @@ MuSail.Value.pp_bit b
  | Bitvector bv -> GC.return @@ PP.annotate [%here] @@ MuSail.Value.pp_bitvector (List.length bv) (Util.convert_bits_to_z bv)
  | Record { type_identifier; fields } -> begin
      let pp_field (field_identifier, field_value) : (PP.t * PP.t) GC.t =
        let* pp_field_value = pp_value field_value
        in
        let pp_field_identifier =
          Identifier.pp @@ Identifier.record_field_projection_name type_identifier field_identifier
        in
        GC.return (pp_field_identifier, PP.(surround parens) pp_field_value)
      in
      let* pp_fields = GC.map ~f:pp_field fields
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_record_value pp_fields
    end
  | Enum { type_identifier = _; constructor_identifier } -> begin
      GC.return @@ PP.annotate [%here] @@ Identifier.pp constructor_identifier
    end


let pp_value_definition (value_definition : Ast.Definition.Value.t) : PP.t GC.t =
  GC.block begin
      let { identifier; value } : Ast.Definition.Value.t = value_definition
      in
      let definition =
        let identifier = PP.annotate [%here] @@ Identifier.pp identifier
        in
        let* body = pp_value value
        in
        let* result_type =
          GC.lift_option (Some (Type.coq_type_of_nanotype @@ Ast.Value.type_of_value value))
        in
        GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~result_type body
      in
      definition
    end


let generate (definitions : (Sail.sail_definition * Ast.Definition.t) list) : PP.t GC.t =
  let* coq_lines =
    let value_definitions =
      Ast.Definition.Select.(select (without_sail_definition value_definition) definitions)
    in
    GC.map ~f:pp_value_definition value_definitions
  in
  GC.return @@ PP.annotate [%here] @@ PP.vertical coq_lines
