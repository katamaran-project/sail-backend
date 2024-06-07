open Base
open PPrint
open Monads.Notations.Star(AnnotationContext)
open Identifier

module AC = AnnotationContext
module PP = PPrint


let generate (record_definition : Ast.record_definition) : document AC.t =
  let generate_field field_identifier field_type =
    let* field_type' = Nanotype.coq_type_of_nanotype field_type
    in
    AC.return (pp_identifier field_identifier, field_type')
  in
  let* identifier  = AC.return @@ pp_identifier record_definition.identifier
  and* type_name   = AC.return @@ pp_identifier @@ Ast.Identifier.mk "Set"
  and* constructor = AC.return @@ pp_identifier @@ Ast.Identifier.add_prefix "Mk" record_definition.identifier (* todo: allow custom name *)
  and* fields      = AC.map ~f:(Auxlib.uncurry generate_field) record_definition.fields
  in
  AC.return @@ Coq.record ~identifier ~type_name ~constructor ~fields

