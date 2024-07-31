open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


(* Name for the inductive type listing all variant/union types *)
let records_inductive_type_identifier = Ast.Identifier.mk "Records"

let derive_constructor_from_identifier identifier =
  Identifier.record_constructor_name identifier


let generate (record_definition : Ast.Definition.Type.Record.t) : PP.document AC.t =
  let generate_field field_identifier field_type =
    let* field_type' = Nanotype.coq_type_of_nanotype field_type
    in
    AC.return (Identifier.pp field_identifier, field_type')
  in
  let identifier  = Identifier.pp record_definition.identifier
  and type_name   = Identifier.pp @@ Ast.Identifier.mk "Set"
  and constructor = Identifier.pp @@ derive_constructor_from_identifier record_definition.identifier
  in
  let* fields     = AC.map ~f:(Auxlib.uncurry generate_field) record_definition.fields
  in
  AC.return @@ Coq.record ~identifier ~type_name ~constructor ~fields


let generate_tags (record_definitions : (Sail.sail_definition * Ast.Definition.Type.Record.t) list) =
  let record_definitions =
    List.map ~f:snd record_definitions
  in
  let identifier = Identifier.pp records_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_record (record_definition : Ast.Definition.Type.Record.t) =
    let id = Identifier.reified_record_name record_definition.identifier
    in
    Identifier.pp id
  in
  let inductive_type =
    Coq.build_inductive_type
      identifier
      typ
      (fun add_constructor ->
        AC.iter
          ~f:(fun record_identifier ->
            add_constructor @@ tag_of_record record_identifier
          )
          record_definitions
      )
  in
  Coq.annotate inductive_type


let generate_tag_match
    ?(scope               : string option                                                   = None)
    ~(matched_identifier  : Ast.Identifier.t                                                      )
    ~(record_definitions  : Ast.Definition.Type.Record.t list                                     )
    ~(record_case_handler : Ast.Definition.Type.Record.t -> (PP.document * PP.document) AC.t      ) () : PP.document AC.t
  =
  let scope = Option.map ~f:PP.string scope
  in
  let* cases = AC.map ~f:record_case_handler record_definitions
  in
  AC.return @@ Coq.match' ~scope (Identifier.pp matched_identifier) cases


let collect_identifiers (record_definitions : (Sail.sail_definition * Ast.Definition.Type.Record.t) list) : Ast.Identifier.t list =
  let record_identifiers =
    List.map ~f:(fun (_, rd) -> rd.identifier) record_definitions
  in
  records_inductive_type_identifier :: record_identifiers


let required_no_confusions = collect_identifiers
let required_eqdecs        = collect_identifiers
