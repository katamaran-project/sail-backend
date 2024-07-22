open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


(* Name for the inductive type listing all variant/union types *)
let records_inductive_type_identifier = Ast.Identifier.mk "Unions"


let generate (record_definition : Ast.Definition.Type.Record.t) : PP.document AC.t =
  let generate_field field_identifier field_type =
    let* field_type' = Nanotype.coq_type_of_nanotype field_type
    in
    AC.return (Identifier.pp_identifier field_identifier, field_type')
  in
  let* identifier  = AC.return @@ Identifier.pp_identifier record_definition.identifier
  and* type_name   = AC.return @@ Identifier.pp_identifier @@ Ast.Identifier.mk "Set"
  and* constructor = AC.return @@ Identifier.pp_identifier @@ Ast.Identifier.add_prefix "Mk" record_definition.identifier (* todo: allow custom name *)
  and* fields      = AC.map ~f:(Auxlib.uncurry generate_field) record_definition.fields
  in
  AC.return @@ Coq.record ~identifier ~type_name ~constructor ~fields


let generate_tags (record_definitions : (Sail.sail_definition * Ast.Definition.Type.Record.t) list) =
  let record_definitions =
    List.map ~f:snd record_definitions
  in
  let identifier = Identifier.pp_identifier records_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_record (record_definition : Ast.Definition.Type.Record.t) =
    let id = TranslationSettings.convert_record_name_to_tag record_definition.identifier
    in
    Identifier.pp_identifier id
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


let generate_no_confusions (record_definitions : (Sail.sail_definition * Ast.Definition.Type.Record.t) list) =
  let generate_derivation (record_definition : Ast.Definition.Type.Record.t) =
    Coq.derive_no_confusion_for record_definition.identifier
  in
  List.map ~f:(Fn.compose generate_derivation snd) record_definitions


let generate_tag_match
    ~(matched_identifier  : Ast.Identifier.t                                         )
    ~(record_definitions  : Ast.Definition.Type.Record.t list                        )
    ~(record_case_handler : Ast.Definition.Type.Record.t -> PP.document * PP.document) : PP.document
  =
  Coq.match' (Identifier.pp_identifier matched_identifier) @@ List.map ~f:record_case_handler record_definitions


let required_eqdecs (record_definitions : (Sail.sail_definition * Ast.Definition.Type.Record.t) list) : Ast.Identifier.t list =
  let record_identifiers =
    List.map ~f:(fun (_, rd) -> rd.identifier) record_definitions
  in
  records_inductive_type_identifier :: record_identifiers
