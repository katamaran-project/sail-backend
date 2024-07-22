open Base
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint


(* Name for the inductive type listing all enum types *)
let enums_identifier = Ast.Identifier.mk "Enums"


let generate (enum_definition : Ast.Definition.Type.Enum.t) : PP.document AC.t =
  let identifier = pp_identifier enum_definition.identifier
  and typ = pp_identifier @@ Ast.Identifier.mk "Set"
  in
  Coq.build_inductive_type identifier typ (fun add_constructor ->
      AC.iter ~f:add_constructor @@ List.map ~f:pp_identifier enum_definition.cases
    )


let generate_tags (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) =
  let enum_definitions =
    List.map ~f:snd enum_definitions
  in
  let identifier = Identifier.pp_identifier enums_identifier
  and typ = PP.string "Set"
  and tag_of_enum (enum_definition : Ast.Definition.Type.Enum.t) =
    let id = TranslationSettings.convert_enum_name_to_tag enum_definition.identifier
    in
    pp_identifier id
  in
  let inductive_type =
    Coq.build_inductive_type
      identifier
      typ
      (fun add_constructor ->
        AC.iter
          ~f:(fun enum_identifier ->
            add_constructor @@ tag_of_enum enum_identifier
          )
          enum_definitions
      )
  in
  Coq.annotate inductive_type


let generate_no_confusions (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) =
  let generate_derivation (enum_definition : Ast.Definition.Type.Enum.t) =
    Coq.derive_no_confusion_for enum_definition.identifier
  in
  List.map ~f:(Fn.compose generate_derivation snd) enum_definitions


let generate_eqdecs (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) =
  let enum_identifiers =
    List.map ~f:(fun (_, ed) -> ed.identifier) enum_definitions
  in
  List.map ~f:Coq.derive_eqdec_for enum_identifiers


let required_eqdecs (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) : Ast.Identifier.t list =
  let enum_identifiers =
    List.map ~f:(fun (_, ed) -> ed.identifier) enum_definitions
  in
  enums_identifier :: enum_identifiers
