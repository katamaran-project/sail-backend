open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


(* Name for the inductive type listing all enum types *)
let enums_inductive_type_identifier = Ast.Identifier.mk "Enums"


let generate (enum_definition : Ast.Definition.Type.Enum.t) : PP.document AC.t =
  let identifier = Identifier.pp enum_definition.identifier
  and typ = Identifier.pp @@ Ast.Identifier.mk "Set"
  in
  Coq.build_inductive_type identifier typ (fun add_constructor ->
      AC.iter ~f:add_constructor @@ List.map ~f:Identifier.pp enum_definition.cases
    )


let generate_tags (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) =
  let enum_definitions =
    List.map ~f:snd enum_definitions
  in
  let identifier = Identifier.pp enums_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_enum (enum_definition : Ast.Definition.Type.Enum.t) =
    let id = Identifier.reified_enum_name enum_definition.identifier
    in
    Identifier.pp id
  in
  let inductive_type =
    Coq.build_inductive_type
      identifier
      typ
      (
        fun add_constructor -> begin
            let* () = add_constructor @@ Identifier.pp Registers.regname_tag
            in
            AC.iter
              ~f:(fun enum_identifier ->
                  add_constructor @@ tag_of_enum enum_identifier
                )
              enum_definitions
          end
      )
  in
  Coq.annotate inductive_type


let collect_identifiers (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) : Ast.Identifier.t list =
  let enum_identifiers =
    List.map ~f:(fun (_, ed) -> ed.identifier) enum_definitions
  in
  enums_inductive_type_identifier :: enum_identifiers


let extra_eqdec_identifiers () =
  [ enums_inductive_type_identifier ]


let required_no_confusions = collect_identifiers
let required_eqdecs        = collect_identifiers


let generate_enum_finiteness
      (enum_definition  : Ast.Definition.Type.Enum.t)
  =
  let identifier = Identifier.pp @@ enum_definition.identifier
  and type_name  = Identifier.pp @@ enum_definition.identifier
  and values     = List.map ~f:Identifier.pp enum_definition.cases
  in
  Coq.finite_instance ~identifier ~type_name ~values


let generate_finiteness (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) =
  let definitions = List.map ~f:snd enum_definitions
  in
  List.map ~f:generate_enum_finiteness definitions
