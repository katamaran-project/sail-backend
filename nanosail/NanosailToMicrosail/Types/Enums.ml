open Base
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint


let generate (enum_definition : Ast.enum_definition) : PP.document AC.t =
  let identifier = pp_identifier enum_definition.identifier
  and typ = pp_identifier @@ Ast.Identifier.mk "Set"
  in
  Coq.build_inductive_type identifier typ (fun add_constructor ->
      AC.iter ~f:add_constructor @@ List.map ~f:pp_identifier enum_definition.cases
    )


let generate_tags (enum_definitions : (Sail.sail_definition * Ast.enum_definition) list) =
  let enum_definitions =
    List.map ~f:snd enum_definitions
  in
  let identifier = PP.string "Enums"
  and typ = PP.string "Set"
  and tag_of_enum (enum_definition : Ast.enum_definition) =
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


let generate_no_confusions (enum_definitions : (Sail.sail_definition * Ast.enum_definition) list) =
  let enum_definitions = List.map ~f:snd enum_definitions
  in
  let contents =
    let set_transparent_obligations =
      PP.string "Local Set Transparent Obligations."
    in
    let derivations =
      let generate_derivation (enum_definition : Ast.enum_definition) =
        PP.string @@ Printf.sprintf "Derive NoConfusion for %s." (Ast.Identifier.string_of enum_definition.identifier)
      in
      let lines =
        List.map ~f:generate_derivation enum_definitions
      in
      PP.separate PP.hardline lines
    in
    PP.(set_transparent_obligations ^^ twice hardline ^^ derivations)
  in
  Coq.section (Ast.Identifier.mk "TransparentObligations") contents


let generate_eqdecs (enum_definitions : (Sail.sail_definition * Ast.enum_definition) list) =
  let enum_definitions = List.map ~f:snd enum_definitions (* todo cleanup *)
  in
  let generate_eqdec (enum_definition : Ast.enum_definition) =
    PP.string @@ Printf.sprintf "Derive EqDec for %s." (Ast.Identifier.string_of @@ enum_definition.identifier)
  in
  let lines =
    List.map ~f:generate_eqdec enum_definitions
  in
  PP.separate PP.hardline lines
