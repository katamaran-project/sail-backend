open Base
open PPrint
open Ast
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint


let generate (enum_definition : enum_definition) : AC.annotation AC.t =
  let identifier = pp_identifier enum_definition.identifier
  and typ = pp_identifier "Set"
  in
  Coq.mbuild_inductive_type identifier typ (fun add_constructor ->
      AC.iter ~f:add_constructor @@ List.map ~f:string enum_definition.cases
    )

let generate_enum_of_enums (enum_definitions : (sail_definition * enum_definition) list) =
  let enum_definitions =
    List.map ~f:snd enum_definitions
  in
  let identifier = string "Enums"
  and typ = string "Set"
  and constructor_of_enum (enum_definition : enum_definition) =
    string @@ "E" ^ String.lowercase enum_definition.identifier
  in
  let inductive_type =
    Coq.mbuild_inductive_type
      identifier
      typ
      (fun add_constructor ->
        AC.iter
          ~f:(fun enum_identifier ->
            add_constructor (constructor_of_enum enum_identifier)
          )
          enum_definitions
      )
  in
  Coq.annotate inductive_type

let generate_no_confusions (enum_definitions : (sail_definition * enum_definition) list) =
  let enum_definitions = List.map ~f:snd enum_definitions
  in
  let contents =
    let set_transparent_obligations =
      string "Local Set Transparent Obligations."
    in
    let derivations =
      let generate_derivation (enum_definition : enum_definition) =
        string @@ Printf.sprintf "Derive NoConfusion for %s." enum_definition.identifier
      in
      let lines =
        List.map ~f:generate_derivation enum_definitions
      in
      separate hardline lines
    in
    set_transparent_obligations ^^ twice hardline ^^ derivations
  in
  Coq.section "TransparentObligations" contents

let generate_eqdecs (enum_definitions : (sail_definition * enum_definition) list) =
  let enum_definitions = List.map ~f:snd enum_definitions (* todo cleanup *)
  in
  let generate_eqdec (enum_definition : enum_definition) =
    string @@ Printf.sprintf "Derive EqDec for %s." enum_definition.identifier
  in
  let lines =
    List.map ~f:generate_eqdec enum_definitions
  in
  separate hardline lines
