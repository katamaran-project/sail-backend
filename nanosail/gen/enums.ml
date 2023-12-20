open PPrint
open Ast


let generate_inductive_type (sail_definition : sail_definition) (enum_definition : enum_definition) =
  let coq_translation =
    let identifier = Sail.pp_identifier enum_definition.identifier
    and typ = Sail.pp_identifier "Set"
    in
    Coq.build_inductive_type identifier typ (fun add_constructor ->
        List.iter add_constructor @@ List.map string enum_definition.cases
      )
  in
  Coq.annotate_with_original_definition sail_definition coq_translation

let generate_constructors_inductive_type (_sail_definition : sail_definition) (enum_definition : enum_definition) =
  let identifier = Sail.pp_identifier @@ enum_definition.identifier ^ "Constructor"
  and typ = Sail.pp_identifier "Set"
  in
  Coq.build_inductive_type identifier typ (fun add_constructor ->
      List.iter
        (fun (case : string) ->
          add_constructor @@ string @@ "K" ^ case)
        enum_definition.cases
    )

let generate_enum_of_enums (enum_definitions : (sail_definition * enum_definition) list) =
  let enum_definitions =
    List.map snd enum_definitions
  in
  let identifier = string "Enums"
  and typ = string "Set"
  and constructor_of_enum (enum_definition : enum_definition) =
    string @@ "E" ^ String.lowercase_ascii enum_definition.identifier
  in
  Coq.build_inductive_type
    identifier
    typ
    (fun add_constructor ->
      List.iter
        (fun enum_identifier ->
          add_constructor (constructor_of_enum enum_identifier)
        )
        enum_definitions
    )

let generate_no_confusions (enum_definitions : (sail_definition * enum_definition) list) =
  let enum_definitions = List.map snd enum_definitions
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
        List.map generate_derivation enum_definitions
      in
      separate hardline lines
    in
    set_transparent_obligations ^^ twice hardline ^^ derivations
  in
  Coq.section "TransparentObligations" contents

let generate_eqdecs (enum_definitions : (sail_definition * enum_definition) list) =
  let enum_definitions = List.map snd enum_definitions
  in
  let generate_eqdec (enum_definition : enum_definition) =
    string @@ Printf.sprintf "Derive EqDec for %s." enum_definition.identifier
  in
  let lines =
    List.map generate_eqdec enum_definitions
  in
  separate hardline lines
