open PPrint
open Ast
open Util


module PP = struct
  include PPrint
  
  module Coq = Coq_generation

  module Katamaran = struct
    module Registers = Pp_registers
  end
end


let generate_inductive_type (enum_definitions : (sail_definition * enum_definition) list) =
  let pp_enum sail_definition enum_definition =
    let coq_translation =
      let identifier = string enum_definition.identifier
      and typ = string "Set"
      in
      PP.Coq.build_inductive_type identifier typ (fun add_constructor ->
          List.iter
            (fun (case : string) ->
              add_constructor (string case)
            )
            enum_definition.cases
        )
    in
    PP.Coq.annotate_with_original_definition sail_definition coq_translation
  in
  List.map (uncurry pp_enum) enum_definitions


let generate_constructors_inductive_type (enum_definitions : (sail_definition * enum_definition) list) =
  let pp _ enum_definition =
    let identifier = string (enum_definition.identifier ^ "Constructor")
    and typ = string "Set"
    in
    PP.Coq.build_inductive_type identifier typ (fun add_constructor ->
        List.iter
          (fun (case : string) ->
            add_constructor (string ("K" ^ case)))
          enum_definition.cases
      )
  in
  List.map (uncurry pp) enum_definitions


let generate_enum_of_enums (enum_definitions : (sail_definition * enum_definition) list) =
  let enum_identifiers =
    let extract_identifier (_, enum_definition) = enum_definition.identifier
    in
    List.map extract_identifier enum_definitions
  and identifier = string "Enums"
  and typ = string "Set"
  in
  PP.Coq.build_inductive_type
    identifier
    typ
    (fun add_constructor ->
      List.iter
        (fun enum_identifier ->
          add_constructor (string enum_identifier)
        )
        enum_identifiers
    )

let generate_no_confusions (enum_definitions : (sail_definition * enum_definition) list) =
  let contents =
    let set_transparent_obligations =
      string "Local Set Transparent Obligations."
    in
    let derivations =
      let generate_derivation (enum_definition : enum_definition) =
        string (
            Printf.sprintf "Derive NoConfusion for %s." enum_definition.identifier
          )
      in
      let lines =
        List.map generate_derivation (List.map snd enum_definitions)
      in
      separate hardline lines
    in
    set_transparent_obligations ^^ twice hardline ^^ derivations
  in
  PP.Coq.section "TransparentObligations" contents

let pp_enums (enum_definitions : (sail_definition * enum_definition) list) =
  if
    List.is_empty enum_definitions
  then
    []
  else
    Util.build_list (fun { add; addall } ->
        addall (generate_inductive_type enum_definitions);
        addall (generate_constructors_inductive_type enum_definitions);
        add (generate_enum_of_enums enum_definitions);
        add (generate_no_confusions enum_definitions);
      )
