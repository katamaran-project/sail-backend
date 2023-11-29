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
      let identifier = string enum_definition.enum_identifier
      and typ = string "Set"
      in
      PP.Coq.build_inductive_type identifier typ (fun add_constructor ->
          List.iter
            (fun (case : string) ->
              add_constructor (string case)
            )
            enum_definition.enum_cases
        )
    in
    PP.Coq.annotate_with_original_definition sail_definition coq_translation
  in
  List.map (uncurry pp_enum) enum_definitions


let generate_constructors_inductive_type (enum_definitions : (sail_definition * enum_definition) list) =
  let pp _ enum_definition =
    let identifier = string (enum_definition.enum_identifier ^ "Constructor")
    and typ = string "Set"
    in
    PP.Coq.build_inductive_type identifier typ (fun add_constructor ->
        List.iter
          (fun (case : string) ->
            add_constructor (string ("K" ^ case)))
          enum_definition.enum_cases
      )
  in
  List.map (uncurry pp) enum_definitions

let pp_enums (enum_definitions : (sail_definition * enum_definition) list) =
  Util.build_list (fun { addall; _ } ->
      addall (generate_inductive_type enum_definitions);
      addall (generate_constructors_inductive_type enum_definitions);
    )
