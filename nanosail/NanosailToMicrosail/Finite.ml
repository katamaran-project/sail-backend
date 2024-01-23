open Base
open Ast
open Auxlib

module PP = PPrint



let generate_enum_finitess (_sail_definition : sail_definition) (enum_definition : enum_definition) =
  let identifier = enum_definition.identifier ^ "_finite"
  and type_name = enum_definition.identifier
  and values = enum_definition.cases
  in
  Coq.finite_instance ~identifier ~type_name ~values
  
let generate (definitions : (sail_definition * definition) list) =
  let finite_enums =
    let enum_definitions = select Extract.enum_definition definitions
    in
    List.map ~f:(uncurry generate_enum_finitess) enum_definitions
  in
  let parts = build_list @@ fun { add; addall } -> begin
      add    @@ Coq.imports [ "stdpp.finite" ];
      addall @@ finite_enums;
    end
  in
  Coq.section "Finite" @@ PP.(separate (twice hardline) parts)
