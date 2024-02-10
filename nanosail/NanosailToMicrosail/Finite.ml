open Base
open Ast
open Auxlib

module PP = PPrint



let generate_enum_finiteness (_sail_definition : sail_definition) (enum_definition : enum_definition) =
  let identifier = enum_definition.identifier
  and type_name = enum_definition.identifier
  and values = enum_definition.cases
  in
  Coq.finite_instance ~identifier ~type_name ~values


let generate_register_finiteness (register_definitions : (sail_definition * register_definition) list) =
  let identifier = "RegName"
  and type_name = "RegName"
  and values = List.map ~f:(fun (_, def) -> def.identifier) register_definitions
  in
  Coq.finite_instance ~identifier ~type_name ~values


let generate (definitions : (sail_definition * definition) list) =
  let finite_enums =
    let enum_definitions = select Extract.(type_definition of_enum) definitions
    in
    List.map ~f:(uncurry generate_enum_finiteness) enum_definitions
  and finite_registers =
    let register_definitions = select Extract.register_definition definitions
    in
    if List.is_empty register_definitions
    then None
    else Some (generate_register_finiteness register_definitions)
  in
  let finite_definitions =
    build_list @@
      fun { addall; addopt; _ } -> begin
          addall finite_enums;
          addopt finite_registers;
        end
  in
  if List.is_empty finite_definitions
  then None
  else begin
      let parts =
        build_list @@
          fun { add; addall; _ } -> begin
              add    @@ Coq.imports [ "stdpp.finite" ];
              addall @@ finite_definitions;
            end
      in
      Option.some @@ Coq.section "Finite" @@ PP.(separate (twice hardline) parts)
    end
