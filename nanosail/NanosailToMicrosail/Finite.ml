open Base
open Auxlib
open Identifier


let generate_enum_finiteness
      (_sail_definition : Sail.sail_definition)
      (enum_definition  : Ast.enum_definition ) =
  let identifier = pp_identifier @@ enum_definition.identifier
  and type_name  = pp_identifier @@ enum_definition.identifier
  and values     = List.map ~f:pp_identifier enum_definition.cases
  in
  Coq.finite_instance ~identifier ~type_name ~values


let generate_register_finiteness (register_definitions : (Sail.sail_definition * Ast.register_definition) list) =
  let register_identifiers =
    List.map ~f:(fun (_, def) -> def.identifier) register_definitions
  in
  let translated_register_identifiers =
    List.map ~f:Registers.translate_regname register_identifiers
  in
  let identifier = pp_identifier @@ Ast.Identifier.mk "RegName"
  and type_name  = pp_identifier @@ Ast.Identifier.mk "RegName"
  and values     = List.map ~f:pp_identifier translated_register_identifiers
  in
  Coq.finite_instance ~identifier ~type_name ~values


let generate (definitions : (Sail.sail_definition * Ast.definition) list) =
  let finite_enums =
    let enum_definitions = Ast.(select Extract.(type_definition of_enum) definitions)
    in
    List.map ~f:(uncurry generate_enum_finiteness) enum_definitions
  and finite_registers =
    let register_definitions = Ast.(select Extract.register_definition definitions)
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
              add    @@ Coq.local_obligation_tactic (Ast.Identifier.mk "finite_from_eqdec");
              addall @@ finite_definitions;
            end
      in
      Option.some @@ Coq.section (Ast.Identifier.mk "Finite") @@ PP.(separate (twice hardline) parts)
    end
