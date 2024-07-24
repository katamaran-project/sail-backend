open Base


let generate (definitions : (Sail.sail_definition * Ast.Definition.t) list) : PP.document =
  let finite_enums =
    let enum_definitions = Ast.(select Extract.(type_definition of_enum) definitions)
    in
    List.map ~f:(Auxlib.uncurry Types.Enums.generate_enum_finiteness) enum_definitions
  and finite_registers =
    let register_definitions = Ast.(select Extract.register_definition definitions)
    in
    Registers.generate_register_finiteness register_definitions
  in
  let finite_definitions =
    Auxlib.build_list @@
      fun { addall; add; _ } -> begin
          addall finite_enums;
          add    finite_registers;
        end
  in
  let parts =
    Auxlib.build_list @@
    fun { add; addall; _ } -> begin
      add    @@ Coq.imports [ "stdpp.finite" ];
      add    @@ Coq.local_obligation_tactic (Ast.Identifier.mk "finite_from_eqdec");
      addall @@ finite_definitions;
    end
  in
  Coq.section (Ast.Identifier.mk "Finite") @@ PP.(separate (twice hardline) parts)
