open Base
open Auxlib


(* todo move to Registers *)
let generate (definitions : (Sail.sail_definition * Ast.Definition.t) list) =
  let has_registers =
    let register_definitions =
      Ast.select Ast.Extract.register_definition definitions
    in
    not @@ List.is_empty register_definitions
  in
  let eqdec_derivations =
    build_list (fun { add; _ } ->
        if has_registers
        then add @@ Coq.derive_eqdec_for (Ast.Identifier.mk "RegName");
      )
  in
  Some PP.(separate hardline eqdec_derivations)
