open Base
open Auxlib
open Ast


let generate (definitions : (sail_definition * definition) list) =
  let has_registers =
    let register_definitions =
      select Extract.register_definition definitions
    in
    not @@ List.is_empty register_definitions
  in
  let eqdec_derivations =
    build_list (fun { add; _ } ->
        if has_registers
        then add @@ Coq.derive_eqdec_for (Id.mk "RegName");
      )
  in
  Some PP.(separate hardline eqdec_derivations)

