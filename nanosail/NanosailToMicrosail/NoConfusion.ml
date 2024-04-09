open Base
open Ast
open Auxlib

module PP = PPrint


let generate (definitions : (sail_definition * definition) list) =
  let has_registers =
    let register_definitions =
      select Extract.register_definition definitions
    in
    not @@ List.is_empty register_definitions
  in
  let contents =
    build_list (fun { add; _ } ->
        add @@ PP.string "Local Set Transparent Obligations.";
        add @@ PP.string "";
        if has_registers
        then add @@ PP.string "Derive NoConfusion for RegName.";
      )  
  in
  let section =
    Coq.section (Id.mk "TransparentObligations") PP.(separate hardline contents)
  in
  Some section
