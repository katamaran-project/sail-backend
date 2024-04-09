open Base
(* open Ast *)
open Auxlib

module PP = PPrint


let generate () =
  let contents =
    build_list (fun { add; _ } ->
        add @@ PP.string "Local Set Transparent Obligations.";
        add @@ PP.string "";
        add @@ PP.string "Derive NoConfusion for RegName.";
      )  
  in
  let section =
    Coq.section (Id.mk "TransparentObligations") PP.(separate hardline contents)
  in
  Some section
