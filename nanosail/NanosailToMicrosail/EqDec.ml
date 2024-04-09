open Base
open Auxlib
open Ast

module PP = PPrint


let generate (definitions : (sail_definition * definition) list) =
  let _ =
    definitions
  in
  let eqdec_derivations =
    build_list (fun { add; _ } ->
        add @@ Coq.derive_eqdec_for (Id.mk "RegName")
      )
  in
  Some PP.(separate hardline eqdec_derivations)
     
