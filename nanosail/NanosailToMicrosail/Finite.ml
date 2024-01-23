open Base
open Ast
open Auxlib

module PP = PPrint


let generate (_definitions : definition list) =
  let parts = build_list @@ fun { add; _ } -> begin
    add @@ Coq.imports [ "stdpp.finite" ];
  end
  in
  Coq.section "Finite" @@ PP.(separate (twice hardline) parts)
