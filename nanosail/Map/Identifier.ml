open Base

include Base.Map

type 'a t = (Ast.Identifier.t, 'a, Ast.Identitier.comparator_witness) Base.Map.t

let of_alist_exn (pairs : (Ast.Identifier.t * 'a) list) = Base.Map.of_alist_exn (module Ast.Identifier) pairs

let empty = Base.Map.empty(module Ast.Identifier)

let overwrite map ~key ~data =
  Base.Map.update map key ~f:(fun _ -> data)
