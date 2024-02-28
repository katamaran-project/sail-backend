open Base

include Map

module Identifier = struct
  module T = struct
    type t = Ast.identifier
    
    let compare (Ast.Id x) (Ast.Id y) =
      String.compare x y
    
    let sexp_of_t (Ast.Id x) =
      String.sexp_of_t x
  end

  include T
  include Comparator.Make(T)
end

let x = Map.of_alist (module Identifier) [Ast.Id "x", 1; Ast.Id "y", 2]

type 'a t = (Ast.identifier, 'a, Identifier.comparator_witness) Map.t

let of_alist_exn (pairs : (Ast.identifier * 'a) list) = Map.of_alist_exn (module Identifier) pairs

let empty = Map.empty(module Identifier)

let overwrite map ~key ~data =
  Map.update map key ~f:(fun _ -> data)
