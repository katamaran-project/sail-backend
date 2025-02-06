open! ExtBase

(* Polymorphic so as to prevent cyclic dependencies *)
type 'a t = (string, 'a, String.comparator_witness) Map.t

let empty = Map.empty (module String)

let bind env identifier value =
  Map.update env identifier ~f:(Fn.const value)

let lookup env identifier =
  Map.find env identifier

let bind_many env pairs =
  List.fold_left ~f:(fun acc (id, v) -> bind acc id v) ~init:env pairs
