open Base

include Map

type 'a t = (Int.t, 'a, Int.comparator_witness) Map.t

let empty = Map.empty(module Int)

let overwrite map ~key ~data =
  Map.update map key ~f:(fun _ -> data)
