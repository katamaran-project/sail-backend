open! ExtBase

include Base.Map

type 'a t = (Int.t, 'a, Int.comparator_witness) Base.Map.t

let empty = Base.Map.empty(module Int)

let overwrite map ~key ~data =
  Base.Map.update map key ~f:(fun _ -> data)
