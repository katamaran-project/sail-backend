open! ExtBase

include Base.Map

type 'a t = (string, 'a, String.comparator_witness) Base.Map.t

let of_alist_exn (pairs : (string * 'a) list) = Base.Map.of_alist_exn (module String) pairs

let empty = Base.Map.empty(module String)

let overwrite map ~key ~data =
  Base.Map.update map key ~f:(fun _ -> data)
