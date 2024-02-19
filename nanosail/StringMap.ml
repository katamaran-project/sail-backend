open Base

include Map

type 'a t = (string, 'a, String.comparator_witness) Map.t

let of_alist_exn (pairs : (string * 'a) list) = Map.of_alist_exn (module String) pairs

let empty = Map.empty(module String)

let overwrite map ~key ~data =
  Map.update map key ~f:(fun _ -> data)
