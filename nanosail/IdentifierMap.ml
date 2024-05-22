open Base

include Map

type 'a t = (Id.t, 'a, Id.comparator_witness) Map.t

let of_alist_exn (pairs : (Id.t * 'a) list) = Map.of_alist_exn (module Id) pairs

let empty = Map.empty(module Id)

let overwrite map ~key ~data =
  Map.update map key ~f:(fun _ -> data)
