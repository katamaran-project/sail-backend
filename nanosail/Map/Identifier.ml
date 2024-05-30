open Base

include Base.Map

type 'a t = (Id.t, 'a, Id.comparator_witness) Base.Map.t

let of_alist_exn (pairs : (Id.t * 'a) list) = Base.Map.of_alist_exn (module Id) pairs

let empty = Base.Map.empty(module Id)

let overwrite map ~key ~data =
  Base.Map.update map key ~f:(fun _ -> data)
