open Base


type 'a t = (string, 'a, String.comparator_witness) Map.t

let of_alist_exn (pairs : (string * 'a) list) = Map.of_alist_exn (module String) pairs

let empty    = Map.empty(module String)
let add      = Map.add
let add_exn  = Map.add_exn
let fold     = Map.fold
let to_list  = Map.to_alist
let find     = Map.find
let find_exn = Map.find_exn

let overwrite map ~key ~data =
  Map.update map key ~f:(fun _ -> data)
