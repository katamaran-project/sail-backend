include Base.Map


let overwrite map ~key ~data =
  update map key ~f:(fun _ -> data)


module String = struct
  include Base.Map

  type 'a t = (string, 'a, Base.String.comparator_witness) Base.Map.t

  let of_alist_exn (pairs : (string * 'a) list) = Base.Map.of_alist_exn (module Base.String) pairs

  let empty = Base.Map.empty(module Base.String)
end
