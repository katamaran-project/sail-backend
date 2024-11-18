open Base


module Impl = struct
  module T = struct
    type t = Id of string

    let compare (Id x) (Id y) =
      String.compare x y

    let sexp_of_t (Id x) =
      String.sexp_of_t x
  end

  include T
  include Comparator.Make(T)

  let equal (Id x) (Id y) =
    String.equal x y

  let mk x = Id x

  let string_of (Id x) = x

  let update f (Id x) = Id (f x)

  let add_prefix prefix =
    update (fun x -> prefix ^ x)

  let add_suffix suffix =
    update (fun x -> x ^ suffix)

  let to_fexpr (identifier : t) : FExpr.t =
    FExpr.mk_application
      ~positional:[FExpr.mk_string @@ string_of identifier]
      "Identifier"
end

module Map = struct
  include Base.Map

  type 'a t = (Impl.t, 'a, Impl.comparator_witness) Base.Map.t

  let of_alist_exn (pairs : (Impl.t * 'a) list) = Base.Map.of_alist_exn (module Impl) pairs

  let empty = Base.Map.empty(module Impl)

  let overwrite map ~key ~data =
    Base.Map.update map key ~f:(fun _ -> data)

  let map_values ~f map =
    let pairs = to_alist map
    in
    let updated_pairs = List.map ~f:(fun (k, v) -> (k, f v)) pairs
    in
    of_alist_exn updated_pairs
end

include Impl
