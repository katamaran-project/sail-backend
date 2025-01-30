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

  let mk_generated name =
    mk @@ "ж" ^ name
  
  let to_string (Id x) = x

  let update f (Id x) = Id (f x)

  let add_prefix prefix =
    update (fun x -> prefix ^ x)

  let add_suffix suffix =
    update (fun x -> x ^ suffix)

  let to_fexpr (identifier : t) : FExpr.t =
    FExpr.mk_application
      ~positional:[FExpr.mk_string @@ to_string identifier]
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

  (* Only adds if key is not element yet *)
  let add_new
      (map   : 'a t  )
      ~(key  : Impl.t)
      ~(data : 'a    ) : 'a t
    =
    match add map ~key ~data with
    | `Duplicate      -> map
    | `Ok updated_map -> updated_map

  let to_fexpr
      (value_formatter : 'a -> FExpr.t)
      (map             : 'a t         ) : FExpr.t
    =
    let pairs : (Impl.t * 'a) list =
      to_alist map
    in
    let formatted_pairs : FExpr.t list =
      let format_pair (key : Impl.t) (value : 'a) =
        let positional =
          [
            Impl.to_fexpr key;
            value_formatter value
          ]
        in
        FExpr.mk_application ~positional "Entry"
      in
      List.map ~f:(Auxlib.uncurry format_pair) pairs
    in
    FExpr.mk_application ~positional:formatted_pairs "Mapping"
end

include Impl
