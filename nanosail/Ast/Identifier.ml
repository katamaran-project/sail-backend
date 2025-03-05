open! ExtBase


(*
   We "temporarily" put the Identifier related functionality in a separate
   module so that we can refer to it later in our specialized Map and Set modules.
*)
module Implementation = struct
  module T = struct
    type t = Id of string

    let compare (Id x) (Id y) =
      String.compare x y

    let sexp_of_t (Id x) =
      String.sexp_of_t x
  end

  include T
  include Comparator.Make(T)

  let equal
      (identifier_1 : t)
      (identifier_2 : t) : bool
    =
    let Id name_1 = identifier_1
    and Id name_2 = identifier_2
    in
    String.equal
      name_1
      name_2

  let mk x = Id x

  let generated_symbol = "ж"

  let mk_generated name =
    mk @@ generated_symbol ^ name

  let is_generated (Id name) =
    String.is_substring ~substring:generated_symbol name 

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

  type 'a t = (Implementation.t, 'a, Implementation.comparator_witness) Base.Map.t

  let of_alist_exn (pairs : (Implementation.t * 'a) list) = Base.Map.of_alist_exn (module Implementation) pairs

  let empty = Base.Map.empty(module Implementation)

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
      (map   : 'a t            )
      ~(key  : Implementation.t)
      ~(data : 'a              ) : 'a t
    =
    match add map ~key ~data with
    | `Duplicate      -> map
    | `Ok updated_map -> updated_map

  let to_fexpr
      (value_formatter : 'a -> FExpr.t)
      (map             : 'a t         ) : FExpr.t
    =
    let pairs : (Implementation.t * 'a) list =
      to_alist map
    in
    let formatted_pairs : FExpr.t list =
      let format_pair (key : Implementation.t) (value : 'a) =
        let positional =
          [
            Implementation.to_fexpr key;
            value_formatter value
          ]
        in
        FExpr.mk_application ~positional "Entry"
      in
      List.map ~f:(Fn.uncurry format_pair) pairs
    in
    FExpr.mk_application ~positional:formatted_pairs "Mapping"
end


module Set = struct
  include Base.Set

  type t = (Implementation.t, Implementation.comparator_witness) Base.Set.t

  let empty     = Base.Set.empty      (module Implementation)
  let singleton = Base.Set.singleton  (module Implementation)
  let of_list   = Base.Set.of_list    (module Implementation)
  let unions    = Base.Set.union_list (module Implementation)
end


(*
   Pull all definitions from Implementation into the Identifier module.
*)
include Implementation
