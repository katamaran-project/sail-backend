open Base


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
