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
