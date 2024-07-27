open Base


type t = Address of int

let equal (Address p) (Address q) =
  Int.equal p q

let to_int (Address p) = p
