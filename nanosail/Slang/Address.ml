open! ExtBase


type t = Address of int


let equal (Address p) (Address q) =
  Int.equal p q


let to_int (Address p) =
  p


let mk n =
  Address n
