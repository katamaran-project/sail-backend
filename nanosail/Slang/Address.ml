(*
   An address is essentially the same as a ref-cell in OCaml:
   it is an opaque value that represents something in memory
   that can be read from and written to.
*)
open ExtBase


type t = Address of int


let equal (Address p) (Address q) : bool =
  Int.equal p q


let to_int (Address p) : int =
  p


let mk (n : int) : t =
  Address n
