(*
   Extends the integers with positive and negative infinity.
*)

type t = Int of int
       | PositiveInfinity
       | NegativeInfinity


let compare (x : t) (y : t) : int =
  match x, y with
  | PositiveInfinity, PositiveInfinity ->  0
  | NegativeInfinity, NegativeInfinity ->  0
  | NegativeInfinity, PositiveInfinity -> -1
  | PositiveInfinity, NegativeInfinity ->  1
  | Int _           , PositiveInfinity -> -1
  | PositiveInfinity, Int _            ->  1
  | Int _           , NegativeInfinity ->  1
  | NegativeInfinity, Int _            -> -1
  | Int x           , Int y            -> Int.compare x y


let equal (x : t) (y : t) : bool =
  Int.equal (compare x y) 0


let to_string (x : t) : string =
  match x with
  | Int k            -> Int.to_string k
  | PositiveInfinity -> "+inf"
  | NegativeInfinity -> "-inf"
