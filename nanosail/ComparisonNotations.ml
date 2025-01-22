module type CMP = sig
  type t
  val compare : t -> t -> int
end

module Make (C : CMP) = struct
  let (<<)  x y = C.compare x y <  0
  let (>>)  x y = C.compare x y >  0
  let (<<=) x y = C.compare x y <= 0
  let (>>=) x y = C.compare x y >= 0
  let (===) x y = C.compare x y =  0
end
