type ('a, 't) getter   = 'a -> 't
type ('a, 't) setter   = 'a -> 't -> 'a
type ('a, 't) accessor = ('a, 't) getter * ('a, 't) setter

exception AccessorError of string

let id : ('a, 'a) accessor =
  let get x   = x
  and set _ x = x
  in
  (get, set)


module Pair = struct
  let first (subaccessor : ('a, 'b * 'c) accessor) =
    let (get', set') = subaccessor
    in
    let get value =
      let (x, _) = get' value
      in
      x
    and set value x =
      let (_, y) = get' value
      in
      set' value (x, y)
    in
    (get, set)

  let second (subaccessor : ('a, 'b * 'c) accessor) =
    let (get', set') = subaccessor
    in
    let get value =
      let (_, y) = get' value
      in
      y
    and set value y =
      let (x, _) = get' value
      in
      set' value (x, y)
    in
    (get, set)
end


module List = struct
  let head (subaccessor : ('a, 'b list) accessor) =
    let (get', set') = subaccessor
    in
    let get value =
      match get' value with
      | []    -> raise (AccessorError "no head")
      | x::_  -> x
    and set value x =
      match get' value with
      | []    -> raise (AccessorError "no head")
      | _::xs -> set' (x::xs)
    in
    (get, set)
end
