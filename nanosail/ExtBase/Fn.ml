include Base.Fn


let uncurry  f (x, y)    = f x y
let uncurry3 f (x, y, z) = f x y z


let fixed_point
    ~(f     :'a -> 'a        )
    ~(equal :'a -> 'a -> bool)
     (value : 'a             ): 'a
  =
  let rec aux (value : 'a) : 'a =
    let next_value = f value
    in
    if
      equal value next_value
    then
      value
    else
      aux next_value
  in
  aux value
