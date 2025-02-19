include Base.Fn


let uncurry  f (x, y)    = f x y
let uncurry3 f (x, y, z) = f x y z


let (&.&) p1 p2 x =
  p1 x && p2 x


let (|.|) p1 p2 x =
  p1 x || p2 x
