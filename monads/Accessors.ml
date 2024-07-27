let full =
  let get x   = x
  and set _ x = x
  in
  (get, set)


module Pair = struct
  let first =
    let get (x, _)   = x
    and set (_, y) x = (x, y)
    in
    (get, set)

  let second =
    let get (_, y)   = y
    and set (x, _) y = (x, y)
    in
    (get, set)
end


module Triple = struct
  let first =
    let get (x, _, _)   = x
    and set (_, y, z) x = (x, y, z)
    in
    (get, set)

  let second =
    let get (_, y, _)   = y
    and set (x, _, z) y = (x, y, z)
    in
    (get, set)

  let third =
    let get (_, _, z)   = z
    and set (x, y, _) z = (x, y, z)
    in
    (get, set)
end
