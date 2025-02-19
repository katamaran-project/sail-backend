module Pair = struct
  let equal
      (fst_equal : 'a -> 'a -> bool)
      (snd_equal : 'b -> 'b -> bool)
      (pair_1    : 'a * 'b         )
      (pair_2    : 'a * 'b         ) : bool
    =
    let fst_1, snd_1 = pair_1
    and fst_2, snd_2 = pair_2
    in
    fst_equal
      fst_1
      fst_2
    &&
    snd_equal
      snd_1
      snd_2
end


module Triple = struct
  let first  (x, _, _) = x
  let second (_, x, _) = x
  let third  (_, _, x) = x
end
