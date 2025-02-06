open Base


let (&.&) p1 p2 x =
  p1 x && p2 x


let (|.|) p1 p2 x =
  p1 x || p2 x


let using ~resource:(x : 'a) ~close ~body =
  begin
    try
      body x
    with e ->
      close x;
      raise e
  end;
  close x


let rec consecutive_overlapping_pairs xs =
  match xs with
  | x::y::xs -> (x, y) :: consecutive_overlapping_pairs (y :: xs)
  | _        -> []


let rec repeat n x =
  if n > 0
  then x :: repeat (n-1) x
  else []


let zip_indices (xs : 'a list) : (int * 'a) list =
  let rec aux (acc : (int * 'a) list) (xs : 'a list) (index : int) : (int * 'a) list =
    match xs with
    | []    -> List.rev acc
    | x::xs -> aux ((index, x) :: acc) xs (index + 1)
  in
  aux [] xs 0


module Pair = struct
  let first = fst
  let second = snd
end

module Triple = struct
  let first  (x, _, _) = x
  let second (_, x, _) = x
  let third  (_, _, x) = x
end
