open Base


let uncurry f (x, y) = f x y


let uncurry3 f (x, y, z) = f x y z


let (&.&) p1 p2 x =
  p1 x && p2 x


let (|.|) p1 p2 x =
  p1 x || p2 x


type 'a list_builder = {
    add    : 'a        -> unit;
    addall : 'a list   -> unit;
    addopt : 'a option -> unit;
  }

let build_list f =
  let list_under_construction = ref []
  in
  let add item = list_under_construction := item :: !list_under_construction
  in
  let addall items = List.iter ~f:add items
  and addopt item =
    match item with
    | Some item -> add item
    | None      -> ()
  in
  let context = { add; addall; addopt }
  in
  f context;
  List.rev !list_under_construction


let split_last xs =
  match List.rev xs with
  | []    -> None
  | x::xs -> Some (List.rev xs, x)


let using ~resource:(x : 'a) ~close ~body =
  begin
    try
      body x
    with e ->
      close x;
      raise e
  end;
  close x


let rec unordered_pairs xs =
  match xs with
  | []    -> []
  | x::xs -> (List.map ~f:(fun y -> (x, y)) xs) @ (unordered_pairs xs)


let rec consecutive_overlapping_pairs xs =
  match xs with
  | x::y::xs -> (x, y) :: consecutive_overlapping_pairs (y :: xs)
  | _        -> []


(* performs a left fold using first element as init *)
let reduce ~(f: 'a -> 'a -> 'a) (list : 'a list) : 'a =
  match list with
  | []    -> failwith "cannot reduce empty list"
  | x::xs -> List.fold_left xs ~init:x ~f


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
