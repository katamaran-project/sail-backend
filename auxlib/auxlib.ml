open Base


let uncurry f (x, y) = f x y


let uncurry3 f (x, y, z) = f x y z


let (&.&) p1 p2 x =
  p1 x && p2 x


let (|.|) p1 p2 x =
  p1 x || p2 x


let minimum ns =
  match ns with
  | []    -> raise (Invalid_argument "Cannot determine minimum of empty list")
  | n::ns -> List.fold_left ~f:min ~init:n ns


let maximum ns =
  match ns with
  | []    -> raise (Invalid_argument "Cannot determine maximum of empty list")
  | n::ns -> List.fold_left ~f:max ~init:n ns


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


let find_index_of ~(f:'a -> bool) (xs : 'a list) : int option =
  let rec aux index xs =
    match xs with
    | []    -> None
    | x::xs -> if f x then Some index else aux (index + 1) xs
  in
  aux 0 xs


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


(* Checks if the given lists contain equal elements in the same order *)
let rec equal_lists ~(eq : 'a -> 'a -> bool) (xs : 'a list) (ys : 'a list) : bool =
  match xs, ys with
  | [], []       -> true
  | x::xs, y::ys -> eq x y && equal_lists ~eq xs ys
  | _, _         -> false


let first2  = fst
let second2 = snd

let first3  (x, _, _) = x
let second3 (_, x, _) = x
let third3  (_, _, x) = x
