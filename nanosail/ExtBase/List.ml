include Base.List


let find_index_of
    ~(f : 'a -> bool)
    (xs : 'a list   ) : int option
  =
  let rec aux index xs =
    match xs with
    | []    -> None
    | x::xs -> if f x then Some index else aux (index + 1) xs
  in
  aux 0 xs


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
  let addall items = iter ~f:add items
  and addopt item =
    match item with
    | Some item -> add item
    | None      -> ()
  in
  let context = { add; addall; addopt }
  in
  f context;
  rev !list_under_construction


let rec unordered_pairs xs =
  match xs with
  | []    -> []
  | x::xs -> (map ~f:(fun y -> (x, y)) xs) @ (unordered_pairs xs)
