open Base


let uncurry f (x, y) = f x y

let (&.&) p1 p2 x =
  p1 x && p2 x

let (|.|) p1 p2 x =
  p1 x || p2 x

let minimum ns =
  match ns with
  | []    -> failwith "Cannot find minimum of empty list"
  | n::ns -> List.fold_left ~f:min ~init:n ns

let maximum ns =
  match ns with
  | []    -> failwith "Cannot find maximum of empty list"
  | n::ns -> List.fold_left ~f:max ~init:n ns

type 'a list_builder = {
    add : 'a -> unit;
    addall : 'a list -> unit
  }

let build_list f =
  let list_under_construction = ref []
  in
  let add_item item = list_under_construction := item :: !list_under_construction
  in
  let add_items items = List.iter ~f:add_item items
  in
  let context = { add = add_item; addall = add_items }
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
