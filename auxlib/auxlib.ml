module Settings = Settings

let uncurry f (x, y) = f x y

let compose f g x = f (g x)

let (&.&) p1 p2 x =
  p1 x && p2 x

let (|.|) p1 p2 x =
  p1 x || p2 x

let last_char string =
  String.get string (String.length string - 1)

let drop_chars_while string predicate =
  let i = ref 0
  in
  while !i < String.length string && predicate (String.get string !i) do
    i := !i + 1
  done;
  String.sub string !i (String.length string - !i)

let minimum ns =
  match ns with
  | []    -> failwith "Cannot find minimum of empty list"
  | n::ns -> List.fold_left min n ns

let maximum ns =
  match ns with
  | []    -> failwith "Cannot find maximum of empty list"
  | n::ns -> List.fold_left max n ns

type 'a list_builder = {
    add : 'a -> unit;
    addall : 'a list -> unit
  }

let build_list f =
  let list_under_construction = ref []
  in
  let add_item item = list_under_construction := item :: !list_under_construction
  in
  let add_items items = List.iter add_item items
  in
  let context = { add = add_item; addall = add_items }
  in
  f context;
  List.rev !list_under_construction

let rec take n xs =
  if n <= 0
  then []
  else
    match xs with
    | []    -> []
    | x::xs -> x :: take (n - 1) xs

let split_last xs =
  match List.rev xs with
  | []    -> None
  | x::xs -> Some (List.rev xs, x)
