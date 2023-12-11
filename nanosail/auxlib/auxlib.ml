let uncurry f (x, y) = f x y

let compose f g x = f (g x)

let count_chars string char =
  let count acc c =
    if c == char
    then acc + 1
    else acc
  in
  String.fold_left count 0 string

let last_char string =
  String.get string (String.length string - 1)

let strip string =
  if String.ends_with ~suffix:"\n" string
  then String.sub string 0 (String.length string - 1)
  else string

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
