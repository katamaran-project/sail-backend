let uncurry f (x, y) = f x y

let compose_functions f g x = f (g x)

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
