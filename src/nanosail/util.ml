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
