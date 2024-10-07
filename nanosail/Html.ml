open Base


let escape_char (char : char) : string =
  match char with
  | '<' -> "&lt;"
  | '>' -> "&gt;"
  | '&' -> "&amp;"
  | _   -> String.make 1 char


let escape_string (string : string) : string =
  let chars = String.to_list string
  in
  let escaped = List.map ~f:escape_char chars
  in
  String.concat ~sep:"" escaped
