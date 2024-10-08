open Base


let escape_char (char : char) : string =
  match char with
  | '<' -> "&lt;"
  | '>' -> "&gt;"
  | '&' -> "&amp;"
  | ' ' -> "&nbsp;"
  | _   -> String.make 1 char


let escape_string (string : string) : string =
  let chars = String.to_list string
  in
  let escaped = List.map ~f:escape_char chars
  in
  String.concat ~sep:"" escaped


let unordered_list (items : string list) : string =
  let html_items =
    let html_item item =
      Printf.sprintf "<li>%s</li>" item
    in
    String.concat ~sep:"" @@ List.map ~f:html_item items
  in
  Printf.sprintf "<ul>%s</ul>" html_items
