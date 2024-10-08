open Base


(*
  We use a separate type to represent HTML strings
  This helps us prevent escaping the same string twice
*)
type t = Html of string


let to_string (Html s) =
  s

let escape_string (string : string) : t =
  let escape_char (char : char) : string =
    match char with
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | ' ' -> "&nbsp;"
    | _   -> String.make 1 char
  in
  let chars = String.to_list string
  in
  let escaped = List.map ~f:escape_char chars
  in
  Html (String.concat ~sep:"" escaped)


let unordered_list (items : t list) : t =
  let html_items : string =
    let html_item (Html item) : string =
      Printf.sprintf "<li>%s</li>" item
    in
    String.concat ~sep:"" @@ List.map ~f:html_item items
  in
  Html (Printf.sprintf "<ul>%s</ul>" html_items)

