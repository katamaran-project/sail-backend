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


let element
      (tag         : string                     )
      ?(attributes : (string * string) list = [])
      (child       : t                          ) : t
  =
  let attributes' =
    let format_attribute name value =
      Printf.sprintf {| %s="%s"|} name value
    in
    String.concat @@ List.map ~f:(Auxlib.uncurry format_attribute) attributes
  in
  Html (
      Printf.sprintf
        "<%s%s>%s</%s>"
        tag
        attributes'
        (to_string child)
        tag
    )


let span
      ?(class_name : string = "")
      (child       : t          ) : t
  =
  let attributes =
    if
      String.is_empty class_name
    then
      []
    else
      [("class", class_name)]
  in
  element "span" ~attributes child
