open! ExtBase


let position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
  Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum


let list
    (string_of_element : 'a -> string)
    (elements          : 'a list     ) : string
  =
  let elements' =
    List.map ~f:string_of_element elements
  in
  "[" ^ String.concat ~sep:"; " elements' ^ "]"
