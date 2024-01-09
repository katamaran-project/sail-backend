open Base


type source_position = Lexing.position

exception NotYetImplemented of source_position * Libsail.Ast.l * string option

let not_yet_implemented ?(message = "") source_position sail_location =
  let message =
    if String.is_empty message
    then None
    else Some message
  in
  raise (NotYetImplemented (source_position, sail_location, message))
