type source_position = string * int * int * int

exception NotYetImplemented of source_position * Libsail.Ast.l * string option

let not_yet_implemented ?(message = "") source_position sail_location =
  let message =
    if message == ""
    then None
    else Some message
  in
  raise (NotYetImplemented (source_position, sail_location, message))
