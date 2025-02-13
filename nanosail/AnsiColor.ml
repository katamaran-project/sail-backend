open ExtBase


module Color = struct
  type t =
    | Red
    | Green
    | Yellow
    | Blue
end


module Decoration = struct
  type t =
    | ForegroundColor of Color.t
    | BackgroundColor of Color.t
    | Underlined
    | Bold

  let code_of (decoration : t) : string =
    match decoration with
    | ForegroundColor Red    -> "31"
    | ForegroundColor Green  -> "32"
    | ForegroundColor Yellow -> "33"
    | ForegroundColor Blue   -> "34"
    | BackgroundColor Red    -> "41"
    | BackgroundColor Green  -> "42"
    | BackgroundColor Yellow -> "43"
    | BackgroundColor Blue   -> "44"
    | Underlined             -> "4"
    | Bold                   -> "1"
end


let to_escape_sequence (decorations : Decoration.t list) : string =
  let build_escape_sequence (commands : string list) : string =
    Printf.sprintf "\027[%sm" @@ String.concat ~sep:";" commands
  in    
  if
    List.is_empty decorations
  then
    build_escape_sequence [ "0" ]
  else
    build_escape_sequence @@ List.map ~f:Decoration.code_of decorations
    
