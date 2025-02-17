(*
   Library (very incomplete but sufficient for our purposes) to
   output colored text.
*)
open ExtBase


module Color = struct
  type t =
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
end


module Decoration = struct
  type t =
    | ForegroundColor of Color.t
    | BackgroundColor of Color.t
    | Underlined
    | Bold

  let code_of (decoration : t) : string =
    match decoration with
    | ForegroundColor Red     -> "31"
    | ForegroundColor Green   -> "32"
    | ForegroundColor Yellow  -> "33"
    | ForegroundColor Blue    -> "34"
    | ForegroundColor Magenta -> "35"
    | ForegroundColor Cyan    -> "36"
    | ForegroundColor White   -> "37"
    | BackgroundColor Red     -> "41"
    | BackgroundColor Green   -> "42"
    | BackgroundColor Yellow  -> "43"
    | BackgroundColor Blue    -> "44"
    | BackgroundColor Magenta -> "45"
    | BackgroundColor Cyan    -> "46"
    | BackgroundColor White   -> "47"
    | Underlined              -> "4"
    | Bold                    -> "1"
end


(*
   Translates the decorations into an escape sequence.
   Printing out these sequence will lead the following text
   to be printed out with the specified decorations.
   Typically you will want to reset it back
   to the default output style,
   which is represented by the empty list.

   Example:

     let red_underlined = to_escape_sequence [ Decoration.Foreground Red; Decoration.Underlined ]
     and plain          = to_escape_sequence []
     in
     Printf.sprintf "This is in a %sred underlined%s font." red_underlined plain
*)
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
    
