let lexing_position_to_string (pos : Lexing.position) : string =
  "filename: " ^ pos.pos_fname ^
  "\nline: " ^ Printf.sprintf "%d" pos.pos_lnum ^
  "\ncolumn: " ^ Printf.sprintf "%d" (pos.pos_cnum - pos.pos_bol)

