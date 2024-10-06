open Base


module Annotation = struct
  type t = string

  let empty = ""

  let combine _ s2 = s2

  let to_html s = s
end


module Doc = Document.Make(Annotation)


type document  = Doc.t


let empty      = Doc.empty
let string     = Doc.string
let horizontal = Doc.horizontal
let vertical   = Doc.vertical

let plus       = string "+"
let star       = string "*"
let minus      = string "-"
let equals     = string "="

let lparen     = string "("
let rparen     = string ")"
let langle     = string "<"
let rangle     = string ">"
let lbracket   = string "["
let rbracket   = string "]"
let lbrace     = string "{"
let rbrace     = string "}"

let squote     = string "'"
let dquote     = string "\""

let surround (left_delimiter, right_delimiter) document =
  horizontal [left_delimiter; document; right_delimiter]

let braces     = (lbrace  , rbrace  )
let parens     = (lparen  , rparen  )
let squotes    = (squote  , squote  )                 
let dquotes    = (dquote  , dquote  )
let brackets   = (lbracket, rbracket)

let dot        = string "."
let bang       = string "!"
let semi       = string ";"
let comma      = string ","
let colon      = string ":"
let space      = string " "

let ampersand  = string "&"
let percent    = string "%"
let bar        = string "|"
let underscore = string "_"


let is_empty (document : document) =
  Doc.is_empty document


let indent ?(level = 2) doc = PPrint.(blank level ^^ align doc)


(* let delimited_sequence left_delimiter right_delimiter separator items = *)
(*   let open PPrint *)
(*   in *)
(*   concat [ *)
(*     left_delimiter; *)
(*     align ( *)
(*       group (separate (separator ^^ break 1) items) *)
(*     ); *)
(*     right_delimiter *)
(*   ] *)


(* (\* *)
(*    Formats elements as *)

(*     x xs[0] xs[1] xs[2] *)

(*     or *)

(*     x xs[0] *)
(*       xs[1] *)
(*       xs[2] *)
(* *\) *)
(* let hanging_list ?(adaptive = true) x xs = *)
(*   let open PPrint *)
(*   in *)
(*   if *)
(*     adaptive *)
(*   then *)
(*     concat [ *)
(*       x; *)
(*       space; *)
(*       align (group (separate (break 1) xs)) *)
(*     ] *)
(*   else *)
(*     concat [ *)
(*       x; *)
(*       space; *)
(*       align (separate hardline xs) *)
(*     ] *)


(* let simple_app terms = *)
(*   let open PPrint *)
(*   in *)
(*   match terms with *)
(*   | []    -> empty *)
(*   | [x]   -> x *)
(*   | x::xs -> *)
(*     let single_line = separate space terms *)
(*     and multi_line = *)
(*       align (concat [ *)
(*           x; *)
(*           hardline; *)
(*           blank 2; *)
(*           align (separate hardline xs) *)
(*         ]) *)
(*     in *)
(*     group @@ ifflat single_line multi_line *)


(* let indented_enclosed_lines starting_line indented ending_line = *)
(*   let open PPrint *)
(*   in *)
(*   separate hardline [ *)
(*     starting_line; *)
(*     indent indented; *)
(*     ending_line *)
(*   ] *)


(* let pad_right width document = *)
(*   let open PPrint *)
(*   in *)
(*   let document_width = *)
(*     requirement document *)
(*   in *)
(*   if *)
(*     document_width < width *)
(*   then *)
(*     document ^^ blank (width - document_width) *)
(*   else *)
(*     document *)


(* let string_of_document ?(page_width = 80) document = *)
(*   let buffer = Stdlib.Buffer.create 10000 *)
(*   in *)
(*   PPrint.ToBuffer.pretty 1.0 page_width buffer document; *)
(*   Stdlib.Buffer.contents buffer *)


(* let html_of_document document = *)
(*   string_of_document document *)


(* let separate_nonempty separator items = *)
(*   let open PPrint *)
(*   in *)
(*   let is_nonempty item = *)
(*     requirement item > 0 *)
(*   in *)
(*   let nonempty_items = *)
(*     List.filter ~f:is_nonempty items *)
(*   in *)
(*   separate separator nonempty_items *)


(* (\* *)
(*   Renders [(header1, description1), (header2, description2)] as *)

(*   header1 *)
(*     description1 *)
(*   header2 *)
(*     description2 *)
(* *\) *)
(* let description_list (items : (document * document) list) : document = *)
(*   let open PPrint *)
(*   in *)
(*   let render_item (header, description) = *)
(*     separate hardline [ header; indent description ] *)
(*   in *)
(*   separate hardline @@ List.map ~f:render_item items *)


(* let line_nl (d : document) : document = *)
(*   PPrint.(d ^^ hardline) *)


(* type build_lines_context = *)
(*   { *)
(*     line       : document -> unit; *)
(*     lines      : document list -> unit; *)
(*     empty_line : unit -> unit; *)
(*   } *)


(* let build_lines (body : build_lines_context -> unit) = *)
(*   let open PPrint *)
(*   in *)
(*   let reversed_accumulated_lines = ref [] *)
(*   in *)
(*   let accumulate (line : document) : unit = *)
(*     reversed_accumulated_lines := line :: !reversed_accumulated_lines *)
(*   in *)
(*   let context = *)
(*     let line d        = accumulate d *)
(*     in *)
(*     let lines ds      = List.iter ~f:accumulate ds *)
(*     and empty_line () = accumulate empty *)
(*     in *)
(*     { line; lines; empty_line } *)
(*   in *)
(*   body context; *)
(*   separate hardline @@ List.rev !reversed_accumulated_lines *)


(* let lines (strings : string list) : document = *)
(*   PPrint.(separate_map hardline string strings) *)


(* let vertical ?(separator = PPrint.hardline) documents = *)
(*   PPrint.(separate separator documents) *)


(* let horizontal ?(separator = space) documents = *)
(*   PPrint.(separate separator documents) *)


(* let vertical_strings ?(separator = PPrint.hardline) strings = *)
(*   PPrint.(vertical ~separator @@ List.map ~f:utf8string strings) *)


(* (\* *)

(*      first second *)

(*    or *)

(*      first *)
(*        second *)

(* *\) *)
(* let horizontal_or_indent (first : document) (second : document) = *)
(*   let open PPrint *)
(*   in *)
(*   group begin *)
(*     concat [ *)
(*       first; *)
(*       break 1; *)
(*       ifflat second (indent second) *)
(*     ] *)
(*   end *)


(* (\* *)
(*      a b c d *)

(*    or *)

(*      a *)
(*      b *)
(*      c *)
(*      d *)
(* *\) *)
(* let horizontal_or_vertical ?(separator = empty) (documents : document list) = *)
(*   PPrint.(group @@ separate PPrint.(separator ^^ break 1) documents) *)


(* let build_horizontal ?(separator = space) (builder : document Auxlib.list_builder -> unit) : document = *)
(*   let items = *)
(*     Auxlib.build_list builder *)
(*   in *)
(*   horizontal ~separator items *)


(* let build_vertical ?(separator = hardline) (builder : document Auxlib.list_builder -> unit) : document = *)
(*   let items = *)
(*     Auxlib.build_list builder *)
(*   in *)
(*   vertical ~separator items *)


(* (\* Checks if document fits on a single line *\) *)
(* let is_single_line (document : document) : bool = *)
(*   let ends_on_newline (string : string) : bool = *)
(*     String.is_suffix string ~suffix:"\n" *)
(*   in *)
(*   let count_newlines (string : string) : int = *)
(*     String.count string ~f:(Char.equal '\n') *)
(*   in *)
(*   let is_single_line (string : string) : bool = *)
(*     let newline_count = count_newlines string *)
(*     in *)
(*     newline_count = 0 || (newline_count = 1 && ends_on_newline string) *)
(*   in *)
(*   is_single_line @@ string_of_document document *)


(* (\* *)

(*   a + b + c + d *)

(* *\) *)
(* let pp_binary_operation (operator : document) (operands : document list) = *)
(*   horizontal ~separator:(space ^^ operator ^^ space) operands *)


(* (\* *)

(*    [ enclosed ] *)

(*    separator can be used to specify how to separate the delimiters from the enclosed document *)

(* *\) *)
(* let enclose *)
(*     ?(separator  : document            = space) *)
(*      (delimiters : document * document        ) *)
(*      (enclosed   : document                   ) : document *)
(*   = *)
(*   let left_delimiter, right_delimiter = delimiters *)
(*   in *)
(*   horizontal ~separator [ *)
(*     left_delimiter; *)
(*     enclosed; *)
(*     right_delimiter *)
(*   ] *)


(* let enclose_vertically *)
(*     ?(separator : document             = hardline) *)
(*      (delimiters : document * document           ) *)
(*      (enclosed   : document                      ) : document *)
(*   = *)
(*   let left_delimiter, right_delimiter = delimiters *)
(*   in *)
(*   vertical ~separator [ *)
(*     left_delimiter; *)
(*     enclosed; *)
(*     right_delimiter *)
(*   ] *)


(*

   [ a; b; c; d ]

   or

   [
     a;
     b;
     c;
     d
   ]

*)
let delimited_list ~delimiters ~items ~separator =
  let left_delimiter, right_delimiter = delimiters
  in
  vertical [
    left_delimiter;
    horizonta
    right_delimiter;
  ]


(*

   head[a,
        b,
        c,
        ...]

*)
let application ~head ~delimiters ~arguments ~separator =
  let left_delimiter, right_delimiter = delimiters
  in
  horizontal [
    head;
    left_delimiter;
    vertical @@ Auxlib.map_except_last
      ~f:(fun arg -> horizontal [arg; separator])
      ~f_last:(fun arg -> arg)
      arguments;
    right_delimiter
  ]
