open Base

include PPrint


let indent' ?(level = 2) doc = blank level ^^ align doc

let small_step = twice hardline

let big_step   = twice small_step


let delimited_sequence left_delimiter right_delimiter separator items =
  concat [
    left_delimiter;
    align (
      group (separate (separator ^^ break 1) items)
    );
    right_delimiter
  ]


(*
   Formats elements as

    x xs[0] xs[1] xs[2]

    or

    x xs[0]
      xs[1]
      xs[2]
*)
let hanging_list ?(adaptive = true) x xs =
  if
    adaptive
  then
    concat [
      x;
      space;
      align (group (separate (break 1) xs))
    ]
  else
    concat [
      x;
      space;
      align (separate hardline xs)
    ]


let simple_app terms =
  match terms with
  | []    -> empty
  | [x]   -> x
  | x::xs ->
    let single_line = separate space terms
    and multi_line =
      align (concat [
          x;
          hardline;
          blank 2;
          align (separate hardline xs)
        ])
    in
    group @@ ifflat single_line multi_line


let indented_enclosed_lines starting_line indented ending_line =
  separate hardline [
    starting_line;
    indent' indented;
    ending_line
  ]


let pad_right width document =
  let document_width =
    requirement document
  in
  if
    document_width < width
  then
    document ^^ blank (width - document_width)
  else
    document


let string_of_document ?(page_width = 80) document =
  let buffer = Stdlib.Buffer.create 1000
  in
  PPrint.ToBuffer.pretty 1.0 page_width buffer document;
  Stdlib.Buffer.contents buffer


let separate_nonempty separator items =
  let is_nonempty item =
    requirement item > 0
  in
  let nonempty_items =
    List.filter ~f:is_nonempty items
  in
  separate separator nonempty_items


(*
  Renders [(header1, description1), (header2, description2)] as

  header1
    description1
  header2
    description2
*)
let description_list (items : (document * document) list) : document =
  let render_item (header, description) =
    separate hardline [ header; indent' description ]
  in
  separate hardline @@ List.map ~f:render_item items


let line_nl (d : document) : document =
  d ^^ hardline


type build_lines_context =
  {
    line       : document -> unit;
    lines      : document list -> unit;
    empty_line : unit -> unit;
  }


let build_lines (body : build_lines_context -> unit) =
  let reversed_accumulated_lines = ref []
  in
  let accumulate (line : document) : unit =
    reversed_accumulated_lines := line :: !reversed_accumulated_lines
  in
  let context =
    let line d        = accumulate d
    in
    let lines ds      = List.iter ~f:accumulate ds
    and empty_line () = accumulate empty
    in
    { line; lines; empty_line }
  in
  body context;
  separate hardline @@ List.rev !reversed_accumulated_lines


let lines (strings : string list) : document =
  separate_map hardline string strings


let vertical ?(spacing = 1) documents =
  separate (repeat spacing hardline) documents


let vertical_strings ?(spacing = 1) strings =
  vertical ~spacing @@ List.map ~f:utf8string strings
