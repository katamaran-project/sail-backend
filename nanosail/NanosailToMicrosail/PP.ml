open Base

include PPrint


let indent' ?(level = 2) doc = blank level ^^ align doc

let small_step = twice hardline
let big_step   = twice small_step

let pp_delimited_sequence left_delimiter right_delimiter separator items =
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

let pp_indented_enclosed_lines starting_line indented ending_line =
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
