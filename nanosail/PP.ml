open Base


type document    = PPrint.document


let empty     = PPrint.empty
let string    = PPrint.utf8string
let utf8string = PPrint.utf8string (* todo remove *)

let plus      = PPrint.plus
let star      = PPrint.star
let minus     = PPrint.minus
let equals    = PPrint.equals

let lparen    = PPrint.lparen
let rparen    = PPrint.rparen
let langle    = PPrint.langle
let rangle    = PPrint.rangle
let lbracket  = PPrint.lbracket
let rbracket  = PPrint.rbracket
let lbrace    = PPrint.lbrace
let rbrace    = PPrint.rbrace

let braces    = PPrint.braces
let parens    = PPrint.parens
let dquotes   = PPrint.dquotes
let brackets  = PPrint.brackets

let dot       = PPrint.dot
let bang      = PPrint.bang
let semi      = PPrint.semi
let comma     = PPrint.comma
let colon     = PPrint.colon
let space     = PPrint.space

let ampersand = PPrint.ampersand
let percent   = PPrint.percent
let bar       = PPrint.bar
let underscore = PPrint.underscore

let (^^)     = PPrint.(^^)

let twice         = PPrint.twice
let hardline      = PPrint.hardline


(* todo remove *)
let separate      = PPrint.separate
let separate_map  = PPrint.separate_map
let concat        = PPrint.concat
let soft_surround = PPrint.soft_surround
let break         = PPrint.break
let align         = PPrint.align
let requirement   = PPrint.requirement


let write_to_channel
    (text_width     : PPrint.requirement )
    (output_channel : Stdio.Out_channel.t)
    (document       : document           )
  =
  PPrint.(ToChannel.pretty 1. text_width output_channel document)


let indent' ?(level = 2) doc = PPrint.(blank level ^^ align doc)


let delimited_sequence left_delimiter right_delimiter separator items =
  let open PPrint
  in
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
  let open PPrint
  in
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
  let open PPrint
  in
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
  let open PPrint
  in
  separate hardline [
    starting_line;
    indent' indented;
    ending_line
  ]


let pad_right width document =
  let open PPrint
  in
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
  let open PPrint
  in
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
  let open PPrint
  in
  let render_item (header, description) =
    separate hardline [ header; indent' description ]
  in
  separate hardline @@ List.map ~f:render_item items


let line_nl (d : document) : document =
  PPrint.(d ^^ hardline)


type build_lines_context =
  {
    line       : document -> unit;
    lines      : document list -> unit;
    empty_line : unit -> unit;
  }


let build_lines (body : build_lines_context -> unit) =
  let open PPrint
  in
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
  PPrint.(separate_map hardline string strings)


let vertical ?(separator = PPrint.hardline) documents =
  PPrint.(separate separator documents)


let horizontal ?(separator = space) documents =
  PPrint.(separate separator documents)


let vertical_strings ?(separator = PPrint.hardline) strings =
  PPrint.(vertical ~separator @@ List.map ~f:utf8string strings)


(*

     first second

   or

     first
       second
   
*)
let horizontal_or_indent (first : document) (second : document) =
  let open PPrint
  in
  group begin
    concat [
      first;
      break 1;
      ifflat second (indent' second)
    ]
  end


(*
     a b c d

   or

     a
     b
     c
     d
*)
let horizontal_or_vertical ?(separator = empty) (documents : document list) =
  PPrint.(group @@ separate PPrint.(separator ^^ break 1) documents)


let build_horizontal ?(separator = space) (builder : document Auxlib.list_builder -> unit) : document =
  let items =
    Auxlib.build_list builder
  in
  horizontal ~separator items


let build_vertical ?(separator = space) (builder : document Auxlib.list_builder -> unit) : document =
  let items =
    Auxlib.build_list builder
  in
  vertical ~separator items


let delimited_list ~left_delimiter ~right_delimiter ~items ~separator =
  let flattened_layout =
    horizontal ~separator:space [
      left_delimiter;
      horizontal ~separator:PPrint.(separator ^^ space) items;
      right_delimiter
    ]

  and unflattened_layout =
    vertical [
      left_delimiter;
      indent' @@ vertical ~separator:PPrint.(separator ^^ hardline) items;
      right_delimiter;
    ]
  in
  
  PPrint.group begin
    PPrint.ifflat flattened_layout unflattened_layout
  end
