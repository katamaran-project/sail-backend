open ExtBase


module Make(Doc : Functor.DOCUMENT) = struct
  open Doc

  let plus       = string "+"
  let star       = string "*"
  let minus      = string "-"
  let equals     = string "="
  let slash      = string "/"

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

  let at         = string "@"

  let format fmt =
    Printf.ksprintf string fmt
  
  let hanging (documents : t list) : t =
    match documents with
    | []            -> empty
    | [d]           -> d
    | first :: rest -> horizontal [first; vertical rest]


  let indent ?(level = 2) (document : t) : t =
    horizontal [
      string @@ String.repeat " " level;
      document
    ]


  let description_list (items : (t * t) list) : t =
    let render_item entry description =
      vertical [
        decorate [Underlined] entry;
        indent description;
      ]
    in
    vertical @@ List.map ~f:(Fn.uncurry render_item) items


  let integer (n : int) =
    string @@ Int.to_string n


    (*
       Arranges items vertically in a table of two columns, where the first column contains indices.

         1: item1
         2: item2
         3: item3
    *)
  let numbered_list
      ?(start_index : int    = 1)
      (items        : t list    ) : t
    =
    let max_index_width : int =
      Option.value ~default:0 @@ List.max_elt ~compare:Int.compare @@ List.map ~f:(fun offset -> fst @@ measure @@ integer @@ start_index + offset) (List.indices items)
    in
    let rows =
      let build_row index item =
        horizontal [
          string @@ String.pad_left ~char:' ' (Int.to_string (start_index + index)) ~len:max_index_width;
          colon;
          space;
          item
        ]
      in
      List.mapi items ~f:build_row
    in
    vertical rows


    (*
       Encloses a document within delimiters.
       Layout determines how delimiters and document are placed with respect to one another.

       E.g., enclose horizontal brackets (string foo) produces [foo]
    *)
  let enclose
      (layout     : t list -> t)
      (delimiters : t * t      )
      (enclosed   : t          ) : t
    =
    let left_delimiter, right_delimiter = delimiters
    in
    layout [ left_delimiter; enclosed; right_delimiter ]


  (*
    Creates horizontal box in which the items are separated
    by the given separator

      item1 separator item2 separator ... itemN
  *)
  let separate_horizontally
      ~(separator : t     )
      (items     : t list) : t
    =
    let rec separate documents =
      match documents with
      | []    -> []
      | [x]   -> [x]
      | x::xs -> x :: separator :: separate xs
    in
    horizontal @@ separate items


  (*
    Creates a vertical box in which the items are separated by
    the given separator

      hbox[item1 separator]
      hbox[item2 separator]
      ...
      itemN
  *)
  let separate_vertically
      ~(separator : t     )
      (items     : t list) : t
    =
    let rec build (items : t list) : t list =
      match items with
      | []    -> []
      | [x]   -> [x]
      | x::xs -> horizontal [ x; separator ] :: build xs
    in
    vertical @@ build items


  let surround
      ?(layout                           : t list -> t = horizontal)
      ((left_delimiter, right_delimiter) : (t * t)                 )
      (document                          : t                       ) : t
    =
    layout [left_delimiter; document; right_delimiter]


  let repeat
      (layout   : t list -> t)
      (n        : int        )
      (document : t          ) : t
    =
    layout @@ List.repeat n document


  let pad_right
      (target_width : int)
      (document     : t  ) : t
    =
    let width = snd @@ measure document
    in
    if
      width < target_width
    then
      let padding =
        repeat horizontal (target_width - width) space
      in
      horizontal [ document; padding ]
    else
      document


  (*
    Arranges items vertically with an empty line in between each
  *)
  let paragraphs (documents : t list) : t =
    vertical @@ List.intersperse ~sep:(string "") documents

  (*

    a + b + c + d

  *)
  let binary_operation (* todo rename to binary_operation *)
      (operator : t     )
      (operands : t list) : t
    =
    let separator =
      horizontal [space; operator; space]
    in
    separate_horizontally ~separator operands

  (*

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
      indent @@ separate_vertically ~separator items;
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
      separate_vertically ~separator arguments;
      right_delimiter
    ]


  let from_multiline_string (str : string) =
    let lines = String.split_lines str
    in
    vertical @@ List.map ~f:string lines  
end
