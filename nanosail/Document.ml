open! ExtBase


let rec repeat_string (string : string) (n : int) : string =
  if n = 0
  then ""
  else string ^ (repeat_string string (n-1))


let indent_strings (indentation : int) (strings : string list) : string list =
  let indent string =
    repeat_string " " indentation ^ string
  in
  List.map ~f:indent strings


module type ANNOTATION = sig
  type t

  val empty    : t
  val is_empty : t -> bool
  val combine  : t -> t -> t
  val to_html  : t -> Html.t
end


module Make(Annotation : ANNOTATION) = struct
  (*
     Should never be created directly by client.
     Factory functions are to be used instead.
  *)
  type t =
    | Empty
    | String     of string
    | Horizontal of t * t
    | Vertical   of t * t
    | Annotated  of t * Annotation.t
    | Decorated  of t * AnsiColor.Decoration.t list
  

  (*
     Note: documents should be built using factory functions, which automatically remove empty subdocuments.
     E.g., Horizontal (Empty, Empty) should never occur.
  *)
  let rec is_empty (document : t) : bool =
    match document with
     | Empty              -> true
     | String _           -> false
     | Horizontal (_, _)  -> false 
     | Vertical (_, _)    -> false
     | Annotated (doc, _) -> is_empty doc
     | Decorated (doc, _) -> is_empty doc


  let rec is_single_line (document : t) : bool =
    match document with
    | Empty               -> true
    | String _            -> true
    | Horizontal (d1, d2) -> is_single_line d1 && is_single_line d2
    | Vertical (_, _)     -> false
    | Annotated (doc, _)  -> is_single_line doc
    | Decorated (doc, _)  -> is_single_line doc


  type annotated_string =
    | AnnotatedString of { string : string; annotation : Annotation.t; undecorated_length : int }
    | Concatenation of annotated_string * annotated_string


  let rec length (annotated_string : annotated_string) : int =
    match annotated_string with
    | AnnotatedString { undecorated_length; _ } -> undecorated_length
    | Concatenation (s1, s2)                    -> length s1 + length s2


  let to_annotated_strings (document : t) : annotated_string list =
    let rec to_annotated_strings
        (accumulated_annotation : Annotation.t               )
        (current_decoration     : AnsiColor.Decoration.t list)
        (document               : t                          ) : annotated_string list
      =
      match document with
      | Empty -> []
                 
      | String string -> begin
          [
            AnnotatedString {
              string;
              annotation=accumulated_annotation;
              undecorated_length = String.length string
            }
          ]
        end
                         
      | Horizontal (left, right) -> begin
          let left'  = to_annotated_strings accumulated_annotation current_decoration left
          and right' = to_annotated_strings accumulated_annotation current_decoration right
          in
          match List.split_last left' with
          | Some (upper_left', last_left') -> begin
              match right' with
              | [] -> failwith "error"
              | first_right' :: bottom_right' -> begin
                  let indentation =
                    AnnotatedString {
                      string             = repeat_string " " (length last_left');
                      annotation         = Annotation.empty;
                      undecorated_length = length last_left';
                    }
                  in
                  List.concat [
                    upper_left';
                    [Concatenation (last_left', first_right')];
                    List.map ~f:(fun s -> Concatenation (indentation, s)) bottom_right'
                  ]
                end
            end
          | None -> failwith "TODO"
        end
        
      | Vertical (top, bottom) -> begin
          List.concat [
            to_annotated_strings accumulated_annotation current_decoration top;
            to_annotated_strings accumulated_annotation current_decoration bottom
          ]
        end
        
      | Decorated (subdocument, decorations) -> begin
          let subresults =
            to_annotated_strings accumulated_annotation decorations subdocument
          in
          let activate_decoration =
            AnsiColor.to_escape_sequence decorations
          and deactivate_decoration =
            AnsiColor.to_escape_sequence current_decoration
          in
          let decorate_string (string : string) : string
            =
            String.concat [
              activate_decoration;
              string;
              deactivate_decoration
            ]
          in
          let rec decorate_annotated_string (annotated_string : annotated_string) : annotated_string =
            match annotated_string with
             | AnnotatedString { string; annotation; undecorated_length } -> AnnotatedString { string = decorate_string string; annotation; undecorated_length }
             | Concatenation (child_1, child_2)                           -> Concatenation (decorate_annotated_string child_1, decorate_annotated_string child_2)
          in
          List.map ~f:decorate_annotated_string subresults
        end
        
      | Annotated (doc, annotation) -> begin
          to_annotated_strings
            (Annotation.combine accumulated_annotation annotation)
            current_decoration
            doc
        end
    in
    to_annotated_strings Annotation.empty [] document


  let rec strip_annotation (s : annotated_string) : string =
    match s with
    | AnnotatedString {string; _} -> string
    | Concatenation (s1, s2)      -> String.concat ~sep:"" [strip_annotation s1; strip_annotation s2]


  let to_strings (document : t) : string list =
    List.map ~f:strip_annotation @@ to_annotated_strings document


  (* todo improve implementation *)
  let measure_width (document : t) : int =
    let strings = to_strings document
    in
    List.fold_left ~f:Int.max ~init:0 @@ List.map ~f:String.length strings


  (* todo improve implementation *)
  let measure_height (document : t) : int =
    let strings = to_strings document
    in
    List.length strings


  (* todo improve implementation *)
  let measure (document : t) : int * int =
    (measure_width document, measure_height document)


  let rec undecorate (document : t) : t =
    match (document : t) with
    | Empty                               -> document
    | String _                            -> document
    | Horizontal (document_1, document_2) -> Horizontal (undecorate document_1, undecorate document_2)
    | Vertical (document_1, document_2)   -> Vertical (undecorate document_1, undecorate document_2)
    | Annotated (document, annotation)    -> Annotated (undecorate document, annotation)
    | Decorated (document, _)             -> document
  

  let to_string (document : t) : string =
    String.concat ~sep:"\n" @@ to_strings document


  let to_html (document : t) : Html.t =
    let rec html_of_annotated_string (s : annotated_string) : Html.t =
      match s with
      | AnnotatedString {string; annotation; undecorated_length = _} -> begin
          if
            Annotation.is_empty annotation
          then
            Html.escape_string string
          else
            Html.span
              ~class_name:"tooltipped"
              begin
                Html.concat [
                    Html.string string;
                    Html.div ~class_name:"tooltip" begin
                        Annotation.to_html annotation
                      end
                  ]
              end
        end
      | Concatenation (s1, s2) -> begin
          Html.concat [
            html_of_annotated_string s1;
            html_of_annotated_string s2
          ]
        end
    in
    let annotated_strings =
      to_annotated_strings document
    in
    let html_lines =
      List.map
        ~f:(fun s -> Html.concat [ html_of_annotated_string s; Html.break ])
        annotated_strings
    in
    Html.concat html_lines


  let string s =
    String s

  let format fmt =
    Printf.ksprintf string fmt

  let empty      = Empty

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


  let rec horizontal (documents : t list) : t =
    let group d1 d2 =
      match d1, d2 with
      | Empty, _ -> d2
      | _, Empty -> d1
      | _, _     -> Horizontal (d1, d2)
    in
    match documents with
    | []       -> empty
    | [d]      -> d
    | [d1; d2] -> group d1 d2
    | d::ds    -> group d @@ horizontal ds


  let rec vertical (documents : t list) : t =
    let group d1 d2 =
      match d1, d2 with
      | Empty, _ -> d2
      | _, Empty -> d1
      | _, _     -> Vertical (d1, d2)
    in
    match documents with
    | []       -> String ""
    | [d]      -> d
    | [d1; d2] -> group d1 d2
    | d::ds    -> group d @@ vertical ds


  let annotate
      (annotation : Annotation.t)
      (document   : t           ) : t
    =
    Annotated (document, annotation)


  let decorate
      (decorations : AnsiColor.Decoration.t list)
      (document    : t                          ) : t
    =
    if
      is_empty document
    then
      Empty
    else
      Decorated (document, decorations)


  let hanging (documents : t list) : t =
    match documents with
    | []            -> empty
    | [d]           -> d
    | first :: rest -> horizontal [first; vertical rest]


  let indent ?(level = 2) (document : t) : t =
    horizontal [
      string @@ repeat_string " " level;
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
      Option.value ~default:0 @@ List.max_elt ~compare:Int.compare @@ List.map ~f:(fun offset -> measure_width @@ integer @@ start_index + offset) (List.indices items)
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
    let width = measure_width document
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

