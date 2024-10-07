let uncurry f (x, y) = f x y


let rec repeat_string (string : string) (n : int) : string =
  if n = 0
  then ""
  else string ^ (repeat_string string (n-1))


let indent_strings (indentation : int) (strings : string list) : string list =
  let indent string =
    repeat_string " " indentation ^ string
  in
  List.map indent strings


module type ANNOTATION = sig
  type t

  val empty   : t
  val combine : t -> t -> t
  val to_html : t -> string
end


module Make(Annotation : ANNOTATION) = struct

  type t =
    | Empty
    | String     of string
    | Horizontal of t * t
    | Vertical   of t * t
    | Annotated  of t * Annotation.t


  let rec is_empty (document : t) : bool =
    match document with
     | Empty              -> true
     | String _           -> false
     | Horizontal (_, _)  -> false
     | Vertical (_, _)    -> false
     | Annotated (doc, _) -> is_empty doc


  let rec is_single_line (document : t) : bool =
    match document with
    | Empty               -> true
    | String _            -> true
    | Horizontal (d1, d2) -> is_single_line d1 && is_single_line d2
    | Vertical (_, _)     -> false
    | Annotated (d, _)    -> is_single_line d
  
  
  type annotated_string =
    | AnnotatedString of { string : string; annotation : Annotation.t }
    | Concatenation of annotated_string * annotated_string


  let rec length (s : annotated_string) =
    match s with
    | AnnotatedString { string; _ } -> String.length string
    | Concatenation (s1, s2)        -> length s1 + length s2


  let to_annotated_strings (document : t) : annotated_string list =
    let rec to_annotated_strings
        (accumulated_annotation : Annotation.t)
        (document               : t           ) : annotated_string list
      =
      match document with
      | Empty -> []
      | String string -> [ AnnotatedString { string; annotation=accumulated_annotation } ]
      | Horizontal (left, right) -> begin
          let left'  = to_annotated_strings accumulated_annotation left
          and right' = to_annotated_strings accumulated_annotation right
          in
          match Auxlib.split_last left' with
          | Some (upper_left', last_left') -> begin
              match right' with
              | [] -> failwith "error"
              | first_right' :: bottom_right' -> begin
                  let indentation =
                    AnnotatedString {
                      string     = repeat_string " " (length last_left');
                      annotation = Annotation.empty
                    }
                  in
                  List.concat [
                    upper_left';
                    [Concatenation (last_left', first_right')];
                    List.map (fun s -> Concatenation (indentation, s)) bottom_right'
                  ]
                end
            end
          | None -> failwith "TODO"
        end
      | Vertical (top, bottom) -> begin
          List.concat [
            to_annotated_strings accumulated_annotation top;
            to_annotated_strings accumulated_annotation bottom
          ]
        end
      | Annotated (doc, annotation) -> to_annotated_strings (Annotation.combine accumulated_annotation annotation) doc
    in
    to_annotated_strings Annotation.empty document

  
  let rec strip_annotation (s : annotated_string) : string =
    match s with
    | AnnotatedString {string; _} -> string
    | Concatenation (s1, s2)      -> String.concat "" [strip_annotation s1; strip_annotation s2]


  let to_strings (document : t) : string list =
    List.map strip_annotation @@ to_annotated_strings document


  let to_string (document : t) : string =
    String.concat "\n" @@ to_strings document


  let to_html (document : t) : string =
    let rec html_of_annotated_string (s : annotated_string) : string =
      match s with
      | AnnotatedString {string; annotation} -> begin
          Printf.sprintf
            {|<div class="tooltipped">%s<div class="tooltip">%s</div></div>|}
            string
            (Annotation.to_html annotation)
        end
      | Concatenation (s1, s2) -> begin
          String.concat "" [
            html_of_annotated_string s1;
            html_of_annotated_string s2
          ]
        end
    in
    let annotated_strings =
      to_annotated_strings document
    in
    let html_lines =
      List.map html_of_annotated_string annotated_strings
    in
    String.concat "\n" html_lines


  let empty = Empty


  let string s =
    if Int.equal 0 @@ String.length s
    then Empty
    else String s


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
  

  let hanging_list (documents : t list) : t =
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
        entry;
        indent description;
      ]
    in
    vertical @@ List.map (uncurry render_item) items


  let enclose
      (layout     : t list -> t)
      (delimiters : t * t      )
      (enclosed   : t          ) : t =
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


  let repeat
        (layout   : t list -> t)
        (n        : int        )
        (document : t          ) : t
    =
    layout @@ Auxlib.repeat n document
end
