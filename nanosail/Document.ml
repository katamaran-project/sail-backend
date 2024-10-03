open Base


let rec repeat_string (string : string) (n : int) : string =
  if n = 0
  then ""
  else string ^ (repeat_string string (n-1))


let indent_strings (indentation : int) (strings : string list) : string list =
  let indent string =
    repeat_string " " indentation ^ string
  in
  List.map ~f:indent strings


let split_last (xs : 'a list) : 'a list * 'a =
  match List.rev xs with
  | []    -> failwith "error"
  | x::xs -> (List.rev xs, x)


type t =
  | Empty
  | String     of string
  | Horizontal of t * t
  | Vertical   of t * t


let space = String " "


let rec to_strings  (document : t) : string list =
  match document with
  | Empty -> []
  | String string -> [ string ]
  | Horizontal (left, right) -> begin
      let left'  = to_strings left
      and right' = to_strings right
      in
      let upper_left', last_left' = split_last left'
      in
      match right' with
      | [] -> failwith "error"
      | first_right' :: bottom_right' -> begin
          List.concat [
              upper_left';
              [last_left' ^ first_right'];
              indent_strings (String.length last_left') bottom_right'
            ]
        end
    end
  | Vertical (top, bottom) -> begin
      List.concat [ to_strings top; to_strings bottom ]
    end


let to_string (document : t) : string =
  String.concat ~sep:"\n" @@ to_strings document


let empty = Empty


let string s = String s


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


let separate
      (separator : t          )
      (documents : t list     ) : t list
  =
  let rec separate documents =
    match documents with
    | [] -> []
    | [d] -> [d]
    | d::ds -> d :: separator :: separate ds
  in
  separate documents


let hanging_list (documents : t list) : t =
  match documents with
  | []            -> empty
  | [d]           -> d
  | first :: rest -> horizontal [first; vertical rest]


let indent ?(n = 2) (document : t) : t =
  horizontal [
      string @@ repeat_string " " n;
      document
    ]


let description_list (items : (t * t) list) : t =
  let render_item entry description =
    vertical [
        entry;
        indent description;
      ]
  in
  vertical @@ List.map ~f:(Auxlib.uncurry render_item) items


let enclose
      (layout     : t list -> t)
      (delimiters : t * t      )
      (enclosed   : t          ) : t =
  let left_delimiter, right_delimiter = delimiters
  in
  layout [ left_delimiter; enclosed; right_delimiter ]
