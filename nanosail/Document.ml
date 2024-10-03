let rec repeat (string : string) (n : int) : string =
  if n = 0
  then ""
  else string ^ (repeat string (n-1))


let indent (indentation : int) (strings : string list) : string list =
  let indent string =
    repeat " " indentation ^ string
  in
  List.map indent strings


let split_last (xs : 'a list) : 'a list * 'a =
  match List.rev xs with
  | []    -> failwith "error"
  | x::xs -> (List.rev xs, x)


type t =
  | String     of string
  | Horizontal of t * t
  | Vertical   of t * t


let space = String " "


let rec to_strings  (document : t) : string list =
  match document with
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
              indent (String.length last_left') bottom_right'
            ]
        end
    end
  | Vertical (top, bottom) -> begin
      List.concat [ to_strings top; to_strings bottom ]
    end


let to_string (document : t) : string =
  String.concat "\n" @@ to_strings document


let empty = String ""


let string s = String s


let rec horizontal (documents : t list) : t =
  match documents with
  | []       -> empty
  | [d]      -> d
  | [d1; d2] -> Horizontal (d1, d2)
  | d::ds    -> Horizontal (d, horizontal ds)


let rec vertical (documents : t list) : t =
  match documents with
  | []       -> String ""
  | [d]      -> d
  | [d1; d2] -> Vertical (d1, d2)
  | d::ds    -> Vertical (d, vertical ds)


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
