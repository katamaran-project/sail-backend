(*

  F-Expressions are used for debugging: they provide a simple way to convert
  data structures to text.

  An F-expression is

  - an integer (1, 2, 3, ...)
  - a boolean (True/False)
  - a string ("something")
  - nil (Nil)
  - a list ([x, y, z, ...])
  - an application, which consists of
    - a head
    - positional arguments
    - keyword arguments
    An application is formatted as Head[pos1, pos2, ..., kw1=val1, kw2=val2, ...]

   F-Expressions can be pretty printed and diffed.
   
*)
open! ExtBase


type t =
  | Integer     of int
  | Bool        of bool
  | String      of string
  | Application of { head : string; positional : t list; keyword : (string * t) list }
  | List        of t list
  | Nil


let rec equal
    (fexpr_1 : t)
    (fexpr_2 : t) : bool
  =
  match fexpr_1 with
  | Integer n_1 -> begin
      match fexpr_2 with
      | Integer n_2 -> begin
          Int.equal
            n_1
            n_2
        end
      | _ -> false
    end
    
  | Bool bool_1 -> begin
      match fexpr_2 with
      | Bool bool_2 -> begin
          Bool.equal
            bool_1
            bool_2
        end
      | _ -> false
    end
    
  | String string_1 -> begin
      match fexpr_2 with
      | String string_2 -> begin
          String.equal
            string_1
            string_2
        end
      | _ -> false
    end
    
  | Application { head = head_1; positional = positional_1; keyword = keyword_1 } -> begin
      match fexpr_2 with
      | Application { head = head_2; positional = positional_2; keyword = keyword_2 } -> begin
          String.equal
            head_1
            head_2
          &&
          List.equal equal
            positional_1
            positional_2
          &&
          List.equal (Tuple.Pair.equal String.equal equal)
            keyword_1
            keyword_2
        end
      | _ -> false
    end
    
  | List elements_1 -> begin
      match fexpr_2 with
      | List elements_2 -> begin
          List.equal
            equal
            elements_1
            elements_2
        end
      | _ -> false
    end
    
  | Nil -> begin
      match fexpr_2 with
      | Nil -> true
      | _   -> false
    end


(*
   Pretty prints a list as

     [
       element1;
       element2;
       ...;
       elementN
     ]

   or just

     []

   if the list is empty.
*)
let pp_list (items : PP.document list) : PP.document =
  if
    List.is_empty items
  then
    PP.horizontal [ PP.lbracket; PP.rbracket ]
  else begin
    let delimiters =
      (PP.lbracket, PP.rbracket)
    and separator =
      PP.string ","
    in
    PP.(delimited_list ~delimiters ~items ~separator)
  end


let pp_application
    (head       : PP.document                     )
    (positional : PP.document list                )
    (keyword    : (PP.document * PP.document) list) : PP.document
  =
  if
    List.is_empty positional && List.is_empty keyword
  then
    head
  else begin
    let delimiters = (PP.lbracket, PP.rbracket)
    and arguments  =
      let keyword =
        List.map ~f:(fun (k, v) -> PP.(horizontal [ k; equals; v ])) keyword
      in
      List.concat [positional; keyword]
    and separator = PP.string ","
    in
    PP.application ~head ~delimiters ~arguments ~separator
  end


(*
   Pretty prints an f-expression.
*)
let rec pp (fexpr : t) : PP.document =
  match fexpr with
  | Integer n        -> PP.string @@ Int.to_string n
  | Bool b           -> PP.string @@ if b then "True" else "False"
  | String s         -> PP.(surround dquotes @@ string s)
  | Nil              -> PP.string "Nil"
  | List items       -> pp_list @@ List.map ~f:pp items
  | Application { head; positional; keyword } -> begin
      let head =
        PP.string head
      and positional =
        List.map ~f:pp positional
      and keyword =
        List.map ~f:(fun (k, v) -> (PP.string k, pp v)) keyword
      in
      pp_application head positional keyword
    end


let rec pp_diff
    (compared_with : t)
    (printed       : t) : PP.document
  =
  let highlight (document : PP.document) : PP.document =
    PP.decorate [ BackgroundColor Red ] document
  in
  if
    equal printed compared_with
  then
    pp printed
  else begin
    match printed with
    | Integer _ -> highlight @@ pp printed
    | Bool _    -> highlight @@ pp printed
    | String _  -> highlight @@ pp printed
    | Nil       -> highlight @@ pp printed
    | List printed_elements -> begin
        match compared_with with
        | List compared_with_elements -> begin
            match List.zip compared_with_elements printed_elements with
            | Ok pairs        -> pp_list @@ List.map ~f:(Fn.uncurry pp_diff) pairs
            | Unequal_lengths -> highlight @@ pp printed
          end
        | _ -> highlight @@ pp printed
      end
    | Application { head = printed_head; positional = printed_positional; keyword = printed_keyword } -> begin
        match compared_with with
        | Application { head = compared_with_head; positional = compared_with_positional; keyword = compared_with_keyword } -> begin
            let head =
              let doc = PP.string printed_head
              in
              if
                String.equal printed_head compared_with_head
              then
                doc
              else
                highlight doc
            in
            let positional =
              let rec pp_positional
                  (printed_positional       : t list)
                  (compared_with_positional : t list) : PP.document list
                =
                match printed_positional, compared_with_positional with
                | []   , []    -> []
                | x::xs, []    -> highlight (pp x) :: pp_positional xs []
                | []   , _     -> [ highlight @@ PP.string "<missing>" ]
                | x::xs, y::ys -> begin
                    let head =
                      pp_diff y x
                    and tail =
                      pp_positional xs ys
                    in
                    head :: tail
                  end
              in
              pp_positional printed_positional compared_with_positional
            in
            let keyword =
              let rec pp_keyword
                  (printed_keyword       : (string * t) list)
                  (compared_with_keyword : (string * t) list) : (PP.document * PP.document) list
                =
                match printed_keyword, compared_with_keyword with
                | []            , []             -> []
                | (k, v) :: xs  , []             -> (highlight @@ PP.string k, highlight @@ pp v) :: pp_keyword xs []
                | []            , _              -> [ (highlight @@ PP.string "<missing>", highlight @@ PP.string "<missing>") ]
                | (k1, v1) :: xs, (k2, v2) :: ys -> begin
                    let keyword =
                      let doc = PP.string k1
                      in
                      if
                        String.equal k1 k2
                      then
                        doc
                      else
                        highlight doc
                    and value =
                      pp_diff v2 v1
                    in
                    (keyword, value) :: pp_keyword xs ys                      
                  end
              in
              pp_keyword printed_keyword compared_with_keyword
            in
            pp_application head positional keyword
          end
        | _ -> highlight @@ pp printed
      end
  end    
  

let to_string (fexpr : t) : string =
  PP.to_string @@ pp fexpr


let mk_int (n : int) : t =
  Integer n


let mk_bool (b : bool) : t =
  Bool b


let mk_string (s : string) : t =
  String s


let mk_application ?(positional = []) ?(keyword = []) head =
  Application { head; positional; keyword }


let mk_symbol (name : string) : t =
  Application { head=name; positional = []; keyword = [] }


let mk_list (items : t list) =
  List items


let mk_nil =
  Nil


let mk_option (fexpr : t option) =
  match fexpr with
  | Some fexpr -> mk_application ~positional:[fexpr] "Some"
  | None       -> mk_symbol "None"


let mk_ocaml_location (location : Lexing.position) =
  let keyword = [
      ("pos_fname", mk_string location.pos_fname);
      ("pos_lnum", mk_int location.pos_lnum);
      ("pos_bol", mk_int location.pos_bol);
      ("pos_cnum", mk_int location.pos_cnum)
    ]
  in
  mk_application ~keyword "OCamlLocation"


let rec mk_sail_location (location : Libsail.Parse_ast.l) =
  let prefix str =
    "SailLoc:" ^ str
  in
  match location with
  | Unknown                -> mk_symbol @@ prefix "Unknown"
  | Unique (n, loc)        -> mk_application ~positional:[mk_int n; mk_sail_location loc]                              @@ prefix "Unique"
  | Generated loc          -> mk_application ~positional:[mk_sail_location loc]                                        @@ prefix "Generated"
  | Hint (str, loc1, loc2) -> mk_application ~positional:[mk_string str; mk_sail_location loc1; mk_sail_location loc2] @@ prefix "Hint"
  | Range (loc1, loc2)     -> mk_application ~positional:[mk_ocaml_location loc1; mk_ocaml_location loc2]              @@ prefix "Range"
