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

*)
open! ExtBase


type t =
  | Integer     of int
  | Bool        of bool
  | String      of string
  | Application of { head : string; positional : t list; keyword : (string * t) list }
  | List        of t list
  | Nil


let rec pp (fexpr : t) : PP.document =
  match fexpr with
  | Integer n        -> PP.string @@ Int.to_string n
  | Bool b           -> PP.string @@ if b then "True" else "False"
  | String s         -> PP.(surround dquotes @@ string s)
  | Nil              -> PP.string "Nil"
  | Application { head; positional; keyword } -> begin
      let head = PP.string head
      and delimiters = (PP.lbracket, PP.rbracket)
      and arguments  =
        let pp_positional =
          List.map ~f:pp positional
        and pp_keyword =
          List.map ~f:(fun (k, v) -> PP.(horizontal [ string k; equals; pp v ])) keyword
        in
        List.concat [pp_positional; pp_keyword]
      and separator = PP.string ","
      in
      PP.application ~head ~delimiters ~arguments ~separator
    end
  | List items -> begin
      let delimiters =
        (PP.lbracket, PP.rbracket)
      and items =
        List.map ~f:pp items
      and separator =
        PP.string ","
      in
      PP.(delimited_list ~delimiters ~items ~separator)
    end


let to_string (fexpr : t) : string =
  PP.string_of_document @@ pp fexpr


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
