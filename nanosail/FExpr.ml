open Base


type t =
  | Integer     of int
  | Bool        of bool
  | String      of string
  | Application of { head : string; positional : t list; keyword : (string * t) list }


let rec pp (fexpr : t) : PP.document =
  match fexpr with
  | Integer n        -> PP.string @@ Int.to_string n
  | Bool b           -> PP.string @@ Bool.to_string b
  | String s         -> PP.(dquotes @@ string s)
  | Application { head; positional; keyword } -> begin
      let head = PP.string head
      and delimiters = (PP.lbracket, PP.rbracket)
      and arguments  =
        let pp_positional =
          List.map ~f:pp positional
        and pp_keyword =
          List.map ~f:(fun (k, v) -> PP.(string k ^^ equals ^^ pp v)) keyword
        in
        List.concat [pp_positional; pp_keyword]
      and separator = PP.string ","
      in
      PP.application ~head ~delimiters ~arguments ~separator
    end


let to_string (fexpr : t) : string =
  PP.string_of_document @@ pp fexpr

