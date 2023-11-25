module PP = PPrint
module PU = Pputil


let eol = PP.dot

let pp_list items =
  PU.pp_delimited_sequence PP.lbracket PP.rbracket PP.semi items

let pp_section section_title contents =
  let open PPrint
  in
  let first_line =
    string "Section" ^^ space ^^ string section_title ^^ eol
  in
  let last_line =
    string "End" ^^ space ^^ string section_title ^^ eol
  in
  PU.pp_indented_enclosed_lines first_line contents last_line

let pp_inductive_type name typ constructors =
  let open PPrint
  in
  let first_line =
    concat [
        string "Inductive";
        space;
        name;
        space;
        colon;
        space;
        typ;
        space;
        string ":="
      ]
  in
  let constructor_lines =
    let pp_constructor (name, typ) =
      concat [
          string "|";
          space;
          name;
          space;
          colon;
          space;
          align typ
        ]
    in
    List.map pp_constructor constructors
  in
  let lines =
    first_line :: constructor_lines
  in
  separate hardline lines ^^ eol
