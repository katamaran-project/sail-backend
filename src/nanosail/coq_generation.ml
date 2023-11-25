open PPrint

module PU = Pputil


let eol = dot

let list items =
  PU.pp_delimited_sequence lbracket rbracket semi items

let product v1 v2 =
  soft_surround 1 0 lparen (v1 ^^ comma ^^ break 1 ^^ v2) rparen

let section section_title contents =
  let open PPrint
  in
  let first_line =
    string "Section" ^^ space ^^ string section_title ^^ eol
  in
  let last_line =
    string "End" ^^ space ^^ string section_title ^^ eol
  in
  PU.pp_indented_enclosed_lines first_line contents last_line

let inductive_type name typ constructors =
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
          align typ;
        ]
    in
    List.map pp_constructor constructors
  in
  let lines =
    first_line :: constructor_lines
  in
  separate hardline lines ^^ eol

let definition identifier parameters result_type body =
  let pp_parameters =
    if requirement parameters == 0
    then empty
    else (break 1) ^^ parameters
  in
  let first_line =
    group (
      concat [
        string "Definition";
        space;
        identifier;
        pp_parameters;
        space;
        colon;
        group (
          concat [
            space;
            align result_type;
          ]
        );
        space;
        string ":="
      ];
    )
  and second_line =
    PU.indent' body
  in
  concat [
    separate hardline [first_line; second_line];
    eol
  ]
