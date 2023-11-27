open PPrint

module Big_int = Nat_big_num
module PU = Pputil


let eol = dot

let left_comment_delimiter = string "(*"
let right_comment_delimiter = string "*)"

let comment comment =
  let str = PU.string_of_document comment
  in
  if
    Util.count_chars str '\n' == 1 && String.ends_with ~suffix:"\n" str
  then
    concat [
        left_comment_delimiter;
        space;
        string (Util.strip str);
        space;
        right_comment_delimiter
      ]
  else
    concat [
        left_comment_delimiter;
        twice hardline;
        PU.indent' comment;
        hardline;
        right_comment_delimiter
      ]

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

let match' expression cases =
  let match_line =
    string "match" ^^ space ^^ expression ^^ space ^^ string "with"
  in
  let case_lines =
    let longest_pattern_width =
      let widths = List.map (fun pattern -> requirement pattern) (List.map fst cases)
      in
      List.fold_left max 0 widths
    in
    let generate_case (pattern, expression) =
      separate space [
          bar;
          PU.pad_right longest_pattern_width pattern;
          string "=>";
          expression
        ]
    in
    List.map generate_case cases
  in
  let final_line =
    string "end"
  in
  let lines =
    List.flatten [
      [ match_line ];
      case_lines;
      [ final_line ]
    ]
  in
  separate hardline lines

let integer i =
  let pp_i = string (Big_int.to_string i ^ "%Z") in
  if i < Z.zero then parens pp_i else pp_i
