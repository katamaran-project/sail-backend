open PPrint

module Big_int = Nat_big_num
module PU = Gen.Util


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
  if
    List.is_empty items
  then
    lbracket ^^ rbracket
  else
    PU.pp_delimited_sequence (lbracket ^^ space) (space ^^ rbracket) semi items

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

let build_inductive_type identifier typ constructor_generator =
  let constructors =
    let result = ref []
    in
    let generate_case
          ?(parameters : document = empty)
          ?(typ        : document = empty)
          (identifier  : document) =
      result := (identifier, parameters, typ) :: !result
    in
    constructor_generator generate_case;
    List.rev !result
  in
  let first_line =
    separate space (
        Util.build_list (fun { add; _ } ->
            add (string "Inductive");
            add identifier;
            if requirement typ > 0
            then
              (
                add colon;
                add typ
              );
            add (string ":=")
          )
      )
  in
  let constructor_lines =
    let pairs =
      List.map (fun (id, params, typ) ->
          separate space (
              Util.build_list (fun { add; _ } ->
                  add id;
                  if requirement params > 0
                  then add params
                )
            ),
          typ
        )
        constructors
    in
    let longest_left_part =
      if List.is_empty pairs
      then 0
      else
        Util.maximum (
            List.map (fun (left, _) -> requirement left) pairs
          )
    in
    let make_line (left, right) =
      separate space (
          Util.build_list (fun { add; _ } ->
              add (string "|");
              add (PU.pad_right longest_left_part left);
              if requirement right > 0
              then (
                add colon;
                add right
              )
            )
        )
    in
    List.map make_line pairs
  in
  let lines =
    Util.build_list (fun { add; addall } ->
        add first_line;
        addall constructor_lines
      )
  in
  separate hardline lines ^^ hardline ^^ eol

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
    separate space [
        string "match";
        expression;
        string "with"
      ]
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
    Util.build_list (fun { add; addall } ->
        add match_line;
        addall case_lines;
        add final_line
      )
  in
  separate hardline lines

let match_pair matched_expressions cases =
  let left_patterns = List.map (Util.compose fst fst) cases
  in
  let left_patterns_max_width = Util.maximum (List.map PPrint.requirement left_patterns)
  in
  let aligned_cases =
    List.map (fun ((left, right), expression) ->
        (
          concat [
              PU.pad_right left_patterns_max_width left;
              comma;
              space;
              right
            ],
          expression
      ))
      cases
  in
  let matched_expression =
    let left, right = matched_expressions
    in
    concat [
        left;
        comma;
        space;
        right
      ]
  in
  match' matched_expression aligned_cases

let integer i =
  let pp_i = string (Big_int.to_string i ^ "%Z") in
  if i < Z.zero then parens pp_i else pp_i

let require_imports src names =
  let first = string src ^^ space ^^ string "Require Import"
  and rest = List.map string names
  in
  PU.pp_hanging_list ~adaptive:false (string "From") (first :: rest) ^^ eol

let imports names =
  PU.pp_hanging_list ~adaptive:false (string "Import") (List.map string names) ^^ eol

let open_scopes scopes =
  let open_scope scope =
    concat [
        string "Local Open Scope";
        space;
        string scope;
        eol
      ]
  in
  separate hardline (List.map open_scope scopes)

let record_value fields =
  let ldelim = string "{| "
  and rdelim = string " |}"
  and items =
    let item_of_field (field_name, field_value) =
      group (
        concat [
          field_name ^^ space ^^ string ":=";
          break 1;
          align field_value
        ]
      )
    in
    List.map item_of_field fields
  in
  PU.pp_delimited_sequence ldelim rdelim semi items

let include_original_sail_code = ref false

let annotate_with_original_definition original translation =
  if
    !include_original_sail_code
  then
    concat [
        comment (Sail_util.pp_sail_definition original);
      hardline;
      translation
    ]
  else
    translation
