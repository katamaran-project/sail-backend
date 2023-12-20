open PPrint
open Auxlib
open Monad

module Big_int = Nat_big_num


let eol = dot

let left_comment_delimiter = string "(*"
let right_comment_delimiter = string "*)"

let comment comment =
  let str = Util.string_of_document comment
  in
  if
    count_chars str '\n' == 1 && String.ends_with ~suffix:"\n" str
  then
    concat [
        left_comment_delimiter;
        space;
        string (strip str);
        space;
        right_comment_delimiter
      ]
  else
    concat [
        left_comment_delimiter;
        hardline;
        Util.indent' comment;
        hardline;
        right_comment_delimiter
      ]

let original_sail_code source =
  let str = Util.string_of_document source
  in
  if
    count_chars str '\n' == 1 && String.ends_with ~suffix:"\n" str
  then
    concat [
        left_comment_delimiter;
        space;
        string (strip str);
        space;
        right_comment_delimiter
      ]
  else
    concat [
        left_comment_delimiter;
        twice hardline;
        Util.indent' source;
        hardline;
        right_comment_delimiter
      ]

let original_sail_codes sources =
  let combined_sources =
    separate hardline sources
  in
  let str =
    Util.string_of_document combined_sources
  in
  if
    count_chars str '\n' == 1 && String.ends_with ~suffix:"\n" str
  then
    concat [
        left_comment_delimiter;
        space;
        string (strip str);
        space;
        right_comment_delimiter
      ]
  else
    concat [
        left_comment_delimiter;
        twice hardline;
        Util.indent' combined_sources;
        hardline;
        right_comment_delimiter
      ]

let list items =
  if
    List.is_empty items
  then
    lbracket ^^ rbracket
  else
    Util.pp_delimited_sequence (lbracket ^^ space) (space ^^ rbracket) semi items

let product v1 v2 =
  soft_surround 1 0 lparen (v1 ^^ comma ^^ break 1 ^^ v2) rparen

let section identifier contents =
  let open PPrint
  in
  let first_line = string "Section" ^^ space ^^ Sail.pp_identifier identifier ^^ eol
  and last_line  = string "End" ^^ space ^^ Sail.pp_identifier identifier ^^ eol
  in
  Util.pp_indented_enclosed_lines first_line contents last_line


type module_flag =
  | Import
  | Export
  | NoFlag

let line contents =
  contents ^^ eol

let module' ?(flag = NoFlag) ?(includes = []) identifier contents =
  let first_line =
    line @@ separate space @@ build_list (fun { add; addall } ->
        add @@ string "Module";
        begin
          match flag with
          | Import -> add @@ string "Import"
          | Export -> add @@ string "Export"
          | NoFlag -> ()
        end;
        add @@ string identifier;
        if not (List.is_empty includes)
        then begin
          add @@ string "<:";
          addall @@ List.map string includes
        end;
      )
  and last_line = line @@ separate space [ string "End"; string identifier ]
  in
  Util.pp_indented_enclosed_lines first_line contents last_line
    

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
        build_list (fun { add; _ } ->
            add @@string "Inductive";
            add identifier;
            if requirement typ > 0
            then
              (
                add colon;
                add typ
              );
            add @@ string ":="
          )
      )
  in
  let constructor_lines =
    let pairs =
      List.map (fun (id, params, typ) ->
          separate space (
              build_list (fun { add; _ } ->
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
        maximum (
            List.map (fun (left, _) -> requirement left) pairs
          )
    in
    let make_line (left, right) =
      separate space (
          build_list (fun { add; _ } ->
              add @@ string "|";
              add @@ Util.pad_right longest_left_part left;
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
    build_list (fun { add; addall } ->
        add first_line;
        addall constructor_lines
      )
  in
  separate hardline lines ^^ hardline ^^ eol

let definition
      ~(identifier  : document)
      ~(parameters  : document list)
      ~(result_type : document option)
      ~(body        : document)        : document =
  group (concat (build_list (fun { add; _ } ->
                     add @@ string "Definition";
                     add space;
                     add identifier;
                     begin
                       if not (List.is_empty parameters)
                       then
                         add space;
                         add @@ align (separate space parameters)
                      end;
                      begin
                        match result_type with
                        | None    -> ()
                        | Some rt ->
                           add space;
                           add colon;
                           add space;
                           add rt
                      end;
                      add space;
                      add @@ string ":=";
                      add @@ break 1;
                      add @@ ifflat body (Util.indent' body)
                    )
    ) ^^ eol)
  
  
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
          Util.pad_right longest_pattern_width pattern;
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
    build_list (fun { add; addall } ->
        add    match_line;
        addall case_lines;
        add    final_line
      )
  in
  separate hardline lines

let match_pair matched_expressions cases =
  let left_patterns = List.map (compose fst fst) cases
  in
  let left_patterns_max_width = maximum (List.map PPrint.requirement left_patterns)
  in
  let aligned_cases =
    List.map (fun ((left, right), expression) ->
        (
          concat [
              Util.pad_right left_patterns_max_width left;
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
  line @@ Util.pp_hanging_list ~adaptive:false (string "From") (first :: rest)

let imports names =
  line @@ Util.pp_hanging_list ~adaptive:false (string "Import") (List.map string names)

let open_scopes scopes =
  let open_scope scope =
    line @@ concat [
                string "Local Open Scope";
                space;
                string scope;
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
  Util.pp_delimited_sequence ldelim rdelim semi items

let annotate_with_original_definition original translation =
  if
    Configuration.(get include_original_code)
  then
    concat [
        original_sail_code (Sail.pp_sail_definition original);
        hardline;
        translation
    ]
  else
    translation

let annotate_with_original_definitions originals translation =
  if
    Configuration.(get include_original_code)
  then
    concat @@
        build_list (fun { add; _ } ->
            add @@ original_sail_codes (List.map Sail.pp_sail_definition originals);
            add hardline;
            add translation
          )
  else
    translation

let annotate f =
  let (state, result) = run f
  in
  let annotations = MetadataMap.bindings state.metadata
  in
  let pp_annotations =
    let pp_annotation index doc =
      PPrint.(string (string_of_int index) ^^ string " : " ^^ align doc)
    in
    List.map (Auxlib.uncurry pp_annotation) annotations
  in
  PPrint.(separate hardline
            (Auxlib.build_list (fun { add; _ } ->
                 if not (List.is_empty annotations)
                 then add @@ comment (separate hardline pp_annotations);
                 add result)))

let mbuild_inductive_type identifier ?(parameters = []) typ constructor_generator =
  let* constructors =
    let result = ref []
    in
    let generate_case
          ?(parameters : document = empty)
          ?(typ        : document = empty)
          (identifier  : document) =
      result := (identifier, parameters, typ) :: !result;
      generate ()
    in
    let* _ = constructor_generator generate_case in
    generate @@ List.rev !result
  in
  let first_line =
    let parameters' =
      List.map
        (
          fun (identifier, typ) ->
            parens @@ separate space [ identifier; colon; typ ]
        )
        parameters
    in
    separate space (
        build_list (fun { add; addall } ->
            add @@ string "Inductive";
            add identifier;
            addall parameters';
            if requirement typ > 0
            then
              (
                add colon;
                add typ
              );
            add @@ string ":="
          )
      )
  in
  let constructor_lines =
    let pairs =
      List.map (fun (id, params, typ) ->
          separate space (
              build_list (fun { add; _ } ->
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
        maximum (
            List.map (fun (left, _) -> requirement left) pairs
          )
    in
    let make_line (left, right) =
      separate space (
          build_list (fun { add; _ } ->
              add @@ string "|";
              add @@ Util.pad_right longest_left_part left;
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
    build_list (fun { add; addall } ->
        add first_line;
        addall constructor_lines
      )
  in
  generate @@ separate hardline lines ^^ hardline ^^ eol
