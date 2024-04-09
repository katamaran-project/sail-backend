open Base
open Auxlib
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext

module Big_int = Nat_big_num


let eol = PP.dot

let left_comment_delimiter = PP.string "(*"
let right_comment_delimiter = PP.string "*)"


let ends_on_newline string =
  String.is_suffix string ~suffix:"\n"

let count_newlines string =
  String.count string ~f:(Char.equal '\n')

let is_single_line string =
  let newline_count = count_newlines string
  in
  newline_count = 0 || (newline_count = 1 && ends_on_newline string)

let inline_comment comment =
  PP.(separate space @@ [
      left_comment_delimiter;
      comment;
      right_comment_delimiter
    ])

let comment comment =
  let str = PP.string_of_document comment
  in
  if
    is_single_line str
  then
    PP.(
      concat [
        left_comment_delimiter;
        space;
        string (String.rstrip str);
        space;
        right_comment_delimiter
      ]
    )
  else
    PP.(
      concat [
        left_comment_delimiter;
        hardline;
        indent' comment;
        hardline;
        right_comment_delimiter
      ]
    )

let original_sail_code source =
  let str = PP.string_of_document source
  in
  if
    is_single_line str
  then
    PP.(
      concat [
        left_comment_delimiter;
        space;
        string (String.rstrip str);
        space;
        right_comment_delimiter
      ]
    )
  else
    PP.(
      concat [
        left_comment_delimiter;
        twice hardline;
        indent' source;
        hardline;
        right_comment_delimiter
      ]
    )

let original_sail_codes sources =
  let combined_sources =
    PP.(separate hardline sources)
  in
  let str =
    PP.string_of_document combined_sources
  in
  if
    is_single_line str
  then
    PP.(
      concat [
        left_comment_delimiter;
        space;
        string (String.rstrip str);
        space;
        right_comment_delimiter
      ]
    )
  else
    PP.(
      concat [
        left_comment_delimiter;
        twice hardline;
        indent' combined_sources;
        hardline;
        right_comment_delimiter
      ]
    )

let list items =
  if
    List.is_empty items
  then
    PP.(lbracket ^^ rbracket)
  else
    PP.(delimited_sequence (lbracket ^^ space) (space ^^ rbracket) semi items)

let product v1 v2 =
  PP.(soft_surround 1 0 lparen (v1 ^^ comma ^^ break 1 ^^ v2) rparen)

let section identifier contents =
  let first_line = PP.(string "Section" ^^ space ^^ pp_identifier identifier ^^ eol)
  and last_line  = PP.(string "End" ^^ space ^^ pp_identifier identifier ^^ eol)
  in
  PP.indented_enclosed_lines first_line contents last_line


type module_flag =
  | Import
  | Export
  | NoFlag

let line contents =
  PP.(contents ^^ eol)

let module' ?(flag = NoFlag) ?(includes = []) identifier contents =
  let first_line =
    PP.(
      line @@ separate space @@ build_list (fun { add; addall; _ } ->
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
            addall @@ List.map ~f:string includes
          end;
        )
    )
  and last_line = PP.(line @@ separate space [ string "End"; string identifier ])
  in
  PP.indented_enclosed_lines first_line contents last_line


let definition
      ~(identifier  : PP.document       )
      ~(parameters  : PP.document list  )
      ~(result_type : PP.document option)
      ~(body        : PP.document       ) : PP.document =
  PP.(
    group begin
      concat begin
        build_list begin fun { add; _ } ->
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
          add @@ ifflat body (indent' body)
        end
      end ^^ eol
    end
  )


let match' expression cases =
  let match_line =
    PP.(
      separate space [
        string "match";
        expression;
        string "with"
      ]
    )
  in
  let case_lines =
    let longest_pattern_width =
      let widths = List.map ~f:(fun pattern -> PP.requirement pattern) (List.map ~f:fst cases)
      in
      maximum (0 :: widths)
    in
    let generate_case (pattern, expression) =
      PP.(
        separate space [
          bar;
          pad_right longest_pattern_width pattern;
          string "=>";
          expression
        ]
      )
    in
    List.map ~f:generate_case cases
  in
  let final_line =
    PP.(string "end")
  in
  let result_lines =
    build_list (fun { add; addall; _ } ->
        add    match_line;
        addall case_lines;
        add    final_line
      )
  in
  PP.(separate hardline result_lines)

let match_pair matched_expressions cases =
  let left_patterns = List.map ~f:(Fn.compose fst fst) cases
  in
  let left_patterns_max_width = maximum (List.map ~f:PPrint.requirement left_patterns)
  in
  let aligned_cases =
    List.map cases ~f:(fun ((left, right), expression) ->
        PP.(
          concat [
            pad_right left_patterns_max_width left;
            comma;
            space;
            right
          ],
          expression
        )
      )
  in
  let matched_expression =
    let left, right = matched_expressions
    in
    PP.(
      concat [
        left;
        comma;
        space;
        right
      ]
    )
  in
  match' matched_expression aligned_cases

let integer i =
  let pp_i = PP.(string (Big_int.to_string i ^ "%Z"))
  in
  if Big_int.less i Z.zero
  then PP.parens pp_i
  else pp_i

let require_imports src names =
  let first = PP.(string src ^^ space ^^ string "Require Import")
  and rest = List.map ~f:PP.string names
  in
  PP.(line @@ hanging_list ~adaptive:false (string "From") (first :: rest))

let imports names =
  PP.(line @@ hanging_list ~adaptive:false (string "Import") (List.map ~f:string names))

let open_scopes scopes =
  let open_scope scope =
    PP.(
      line @@ concat [
        string "Local Open Scope";
        space;
        string scope;
      ]
    )
  in
  PP.(separate hardline (List.map ~f:open_scope scopes))

let record_value fields =
  let ldelim = PP.string "{| "
  and rdelim = PP.string " |}"
  and items =
    let item_of_field (field_name, field_value) =
      PP.(
        group begin
          concat [
            field_name ^^ space ^^ string ":=";
            break 1;
            align field_value
          ]
        end
      )
    in
    List.map ~f:item_of_field fields
  in
  PP.(delimited_sequence ldelim rdelim semi items)

let annotate_with_original_definition original translation =
  if
    Configuration.(get include_original_code)
  then
    PP.(
      concat [
        original_sail_code (Sail.pp_sail_definition original);
        hardline;
        translation
      ]
    )
  else
    translation

let annotate_with_original_definitions originals translation =
  if
    Configuration.(get include_original_code)
  then
    PP.(
      concat begin
        build_list begin fun { add; _ } ->
          add @@ original_sail_codes (List.map ~f:Sail.pp_sail_definition originals);
          add hardline;
          add translation
        end
      end
    )
  else
    translation

let annotate f =
  let (document, annotations) = AC.collect_annotations f
  in
  let pp_annotations =
    let pp_annotation index doc =
      PPrint.(string (Int.to_string index) ^^ string " : " ^^ align doc)
    in
    List.mapi ~f:pp_annotation annotations
  in
  PPrint.(separate hardline
            (Auxlib.build_list (fun { add; _ } ->
                 if not (List.is_empty annotations)
                 then add @@ comment (separate hardline pp_annotations);
                 add document)))

let mbuild_inductive_type identifier ?(parameters = []) typ constructor_generator =
  let* constructors =
    let result = ref []
    in
    let generate_case
          ?(parameters  : PP.document = PP.empty)
          ?(typ         : PP.document = PP.empty)
           (identifier  : PP.document           ) =
      result := (identifier, parameters, typ) :: !result;
      AC.return ()
    in
    let* _ = constructor_generator generate_case in
    AC.return @@ List.rev !result
  in
  let first_line =
    let parameters' =
      List.map parameters ~f:(
          fun (identifier, typ) ->
            PP.(parens @@ separate space [ identifier; colon; typ ])
        )
    in
    PP.(
      separate space (
        build_list (fun { add; addall; _ } ->
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
    )
  in
  let constructor_lines =
    let pairs =
      List.map constructors ~f:(fun (id, params, typ) ->
          PP.(
            separate space (
              build_list (fun { add; _ } ->
                  add id;
                  if requirement params > 0
                  then add params
                )
            ),
            typ
          )
        )
    in
    let longest_left_part =
      if List.is_empty pairs
      then 0
      else
        maximum (
            List.map ~f:(fun (left, _) -> PP.requirement left) pairs
          )
    in
    let make_line (left, right) =
      PP.(
        (twice space) ^^ separate space (
          build_list (fun { add; _ } ->
              add @@ string "|";
              add @@ pad_right longest_left_part left;
              if requirement right > 0
              then (
                add colon;
                add right
              )
            )
        )
      )
    in
    List.map ~f:make_line pairs
  in
  let result_lines =
    build_list (fun { add; addall; _ } ->
        add first_line;
        addall constructor_lines
      )
  in
  AC.return @@ PP.(separate hardline result_lines ^^ hardline ^^ eol)

let finite_instance
      ~(identifier : PP.document     )
      ~(type_name  : PP.document     )
      ~(values     : PP.document list)
  =
  let enum_values =
    PP.(group (separate (semi ^^ break 1) values))
  in
  let declaration =
  PP.(
    separate hardline [
      separate space [
        string "#[export,program]";
        string "Instance";
        identifier ^^ string "_finite";
        colon;
        string "Finite";
        type_name;
        string ":=";
      ];
      twice space ^^ PP.separate space [
        string "{|";
        string "enum";
        string ":=";
        string "[";
        align @@ enum_values;
        string "]";
        string "|}"
      ]
    ]
  )
  in
  line declaration

(* fields as (identifier, type) pairs *)
let record
      ~(identifier  : PP.document                     )
      ~(type_name   : PP.document                     )
      ~(constructor : PP.document                     )
      ~(fields      : (PP.document * PP.document) list) : PP.document
  =
  let first_line =
    PP.(
      separate space [
        string "Record";
        identifier;
        colon;
        type_name;
        string ":="
      ]
    )
  in
  let fields' =
    let longest_field_length =
      Auxlib.maximum @@ List.map ~f:(Fn.compose PP.requirement fst) fields
    in
    List.map fields ~f:(
      fun (id, t) -> 
        PP.(separate space [ PP.pad_right longest_field_length id; colon; t ] ^^ semi)
    )
  in
  let body =
    PP.(separate hardline [
            lbrace;
            twice space ^^ align (separate hardline fields');
            rbrace
          ]
    )
  in
  line @@ PP.(first_line ^^ hardline ^^ indent' (constructor ^^ hardline ^^ indent' body))


let local_obligation_tactic (identifier : Ast.identifier) : PP.document =
  let lines_of_code = [
      PP.string "Local Obligation Tactic :=";
      PP.(twice space ^^ pp_identifier identifier)
    ]
  in
  line PP.(separate hardline lines_of_code)


let derive
      (class_identifier : Ast.identifier)
      (type_identifier  : Ast.identifier) : PP.document =
  let str =
    Printf.sprintf
      "Derive %s for %s."
      (Id.string_of class_identifier)
      (Id.string_of type_identifier)
  in
  PP.string str


let derive_eqdec_for (identifier : Ast.identifier) =
  derive (Id.mk "EqDec") identifier
