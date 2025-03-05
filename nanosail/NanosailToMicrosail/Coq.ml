open ExtBase

module Big_int = Nat_big_num


(* Operators *)
module Operator = struct
  let addition                               = PP.plus
  let multiplication                         = PP.star
  let subtraction                            = PP.minus
  let division                               = PP.slash
  let modulo                                 = PP.percent
  let conjunction                            = PP.(repeat horizontal 2 ampersand)
  let disjunction                            = PP.(repeat horizontal 2 bar)
  let equality                               = PP.equals
  let inequality                             = PP.(horizontal [bang; equals])
  let less_than                              = PP.langle
  let greater_than                           = PP.rangle
  let less_than_or_equal_to                  = PP.(horizontal [langle; equals])
  let greater_than_or_equals_to              = PP.(horizontal [rangle; equals])
end

let arrow = PP.string "->"

(* End of line symbol *)
let eol = PP.dot

(* Comment delimiters *)
let comment_left_delimiter  = PP.string "(*"
let comment_right_delimiter = PP.string "*)"
let comment_delimiters      = (comment_left_delimiter, comment_right_delimiter)

let list_left_delimiter    = PP.lbracket
let list_right_delimiter   = PP.rbracket
let list_item_separator    = PP.semi
let list_delimiters        = (list_left_delimiter, list_right_delimiter)

let record_left_delimiter  = PP.string "{|"
let record_right_delimiter = PP.string "|}"
let record_field_separator = PP.semi
let record_delimiters      = (record_left_delimiter, record_right_delimiter)

let tuple_left_delimiter   = PP.lparen
let tuple_right_delimiter  = PP.rparen
let tuple_delimiters       = (tuple_left_delimiter, tuple_right_delimiter)


let pp_sentence contents =
  PP.(horizontal [contents; eol])


let pp_inline_comment (comment : PP.document) : PP.document =
  PP.surround comment_delimiters comment


let pp_multiline_comment (comment : PP.document) : PP.document =
  PP.surround ~layout:PP.vertical comment_delimiters @@ PP.indent comment


let pp_comment (comment : PP.document) : PP.document =
  if PP.is_single_line comment
  then pp_inline_comment comment
  else pp_multiline_comment comment


let add_comments
    ~(comments : PP.document)
    ~(document : PP.document) : PP.document
  =
  PP.annotate [%here] @@ PP.vertical [ pp_comment comments; document ]


let pp_hanging_application
      (func      : PP.document     )
      (arguments : PP.document list) : PP.document
  =
  PP.annotate [%here] @@ PP.(hanging @@ horizontal [ func; space ] :: arguments)


let pp_list_using_notation (items : PP.document list) : PP.document =
  if
    List.is_empty items
  then
    PP.annotate [%here] @@ PP.(horizontal [lbracket; rbracket])
  else
    PP.annotate [%here] @@ PP.(delimited_list
          ~delimiters:brackets
          ~items
          ~separator:semi)


let pp_list_using_cons (items : PP.document list) : PP.document =
  let rec pp items =
    match items with
    | []         -> PP.string "nil"
    | head::tail -> PP.(surround parens @@ pp_hanging_application (string "cons") [ PP.(surround parens) head; pp tail])
  in
  PP.annotate [%here] @@ pp items


let pp_list
      ?(use_notation : bool = true     )
      (items         : PP.document list) : PP.document
  =
  if use_notation
  then pp_list_using_notation items
  else pp_list_using_cons items


(*
  (v1, v2)
*)
let pp_product
    (v1 : PP.document)
    (v2 : PP.document) : PP.document
  =
  PP.annotate [%here] @@ PP.(surround tuple_delimiters @@ separate_horizontally ~separator:comma [v1; v2])


let pp_section identifier contents =
  let first_line = PP.annotate [%here] @@ pp_sentence @@ PP.(horizontal [ string "Section"; space; Identifier.pp identifier ])
  and last_line  = PP.annotate [%here] @@ pp_sentence @@ PP.(horizontal [ string "End"; space; Identifier.pp identifier ])
  in
  let delimiters = (first_line, last_line)
  in
  PP.annotate [%here] begin
      PP.(surround
            ~layout:vertical
            delimiters
            (indent contents))
    end


type module_flag =
  | Import
  | Export
  | NoFlag


let pp_module ?(flag = NoFlag) ?(includes = []) identifier contents =
  let first_line =
    PP.annotate [%here] @@ PP.(
      pp_sentence @@ separate_horizontally ~separator:space @@ List.build_list (fun { add; addall; _ } ->
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
  and last_line =
    PP.annotate [%here] @@ PP.(pp_sentence @@ separate_horizontally ~separator:space [ string "End"; string identifier ])
  in
  let delimiters =
    (first_line, last_line)
  in
  PP.annotate [%here] @@ PP.(surround ~layout:vertical delimiters (indent contents))


let pp_definition
      ~(identifier          : PP.document                                   )
      ?(implicit_parameters : (PP.document * PP.document option) list = []  )
      ?(parameters          : (PP.document * PP.document option) list = []  )
      ?(result_type         : PP.document option                      = None)
       (body                : PP.document                                   ) : PP.document
  =
  let pp_parameters =
    let pp_implicit_parameters =
      let pp_implicit_parameter (var, typ) =
        match typ with
        | Some typ -> PP.(annotate [%here] @@ surround braces @@ separate_horizontally ~separator:space [ var; colon; typ ])
        | None     -> PP.(annotate [%here] @@ surround braces @@ var)
      in
      List.map ~f:pp_implicit_parameter implicit_parameters
    and pp_explicit_parameters =
      let pp_parameter (var, typ) =
        match typ with
        | Some typ -> PP.(annotate [%here] @@ surround parens @@ horizontal [ var; string " : "; typ ])
        | None     -> PP.annotate [%here] @@ var
      in
      List.map ~f:pp_parameter parameters
    in
    let pp_explicit_and_implicit_parameters =
      List.concat [ pp_implicit_parameters; pp_explicit_parameters ]
    in
    if List.is_empty pp_explicit_and_implicit_parameters
    then None
    else Some (PP.vertical pp_explicit_and_implicit_parameters)
  in
  let pp_return_type =
    match result_type with
    | None    -> None
    | Some rt -> Some PP.(annotate [%here] @@ separate_horizontally ~separator:PP.space [ PP.colon; rt ])
  in
  let definition_line =
    PP.separate_horizontally ~separator:PP.space @@ List.build_list begin fun { add; addopt; _ } ->
      add    @@ PP.(annotate [%here] @@ string "Definition");
      add    @@ PP.annotate [%here] @@ identifier;
      addopt @@ Option.(pp_parameters >>| PP.annotate [%here]);
      addopt @@ Option.(pp_return_type >>| PP.annotate [%here]);
      add    @@ PP.(annotate [%here] @@ string ":=");
    end
  in
  PP.(annotate [%here] @@ pp_sentence @@ vertical [ definition_line; indent body ])


let pp_match
    ?(scope     : PP.document option              = None)
    (expression : PP.document                           )
    (cases      : (PP.document * PP.document) list      ) : PP.document
  =
  let match_line =
    PP.(
      separate_horizontally ~separator:space [
        string "match";
        expression;
        string "with"
      ]
    )
  in
  let case_lines =
    let longest_pattern_width =
      let widths =
        List.map ~f:(Fn.compose fst PP.measure) (List.map ~f:fst cases)
      in
      Option.value ~default:0 @@ List.max_elt ~compare:Int.compare widths
    in
    let generate_case (pattern, expression) =
      PP.annotate [%here] @@ PP.(
        separate_horizontally ~separator:space [
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
    match scope with
    | Some scope -> PP.(horizontal [ string "end"; percent; scope ])
    | None       -> PP.string "end"
  in
  let result_lines =
    List.build_list (fun { add; addall; _ } ->
        add    match_line;
        addall case_lines;
        add    final_line
      )
  in
  PP.annotate [%here] @@ PP.(vertical result_lines)


let pp_match_pair matched_expressions cases =
  let left_patterns =
    List.map ~f:(Fn.compose fst fst) cases
  in
  let left_patterns_max_width =
    Option.value
      ~default:0
      (List.max_elt ~compare:Int.compare @@ List.map ~f:(Fn.compose fst PP.measure) left_patterns)
  in
  let aligned_cases =
    List.map cases ~f:(fun ((left, right), expression) ->
        PP.(
          annotate [%here] @@ separate_horizontally ~separator:(horizontal [comma; space]) [
            pad_right left_patterns_max_width left;
            right
          ],
          PP.annotate [%here] @@ expression
        )
      )
  in
  let matched_expression =
    let left, right = matched_expressions
    in
    PP.(
      annotate [%here] @@ separate_horizontally ~separator:(horizontal [comma; space]) [
        left;
        right
      ]
    )
  in
  PP.annotate [%here] @@ pp_match matched_expression aligned_cases


(*
   <value>%Z

   Put inside parentheses if necessary (i.e., if <value> is negative)
*)
let pp_integer value =
  let pp_i =
    PP.annotate [%here] begin
      PP.string @@ Big_int.to_string value ^ "%Z"
    end
  in
  if
    Big_int.less value Z.zero
  then
    PP.annotate [%here] @@ PP.(surround parens) pp_i
  else
    PP.annotate [%here] @@ pp_i


(*
   "<value>"
*)
let pp_string (value : string) =
  PP.annotate [%here] begin
    PP.(surround dquotes @@ string value)
  end


let pp_require
    ?(from     : string option = None )
    ?(import   : bool          = false)
    (libraries : string list          )
  =
  let from_words =
    match from with
    | Some s ->
       [
         PP.annotate [%here] @@ PP.string "From";
         PP.annotate [%here] @@ PP.string s;
       ]
    | None   -> []
  and require_words =
    [
      PP.annotate [%here] @@ PP.string "Require"
    ]
  and import_words =
    if import
    then [ PP.annotate [%here] @@ PP.string "Import" ]
    else []
  in
  let words     = List.concat [ from_words; require_words; import_words ]
  and libraries = List.map ~f:PP.string libraries
  in
  PP.annotate [%here] @@ pp_sentence @@ PP.hanging @@ (PP.horizontal [ PP.separate_horizontally ~separator:PP.space words; PP.space ]) :: libraries


let pp_imports names =
  PP.annotate [%here] @@ PP.(pp_sentence @@ hanging (string "Import " :: List.map ~f:string names))


let pp_open_scopes scopes =
  let open_scope scope =
    PP.(
      annotate [%here] @@ pp_sentence @@ separate_horizontally ~separator:space [
        string "Local Open Scope";
        string scope;
      ]
    )
  in
  PP.(vertical @@ List.map ~f:open_scope scopes)


let pp_record_value (fields : (PP.document * PP.document) list) : PP.document =
  let items =
    let item_of_field (field_name, field_value) =
      PP.annotate [%here] @@ PP.separate_horizontally ~separator:PP.space [
        field_name;
        PP.string ":=";
        field_value
      ]
    in
    List.map ~f:item_of_field fields
  in
  let separator =
    PP.annotate [%here] @@ PP.(horizontal [ record_field_separator; space ])
  in
  PP.annotate [%here] @@ PP.(delimited_list ~delimiters:record_delimiters ~items ~separator)


(*

  #[export,program] Instance <identifier>_finite : Finite <type_name> :=
    {| enum := [ <values> ] |}.

 *)
let pp_finite_instance
      ~(identifier : PP.document     )
      ~(type_name  : PP.document     )
      ~(values     : PP.document list) : PP.document
  =
  let enum_values =
    PP.annotate [%here] begin
        PP.delimited_list
          ~delimiters:list_delimiters
          ~separator:list_item_separator
          ~items:values
      end
  in
  let declaration =
    PP.annotate [%here] begin
        PP.(
        vertical [
            separate_horizontally ~separator:space [
                string "#[export,program]";
                string "Instance";
                horizontal [identifier; string "_finite"];
                colon;
                string "Finite";
                type_name;
                string ":=";
              ];
            PP.indent begin
                PP.horizontal [
                    string "{|";
                    string "enum";
                    string ":=";
                    enum_values;
                    string "|}"
                  ]
              end
          ]
        )
      end
  in
  PP.annotate [%here] @@ pp_sentence declaration


(* fields as (identifier, type) pairs *)
let pp_record
      ~(identifier  : PP.document                     )
      ~(type_name   : PP.document                     )
      ~(constructor : PP.document                     )
      ~(fields      : (PP.document * PP.document) list) : PP.document
  =
  let first_line =
    PP.annotate [%here] begin
        PP.(
        separate_horizontally ~separator:space [
            string "Record";
            identifier;
            colon;
            type_name;
            string ":="
          ]
        )
      end
  in
  let pp_fields =
    let longest_field_length =
      Option.value
        ~default:0
        (List.max_elt ~compare:Int.compare @@ List.map ~f:(fun (field, _) -> fst @@ PP.measure field) fields)
    in
    List.map fields ~f:(
        fun (id, t) -> begin
            PP.annotate [%here] begin
                PP.(horizontal [
                        separate_horizontally ~separator:space [ PP.pad_right longest_field_length id; colon; t ];
                        semi
                ])
              end
          end
      )
  in
  let body =
    PP.annotate [%here] @@ PP.(surround ~layout:vertical braces (indent @@ vertical pp_fields))
  in
  PP.annotate [%here] begin
      pp_sentence begin
          PP.vertical [
              first_line;
              PP.indent begin
                  PP.vertical [
                      constructor;
                      PP.indent body
                    ]
                end
            ]
        end
    end


let pp_local_obligation_tactic (identifier : Ast.Identifier.t) : PP.document =
  let lines_of_code = [
      PP.annotate [%here] @@ PP.string "Local Obligation Tactic :=";
      PP.annotate [%here] @@ PP.(horizontal [ space; space; Identifier.pp identifier ])
    ]
  in
  PP.annotate [%here] @@ pp_sentence @@ PP.vertical lines_of_code


let pp_derive
      (class_identifier : Ast.Identifier.t)
      (type_identifier  : Ast.Identifier.t) : PP.document =
  let str =
    Printf.sprintf
      "Derive %s for %s."
      (Ast.Identifier.to_string class_identifier)
      (Ast.Identifier.to_string type_identifier)
  in
  PP.annotate [%here] @@ PP.string str


let pp_derive_eqdec_for (identifier : Ast.Identifier.t) =
  PP.annotate [%here] @@ pp_derive (Ast.Identifier.mk "EqDec") identifier


let pp_derive_no_confusion_for (identifier : Ast.Identifier.t) =
  PP.annotate [%here] @@ pp_derive (Ast.Identifier.mk "NoConfusion") identifier


let pp_lambda parameter body =
  PP.annotate [%here] begin
      PP.(
      separate_horizontally ~separator:space [
          string "fun";
          parameter;
          string "=>";
          body
        ]
      )
    end


let pp_application f args =
  PP.annotate [%here] @@ PP.(separate_horizontally ~separator:space @@ f :: args)


let pp_explicit_application f args =
  PP.annotate [%here] @@ PP.(separate_horizontally ~separator:space @@ horizontal [ PP.at; f ] :: args)


let pp_function_type parameter_types result_type =
  PP.annotate [%here] @@ PP.separate_horizontally ~separator:PP.space @@ List.build_list @@ fun { addall; add; _ } -> begin
    addall parameter_types;
    add arrow;
    add result_type
  end


let pp_canonical identifier =
  PP.annotate [%here] @@ pp_sentence @@ PP.(separate_horizontally ~separator:space [ PP.string "Canonical"; Identifier.pp identifier ])


let pp_include_module (name : PP.document) =
  PP.annotate [%here] @@ pp_sentence @@ PP.(separate_horizontally ~separator:space [ string "Include"; name ])


let pp_tuple_type ts =
  PP.annotate [%here] @@ PP.separate_horizontally ~separator:(PP.string " * ") ts


let pp_notation notation expression =
  PP.annotate [%here] begin
      pp_sentence begin
          PP.(
          separate_horizontally
            ~separator:PP.space
            [
              string "Notation";
              surround dquotes notation;
              string ":=";
              surround parens expression
            ]
          )
        end
    end


let pp_scope scope_name scoped_expression =
  PP.annotate [%here] begin
      PP.(
      horizontal [
          surround parens scoped_expression;
          string "%";
          scope_name
        ]
      )
    end


let pp_bool (value : bool) : PP.document =
  if value
  then PP.string "true"
  else PP.string "false"
