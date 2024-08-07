open Base

module Big_int = Nat_big_num


(* Operators *)
module Operator = struct
  let addition                  = PP.plus
  let multiplication            = PP.star
  let subtraction               = PP.minus
  let conjunction               = PP.(twice ampersand)
  let disjunction               = PP.(twice bar)
  let equality                  = PP.equals
  let inequality                = PP.(bang ^^ equals)
  let less_than                 = PP.langle
  let greater_than              = PP.rangle
  let less_than_or_equal_to     = PP.(langle ^^ equals)
  let greater_than_or_equals_to = PP.(rangle ^^ equals)
end

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


let pp_inline_comment (comment : PP.document) : PP.document =
  PP.enclose comment_delimiters comment


let pp_multiline_comment (comment : PP.document) : PP.document =
  PP.separate PP.hardline [
    comment_left_delimiter;
    PP.indent comment;
    comment_right_delimiter
  ]


let pp_comment (comment : PP.document) : PP.document =
  if PP.is_single_line comment
  then pp_inline_comment comment
  else pp_multiline_comment comment


let add_comments
    ~(comments : PP.document)
    ~(document : PP.document) : PP.document
  =
  PP.separate PP.hardline [ pp_comment comments; document ]


let pp_list (items : PP.document list) : PP.document =
  if
    List.is_empty items
  then
    PP.(lbracket ^^ rbracket)
  else
    PP.(delimited_sequence (lbracket ^^ space) (space ^^ rbracket) semi items)


let pp_product
    (v1 : PP.document)
    (v2 : PP.document) : PP.document
  =
  let open PP
  in
  horizontal ~separator:empty [
    tuple_left_delimiter;
    horizontal_or_vertical ~separator:comma [v1; v2];
    tuple_right_delimiter;
  ]


let pp_section identifier contents =
  let first_line = PP.(string "Section" ^^ space ^^ Identifier.pp identifier ^^ eol)
  and last_line  = PP.(string "End" ^^ space ^^ Identifier.pp identifier ^^ eol)
  in
  PP.indented_enclosed_lines first_line contents last_line


type module_flag =
  | Import
  | Export
  | NoFlag


let pp_sentence contents =
  PP.(contents ^^ eol)


let module' ?(flag = NoFlag) ?(includes = []) identifier contents =
  let first_line =
    PP.(
      pp_sentence @@ separate space @@ Auxlib.build_list (fun { add; addall; _ } ->
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
  and last_line = PP.(pp_sentence @@ separate space [ string "End"; string identifier ])
  in
  PP.indented_enclosed_lines first_line contents last_line


let definition
      ~(identifier          : PP.document                                   )
      ?(implicit_parameters : (PP.document * PP.document option) list = []  )
      ?(parameters          : (PP.document * PP.document option) list = []  )
      ?(result_type         : PP.document option                      = None)
       (body                : PP.document                                   ) : PP.document
  =
  let open PP (* todo remove *)
  in
  let pp_parameters =
    let pp_implicit_parameters =
      let pp_implicit_parameter (var, typ) =
        match typ with
        | Some typ -> braces @@ separate space [ var; colon; typ ]
        | None     -> braces @@ var
      in
      List.map ~f:pp_implicit_parameter implicit_parameters
    and pp_explicit_parameters =
      let pp_parameter (var, typ) =
        match typ with
        | Some typ -> parens @@ var ^^ string " : " ^^ typ
        | None     -> var
      in
      List.map ~f:pp_parameter parameters
    in
    let pp_explicit_and_implicit_parameters =
      List.concat [ pp_implicit_parameters; pp_explicit_parameters ]
    in
    if List.is_empty pp_explicit_and_implicit_parameters
    then None
    else Some (PP.horizontal_or_vertical pp_explicit_and_implicit_parameters)
  in
  let pp_return_type =
    match result_type with
    | None    -> None
    | Some rt -> Some (PP.(horizontal [ PP.colon; rt ]))
  in
  let definition_line =
    PP.build_horizontal @@ fun { add; addopt; _ } -> begin
      add    @@ PP.string "Definition";
      add    @@ identifier;
      addopt @@ pp_parameters;
      addopt @@ pp_return_type;
      add    @@ string ":=";
    end
  in
  pp_sentence @@ PP.horizontal_or_indent definition_line body


let pp_match
    ?(scope     : PP.document option              = None)
    (expression : PP.document                           )
    (cases      : (PP.document * PP.document) list      ) : PP.document
  =
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
      let widths = List.map ~f:(fun pattern -> PP.measure pattern) (List.map ~f:fst cases)
      in
      Auxlib.maximum (0 :: widths)
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
    match scope with
    | Some scope -> PP.(string "end" ^^ percent ^^ scope)
    | None       -> PP.string "end"
  in
  let result_lines =
    Auxlib.build_list (fun { add; addall; _ } ->
        add    match_line;
        addall case_lines;
        add    final_line
      )
  in
  PP.(separate hardline result_lines)


let match_pair matched_expressions cases =
  let left_patterns = List.map ~f:(Fn.compose fst fst) cases
  in
  let left_patterns_max_width =
    if List.is_empty left_patterns
    then 0
    else Auxlib.maximum (List.map ~f:PPrint.requirement left_patterns)
  in
  let aligned_cases =
    List.map cases ~f:(fun ((left, right), expression) ->
        PP.(
          horizontal ~separator:(comma ^^ space) [
            pad_right left_patterns_max_width left;
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
      horizontal ~separator:(comma ^^ space) [
        left;
        right
      ]
    )
  in
  pp_match matched_expression aligned_cases


let integer i =
  let pp_i = PP.(string (Big_int.to_string i ^ "%Z"))
  in
  if Big_int.less i Z.zero
  then PP.parens pp_i
  else pp_i


let require
    ?(from     : string option = None )
    ?(import   : bool          = false)
    (libraries : string list          )
  =
  let from_words =
    match from with
    | Some s -> [ PP.string "From"; PP.string s ]
    | None   -> []
  and require_words =
    [ PP.string "Require" ]
  and import_words =
    if import
    then [ PP.string "Import" ]
    else []
  in
  let words     = List.concat [ from_words; require_words; import_words ]
  and libraries = List.map ~f:PP.string libraries
  in
  pp_sentence @@ PP.hanging_list ~adaptive:false (PP.separate PP.space words) libraries


let imports names =
  PP.(pp_sentence @@ hanging_list ~adaptive:false (string "Import") (List.map ~f:string names))


let open_scopes scopes =
  let open_scope scope =
    PP.(
      pp_sentence @@ horizontal [
        string "Local Open Scope";
        string scope;
      ]
    )
  in
  PP.(separate hardline (List.map ~f:open_scope scopes))


let pp_record_value (fields : (PP.document * PP.document) list) : PP.document =
  let items =
    let item_of_field (field_name, field_value) =
      PP.separate PP.space [
        field_name;
        PP.string ":=";
        field_value
      ]
    in
    List.map ~f:item_of_field fields
  in
  PP.(delimited_list ~delimiters:record_delimiters ~items:items ~separator:PP.(record_field_separator ^^ space)) 


(*

  #[export,program] Instance <identifier>_finite : Finite <type_name> :=
    {| enum := [ <values> ] |}.

 *)
let finite_instance
      ~(identifier : PP.document     )
      ~(type_name  : PP.document     )
      ~(values     : PP.document list) : PP.document
  =
  let enum_values =
    PP.delimited_list
      ~delimiters:list_delimiters
      ~separator:list_item_separator
      ~items:values
  in
  let declaration =
  PP.(
    vertical [
      separate space [
        string "#[export,program]";
        string "Instance";
        identifier ^^ string "_finite";
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
          align @@ enum_values;
          string "|}"
        ]
      end
    ]
  )
  in
  pp_sentence declaration


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
      Auxlib.maximum @@ List.map ~f:(Fn.compose PP.measure fst) fields
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
  pp_sentence @@ PP.(first_line ^^ hardline ^^ indent (constructor ^^ hardline ^^ indent body))


let local_obligation_tactic (identifier : Ast.Identifier.t) : PP.document =
  let lines_of_code = [
      PP.string "Local Obligation Tactic :=";
      PP.(twice space ^^ Identifier.pp identifier)
    ]
  in
  pp_sentence PP.(separate hardline lines_of_code)


let derive
      (class_identifier : Ast.Identifier.t)
      (type_identifier  : Ast.Identifier.t) : PP.document =
  let str =
    Printf.sprintf
      "Derive %s for %s."
      (Ast.Identifier.string_of class_identifier)
      (Ast.Identifier.string_of type_identifier)
  in
  PP.string str


let derive_eqdec_for (identifier : Ast.Identifier.t) =
  derive (Ast.Identifier.mk "EqDec") identifier


let derive_no_confusion_for (identifier : Ast.Identifier.t) =
  derive (Ast.Identifier.mk "NoConfusion") identifier


type build_lines_context =
  {
    line       : PP.document      -> unit;
    lines      : PP.document list -> unit;
    comment    : PP.document      -> unit;
    empty_line : unit             -> unit;
  }


let build_lines (f : build_lines_context -> unit) : PP.document =
  PP.build_lines begin fun { line; lines; empty_line } ->
    let ctx = {
      line;
      lines;
      empty_line;
      comment = fun d -> line @@ pp_comment d;
    }
    in
    f ctx
  end


let lambda parameter body =
  PP.(separate space [ string "fun"; parameter; string "=>"; align body ])


let application f args =
  PP.(f ^^ space ^^ align (separate space args))


let arrow = PP.string "->"


let function_type parameter_types result_type =
  PP.separate PP.space @@ Auxlib.build_list @@ fun { addall; add; _ } -> begin
    addall parameter_types;
    add arrow;
    add result_type
  end


let canonical identifier =
  pp_sentence @@ PP.simple_app [ PP.string "Canonical"; Identifier.pp identifier ]


let include_module (name : PP.document) =
  pp_sentence @@ PP.simple_app [ PP.string "Include"; name ]


let generation_block
    (position : Lexing.position)
    (label    : PP.document    )
    (contents : PP.document    ) : PP.document
  =
  if
    Configuration.(get show_generation_blocks)
  then
    let position_string =
      let filename    = position.pos_fname
      and line_number = position.pos_lnum
      in
      Printf.sprintf "%s:%d" filename line_number
    in
    let entry_block =
      pp_inline_comment @@ PP.separate PP.space [
        PP.string "<<<<<";
        PP.string position_string;
        label
      ]
    and exit_block =
      pp_inline_comment @@ PP.separate PP.space [
        PP.string ">>>>>";
        PP.string position_string;
        label
      ]
    in
    PP.separate PP.hardline [
      entry_block;
      PP.indent contents;
      exit_block;
    ]
  else
    contents


let pp_tuple_type ts =
  PP.separate (PP.string " * ") ts


let pp_notation notation expression =
  pp_sentence @@ PP.separate PP.space [
    PP.string "Notation";
    PP.dquotes notation;
    PP.string ":=";
    PP.parens expression
  ]
