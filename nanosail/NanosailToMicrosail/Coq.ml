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
let left_comment_delimiter  = PP.string "(*"
let right_comment_delimiter = PP.string "*)"


let ends_on_newline (string : string) : bool =
  String.is_suffix string ~suffix:"\n"


let count_newlines (string : string) : int =
  String.count string ~f:(Char.equal '\n')


let is_single_line (string : string) : bool =
  let newline_count = count_newlines string
  in
  newline_count = 0 || (newline_count = 1 && ends_on_newline string)


let pp_inline_comment (comment : PP.document) : PP.document =
  PP.(separate space [
      left_comment_delimiter;
      comment;
      right_comment_delimiter
    ])


let pp_multiline_comment (comment : PP.document) : PP.document =
  PP.separate PP.hardline [
    left_comment_delimiter;
    PP.indent' comment;
    right_comment_delimiter
  ]


let pp_comment (comment : PP.document) : PP.document =
  let str = PP.string_of_document comment
  in
  if is_single_line str
  then pp_inline_comment comment
  else pp_multiline_comment comment


let add_comments
    ~(comments : PP.document)
    ~(document : PP.document) : PP.document
  =
  PP.separate PP.hardline [ pp_comment comments; document ]


let original_sail_code (source : PP.document) : PP.document =
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


let original_sail_codes (sources : PP.document list) : PP.document =
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


(* todo rename to pp_list *)
let list (items : PP.document list) : PP.document =
  if
    List.is_empty items
  then
    PP.(lbracket ^^ rbracket)
  else
    PP.(delimited_sequence (lbracket ^^ space) (space ^^ rbracket) semi items)


(* todo rename to pp_product *)
let product
    (v1 : PP.document)
    (v2 : PP.document) : PP.document
  =
  PP.(soft_surround 1 0 lparen (v1 ^^ comma ^^ break 1 ^^ v2) rparen)


let section identifier contents =
  let first_line = PP.(string "Section" ^^ space ^^ Identifier.pp identifier ^^ eol)
  and last_line  = PP.(string "End" ^^ space ^^ Identifier.pp identifier ^^ eol)
  in
  PP.indented_enclosed_lines first_line contents last_line


type module_flag =
  | Import
  | Export
  | NoFlag


let sentence contents =
  PP.(contents ^^ eol)


let module' ?(flag = NoFlag) ?(includes = []) identifier contents =
  let first_line =
    PP.(
      sentence @@ separate space @@ Auxlib.build_list (fun { add; addall; _ } ->
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
  and last_line = PP.(sentence @@ separate space [ string "End"; string identifier ])
  in
  PP.indented_enclosed_lines first_line contents last_line


let definition
      ~(identifier          : PP.document                                   )
      ?(implicit_parameters : (PP.document * PP.document option) list = []  )
      ?(parameters          : (PP.document * PP.document option) list = []  )
      ?(result_type         : PP.document option                      = None)
       (body                : PP.document                                   ) : PP.document
  =
  let open PP
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
    Auxlib.build_list @@ fun { add; _ } -> begin
                      if not @@ List.is_empty parameters then add space;
                      add @@ align @@ separate space @@ pp_explicit_and_implicit_parameters
                    end
  in
  group begin
      concat begin
          Auxlib.build_list begin fun { add; addall; _ } ->
            add @@ string "Definition";
            add space;
            add identifier;
            addall @@ pp_parameters;
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


(* todo pp_match *)
let match'
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
      let widths = List.map ~f:(fun pattern -> PP.requirement pattern) (List.map ~f:fst cases)
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
  sentence @@ PP.hanging_list ~adaptive:false (PP.separate PP.space words) libraries


let imports names =
  PP.(sentence @@ hanging_list ~adaptive:false (string "Import") (List.map ~f:string names))


let open_scopes scopes =
  let open_scope scope =
    PP.(
      sentence @@ concat [
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
  sentence declaration


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
  sentence @@ PP.(first_line ^^ hardline ^^ indent' (constructor ^^ hardline ^^ indent' body))


let local_obligation_tactic (identifier : Ast.Identifier.t) : PP.document =
  let lines_of_code = [
      PP.string "Local Obligation Tactic :=";
      PP.(twice space ^^ Identifier.pp identifier)
    ]
  in
  sentence PP.(separate hardline lines_of_code)


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
  sentence @@ PP.simple_app [ PP.string "Canonical"; Identifier.pp identifier ]


let include_module (name : PP.document) =
  sentence @@ PP.simple_app [ PP.string "Include"; name ]


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
      PP.indent' contents;
      exit_block;
    ]
  else
    contents


let pp_tuple_type ts =
  PP.separate (PP.string " * ") ts


let pp_notation notation expression =
  sentence @@ PP.separate PP.space [
    PP.string "Notation";
    PP.dquotes notation;
    PP.string ":=";
    PP.parens expression
  ]
