(*
   Collection of functions to generate Coq code.

   Each of these functions operate outside the generation monad.
   muSail specific generation functionality resides in the MuSail module.
*)
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


(*
   contents.
*)
let pp_sentence (contents : PP.t) : PP.t =
  PP.(horizontal [contents; eol])


(*
   (* comment *)
*)
let pp_inline_comment (comment : PP.t) : PP.t =
  PP.surround comment_delimiters comment


(*
   (*
     comment
   *)
*)
let pp_multiline_comment (comment : PP.t) : PP.t =
  PP.surround ~layout:PP.vertical comment_delimiters @@ PP.indent comment


(*
   If <comment> fits on a single line, returns

     (* <comment> *)

   Otherwise, returns

     (*
        <comment>
     *)
*)
let pp_comment (comment : PP.t) : PP.t =
  if PP.is_single_line comment
  then pp_inline_comment comment
  else pp_multiline_comment comment


(*
   (*
     comments
   *)
   document
*)
let add_comments
    ~(comments : PP.t)
    ~(document : PP.t) : PP.t
  =
  PP.annotate [%here] begin
    PP.vertical [ pp_comment comments; document ]
  end


(*
   func arg1
        arg2
        ...
        argn
*)
let pp_hanging_application
    (func      : PP.t     )
    (arguments : PP.t list) : PP.t
  =
  PP.annotate [%here] begin
    PP.(hanging @@ horizontal [ func; space ] :: arguments)
  end


(*
   [item1;item2;...;itemn]
*)
let pp_list_using_notation (items : PP.t list) : PP.t =
  if
    List.is_empty items
  then
    PP.annotate [%here] begin
      PP.(horizontal [lbracket; rbracket])
    end
  else
    PP.annotate [%here] begin
      PP.(delimited_list
            ~delimiters:brackets
            ~items
            ~separator:semi)
    end


(*
   (cons (item1)
         (cons (item2)
               (cons (item3)
                     nil)))
*)
let pp_list_using_cons (items : PP.t list) : PP.t =
  let rec pp items =
    match items with
    | []         -> PP.string "nil"
    | head::tail -> PP.(surround parens @@ pp_hanging_application (string "cons") [ PP.(surround parens) head; pp tail])
  in
  PP.annotate [%here] begin
    pp items
  end


(*
   Pretty prints list using cons or [] notation depending on value of use_notation
*)
let pp_list
      ?(use_notation : bool = true)
      (items         : PP.t list  ) : PP.t
  =
  if use_notation
  then pp_list_using_notation items
  else pp_list_using_cons items


(*
  (v1, v2)
*)
let pp_product
    (v1 : PP.t)
    (v2 : PP.t) : PP.t
  =
  PP.annotate [%here] begin
    PP.(surround tuple_delimiters @@ separate_horizontally ~separator:comma [v1; v2])
  end


(*
   Section identifier.
     contents
   End identifier.
*)
let pp_section
    (identifier : PP.t)
    (contents   : PP.t) : PP.t
  =
  let first_line = pp_sentence @@ PP.(horizontal [ string "Section"; space; identifier ])
  and last_line  = pp_sentence @@ PP.(horizontal [ string "End"; space; identifier ])
  in
  let delimiters = (first_line, last_line)
  in
  PP.annotate [%here] begin
      PP.(surround
            ~layout:vertical
            delimiters
            (indent contents))
    end


type module_mode =
  | Import
  | Export


(*
   Module [Import|Export|] identifier [<: module_type]*.
     contents
   End identifier.
*)
let pp_module
    ?(mode         : module_mode option = None)
    ?(module_types : PP.t list          = []  )
    (identifier    : string                   )
    (contents      : PP.t                     ) : PP.t
  =
  let pp_module_type (module_type : PP.t) : PP.t =
    PP.separate_horizontally ~separator:PP.space [ PP.string "<:"; module_type ]
  in  
  let first_line =
    PP.annotate [%here] @@ PP.(
      pp_sentence @@ separate_horizontally ~separator:space @@ List.build_list (fun { add; addall; _ } ->
          add @@ string "Module";
          begin
            match mode with
            | Some Import -> add @@ string "Import"
            | Some Export -> add @@ string "Export"
            | None        -> ()
          end;
          add @@ string identifier;
          addall @@ List.map ~f:pp_module_type module_types;
        )
    )
  and last_line =
    PP.(pp_sentence @@ separate_horizontally ~separator:space [ string "End"; string identifier ])
  in
  let delimiters =
    (first_line, last_line)
  in
  PP.annotate [%here] begin
    PP.(surround ~layout:vertical delimiters (indent contents))
  end


(*
   Definition identifier [implicit_parameter : implicit_parameter_type?]* [parameter : parameter_type]* [: result_type]? :=
     body.
*)
let pp_definition
      ~(identifier          : PP.t                                 )
      ?(implicit_parameters : (PP.t * PP.t option) list = []       )
      ?(parameters          : (PP.t * PP.t option) list = []       )
      ?(result_type         : PP.t option                    = None)
       (body                : PP.t                                 ) : PP.t
  =
  let pp_parameters =
    let pp_implicit_parameters =
      let pp_implicit_parameter (var, typ) =
        match typ with
        | Some typ -> PP.(surround braces @@ separate_horizontally ~separator:space [ var; colon; typ ])
        | None     -> PP.(surround braces @@ var)
      in
      List.map ~f:pp_implicit_parameter implicit_parameters
    and pp_explicit_parameters =
      let pp_parameter (var, typ) =
        match typ with
        | Some typ -> PP.(surround parens @@ horizontal [ var; string " : "; typ ])
        | None     -> var
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
    | Some rt -> Some PP.(separate_horizontally ~separator:PP.space [ PP.colon; rt ])
  in
  let definition_line =
    PP.separate_horizontally ~separator:PP.space @@ List.build_list begin fun { add; addopt; _ } ->
      add    @@ PP.string "Definition";
      add    @@ identifier;
      addopt @@ pp_parameters;
      addopt @@ pp_return_type;
      add    @@ PP.string ":=";
    end
  in
  PP.annotate [%here] begin
    PP.(pp_sentence @@ vertical [ definition_line; indent body ])
  end


(*
   match expression with
   | case_pattern_1 => case_expression_1
   | case_pattern_2 => case_expression_2
   ...
   end[%scope]?
*)
let pp_match
    ?(scope     : PP.t option        = None)
    (expression : PP.t                     )
    (cases      : (PP.t * PP.t) list       ) : PP.t
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
      PP.(
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
  PP.annotate [%here] begin
    PP.(vertical result_lines)
  end


let pp_match_pair
    (matched_expressions : PP.t * PP.t                )
    (cases               : ((PP.t * PP.t) * PP.t) list) : PP.t
  =
  let left_patterns =
    List.map ~f:(Fn.compose fst fst) cases
  in
  let left_patterns_max_width =
    Option.value ~default:0 begin
      List.max_elt ~compare:Int.compare @@ List.map ~f:(Fn.compose fst PP.measure) left_patterns
    end
  in
  let aligned_cases =
    List.map cases ~f:(fun ((left, right), expression) ->
        PP.(
          separate_horizontally ~separator:(horizontal [comma; space]) [
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
      separate_horizontally ~separator:(horizontal [comma; space]) [
        left;
        right
      ]
    )
  in
  PP.annotate [%here] begin
    pp_match matched_expression aligned_cases
  end


(*
   <value>%Z

   Put inside parentheses if necessary (i.e., if <value> is negative)
*)
let pp_z_integer (value : Z.t) : PP.t =
  let pp_i =
    PP.string @@ Big_int.to_string value ^ "%Z"
  in
  let requires_parentheses =
    Big_int.less value Z.zero
  in
  if
    requires_parentheses
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


(*
   [From from]? Require [Import]? library_1
                                  library_2
                                  ...
                                  library_n.
*)
let pp_require
    ?(from     : string option      = None)
    ?(mode     : module_mode option = None)
    (libraries : string list              ) : PP.t
  =
  let from_words =
    match from with
    | Some s ->
       [
         PP.string "From";
         PP.string s;
       ]
    | None   -> []
  and require_words =
    [
      PP.string "Require"
    ]
  and mode_words =
    match mode with
    | Some Import -> [ PP.string "Import" ]
    | Some Export -> [ PP.string "Export" ]
    | None        -> []
  in
  let words     = List.concat [ from_words; require_words; mode_words ]
  and libraries = List.map ~f:PP.string libraries
  in
  PP.annotate [%here] begin
    pp_sentence @@ PP.hanging @@ (PP.horizontal [ PP.separate_horizontally ~separator:PP.space words; PP.space ]) :: libraries
  end


(*
   Import library_1
          library_2
          ...
          library_n.
*)
let pp_imports (libraries : string list) : PP.t =
  PP.annotate [%here] begin
    PP.(pp_sentence @@ hanging (string "Import " :: List.map ~f:string libraries))
  end


let pp_open_scopes scopes =
  let open_scope scope =
    PP.(
      pp_sentence @@ separate_horizontally ~separator:space [
        string "Local Open Scope";
        string scope;
      ]
    )
  in
  PP.annotate [%here] begin
    PP.(vertical @@ List.map ~f:open_scope scopes)
  end


let pp_record_value (fields : (PP.t * PP.t) list) : PP.t =
  let items =
    let item_of_field (field_name, field_value) =
      PP.separate_horizontally ~separator:PP.space [
        field_name;
        PP.string ":=";
        field_value
      ]
    in
    List.map ~f:item_of_field fields
  in
  let separator =
    PP.(horizontal [ record_field_separator; space ])
  in
  PP.annotate [%here] begin
    PP.(delimited_list ~delimiters:record_delimiters ~items ~separator)
  end


(*

  #[export,program] Instance <identifier>_finite : Finite <type_name> :=
    {| enum := [ <values> ] |}.

 *)
let pp_finite_instance
      ~(identifier : PP.t     )
      ~(type_name  : PP.t     )
      ~(values     : PP.t list) : PP.t
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
  in
  PP.annotate [%here] @@ pp_sentence declaration


(* fields as (identifier, type) pairs *)
let pp_record
      ~(identifier  : PP.t              )
      ~(type_name   : PP.t              )
      ~(constructor : PP.t              )
      ~(fields      : (PP.t * PP.t) list) : PP.t
  =
  let first_line =
    PP.(
      separate_horizontally ~separator:space [
        string "Record";
        identifier;
        colon;
        type_name;
        string ":="
      ]
    )
  in
  let pp_fields =
    let longest_field_length =
      Option.value
        ~default:0
        (List.max_elt ~compare:Int.compare @@ List.map ~f:(fun (field, _) -> fst @@ PP.measure field) fields)
    in
    List.map fields ~f:(
        fun (id, t) -> begin
            PP.(horizontal [
                separate_horizontally ~separator:space [ PP.pad_right longest_field_length id; colon; t ];
                semi
              ])
          end
      )
  in
  let body =
    PP.(surround ~layout:vertical braces (indent @@ vertical pp_fields))
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


let pp_local_obligation_tactic (identifier : Ast.Identifier.t) : PP.t =
  let lines_of_code = [
      PP.annotate [%here] @@ PP.string "Local Obligation Tactic :=";
      PP.annotate [%here] @@ PP.(horizontal [ space; space; Identifier.pp identifier ])
    ]
  in
  PP.annotate [%here] @@ pp_sentence @@ PP.vertical lines_of_code


let pp_derive
      (class_identifier : Ast.Identifier.t)
      (type_identifier  : Ast.Identifier.t) : PP.t =
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
  PP.annotate [%here] begin
    PP.(separate_horizontally ~separator:space @@ f :: args)
  end


let pp_explicit_application f args =
  PP.annotate [%here] begin
    PP.(separate_horizontally ~separator:space @@ horizontal [ PP.at; f ] :: args)
  end


let pp_function_type parameter_types result_type =
  PP.annotate [%here] begin
    PP.separate_horizontally ~separator:PP.space @@ List.build_list @@ fun { addall; add; _ } -> begin
      addall parameter_types;
      add arrow;
      add result_type
    end
  end


let pp_canonical (identifier : Ast.Identifier.t) : PP.t =
  PP.annotate [%here] begin
    pp_sentence @@ PP.(separate_horizontally ~separator:space [ PP.string "Canonical"; Identifier.pp identifier ])
  end


let pp_include_module (name : PP.t) : PP.t =
  PP.annotate [%here] begin
    pp_sentence @@ PP.(separate_horizontally ~separator:space [ string "Include"; name ])
  end


let pp_tuple_type ts =
  PP.annotate [%here] begin
    PP.separate_horizontally ~separator:(PP.string " * ") ts
  end


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


let pp_bool (value : bool) : PP.t =
  PP.annotate [%here] begin
    if value
    then PP.string "true"
    else PP.string "false"
  end
