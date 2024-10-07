open Base


(*
   "<argument>" ∷ <typ>
*)
let pp_bind
    (argument : PP.document)
    (typ      : PP.document) : PP.document
  =
  PP.separate_horizontally
    ~separator:PP.space
    [
      PP.surround PP.dquotes argument;
      PP.string " ∷ ";
      typ
    ]


(*
   exp_var "<id>"
*)
let pp_expression_of_identifier (identifier : PP.document) : PP.document =
  PP.separate_horizontally
    ~separator:PP.space
    [
      PP.string "exp_var";
      PP.surround PP.dquotes identifier
    ]


(*
   stm_exp (<expression>)
*)
let pp_statement_of_expression (expression : PP.document) : PP.document =
  Coq.pp_application (PP.string "stm_exp") [ PP.(surround parens) expression ]


(*
   (call <function_identifier> <arguments[0]> <arguments[1]> ...)%exp
*)
let pp_call
    (function_identifier : Ast.Identifier.t)
    (arguments           : PP.document list) : PP.document
  =
  Coq.pp_scope (PP.string "exp") begin
      Coq.pp_application (PP.string "call") (Identifier.pp function_identifier :: arguments)
    end


let pp_expression_value
    (typ   : PP.document)
    (value : PP.document) : PP.document
  =
  Coq.pp_application
    (PP.string "exp_val")
    [
      PP.(surround parens) typ;
      PP.(surround parens) value;
    ]


let string_of_pprint_document (document : PPrint.document) =
  let text_width = Configuration.(get output_width)
  and buffer     = Stdlib.Buffer.create 10000
  in
  PPrint.ToBuffer.pretty 1.0 text_width buffer document;
  Stdlib.Buffer.contents buffer


let pp_sail_definition sail_definition =
  let document =
    Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)
  in
  let str =
    String.rstrip @@ string_of_pprint_document document
  in
  let lines =
    List.map ~f:String.rstrip @@ String.split_lines str
  in
  PP.vertical @@ List.map ~f:PP.string lines
