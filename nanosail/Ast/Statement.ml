module Type = Recursive.Type


type t =
  | Match             of match_pattern
  | Expression        of Expression.t
  | Call              of Identifier.t * Expression.t list
  | Let               of let_data
  | DestructureRecord of destructure_record
  | Seq               of t * t
  | ReadRegister      of Identifier.t
  | WriteRegister     of Identifier.t * t
  | Cast              of t * Type.t
  | Fail              of string

(*
  let <variable_identifier> : <binding_statement_type> = <binding_statement>
  in
  <body>
*)
and let_data =
  {
    variable_identifier    : Identifier.t;
    binding_statement_type : Type.t;
    binding_statement      : t;
    body_statement         : t;
  }

and match_pattern =
  | List    of match_pattern_list
  | Product of match_pattern_product
  | Bool    of match_pattern_bool
  | Enum    of match_pattern_enum
  | Variant of match_pattern_variant

and match_pattern_list =
  {
    matched   : t;
    when_cons : Identifier.t * Identifier.t * t;
    when_nil  : t;
  }

and match_pattern_product =
  {
    matched   : t;
    id_fst    : Identifier.t;
    id_snd    : Identifier.t;
    body      : t;
  }

and match_pattern_bool =
  {
    condition  : t;
    when_true  : t;
    when_false : t;
  }

and match_pattern_enum =
  {
    matched      : t;
    matched_type : Identifier.t;
    cases        : t Identifier.Map.t
  }

and match_pattern_variant =
  {
    matched    : t;
    cases      : (Identifier.t list * t) Identifier.Map.t
  }

and destructure_record =
  {
    record_type_identifier : Identifier.t     ;   (* name of the record                                              *)
    field_identifiers      : Identifier.t list;   (* names of the record's fields                                    *)
    variable_identifiers   : Identifier.t list;   (* names of the variables receiving the record's fields' values    *)
    destructured_record    : t                ;   (* statement yield the record object                               *)
    body                   : t                ;   (* body that can refer to record fields using variable_identifiers *)
  }
