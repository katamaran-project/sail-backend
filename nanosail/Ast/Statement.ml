module Type = Recursive.Type


type t =
  | Match             of match_pattern
  | Expression        of Expression.t
  | Call              of Identifier.t * Expression.t list
  | Let               of let_arguments
  | DestructureRecord of destructure_record_arguments
  | Seq               of t * t
  | ReadRegister      of Identifier.t
  | WriteRegister     of write_register_arguments
  | Cast              of t * Type.t
  | Fail              of string

(*
  let <variable_identifier> : <binding_statement_type> = <binding_statement>
  in
  <body>
*)
and let_arguments =
  {
    variable_identifier    : Identifier.t;
    binding_statement_type : Type.t;
    binding_statement      : t;
    body_statement         : t;
  }

and write_register_arguments =
  {
    register_identifier  : Identifier.t;
    written_value        : Identifier.t;
  }

and match_pattern =
  | MatchList    of match_pattern_list
  | MatchProduct of match_pattern_product
  | MatchBool    of match_pattern_bool
  | MatchEnum    of match_pattern_enum
  | MatchVariant of match_pattern_variant

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
    matched      : Identifier.t;
    matched_type : Identifier.t;
    cases        : t Identifier.Map.t
  }

and match_pattern_variant =
  {
    matched      : Identifier.t;
    matched_type : Identifier.t;
    cases        : (Identifier.t list * t) Identifier.Map.t
  }

and destructure_record_arguments =
  {
    record_type_identifier : Identifier.t     ;   (* name of the record                                              *)
    field_identifiers      : Identifier.t list;   (* names of the record's fields                                    *)
    variable_identifiers   : Identifier.t list;   (* names of the variables receiving the record's fields' values    *)
    destructured_record    : t                ;   (* statement yield the record object                               *)
    body                   : t                ;   (* body that can refer to record fields using variable_identifiers *)
  }
