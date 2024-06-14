module Type = Recursive.Type


type t =
  | Stm_match              of match_pattern
  | Stm_exp                of Expression.t
  | Stm_call               of Identifier.t * Expression.t list
  | Stm_let                of Identifier.t * t * t
  | Stm_destructure_record of destructure_record
  | Stm_seq                of t * t
  | Stm_read_register      of Identifier.t
  | Stm_write_register     of Identifier.t * t
  | Stm_cast               of t * Type.t
  | Stm_fail               of string

and match_pattern =
  | MP_list    of match_pattern_list
  | MP_product of match_pattern_product
  | MP_bool    of match_pattern_bool
  | MP_enum    of match_pattern_enum
  | MP_variant of match_pattern_variant

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
