open Base

module Type = Recursive.Type


type t =
  | Match             of match_pattern
  | Expression        of Expression.t
  | Call              of Identifier.t * Expression.t list
  (*
    let <variable_identifier> : <binding_statement_type> = <binding_statement>
    in
    <body>
  *)
  | Let               of { variable_identifier    : Identifier.t      ;
                           binding_statement_type : Type.t            ;
                           binding_statement      : t                 ;
                           body_statement         : t                 }
  | DestructureRecord of { record_type_identifier : Identifier.t      ;
                           field_identifiers      : Identifier.t list ;
                           variable_identifiers   : Identifier.t list ;
                           destructured_record    : t                 ;
                           body                   : t                 }
  | Seq               of t * t
  | ReadRegister      of Identifier.t
  | WriteRegister     of { register_identifier : Identifier.t ;
                           written_value       : Identifier.t }
  | Cast              of t * Type.t
  | Fail              of string

and match_pattern =
  | MatchList    of { matched      : Identifier.t                    ;
                      element_type : Nanotype.t                      ;
                      when_cons    : Identifier.t * Identifier.t * t ;
                      when_nil     : t                               }
  | MatchProduct of { matched  : Identifier.t ;
                      type_fst : Nanotype.t   ;
                      type_snd : Nanotype.t   ;
                      id_fst   : Identifier.t ;
                      id_snd   : Identifier.t ;
                      body     : t            }
  | MatchTuple   of { matched  : Identifier.t                     ;
                      elements : (Identifier.t * Nanotype.t) list ;
                      body     : t                                }
  | MatchBool    of { condition  : Identifier.t ;
                      when_true  : t            ;
                      when_false : t            }
  | MatchEnum    of { matched      : Identifier.t       ;
                      matched_type : Identifier.t       ;
                      cases        : t Identifier.Map.t }
  | MatchVariant of { matched      : Identifier.t                             ;
                      matched_type : Identifier.t                             ;
                      cases        : (Identifier.t list * t) Identifier.Map.t }


let rec to_fexpr (statement : t) : FExpr.t =
  let match_pattern_to_fexpr (pattern : match_pattern) : FExpr.t =
    match pattern with
    | MatchList { matched; element_type; when_cons; when_nil } -> begin
        let matched' =
          Identifier.to_fexpr matched
        and element_type' =
          Nanotype.to_fexpr element_type
        and when_cons' =
          let head_identifier, tail_identifier, body = when_cons
          in
          FExpr.mk_application
            ~keyword:[
              ("head", Identifier.to_fexpr head_identifier);
              ("tail", Identifier.to_fexpr tail_identifier);
              ("body", to_fexpr body)
            ]
            "WhenCons"
        and when_nil' =
          to_fexpr when_nil
        in
        let keyword =
          [
            ("matched", matched');
            ("element_type", element_type');
            ("when_cons", when_cons');
            ("when_nil", when_nil');
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchList"
      end

    | MatchProduct { matched; id_fst; id_snd; type_fst; type_snd; body } -> begin
        let keyword =
          [
            ("matched", Identifier.to_fexpr matched);
            ("id_fst", Identifier.to_fexpr id_fst);
            ("id_snd", Identifier.to_fexpr id_snd);
            ("type_fst", Nanotype.to_fexpr type_fst);
            ("type_snd", Nanotype.to_fexpr type_snd);
            ("body", to_fexpr body)
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchProduct"
      end

    | MatchTuple { matched; elements; body } -> begin
        let fexpr_of_pair (identifier, typ) =
          FExpr.mk_application ~positional:[Identifier.to_fexpr identifier; Type.to_fexpr typ] "Stm:MatchTuple"
        in
        let keyword =
          [
            ("matched", Identifier.to_fexpr matched);
            ("elements", FExpr.mk_list @@ List.map elements ~f:fexpr_of_pair);
            ("body", to_fexpr body)
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchTuple"
      end

    | MatchBool { condition; when_true; when_false } -> begin
        let keyword =
          [
            ("condition", Identifier.to_fexpr condition);
            ("when_true", to_fexpr when_true);
            ("when_false", to_fexpr when_false);
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchBool"
      end

    | MatchEnum { matched; matched_type; cases } -> begin
        let cases' =
          let case_to_fexpr (enum_identifier, statement) =
            FExpr.mk_application ~positional:[Identifier.to_fexpr enum_identifier; to_fexpr statement] "Case"
          in
          let items =
            List.map ~f:case_to_fexpr @@ Identifier.Map.to_alist cases
          in
          FExpr.mk_list items
        in
        let keyword =
          [
            ("matched", Identifier.to_fexpr matched);
            ("matched_type", Identifier.to_fexpr matched_type);
            ("cases", cases')
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchEnum"
      end

    | MatchVariant { matched; matched_type; cases } -> begin
        let cases' =
          let case_to_fexpr (constructor, (identifiers, clause)) =
            let keyword =
              [
                ("constructor", Identifier.to_fexpr constructor);
                ("binders", FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr identifiers);
                ("clause", to_fexpr clause);
              ]
            in
            FExpr.mk_application ~keyword:keyword "Case"
          in
          let items =
            List.map ~f:case_to_fexpr @@ Identifier.Map.to_alist cases
          in
          FExpr.mk_list items
        in
        let keyword =
          [
            ("matched", Identifier.to_fexpr matched);
            ("matched_type", Identifier.to_fexpr matched_type);
            ("cases", cases')
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchVariant"
      end

  and expression_to_fexpr (expression : Expression.t) : FExpr.t =
    FExpr.mk_application ~positional:[Expression.to_fexpr expression] "Stm:Expression"

  and call_to_fexpr
      (identifier : Identifier.t)
      (arguments : Expression.t list)
    =
    let keyword =
      [
        ("function_id", Identifier.to_fexpr identifier);
        ("arguments", FExpr.mk_list @@ List.map ~f:Expression.to_fexpr arguments)
      ]
    in
    FExpr.mk_application ~keyword "Stm:Call"

  and let_to_fexpr
      (variable_identifier    : Identifier.t)
      (binding_statement_type : Type.t      )
      (binding_statement      : t           )
      (body_statement         : t           ) : FExpr.t
    =
    let keyword =
      [
        ("id", Identifier.to_fexpr variable_identifier);
        ("type", Type.to_fexpr binding_statement_type);
        ("value", to_fexpr binding_statement);
        ("body", to_fexpr body_statement);
      ]
    in
    FExpr.mk_application ~keyword "Stm:Let"

  and destructure_record_to_fexpr
      (record_type_identifier : Identifier.t     )
      (field_identifiers      : Identifier.t list)
      (variable_identifiers   : Identifier.t list)
      (destructured_record    : t                )
      (body                   : t                ) : FExpr.t
    =
    let keyword =
      [
        ("record_type", Identifier.to_fexpr record_type_identifier);
        ("fields", FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr field_identifiers);
        ("variables", FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr variable_identifiers);
        ("record", to_fexpr destructured_record);
        ("body", to_fexpr body);
      ]
    in
    FExpr.mk_application ~keyword "Stm:DestructureRecord"

  and seq_to_fexpr
      (e1 : t)
      (e2 : t) : FExpr.t
    =
    FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] "Sequence"

  and read_register_to_fexpr (register_identifier : Identifier.t) : FExpr.t =
    FExpr.mk_application ~positional:[Identifier.to_fexpr register_identifier] "ReadRegister"

  and write_register_to_fexpr
      (register_identifier : Identifier.t)
      (value               : Identifier.t) : FExpr.t
    =
    let keyword =
      [
        ("register", Identifier.to_fexpr register_identifier);
        ("value", Identifier.to_fexpr value)
      ]
    in
    FExpr.mk_application ~keyword "Stm:WriteRegister"

  and cast_to_fexpr
      (expression : t     )
      (typ        : Type.t) : FExpr.t
    =
    let keyword =
      [
        ("type", Type.to_fexpr typ);
        ("value", to_fexpr expression)
      ]
    in
    FExpr.mk_application ~keyword "Stm:Cast"

  and fail_to_fexpr (message : string) : FExpr.t =
    FExpr.mk_application ~positional:[FExpr.mk_string message] "Stm:Fail"

  in
  match statement with
  | Match pattern                -> match_pattern_to_fexpr pattern
  | Expression expression        -> expression_to_fexpr expression
  | Call (identifier, arguments) -> call_to_fexpr identifier arguments
  | Seq (t1, t2)                 -> seq_to_fexpr t1 t2
  | ReadRegister id              -> read_register_to_fexpr id
  | Cast (expression, typ)       -> cast_to_fexpr expression typ
  | Fail message                 -> fail_to_fexpr message
  | Let {
      variable_identifier   ;
      binding_statement_type;
      binding_statement     ;
      body_statement
    }                            -> let_to_fexpr variable_identifier binding_statement_type binding_statement body_statement
  | DestructureRecord {
      record_type_identifier;
      field_identifiers;
      variable_identifiers;
      destructured_record;
      body
    }                            -> destructure_record_to_fexpr record_type_identifier field_identifiers variable_identifiers destructured_record body
  | WriteRegister {
      register_identifier;
      written_value
    }                            -> write_register_to_fexpr register_identifier written_value
