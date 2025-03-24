(*

   Unlike muSail, Sail does not make the distinction
   between statemens and expressions.

   Nanosail follows the same design as muSaqil.

*)
open ExtBase

module Type = Recursive.Type


type t =
  | Match             of match_pattern
  | Expression        of Expression.t
  | Call              of Identifier.t * Expression.t list
  (*
    let <binder> : <binding_statement_type> = <binding_statement>
    in
    <body>
  *)
  | Let               of { binder                 : Identifier.t      ;
                           binding_statement_type : Type.t            ;
                           binding_statement      : t                 ;
                           body_statement         : t                 }
  | DestructureRecord of { record_type_identifier : Identifier.t      ;
                           field_identifiers      : Identifier.t list ;
                           binders                : Identifier.t list ;
                           destructured_record    : t                 ;
                           body                   : t                 }
  | Seq               of t * t
  | ReadRegister      of Identifier.t
  | WriteRegister     of { register_identifier : Identifier.t ;
                           written_value       : Identifier.t }
  | Cast              of t * Type.t
  | Fail              of Type.t * string

and match_pattern =
  | MatchList    of { matched      : Identifier.t                    ;
                      element_type : Type.t                          ;
                      when_cons    : Identifier.t * Identifier.t * t ;
                      when_nil     : t                               }
  | MatchProduct of { matched      : Identifier.t ;
                      type_fst     : Type.t       ;
                      type_snd     : Type.t       ;
                      id_fst       : Identifier.t ;
                      id_snd       : Identifier.t ;
                      body         : t            }
  | MatchTuple   of { matched      : Identifier.t                 ;
                      binders      : (Identifier.t * Type.t) list ;
                      body         : t                            }
  | MatchBool    of { condition    : Identifier.t ;
                      when_true    : t            ;
                      when_false   : t            }
  | MatchEnum    of { matched      : Identifier.t       ;
                      matched_type : Identifier.t       ;
                      cases        : t Identifier.Map.t }
  | MatchVariant of { matched      : Identifier.t                             ;
                      matched_type : Identifier.t                             ;
                      cases        : (Identifier.t list * t) Identifier.Map.t }


(* todo get rid of this exception once all equality rules have been implemented *)
exception UnimplementedStatementEquality

(* todo complete implementation; this partially implemented equality is used in tests *)
let rec equal
    (statement_1 : t)
    (statement_2 : t) : bool
  =
  let equal_match_patterns
    (pattern_1 : match_pattern)
    (pattern_2 : match_pattern) : bool
    =
    match pattern_1 with
    | MatchEnum { matched = matched_1; matched_type = matched_type_1; cases = cases_1 } -> begin
        match pattern_2 with
        | MatchEnum { matched = matched_2; matched_type = matched_type_2; cases = cases_2 } -> begin
            Identifier.equal
              matched_1
              matched_2
            &&
            Identifier.equal
              matched_type_1
              matched_type_2
            &&
            Identifier.Map.equal equal
              cases_1
              cases_2
          end
        | _ -> false
      end
    | MatchVariant { matched = matched_1; matched_type = matched_type_1; cases = cases_1 } -> begin
        match pattern_2 with
        | MatchVariant { matched = matched_2; matched_type = matched_type_2; cases = cases_2 } -> begin
            let equal_cases
                ((binder_identifiers_1, statement_1) : Identifier.t list * t)
                ((binder_identifiers_2, statement_2) : Identifier.t list * t)
              =
              List.equal Identifier.equal
                binder_identifiers_1
                binder_identifiers_2
              &&
              equal
                statement_1
                statement_2
            in
            Identifier.equal
              matched_1
              matched_2
            &&
            Identifier.equal
              matched_type_1
              matched_type_2
            &&
            Identifier.Map.equal equal_cases
              cases_1
              cases_2
          end
        | _ -> false
      end
    | MatchBool { condition = condition_1; when_true = when_true_1; when_false = when_false_1 } -> begin
        match pattern_2 with
        | MatchBool { condition = condition_2; when_true = when_true_2; when_false = when_false_2 } -> begin
            Identifier.equal
              condition_1
              condition_2
            &&
            equal
              when_true_1
              when_true_2
            &&
            equal
              when_false_1
              when_false_2
          end
        | _ -> false
      end
    | MatchTuple { matched = matched_1; binders = binders_1; body = body_1 } -> begin
        match pattern_2 with
        | MatchTuple { matched = matched_2; binders = binders_2; body = body_2 } -> begin
            Identifier.equal
              matched_1
              matched_2
            &&
            List.equal
              (Tuple.Pair.equal Identifier.equal Type.equal)
              binders_1
              binders_2
            &&
            equal
              body_1
              body_2
          end
        | _ -> false
      end
    | MatchProduct data_1 -> begin
        match pattern_2 with
        | MatchProduct data_2 -> begin
            Identifier.equal
              data_1.matched
              data_2.matched
            &&
            Type.equal
              data_1.type_fst
              data_2.type_fst
            &&
            Type.equal
              data_1.type_snd
              data_2.type_snd
            &&
            Identifier.equal
              data_1.id_fst
              data_2.id_fst
            &&
            Identifier.equal
              data_1.id_snd
              data_2.id_snd
            &&
            equal
              data_1.body
              data_2.body
          end
        | _ -> false
      end
    | MatchList _    -> raise UnimplementedStatementEquality
  in

  match statement_1 with
  | Match match_pattern_1 -> begin
      match statement_2 with
      | Match match_pattern_2 -> equal_match_patterns match_pattern_1 match_pattern_2
      | _                     -> false
    end

  | Expression expression_1 -> begin
      match statement_2 with
      | Expression expression_2 -> Expression.equal expression_1 expression_2
      | _                       -> false
    end

  | Call (_, _) -> begin
      match statement_2 with
      | Call (_, _) -> raise UnimplementedStatementEquality
      | _           -> false
    end

  | Let data_1 -> begin
      match statement_2 with
      | Let data_2 -> begin
          Identifier.equal
            data_1.binder
            data_2.binder
          &&
          Type.equal
            data_1.binding_statement_type
            data_2.binding_statement_type
          &&
          equal
            data_1.binding_statement
            data_2.binding_statement
          &&
          equal
            data_1.body_statement
            data_2.body_statement
        end
      | _ -> false
    end

  | DestructureRecord data_1 -> begin
      match statement_2 with
      | DestructureRecord data_2 -> begin
          Identifier.equal
            data_1.record_type_identifier
            data_2.record_type_identifier
          &&
          List.equal Identifier.equal
            data_1.field_identifiers
            data_2.field_identifiers
          &&
          List.equal Identifier.equal
            data_1.binders
            data_2.binders
          &&
          equal
            data_1.destructured_record
            data_2.destructured_record
          &&
          equal
            data_1.body
            data_2.body
        end
      | _ -> false
    end

  | Seq (left_1, right_1) -> begin
      match statement_2 with
      | Seq (left_2, right_2) -> equal left_1 left_2 && equal right_1 right_2
      | _                     -> false
    end

  | ReadRegister register_identifier_1 -> begin
      match statement_2 with
      | ReadRegister register_identifier_2 -> Identifier.equal register_identifier_1 register_identifier_2
      | _                                  -> false
    end

  | WriteRegister data_1 -> begin
      match statement_2 with
      | WriteRegister data_2 -> begin
          Identifier.equal
            data_1.register_identifier
            data_2.register_identifier
          &&
          Identifier.equal
            data_1.written_value
            data_2.written_value
        end
      | _ -> false
    end

  | Cast (statement_1, type_1) -> begin
      match statement_2 with
      | Cast (statement_2, type_2) -> begin
          equal
            statement_1
            statement_2
          &&
          Type.equal
            type_1
            type_2
        end
      | _ -> false
    end

  | Fail (type_1, message_1) -> begin
      match statement_2 with
      | Fail (type_2, message_2) -> begin
        Type.equal
          type_1
          type_2
        &&
        String.equal
          message_1
          message_2
        end
      | _ -> false
    end


let rec to_fexpr (statement : t) : FExpr.t =
  let match_pattern_to_fexpr (pattern : match_pattern) : FExpr.t =
    match pattern with
    | MatchList { matched; element_type; when_cons; when_nil } -> begin
        let matched' =
          Identifier.to_fexpr matched
        and element_type' =
          Type.to_fexpr element_type
        and when_cons' =
          let head_identifier, tail_identifier, body = when_cons
          in
          FExpr.mk_application
            ~keyword:[
              ("head", Identifier.to_fexpr head_identifier);
              ("tail", Identifier.to_fexpr tail_identifier);
              ("body", to_fexpr body                      );
            ]
            "WhenCons"
        and when_nil' =
          to_fexpr when_nil
        in
        let keyword =
          [
            ("matched"     , matched'     );
            ("element_type", element_type');
            ("when_cons"   , when_cons'   );
            ("when_nil"    , when_nil'    );
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchList"
      end

    | MatchProduct { matched; id_fst; id_snd; type_fst; type_snd; body } -> begin
        let keyword =
          [
            ("matched" , Identifier.to_fexpr matched);
            ("id_fst"  , Identifier.to_fexpr id_fst );
            ("id_snd"  , Identifier.to_fexpr id_snd );
            ("type_fst", Type.to_fexpr type_fst     );
            ("type_snd", Type.to_fexpr type_snd     );
            ("body"    , to_fexpr body              );
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchProduct"
      end

    | MatchTuple { matched; binders; body } -> begin
        let fexpr_of_pair (identifier, typ) =
          FExpr.mk_application ~positional:[Identifier.to_fexpr identifier; Type.to_fexpr typ] "Stm:MatchTuple"
        in
        let keyword =
          [
            ("matched", Identifier.to_fexpr matched                       );
            ("binders", FExpr.mk_list @@ List.map binders ~f:fexpr_of_pair);
            ("body"   , to_fexpr body                                     )
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchTuple"
      end

    | MatchBool { condition; when_true; when_false } -> begin
        let keyword =
          [
            ("condition" , Identifier.to_fexpr condition);
            ("when_true" , to_fexpr when_true           );
            ("when_false", to_fexpr when_false          );
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
            ("matched"     , Identifier.to_fexpr matched     );
            ("matched_type", Identifier.to_fexpr matched_type);
            ("cases"       , cases'                          );
          ]
        in
        FExpr.mk_application ~keyword "Stm:MatchEnum"
      end

    | MatchVariant { matched; matched_type; cases } -> begin
        let cases' =
          let case_to_fexpr (constructor, (identifiers, clause)) =
            let keyword =
              [
                ("constructor", Identifier.to_fexpr constructor                             );
                ("binders"    , FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr identifiers);
                ("clause"     , to_fexpr clause                                             );
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
            ("matched"     , Identifier.to_fexpr matched     );
            ("matched_type", Identifier.to_fexpr matched_type);
            ("cases"       , cases'                          );
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
        ("function_id", Identifier.to_fexpr identifier                            );
        ("arguments"  , FExpr.mk_list @@ List.map ~f:Expression.to_fexpr arguments);
      ]
    in
    FExpr.mk_application ~keyword "Stm:Call"

  and let_to_fexpr
      (binder                 : Identifier.t)
      (binding_statement_type : Type.t      )
      (binding_statement      : t           )
      (body_statement         : t           ) : FExpr.t
    =
    let keyword =
      [
        ("binder"     , Identifier.to_fexpr binder          );
        ("bound_type" , Type.to_fexpr binding_statement_type);
        ("bound_value", to_fexpr binding_statement          );
        ("body"       , to_fexpr body_statement             );
      ]
    in
    FExpr.mk_application ~keyword "Stm:Let"

  and destructure_record_to_fexpr
      (record_type_identifier : Identifier.t     )
      (field_identifiers      : Identifier.t list)
      (binders                : Identifier.t list)
      (destructured_record    : t                )
      (body                   : t                ) : FExpr.t
    =
    let keyword =
      [
        ("record_type", Identifier.to_fexpr record_type_identifier                        );
        ("fields"     , FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr field_identifiers);
        ("binders"    , FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr binders          );
        ("record"     , to_fexpr destructured_record                                      );
        ("body"       , to_fexpr body                                                     );
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

  and fail_to_fexpr (typ : Type.t) (message : string) : FExpr.t =
    let keyword =
      [
        ("type", Type.to_fexpr typ);
        ("message", FExpr.mk_string message)
      ]
    in
    FExpr.mk_application ~keyword "Stm:Fail"

  in
  match statement with
  | Match pattern                -> match_pattern_to_fexpr pattern
  | Expression expression        -> expression_to_fexpr expression
  | Call (identifier, arguments) -> call_to_fexpr identifier arguments
  | Seq (t1, t2)                 -> seq_to_fexpr t1 t2
  | ReadRegister id              -> read_register_to_fexpr id
  | Cast (expression, typ)       -> cast_to_fexpr expression typ
  | Fail (typ, message)          -> fail_to_fexpr typ message
  | Let {
      binder;
      binding_statement_type;
      binding_statement;
      body_statement
    }                            -> let_to_fexpr binder binding_statement_type binding_statement body_statement
  | DestructureRecord {
      record_type_identifier;
      field_identifiers;
      binders;
      destructured_record;
      body
    }                            -> destructure_record_to_fexpr record_type_identifier field_identifiers binders destructured_record body
  | WriteRegister {
      register_identifier;
      written_value
    }                            -> write_register_to_fexpr register_identifier written_value


let rec free_variables (statement : t) : Identifier.Set.t =
  match statement with
  | Match (MatchList { matched; element_type = _; when_cons; when_nil }) -> begin
      let head_identifier, tail_identifier, when_cons_statement = when_cons
      in
      Identifier.Set.diff
        (
          Identifier.Set.unions [
            Identifier.Set.singleton matched;
            free_variables when_cons_statement;
            free_variables when_nil
          ]
        )
        (
          Identifier.Set.of_list [ head_identifier; tail_identifier ]
        )
    end

  | Match (MatchProduct { matched; type_fst = _; type_snd = _; id_fst; id_snd; body }) -> begin
      Identifier.Set.diff
        (
          Identifier.Set.unions [
            Identifier.Set.singleton matched;
            free_variables body
          ]
        )
        (
          Identifier.Set.of_list [ id_fst; id_snd ]
        )
    end

  | Match (MatchTuple { matched; binders; body }) -> begin
      let binder_free_variables =
        Identifier.Set.of_list @@ List.map ~f:fst binders
      in
      Identifier.Set.diff
        (
          Identifier.Set.unions [
            Identifier.Set.singleton matched;
            binder_free_variables;
            free_variables body;
          ]
        )
        binder_free_variables
    end

  | Match (MatchBool { condition; when_true; when_false }) -> begin
      Identifier.Set.unions [
        Identifier.Set.singleton condition;
        free_variables when_true;
        free_variables when_false;
      ]
    end

  | Match (MatchEnum { matched; matched_type = _; cases }) -> begin
      let free_variables_in_cases =
        Identifier.Set.unions begin
          List.map ~f:free_variables @@ Identifier.Map.data cases
        end
      in
      Identifier.Set.unions [
        Identifier.Set.singleton matched;
        free_variables_in_cases;
      ]
    end

  | Match (MatchVariant { matched; matched_type = _; cases }) -> begin
      let free_variables_in_cases =
        let free_variables_in_case field_binders statement =
          let field_binders =
            Identifier.Set.of_list field_binders
          and statement_free_variables =
            free_variables statement
          in
          Identifier.Set.diff statement_free_variables field_binders
        in
        Identifier.Set.unions begin
          List.map ~f:(Fn.uncurry free_variables_in_case) @@ Identifier.Map.data cases
        end
      in
      Identifier.Set.unions [
        Identifier.Set.singleton matched;
        free_variables_in_cases
      ]
    end

  | Expression expression -> Expression.free_variables expression

  | Call (function_identifier, expressions) -> begin
      Identifier.Set.unions [
        Identifier.Set.singleton function_identifier; (* todo probably should not be included, depends on whether functions are first class citizens *)
        Identifier.Set.unions @@ List.map ~f:Expression.free_variables expressions;
      ]
    end

  | Let { binder; binding_statement_type = _; binding_statement; body_statement } -> begin
      Identifier.Set.unions [
        free_variables binding_statement;
        Identifier.Set.diff
          (free_variables body_statement)
          (Identifier.Set.singleton binder)
      ]
    end

  | DestructureRecord { record_type_identifier = _; field_identifiers = _; binders; destructured_record; body } -> begin
      Identifier.Set.unions [
        free_variables destructured_record;
        Identifier.Set.diff
          (free_variables body)
          (Identifier.Set.of_list binders)
      ]
    end

  | Seq (left, right)                                    -> Identifier.Set.union (free_variables left) (free_variables right)
  | ReadRegister identifier                              -> Identifier.Set.singleton identifier
  | WriteRegister { register_identifier; written_value } -> Identifier.Set.of_list [ register_identifier; written_value ] (* todo check inclusion of register_identifier *)
  | Cast (statement, _)                                  -> free_variables statement
  | Fail _                                               -> Identifier.Set.empty


let substitute_numeric_expression_identifier
    (substitution : Identifier.t -> Numeric.Expression.t)
    (statement    : t                                   ) : t
  =
  let typsubst  = Type.substitute_numeric_expression_identifier substitution
  and exprsubst = Expression.substitute_numeric_expression_identifier substitution
  in
  let rec subst (statement : t) : t =
    match statement with
    | Match (MatchList { matched; element_type; when_cons = (head, tail, when_cons); when_nil }) -> begin
        Match begin
          MatchList {
            matched;
            element_type = typsubst element_type;
            when_cons    = (head, tail, subst when_cons);
            when_nil     = subst when_nil
          }
        end
      end
    | Match (MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body }) -> begin
        Match begin
          MatchProduct {
            matched;
            type_fst = typsubst type_fst;
            type_snd = typsubst type_snd;
            id_fst;
            id_snd;
            body     = subst body;
          }
        end
      end
    | Match (MatchTuple { matched; binders; body }) -> begin
        Match begin
          MatchTuple {
            matched;
            binders = List.map ~f:(fun (id, t) -> (id, typsubst t)) binders;
            body    = subst body;
          }
        end
      end
    | Match (MatchBool { condition; when_true; when_false }) -> begin
        Match begin
          MatchBool {
            condition;
            when_true  = subst when_true;
            when_false = subst when_false;
          }
        end
      end
    | Match (MatchEnum { matched; matched_type; cases }) -> begin
        Match begin
          MatchEnum {
            matched;
            matched_type;
            cases = Identifier.Map.map_values cases ~f:subst
          }
        end
      end
    | Match (MatchVariant { matched; matched_type; cases }) -> begin
        Match begin
          MatchVariant {
            matched;
            matched_type;
            cases = Identifier.Map.map_values cases ~f:(fun (ids, body) -> (ids, subst body))
          }
        end
      end
    | Let { binder; binding_statement_type; binding_statement; body_statement } -> begin
        Let {
          binder;
          binding_statement_type = typsubst binding_statement_type;
          binding_statement = subst binding_statement;
          body_statement = subst body_statement
        }
      end
    | DestructureRecord { record_type_identifier; field_identifiers; binders; destructured_record; body } -> begin
        DestructureRecord {
          record_type_identifier;
          field_identifiers;
          binders;
          destructured_record = subst destructured_record;
          body = subst body;
        }
      end
    | Expression expression      -> Expression (exprsubst expression)
    | Call (receiver, arguments) -> Call (receiver, List.map ~f:exprsubst arguments)
    | Seq (left, right)          -> Seq (subst left, subst right)
    | Cast (statement, typ)      -> Cast (subst statement, typsubst typ)
    | ReadRegister _             -> statement
    | WriteRegister _            -> statement
    | Fail _                     -> statement
  in
  subst statement


let rec simplify (statement : t) : t =
  match (statement : t) with
  | Match (MatchList { matched; element_type; when_cons = (head_identifier, tail_identifier, when_cons); when_nil }) -> begin
      Match begin
        MatchList {
          matched;
          element_type = Type.simplify element_type;
          when_cons    = (head_identifier, tail_identifier, simplify when_cons);
          when_nil     = simplify when_nil;
        }
      end
    end

  | Match (MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body }) -> begin
      Match begin
        MatchProduct {
          matched;
          type_fst = Type.simplify type_fst;
          type_snd = Type.simplify type_snd;
          id_fst;
          id_snd;
          body     = simplify body;
        }
      end
    end

  | Match (MatchTuple { matched; binders; body }) -> begin
      Match begin
        MatchTuple {
          matched;
          binders = List.map ~f:(fun (id, t) -> (id, Type.simplify t)) binders;
          body    = simplify body;
        }
      end
    end

  | Match (MatchBool { condition; when_true; when_false }) -> begin
      Match begin
        MatchBool {
          condition;
          when_true  = simplify when_true;
          when_false = simplify when_false;
        }
      end
    end

  | Match (MatchEnum { matched; matched_type; cases }) -> begin
      Match begin
        MatchEnum {
          matched;
          matched_type;
          cases = Identifier.Map.map_values ~f:simplify cases;
        }
      end
    end

  | Match (MatchVariant { matched; matched_type; cases }) -> begin
      Match begin
        MatchVariant {
          matched;
          matched_type;
          cases = Identifier.Map.map_values ~f:(fun (ids, stm) -> (ids, simplify stm)) cases;
        }
      end
    end

  | Let { binder; binding_statement_type; binding_statement; body_statement } -> begin
      let binding_statement_type = Type.simplify binding_statement_type
      and binding_statement      = simplify binding_statement
      and body_statement         = simplify body_statement
      in
      if
        Identifier.Set.mem (free_variables body_statement) binder
      then
        Let {
          binder;
          binding_statement_type;
          binding_statement;
          body_statement;
        }
      else
        simplify @@ Seq (binding_statement, body_statement)
    end

  | DestructureRecord { record_type_identifier; field_identifiers; binders; destructured_record; body } -> begin
      DestructureRecord {
        record_type_identifier;
        field_identifiers;
        binders;
        destructured_record = simplify destructured_record;
        body                = simplify body;
      }
    end

  | Seq (left, right) -> begin
      let left  = simplify left
      and right = simplify right
      in
      match left, right with
      | Expression (Value Unit)                 , _ -> right
      | Expression (Variable (_, Type.Unit)), _ -> right
      | _                                           -> Seq (left, right)
    end

  | Expression expression      -> Expression (Expression.simplify expression)
  | Call (receiver, arguments) -> Call (receiver, List.map ~f:Expression.simplify arguments)
  | ReadRegister _             -> statement
  | WriteRegister _            -> statement
  | Cast (statement, typ)      -> Cast (simplify statement, Type.simplify typ)
  | Fail _                     -> statement


class virtual rewriter =
  object
    method virtual rewrite : t -> t
    method virtual rewrite_match : pattern : match_pattern -> t
    method virtual rewrite_match_list : matched : Identifier.t -> element_type : Type.t -> when_cons : (Identifier.t * Identifier.t * t) -> when_nil : t -> t
    method virtual rewrite_call : receiver : Identifier.t -> arguments : Expression.t list -> t
    method virtual rewrite_cast : statement : t -> cast_to : Type.t -> t
    method virtual rewrite_destructure_record : record_type_identifier : Identifier.t -> field_identifiers : Identifier.t list -> binders : Identifier.t list -> destructured_record : t -> body : t -> t
    method virtual private rewrite_expr : Expression.t -> Expression.t
    method virtual rewrite_expression : expression : Expression.t -> t
    method virtual rewrite_fail : typ : Type.t -> message : string -> t
    method virtual rewrite_let : binder : Identifier.t -> binding_statement_type : Type.t -> binding_statement : t -> body_statement : t -> t
    method virtual rewrite_match : pattern : match_pattern -> t
    method virtual rewrite_match_bool : condition : Identifier.t -> when_true : t -> when_false : t -> t
    method virtual rewrite_match_enum : matched : Identifier.t -> matched_type : Identifier.t -> cases : t Identifier.Map.t -> t
    method virtual rewrite_match_list : matched : Identifier.t -> element_type : Type.t -> when_cons : Identifier.t * Identifier.t * t -> when_nil : t -> t
    method virtual rewrite_match_product : matched : Identifier.t -> type_fst : Type.t -> type_snd : Type.t -> id_fst : Identifier.t -> id_snd : Identifier.t -> body : t -> t
    method virtual rewrite_match_tuple : matched : Identifier.t -> binders : (Identifier.t * Type.t) list -> body : t -> t
    method virtual rewrite_match_variant : matched : Identifier.t -> matched_type : Identifier.t -> cases : (Identifier.t list * t) Identifier.Map.t -> t
    method virtual rewrite_read_register : register : Identifier.t -> t
    method virtual rewrite_seq : left : t -> right : t -> t
    method virtual rewrite_write_register : register_identifier : Identifier.t -> written_value : Identifier.t -> t
  end


class identity_rewriter =
  object(self)
    inherit rewriter

    method rewrite (statement : t) : t =
      match statement with
      | Match pattern -> self#rewrite_match ~pattern
      | Expression expression -> self#rewrite_expression ~expression
      | Call (receiver, arguments) -> self#rewrite_call ~receiver ~arguments
      | Let { binder; binding_statement_type; binding_statement; body_statement } -> self#rewrite_let ~binder ~binding_statement_type ~binding_statement ~body_statement
      | DestructureRecord { record_type_identifier; field_identifiers; binders; destructured_record; body } -> self#rewrite_destructure_record ~record_type_identifier ~field_identifiers ~binders ~destructured_record ~body
      | Seq (left, right) -> self#rewrite_seq ~left ~right
      | ReadRegister register -> self#rewrite_read_register ~register
      | WriteRegister { register_identifier; written_value } -> self#rewrite_write_register ~register_identifier ~written_value
      | Cast (statement, cast_to) -> self#rewrite_cast ~statement ~cast_to
      | Fail (typ, message) -> self#rewrite_fail ~typ ~message
    
    method rewrite_match ~(pattern : match_pattern) : t =
      match pattern with
      | MatchList { matched; element_type; when_cons; when_nil } -> self#rewrite_match_list ~matched ~element_type ~when_cons ~when_nil
      | MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body } -> self#rewrite_match_product ~matched ~type_fst ~type_snd ~id_fst ~id_snd ~body
      | MatchTuple { matched; binders; body } -> self#rewrite_match_tuple ~matched ~binders ~body
      | MatchBool { condition; when_true; when_false } -> self#rewrite_match_bool ~condition ~when_true ~when_false
      | MatchEnum { matched; matched_type; cases } -> self#rewrite_match_enum ~matched ~matched_type ~cases
      | MatchVariant { matched; matched_type; cases } -> self#rewrite_match_variant ~matched ~matched_type ~cases

    method rewrite_match_list
        ~(matched : Identifier.t)
        ~(element_type : Type.t)
        ~(when_cons : Identifier.t * Identifier.t * t)
        ~(when_nil : t) : t
      =
      let (id_head, id_tail, when_cons) = when_cons
      in
      Match begin
        MatchList {
          matched;
          element_type;
          when_cons = (id_head, id_tail, self#rewrite when_cons);
          when_nil = self#rewrite when_nil
        }
      end

    method rewrite_match_product ~matched ~type_fst ~type_snd ~id_fst ~id_snd ~body =
      Match begin
        MatchProduct {
          matched;
          type_fst;
          type_snd;
          id_fst;
          id_snd;
          body = self#rewrite body;
        }
      end

    method rewrite_match_tuple ~matched ~binders ~body =
      Match begin
        MatchTuple {
          matched;
          binders;
          body = self#rewrite body;
        }
      end

    method rewrite_match_bool ~condition ~when_true ~when_false =
      Match begin
        MatchBool {
          condition;
          when_true = self#rewrite when_true;
          when_false = self#rewrite when_false;
        }
      end

    method rewrite_match_enum ~matched ~matched_type ~cases =
      Match begin
        MatchEnum {
          matched;
          matched_type;
          cases = Identifier.Map.map ~f:self#rewrite cases
        }
      end

    method rewrite_match_variant ~matched ~matched_type ~cases =
      Match begin
        MatchVariant {
          matched;
          matched_type;
          cases = Identifier.Map.map_values ~f:(fun (id, stm) -> (id, self#rewrite stm)) cases
        }
      end

    method rewrite_expression ~expression =
      Expression (self#rewrite_expr expression)

    method rewrite_call ~receiver ~arguments =
      Call (receiver, List.map ~f:self#rewrite_expr arguments)

    method rewrite_destructure_record ~record_type_identifier ~field_identifiers ~binders ~destructured_record ~body =
      DestructureRecord {
        record_type_identifier;
        field_identifiers;
        binders;
        destructured_record = self#rewrite destructured_record;
        body = self#rewrite body;
      }        

    method rewrite_let ~binder ~binding_statement_type ~binding_statement ~body_statement =
      Let {
        binder;
        binding_statement_type;
        binding_statement = self#rewrite binding_statement;
        body_statement = self#rewrite body_statement;
      }

    method rewrite_seq ~left ~right =
      Seq (self#rewrite left, self#rewrite right)

    method rewrite_read_register ~register =
      ReadRegister register

    method rewrite_write_register ~register_identifier ~written_value =
      WriteRegister {
        register_identifier;
        written_value
      }

    method rewrite_cast ~statement ~cast_to =
      Cast (self#rewrite statement, cast_to)

    method rewrite_fail ~typ ~message =
      Fail (typ, message)

    method private rewrite_expr expression : Expression.t =
      failwith "not yet implemented" (* todo!! *)
  end

