(* Should not be useful anymore *)

module Signedness = Ast.BinaryOperator.Signedness


type bitvector_conversion =
  | Conversion of Signedness.t * Ast.Expression.t
  | SomethingElse


let string_of_signedness (signedness : Signedness.t) =
  match signedness with
  | Signed   -> "signed"
  | Unsigned -> "unsigned"


let identify_bitvector_conversion (statement : Ast.Statement.t) : bitvector_conversion =
  match statement with
  | Ast.Statement.Call (identifier, [argument]) -> begin
      match identifier with
      | Ast.Identifier.Id "signed"   -> Conversion (Signed, argument)
      | Ast.Identifier.Id "unsigned" -> Conversion (Unsigned, argument)
      | _                            -> SomethingElse
    end
  | _                                -> SomethingElse


let convert_comparison_operator
    (left_signedness  : Signedness.t        )
    (right_signedness : Signedness .t       )
    (binary_operator  : Ast.BinaryOperator.t) : Ast.BinaryOperator.t option
  =
  match left_signedness, right_signedness, binary_operator with
  | Signed  , Signed  , StandardComparison LessThan             -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Signed, Ast.BinaryOperator.Comparison.LessThan))
  | Signed  , Signed  , StandardComparison LessThanOrEqualTo    -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Signed, Ast.BinaryOperator.Comparison.LessThanOrEqualTo))
  | Signed  , Signed  , StandardComparison GreaterThan          -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Signed, Ast.BinaryOperator.Comparison.GreaterThan))
  | Signed  , Signed  , StandardComparison GreaterThanOrEqualTo -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Signed, Ast.BinaryOperator.Comparison.GreaterThanOrEqualTo))
  | Unsigned, Unsigned, StandardComparison LessThan             -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Unsigned, Ast.BinaryOperator.Comparison.LessThan))
  | Unsigned, Unsigned, StandardComparison LessThanOrEqualTo    -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Unsigned, Ast.BinaryOperator.Comparison.LessThanOrEqualTo))
  | Unsigned, Unsigned, StandardComparison GreaterThan          -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Unsigned, Ast.BinaryOperator.Comparison.GreaterThan))
  | Unsigned, Unsigned, StandardComparison GreaterThanOrEqualTo -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signedness.Unsigned, Ast.BinaryOperator.Comparison.GreaterThanOrEqualTo))
  | _ -> begin
      Logging.warning @@ lazy "encountered mixed signed/unsigned comparison; ignoring it";
      None
    end


let rec rewrite (statement : Ast.Statement.t) : Ast.Statement.t =
  match statement with
  | Let { variable_identifier; binding_statement_type; binding_statement; body_statement } -> begin
      let pass_through () =
        Ast.Statement.Let {
          variable_identifier;
          binding_statement_type;
          binding_statement = rewrite binding_statement;
          body_statement = rewrite body_statement
        }
      in
      let var1 = variable_identifier
      and val1 = binding_statement
      in
      match body_statement with
      | Ast.Statement.Let { variable_identifier; binding_statement; body_statement; _ } -> begin
          let var2 = variable_identifier
          and val2 = binding_statement
          in
          match body_statement with
          | Ast.Statement.Expression (Ast.Expression.BinaryOperation (binary_operator, Variable (v1, _), Variable (v2, _))) -> begin
              match identify_bitvector_conversion val1, identify_bitvector_conversion val2 with
              | Conversion (left_signedness, expr1), Conversion (right_signedness, expr2) -> begin
                  if
                    Ast.Identifier.equal var1 v1 && Ast.Identifier.equal var2 v2
                  then begin
                    match convert_comparison_operator left_signedness right_signedness binary_operator with
                    | Some operator -> begin
                        Ast.Statement.Expression (Ast.Expression.BinaryOperation (operator, expr1, expr2))
                      end
                    | None -> pass_through ()
                  end
                  else if
                    Ast.Identifier.equal var1 v2 && Ast.Identifier.equal var2 v1
                  then
                    (* todo check this case; may be difficult to write a test for though *)
                    match convert_comparison_operator left_signedness right_signedness binary_operator with
                    | Some operator -> begin
                        Ast.Statement.Expression (Ast.Expression.BinaryOperation (operator, expr2, expr1))
                      end
                    | None -> pass_through ()
                  else
                    pass_through ()
                end
              | _ -> pass_through ()
            end
          | _ -> pass_through ()
        end
      | _ -> pass_through ()
    end
  | Match pattern -> begin
      match pattern with
      | MatchList { matched; when_cons; when_nil } -> begin
          let id1, id2, when_cons_body = when_cons
          in
          Match begin
            MatchList {
              matched = rewrite matched;
              when_cons = (id1, id2, rewrite when_cons_body);
              when_nil = rewrite when_nil;
            }
          end
        end
      | MatchProduct { matched; id_fst; id_snd; body } -> begin
          Match begin
            MatchProduct {
              matched = rewrite matched;
              id_fst;
              id_snd;
              body = rewrite body
            }
          end
        end
      | MatchBool { condition; when_true; when_false } -> begin
          Match begin
            MatchBool {
              condition = rewrite condition;
              when_true = rewrite when_true;
              when_false = rewrite when_false;
            }
          end
        end
      | MatchEnum { matched; matched_type; cases } -> begin
          Match begin
            MatchEnum {
              matched = matched;
              matched_type;
              cases = Ast.Identifier.Map.map_values ~f:rewrite cases
            }
          end
        end
      | MatchVariant { matched; matched_type; cases } -> begin
          Match begin
            MatchVariant {
              matched;
              matched_type;
              cases = Ast.Identifier.Map.map_values ~f:(fun (ks, stm) -> (ks, rewrite stm)) cases
            }
          end
        end
    end
  | Expression _ -> statement
  | Call (_, _) -> statement
  | DestructureRecord { record_type_identifier; field_identifiers; variable_identifiers; destructured_record; body } -> begin
      DestructureRecord {
        record_type_identifier;
        field_identifiers;
        variable_identifiers;
        destructured_record = rewrite destructured_record;
        body = rewrite body;
      }
    end
  | Seq (stm1, stm2) -> begin
      Seq (rewrite stm1, rewrite stm2)
    end
  | ReadRegister _ -> statement
  | WriteRegister _ -> statement
  | Cast (stm, typ) -> Cast (rewrite stm, typ)
  | Fail _ -> statement
