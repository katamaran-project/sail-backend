type signedness =
  | Signed
  | Unsigned


type bitvector_conversion =
  | Conversion of signedness * Ast.Expression.t
  | SomethingElse


let string_of_signedness (signedness : signedness) =
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
    (left_signedness  : signedness          )
    (right_signedness : signedness          )
    (binary_operator  : Ast.BinaryOperator.t) : Ast.BinaryOperator.t option
  =
  match left_signedness, right_signedness, binary_operator with
  | Signed, Signed, Ast.BinaryOperator.LessThan          -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signed, Ast.BinaryOperator.Comparison.LessThan))
  | Signed, Signed, Ast.BinaryOperator.LessThanOrEqualTo -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signed, Ast.BinaryOperator.Comparison.LessThanOrEqualTo))
  | Signed, Signed, Ast.BinaryOperator.GreaterThan -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signed, Ast.BinaryOperator.Comparison.GreaterThan))
  | Signed, Signed, Ast.BinaryOperator.GreaterThanOrEqualTo -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Signed, Ast.BinaryOperator.Comparison.GreaterThanOrEqualTo))
  | Unsigned, Unsigned, Ast.BinaryOperator.LessThan          -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Unsigned, Ast.BinaryOperator.Comparison.LessThan))
  | Unsigned, Unsigned, Ast.BinaryOperator.LessThanOrEqualTo -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Unsigned, Ast.BinaryOperator.Comparison.LessThanOrEqualTo))
  | Unsigned, Unsigned, Ast.BinaryOperator.GreaterThan -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Unsigned, Ast.BinaryOperator.Comparison.GreaterThan))
  | Unsigned, Unsigned, Ast.BinaryOperator.GreaterThanOrEqualTo -> Some (Ast.BinaryOperator.BitvectorComparison (Ast.BinaryOperator.Unsigned, Ast.BinaryOperator.Comparison.GreaterThanOrEqualTo))
  | _ -> begin
      Logging.warning @@ lazy "encountered mixed signed/unsigned comparison; ignoring it";
      None
    end


let rec rewrite (statement : Ast.Statement.t) : Ast.Statement.t =
  match statement with
  | Ast.Statement.Let { variable_identifier; binding_statement_type; binding_statement; body_statement } -> begin
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
          | Ast.Statement.Expression (Ast.Expression.BinaryOperation (binary_operator, Variable v1, Variable v2)) -> begin
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
  | Ast.Statement.Match _ -> statement
  | Ast.Statement.Expression _ -> statement
  | Ast.Statement.Call (_, _) -> statement
  | Ast.Statement.DestructureRecord _ -> statement
  | Ast.Statement.Seq (_, _) -> statement
  | Ast.Statement.ReadRegister _ -> statement
  | Ast.Statement.WriteRegister _ -> statement
  | Ast.Statement.Cast (_, _) -> statement
  | Ast.Statement.Fail _ -> statement
