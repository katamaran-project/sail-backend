open! ExtBase


let rec simplify_statement (statement : Ast.Statement.t) : Ast.Statement.t =
  match statement with
  | Match (MatchList { matched; element_type; when_cons = (head, tail, body); when_nil }) -> begin
      Match begin
        MatchList {
          matched;
          element_type;
          when_cons = (head, tail, simplify_statement body);
          when_nil = simplify_statement when_nil;
        }
      end
    end

  | Match (MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body }) -> begin
      Match begin
        MatchProduct {
          matched;
          type_fst;
          type_snd;
          id_fst;
          id_snd;
          body = simplify_statement body;
        }
      end
    end

  | Match (MatchTuple { matched; binders; body }) -> begin
      Match begin
        MatchTuple {
          matched;
          binders;
          body = simplify_statement body;
        }
      end
    end

  | Match (MatchBool { condition; when_true; when_false }) -> begin
      Match begin
        MatchBool {
          condition;
          when_true = simplify_statement when_true;
          when_false = simplify_statement when_false;
        }
      end
    end

  | Match (MatchEnum { matched; matched_type; cases }) -> begin
      Match begin
        MatchEnum {
          matched;
          matched_type;
          cases = Ast.Identifier.Map.map_values ~f:simplify_statement cases
        }
      end
    end

  | Match (MatchVariant { matched; matched_type; cases }) -> begin
      Match begin
        MatchVariant {
          matched;
          matched_type;
          cases = Ast.Identifier.Map.map_values ~f:(fun (ids, stm) -> (ids, simplify_statement stm)) cases;
        }
      end
    end

  | Let { variable_identifier; binding_statement_type; binding_statement; body_statement } -> begin
      let body_statement =
        simplify_statement body_statement
      in
      let binding_statement =
        simplify_statement binding_statement
      in
      let free_variables_in_body =
        Ast.Statement.free_variables body_statement
      in
      if
        Ast.Identifier.Set.mem free_variables_in_body variable_identifier
      then
        Let {
          variable_identifier;
          binding_statement_type;
          binding_statement;
          body_statement;
        }
      else
        simplify_statement @@ Seq (binding_statement, body_statement)
    end

  | DestructureRecord { record_type_identifier; field_identifiers; variable_identifiers; destructured_record; body } -> begin
      DestructureRecord {
        record_type_identifier;
        field_identifiers;
        variable_identifiers;
        destructured_record = simplify_statement destructured_record;
        body = simplify_statement body;
      }
    end

  | Seq (left, right) -> begin
      (*
         a; () should not be simplified to a, since it changes the return type.
      *)
      let simplified_left  = simplify_statement left
      and simplified_right = simplify_statement right
      in
      match simplified_left, simplified_right with
      | Expression (Val Unit)                   , _ -> simplified_right
      | Expression (Variable (_, Ast.Type.Unit)), _ -> simplified_right
      | _                                           -> Seq (simplified_left, simplified_right)
    end

  | Call (identifier, arguments) -> Call (identifier, List.map ~f:simplify_expression arguments)
  | Expression expression        -> Expression (simplify_expression expression)
  | Cast (statement, typ)        -> Cast (simplify_statement statement, typ)
  | ReadRegister _               -> statement
  | WriteRegister _              -> statement
  | Fail _                       -> statement


and simplify_expression (expression : Ast.Expression.t) : Ast.Expression.t =
  expression
