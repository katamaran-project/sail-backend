open ExtBase


module Context = struct
  type t = {
    renamer  : Identifier.t -> Identifier.t;
    binders  : Identifier.t list;
  }

  let renamer =
    let get (context : t) : Identifier.t -> Identifier.t =
      context.renamer
    and set
        (context : t                           )
        (renamer : Identifier.t -> Identifier.t) : t
      =
      { context with renamer }
    in
    (get, set)

  let binders =
    let get (context : t) : Identifier.t list =
      context.binders
    and set
        (context : t                )
        (binders : Identifier.t list) : t
      =
      { context with binders }
    in
    (get, set)
end

module Monad = Monads.ComponentState.Make(Context)
open Monads.Notations.Star(Monad)

include Monads.Util.Make(Monad)


let return = Monad.return


let create_context (renamer : Identifier.t -> Identifier.t) : Context.t =
  {
    renamer;
    binders = []
  }


let with_binders
    (additional_binders : Identifier.t list)
    (f                  : 'a Monad.t       ) : 'a Monad.t
  =
  let* old_binders = Monad.get Context.binders
  in
  let* ()          = Monad.put Context.binders @@ List.append additional_binders old_binders
  in
  let* result      = f
  in
  let* ()          = Monad.put Context.binders old_binders
  in
  return result


let rename (identifier : Identifier.t) : Identifier.t Monad.t =
  let* renamer = Monad.get Context.renamer
  and* binders = Monad.get Context.binders
  in
  if
    List.mem ~equal:Identifier.equal binders identifier
  then
    return @@ identifier
  else
    return @@ renamer identifier


let rec rename_in_expression (expression : Expression.t) : Expression.t Monad.t
  =
  match expression with
  | Variable (identifier, typ) -> begin
      let* identifier = rename identifier
      in
      return @@ Expression.Variable (identifier, typ)
    end
    
  | List elements -> begin
      let* elements =
        map ~f:rename_in_expression elements
      in
      return begin
        Expression.List elements
      end
    end
    
  | UnaryOperation (operator, operand) -> begin
      let* operand = rename_in_expression operand
      in
      return @@ Expression.UnaryOperation (operator, operand)
    end
    
  | BinaryOperation (operator, left_operand, right_operand) -> begin
      let* left_operand  = rename_in_expression left_operand
      and* right_operand = rename_in_expression right_operand
      in
      return @@ Expression.BinaryOperation (operator, left_operand, right_operand)
    end
    
  | Record { type_identifier; fields } -> begin
      let* fields = map ~f:rename fields
      in
      return begin
        Expression.Record {
          type_identifier;
          fields;
        }
      end
    end
    
  | Variant { type_identifier; constructor_identifier; fields } -> begin
      let* fields = map ~f:rename_in_expression fields
      in
      return begin
        Expression.Variant {
          type_identifier;
          constructor_identifier;
          fields;
        }
      end
    end
    
  | Tuple elements -> begin
      let* elements = map ~f:rename_in_expression elements
      in
      return @@ Expression.Tuple elements
    end
    
  | Bitvector elements -> begin
      let* elements = map ~f:rename_in_expression elements
      in
      return @@ Expression.Bitvector elements
    end

  | Val _  -> return expression
  | Enum _ -> return expression


let rec rename_in_statement (statement : Statement.t) : Statement.t Monad.t =
  match (statement : Statement.t) with
  | Match (MatchList { matched; element_type; when_cons = (head_identifier, tail_identifier, when_cons); when_nil }) -> begin
      let* matched   = rename matched
      and* when_cons = with_binders [head_identifier; tail_identifier] @@ rename_in_statement when_cons
      and* when_nil  = rename_in_statement when_nil
      in
      return begin
        Statement.Match begin
          Statement.MatchList {
            matched;
            element_type;
            when_cons = (
              head_identifier,
              tail_identifier,
              when_cons
            );
            when_nil;
          }
        end
      end
    end

  | Match (MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body }) -> begin
      let* matched = rename matched
      and* body    = with_binders [id_fst; id_snd] @@ rename_in_statement body
      in
      return begin
        Statement.Match begin
          Statement.MatchProduct {
            matched;
            type_fst;
            type_snd;
            id_fst;
            id_snd;
            body
          }
        end
      end
    end

  | Match (MatchTuple { matched; binders; body }) -> begin
      let* matched =
        rename matched
      and* body =
        let binder_identifiers =
          List.map ~f:fst binders
        in
        with_binders
          binder_identifiers
          (rename_in_statement body)
      in
      return begin
        Statement.Match begin
          Statement.MatchTuple {
            matched;
            binders;
            body;
          }
        end
      end
    end

  | Match (MatchBool { condition; when_true; when_false }) -> begin
      let* condition  = rename condition
      and* when_true  = rename_in_statement when_true
      and* when_false = rename_in_statement when_false
      in
      return begin
        Statement.Match begin
          Statement.MatchBool {
            condition;
            when_true;
            when_false;
          }
        end
      end
    end

  | Match (MatchEnum { matched; matched_type; cases }) -> begin
      let* matched =
        rename matched
      in
      let* cases =
        let convert_case (identifier, statement) =
          let* statement = rename_in_statement statement
          in
          return (identifier, statement)
        in
        let case_pairs =
          Identifier.Map.to_alist cases
        in
        let* converted_case_pairs =
          map ~f:convert_case case_pairs
        in
        return begin          
          Identifier.Map.of_alist_exn converted_case_pairs
        end
      in
      return begin
        Statement.Match begin
          Statement.MatchEnum {
            matched;
            matched_type;
            cases;
          }
        end
      end
    end

  | Match (MatchVariant { matched; matched_type; cases }) -> begin
      let* matched =
        rename matched
      and* cases =
        let case_pairs =
          Identifier.Map.to_alist cases
        in
        let convert_case (case_identifier, (binders, statement)) =
          let* statement =
            with_binders
              binders
              (rename_in_statement statement)
          in
          return (case_identifier, (binders, statement))
        in
        let* converted_case_pairs =
          map ~f:convert_case case_pairs
        in
        return begin
          Identifier.Map.of_alist_exn converted_case_pairs
        end
      in
      return begin
        Statement.Match begin
          Statement.MatchVariant {
            matched;
            matched_type;
            cases;
          }
        end
      end
    end

  | Expression expression -> begin
      let* expression = rename_in_expression expression
      in
      return begin
        Statement.Expression expression
      end
    end

  | Call (function_identifier, arguments) -> begin
      let* function_identifier = rename function_identifier (* todo check this *)
      and* arguments           = map ~f:rename_in_expression arguments
      in
      return @@ Statement.Call (function_identifier, arguments)
    end

  | Let { variable_identifier; binding_statement_type; binding_statement; body_statement } -> begin
      let* binding_statement = rename_in_statement binding_statement
      and* body_statement    = with_binders [ variable_identifier ] @@ rename_in_statement body_statement
      in
      return begin
        Statement.Let {
          variable_identifier;
          binding_statement_type;
          binding_statement;
          body_statement;
        }
      end
    end

  | DestructureRecord { record_type_identifier; field_identifiers; binders; destructured_record; body } -> begin
      let* destructured_record = rename_in_statement destructured_record
      and* body                = with_binders binders @@ rename_in_statement body
      in
      return begin
        Statement.DestructureRecord {
          record_type_identifier;
          field_identifiers;
          binders;
          destructured_record;
          body;
        }
      end
    end

  | Seq (left, right) -> begin
      let* left  = rename_in_statement left
      and* right = rename_in_statement right
      in
      return @@ Statement.Seq (left, right)
    end

  | WriteRegister { register_identifier; written_value } -> begin
      let* written_value = rename written_value
      in
      return begin
        Statement.WriteRegister {
          register_identifier;
          written_value;
        }
      end
    end

  | Cast (statement, typ) -> begin
      let* statement = rename_in_statement statement
      in
      return begin
        Statement.Cast (statement, typ)
      end
    end

  | ReadRegister _ -> return statement
  | Fail _         -> return statement


let rename_in_statement
    (renamer   : Identifier.t -> Identifier.t)
    (statement : Statement.t                 ) : Statement.t
  =
  let context = create_context renamer
  in
  let result, _ = Monad.run (rename_in_statement statement) context
  in
  result


let rename_in_expression
    (renamer    : Identifier.t -> Identifier.t)
    (expression : Expression.t                ) : Expression.t
  =
  let context = create_context renamer
  in
  let result, _ = Monad.run (rename_in_expression expression) context
  in
  result
