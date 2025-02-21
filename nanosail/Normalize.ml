open! ExtBase


module Implementation = struct
  module Context = struct
    type t = {
      substitutions : Ast.Identifier.t Ast.Identifier.Map.t
    }

    let empty : t =
      {
        substitutions = Ast.Identifier.Map.empty
      }

    let substitutions =
      let get (context : t) : Ast.Identifier.t Ast.Identifier.Map.t =
        context.substitutions
      and set
          (_context      : t                                    )
          (substitutions : Ast.Identifier.t Ast.Identifier.Map.t) : t
        =
        { substitutions }
      in
      (get, set)
  end

  module Monad = Monads.ComponentState.Make(Context)
  open Monads.Notations.Star(Monad)

  include Monads.Util.Make(Monad)


  let return = Monad.return


  let requires_substitution (identifier : Ast.Identifier.t) : bool =
    Ast.Identifier.is_generated identifier


  let substitute_identifier (identifier : Ast.Identifier.t) : Ast.Identifier.t Monad.t =
    if
      not @@ requires_substitution identifier
    then
      Monad.return identifier
    else begin
      let* substitutions = Monad.get Context.substitutions
      in
      match Ast.Identifier.Map.find substitutions identifier with
      | Some identifier' -> return identifier'
      | None -> begin
          let index =
            Ast.Identifier.Map.length substitutions
          in
          let identifier' =
            Ast.Identifier.mk_generated @@ Int.to_string index
          in
          let substitutions' =
            Ast.Identifier.Map.add_exn
              substitutions
              ~key:identifier
              ~data:identifier'
          in
          let* () = Monad.put Context.substitutions substitutions'
          in
          return identifier'
        end
    end


  let rec normalize_expression (expression : Ast.Expression.t) : Ast.Expression.t Monad.t =
    match expression with
    | Variable (identifier, typ) -> begin
        let* identifier = substitute_identifier identifier
        in
        return @@ Ast.Expression.Variable (identifier, typ)
      end

    | Value _ -> return expression

    | List elements -> begin
        let* elements = map ~f:normalize_expression elements
        in
        return @@ Ast.Expression.List elements
      end

    | UnaryOperation (operator, operand) -> begin
        let* operand = normalize_expression operand
        in
        return @@ Ast.Expression.UnaryOperation (operator, operand)
      end

    | BinaryOperation (operator, left_operand, right_operand) -> begin
        let* left_operand  = normalize_expression left_operand
        and* right_operand = normalize_expression right_operand
        in
        return @@ Ast.Expression.BinaryOperation (operator, left_operand, right_operand)
      end

    | Record { type_identifier; fields } -> begin
        let* fields = map ~f:substitute_identifier fields
        in
        return @@ Ast.Expression.Record { type_identifier; fields }
      end

    | Enum _ -> return @@ expression

    | Variant { type_identifier; constructor_identifier; fields } -> begin
        let* fields = map ~f:normalize_expression fields
        in
        return @@ Ast.Expression.Variant { type_identifier; constructor_identifier; fields }
      end

    | Tuple elements -> begin
        let* elements = map ~f:normalize_expression elements
        in
        return @@ Ast.Expression.Tuple elements
      end

    | Bitvector elements -> begin
        let* elements = map ~f:normalize_expression elements
        in
        return @@ Ast.Expression.List elements
      end


  let rec normalize_statement (statement : Ast.Statement.t) : Ast.Statement.t Monad.t =
    match statement with
    | Match (MatchList { matched; element_type; when_cons = (head, tail, when_cons); when_nil }) -> begin
        let* matched   = substitute_identifier matched
        and* head      = substitute_identifier head
        and* tail      = substitute_identifier tail
        and* when_cons = normalize_statement when_cons
        and* when_nil  = normalize_statement when_nil
        in
        return begin
          Ast.Statement.Match begin
            MatchList { matched; element_type; when_cons = (head, tail, when_cons); when_nil }
          end
        end
      end

    | Match (MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body }) -> begin
        let* matched = substitute_identifier matched
        and* id_fst  = substitute_identifier id_fst
        and* id_snd  = substitute_identifier id_snd
        and* body    = normalize_statement body
        in
        return begin
          Ast.Statement.Match begin
            MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body }
          end
        end
      end

    | Match (MatchTuple { matched; binders; body }) -> begin
        let normalize_binders (identifier, typ) =
          let* identifier = substitute_identifier identifier
          in
          return (identifier, typ)
        in
        let* matched = substitute_identifier matched
        and* binders = map binders ~f:normalize_binders
        and* body    = normalize_statement body
        in
        return begin
          Ast.Statement.Match begin
            MatchTuple { matched; binders; body }
          end
        end
      end

    | Match (MatchBool { condition; when_true; when_false }) -> begin
        let* condition  = substitute_identifier condition
        and* when_true  = normalize_statement when_true
        and* when_false = normalize_statement when_false
        in
        return begin
          Ast.Statement.Match begin
            MatchBool { condition; when_true; when_false }
          end
        end
      end

    | Match (MatchEnum { matched; matched_type; cases }) -> begin
        let* matched = substitute_identifier matched
        and* cases   = begin
          let normalize_pair enum_case_identifier statement =
            let* statement = normalize_statement statement
            in
            return (enum_case_identifier, statement)
          in
          let pairs =
            Ast.Identifier.Map.to_alist cases
          in
          let* normalized_pairs =
            map ~f:(Fn.uncurry normalize_pair) pairs
          in
          return begin
            Ast.Identifier.Map.of_alist_exn normalized_pairs
          end
        end
        in
        return begin
          Ast.Statement.Match begin
            Ast.Statement.MatchEnum { matched; matched_type; cases }
          end
        end
      end

    | Match (MatchVariant { matched; matched_type; cases }) -> begin
        let* matched = substitute_identifier matched
        and* cases   = begin
          let normalize_pair variant_case_identifier (binders, statement) =
            let* binders   = map ~f:substitute_identifier binders
            and* statement = normalize_statement statement
            in
            return (variant_case_identifier, (binders, statement))
          in
          let pairs = Ast.Identifier.Map.to_alist cases
          in
          let* normalized_pairs =
            map ~f:(Fn.uncurry normalize_pair) pairs
          in
          return begin
            Ast.Identifier.Map.of_alist_exn normalized_pairs
          end
        end
        in
        return begin
          Ast.Statement.Match begin
            Ast.Statement.MatchVariant { matched; matched_type; cases }
          end
        end
      end

    | Expression expression -> begin
        let* expression = normalize_expression expression
        in
        return @@ Ast.Statement.Expression expression
      end

    | Call (function_identifier, arguments) -> begin
        let* function_identifier = substitute_identifier function_identifier (* todo probably not necessary; check if functions are first class citizens of sail *)
        and* arguments           = map ~f:normalize_expression arguments
        in
        return @@ Ast.Statement.Call (function_identifier, arguments)
      end

    | Let { binder; binding_statement_type; binding_statement; body_statement } -> begin
        let* binder              = substitute_identifier binder
        and* binding_statement   = normalize_statement binding_statement
        and* body_statement      = normalize_statement body_statement
        in
        return @@ Ast.Statement.Let { binder; binding_statement_type; binding_statement; body_statement }
      end

    | DestructureRecord { record_type_identifier; field_identifiers; binders; destructured_record; body } -> begin
        let* binders              = map ~f:substitute_identifier binders
        and* destructured_record  = normalize_statement destructured_record
        and* body                 = normalize_statement body
        in
        return @@ Ast.Statement.DestructureRecord { record_type_identifier; field_identifiers; binders; destructured_record; body }
      end

    | Seq (first, second) -> begin
        let* first  = normalize_statement first
        and* second = normalize_statement second
        in
        return @@ Ast.Statement.Seq (first, second)
      end

    | ReadRegister register_id -> begin
        return @@ Ast.Statement.ReadRegister register_id
      end

    | WriteRegister { register_identifier; written_value } -> begin
        let* written_value = substitute_identifier written_value
        in
        return @@ Ast.Statement.WriteRegister { register_identifier; written_value }
      end

    | Cast (statement, typ) -> begin
        let* statement = normalize_statement statement
        in
        return @@ Ast.Statement.Cast (statement, typ)
      end

    | Fail _ -> return statement


  let rec normalize_pattern_tree (tree : SailToNanosail.Translate.Match.PatternTree.t) : SailToNanosail.Translate.Match.PatternTree.t Monad.t =
    let open SailToNanosail.Translate.Match
    in
    let normalize_binder (binder : Binder.t) : Binder.t Monad.t =
      let* identifier = substitute_identifier binder.identifier
      in
      let wildcard = binder.wildcard
      in
      let binder : Binder.t = { identifier; wildcard }
      in
      return binder
    in
    match tree with
    | Bool { when_true; when_false } -> begin
        let* when_true  = normalize_pattern_tree when_true
        and* when_false = normalize_pattern_tree when_false
        in
        return begin
          PatternTree.Bool { when_true; when_false }
          end
      end

    | Enum { enum_identifier; table } -> begin
        let* table =
          let pairs =
            Ast.Identifier.Map.to_alist table
          in
          let* normalized_pairs =
            let normalize_pair
                (enum_case_identifier : Ast.Identifier.t                      )
                ((binder, subtree)    : Binder.t * PatternTree.t) : (Ast.Identifier.t * (Binder.t * PatternTree.t)) Monad.t =
              let* binder  = normalize_binder binder
              and* subtree = normalize_pattern_tree subtree
              in
              return (enum_case_identifier, (binder, subtree))
            in
            map ~f:(Fn.uncurry normalize_pair) pairs
          in
          return @@ Ast.Identifier.Map.of_alist_exn normalized_pairs
        in
        return @@ PatternTree.Enum { enum_identifier; table }
      end

    | Variant { variant_identifier; table } -> begin
        let* table =
          let pairs =
            Ast.Identifier.Map.to_alist table
          in
          let* normalized_pairs =
            let normalize_pair
                (constructor_identifier           : Ast.Identifier.t                                      )
                ((binder, field_binders, subtree) : Binder.t * PatternTree.variant_binders * PatternTree.t) : (Ast.Identifier.t * (Binder.t * PatternTree.variant_binders * PatternTree.t)) Monad.t
              =
              let* binder        = normalize_binder binder
              and* subtree       = normalize_pattern_tree subtree
              and* field_binders =
                match field_binders with
                | NullaryConstructor field_binder -> begin
                    let* field_binder = normalize_binder field_binder
                    in
                    return @@ PatternTree.NullaryConstructor field_binder
                  end
                | UnaryConstructor field_binder -> begin
                    let* field_binder = normalize_binder field_binder
                    in
                    return @@ PatternTree.UnaryConstructor field_binder
                  end
                | NAryConstructor field_binders -> begin
                    let* field_binders =
                      map ~f:normalize_binder field_binders
                    in
                    return @@ PatternTree.NAryConstructor field_binders
                  end
              in
              return (constructor_identifier, (binder, field_binders, subtree))
            in
            map ~f:(Fn.uncurry normalize_pair) pairs
          in
          return @@ Ast.Identifier.Map.of_alist_exn normalized_pairs
        in
        return @@ PatternTree.Variant { variant_identifier; table }
      end

    | Binder { matched_type; binder; subtree } -> begin
        let* binder  = normalize_binder binder
        and* subtree = normalize_pattern_tree subtree
        in
        return @@ PatternTree.Binder { matched_type; binder; subtree }
      end

    | Leaf statement -> begin
        let* statement =
          match statement with
          | Some statement -> let* statement = normalize_statement statement in return @@ Some statement
          | None           -> return None
        in
        return @@ PatternTree.Leaf statement
      end
end


let normalize_statement (statement : Ast.Statement.t) : Ast.Statement.t =
  let open Implementation
  in
  let (result, _substitutions) =
    Monad.run (normalize_statement statement) Context.empty
  in
  result


let normalize_expression (expression : Ast.Expression.t) : Ast.Expression.t =
  let open Implementation
  in
  let (result, _substitutions) =
    Monad.run (normalize_expression expression) Context.empty
  in
  result


let normalize_pattern_tree (pattern_tree : SailToNanosail.Translate.Match.PatternTree.t) : SailToNanosail.Translate.Match.PatternTree.t =
  let open Implementation
  in
  let (result, _substitutions) =
    Monad.run (normalize_pattern_tree pattern_tree) Context.empty
  in
  result
