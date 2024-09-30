open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_infix_binary_operation (binary_operator : Ast.BinaryOperator.t) : PP.document GC.t =
  match binary_operator with
  | Plus                 -> GC.return Coq.Operator.addition
  | Minus                -> GC.return Coq.Operator.subtraction
  | Times                -> GC.return Coq.Operator.multiplication
  | And                  -> GC.return Coq.Operator.conjunction
  | Or                   -> GC.return Coq.Operator.disjunction
  | EqualTo              -> GC.return Coq.Operator.equality
  | NotEqualTo           -> GC.return Coq.Operator.inequality
  | LessThan             -> GC.return Coq.Operator.less_than
  | GreaterThan          -> GC.return Coq.Operator.greater_than
  | LessThanOrEqualTo    -> GC.return Coq.Operator.less_than_or_equal_to
  | GreaterThanOrEqualTo -> GC.return Coq.Operator.greater_than_or_equals_to
  | Pair                 -> GC.not_yet_implemented [%here] (* Should not occur *) (* use fail *)
  | Cons                 -> GC.not_yet_implemented [%here] (* Should not occur *)
  | Append               -> GC.not_yet_implemented [%here] (* Should not occur *)


let rec pp_expression (expression : Ast.Expression.t) : PP.document GC.t =
  let rec pp_value (value : Ast.Value.t) : PP.document GC.t =
    match value with
    | Bool true        -> GC.return @@ PP.string "exp_true"
    | Bool false       -> GC.return @@ PP.string "exp_false"
    | Int n            -> GC.return @@ PP.(simple_app [string "exp_int"; Coq.pp_integer n])
    | String s         -> GC.return @@ PP.(simple_app [string "exp_string"; dquotes (string s)])
    | Unit             -> GC.return @@ PP.(simple_app [string "exp_val"; string "ty.unit"; string "tt"])
    | Prod (_, _) as v -> begin
        let* pp_tuple_type = Nanotype.pp_nanotype (Ast.Value.type_of_value v)
        in
        let* pp_value' = pp_value v
        in
        GC.return @@ PP.simple_app [
          PP.string "exp_val";
          pp_tuple_type;
          pp_value'
        ]
      end
      
  and pp_binary_operation
      (binary_operator : Ast.BinaryOperator.t)
      (left_operand    : Ast.Expression.t    )
      (right_operand   : Ast.Expression.t    ) : PP.document GC.t
    =
    let* pp_left_operand  = GC.lift ~f:PP.parens @@ pp_expression left_operand
    and* pp_right_operand = GC.lift ~f:PP.parens @@ pp_expression right_operand
    in
    let pp id =
      GC.return @@ PP.(simple_app [
         string "exp_binop";
         string id;
         pp_left_operand;
         pp_right_operand
       ])
    in
    match binary_operator with
    | Pair   -> pp "bop.pair"
    | Cons   -> pp "bop.cons"
    | Append -> pp "bop.append"
    | _      -> begin
        let* binop' = pp_infix_binary_operation binary_operator
        in
        GC.return begin
          PP.horizontal [
            PP.parens pp_left_operand;
            binop';
            PP.parens pp_right_operand;
          ]
        end
      end

  and pp_variable (identifier : Ast.Identifier.t) : PP.document GC.t =
    GC.return @@ PPSail.pp_expression_of_identifier @@ Identifier.pp identifier

  and pp_negation (operand : Ast.Expression.t) : PP.document GC.t =
    let* pp_operand = GC.lift ~f:PP.parens @@ pp_expression operand
    in
    GC.return @@ PP.(separate space [minus; pp_operand])

  and pp_logical_negation (operand : Ast.Expression.t) : PP.document GC.t =
    let* pp_operand = GC.lift ~f:PP.parens @@ pp_expression operand
    in
    GC.return @@ PP.(simple_app [string "exp_not"; pp_operand])

  and pp_list (elements : Ast.Expression.t list) : PP.document GC.t =
    let rec pp_list_using_cons expressions =
      match expressions with
      | []           -> GC.return @@ PP.string "nil"
      | head :: tail -> begin
          let* pp_head = GC.lift ~f:PP.parens @@ pp_expression head
          and* pp_tail = pp_list_using_cons tail
          in
          GC.return @@ PP.(parens @@ simple_app [string "cons"; pp_head; pp_tail])
        end
    in
    let* pp_elements =
      if
        Configuration.(get use_list_notations)
      then
        let* expressions = GC.map ~f:pp_expression elements
        in
        GC.return @@ Coq.pp_list expressions
      else
        pp_list_using_cons elements
    in
    GC.return @@ PP.(simple_app [string "exp_list"; pp_elements])

  and pp_record
      (type_identifier      : Ast.Identifier.t     )
      (variable_identifiers : Ast.Identifier.t list) : PP.document GC.t
    =
    GC.return @@ PP.(simple_app [
        string "exp_record";
        Identifier.pp type_identifier;
        Coq.pp_list begin
          List.map variable_identifiers ~f:(fun id -> simple_app [ string "exp_var"; Identifier.pp id ])
        end
      ])

  in
  match expression with
  | Variable identifier -> pp_variable identifier
  | Val value           -> pp_value value
  | Neg expression      -> pp_negation expression
  | Not expression      -> pp_logical_negation expression
  | Binop (op, e1, e2)  -> pp_binary_operation op e1 e2
  | List elements       -> pp_list elements
  | Record { type_identifier; variable_identifiers } -> pp_record type_identifier variable_identifiers
  | Enum args -> begin
      let enum_type =
        PP.simple_app [ PP.string "ty.enum"; Identifier.pp @@ Configuration.reified_enum_name args.type_identifier ]
      and enum_constructor =
        Identifier.pp args.constructor_identifier
      in
      GC.return @@ PPSail.pp_expression_value enum_type enum_constructor
    end
