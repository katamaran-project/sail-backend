open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


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


let rec ty_of_val (value : Ast.Value.t) : Ast.Type.t =
  match value with
  | Unit          -> Unit
  | Bool _        -> Bool
  | Int _         -> Int
  | String _      -> String
  | Prod (v1, v2) -> Tuple [ty_of_val v1; ty_of_val v2]


let rec pp_value (value : Ast.Value.t) : PP.document =
  match value with
  | Unit          -> PP.string "tt"
  | Bool b        -> PP.string (Bool.to_string b)
  | Int i         -> Coq.integer i
  | String s      -> PP.(dquotes @@ string s)
  | Prod (v1, v2) -> Coq.product (pp_value v1) (pp_value v2)


let rec pp_expression (expression : Ast.Expression.t) : PP.document GC.t =
  let rec pp_list expressions =
    match expressions with
    | []      -> GC.return @@ PP.string "nil"
    | x :: xs -> begin
        let* x'  = pp_par_expression x
        and* xs' = pp_list xs
        in
        GC.return @@ PP.(parens @@ simple_app [string "cons"; x'; xs'])
      end
  in
  let pp_value (value : Ast.Value.t) : PP.document GC.t =
    match value with
    | Bool true        -> GC.return @@ PP.string "exp_true"
    | Bool false       -> GC.return @@ PP.string "exp_false"
    | Int n            -> GC.return @@ PP.(simple_app [string "exp_int"; Coq.integer n])
    | String s         -> GC.return @@ PP.(simple_app [string "exp_string"; dquotes (string s)])
    | Unit             -> GC.return @@ PP.(simple_app [string "exp_val"; string "ty.unit"; string "tt"])
    | Prod (_, _) as v -> begin
        let* tuple_type' = Nanotype.pp_nanotype' (ty_of_val v)
        in
        let value' = pp_value v
        in
        GC.return @@ PP.simple_app [
          PP.string "exp_val";
          tuple_type';
          value'
        ]
      end
  in
  let pp_binary_operation
      (binary_operator : Ast.BinaryOperator.t)
      (e1              : Ast.Expression.t    )
      (e2              : Ast.Expression.t    ) : PP.document GC.t
    =
    let* e1' = pp_par_expression e1
    and* e2' = pp_par_expression e2
    in
    let pp id =
      GC.return @@ PP.(simple_app [
         string "exp_binop";
         string id;
         e1';
         e2'
       ])
    in
    match binary_operator with
    | Pair   -> pp "bop.pair"
    | Cons   -> pp "bop.cons"
    | Append -> pp "bop.append"
    | _      -> begin
        let* binop' = pp_infix_binary_operation binary_operator
        in
        GC.return @@ PP.infix 2 1 binop' e1' e2'
      end
  in
  match expression with
  | Var v              -> GC.return PP.(simple_app [string "exp_var"; dquotes (Identifier.pp v)])
  | Val v              -> pp_value v
  | Neg e              -> let* e' = pp_par_expression e in GC.return @@ PP.(string "- " ^^ e')
  | Not e              -> let* e' = pp_par_expression e in GC.return @@ PP.(simple_app [string "exp_not"; e'])
  | Binop (bo, e1, e2) -> pp_binary_operation bo e1 e2
  | List lst           -> begin
      let* lst' =
        if
          Configuration.(get use_list_notations)
        then
          let* expressions = GC.map ~f:pp_expression lst
          in
          GC.return @@ Coq.list expressions
        else
          pp_list lst
      in
      GC.return @@ PP.(simple_app [string "exp_list"; lst'])
    end
  | Record { type_identifier; variable_identifiers } -> begin
      GC.return @@ PP.(simple_app [
                       string "exp_record";
                       Identifier.pp type_identifier;
                       Coq.list @@ List.map
                                     variable_identifiers
                                     ~f:(fun id -> simple_app [ string "exp_var"; Identifier.pp id ])
                     ])
    end
  | Enum identifier -> GC.return @@ Identifier.pp identifier

and pp_par_expression (expression : Ast.Expression.t) : PP.document GC.t =
  let* expression' = pp_expression expression
  in
  GC.return @@ PP.parens expression'
