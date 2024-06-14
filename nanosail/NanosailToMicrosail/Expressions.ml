open Base
open Monads.Notations.Star(AnnotationContext)
open Identifier

module AC = AnnotationContext


let pp_infix_binOp (binary_operator : Ast.BinaryOperator.t) =
  match binary_operator with
  | Plus                 -> AC.return @@ PP.plus
  | Times                -> AC.return @@ PP.star
  | Minus                -> AC.return @@ PP.minus
  | And                  -> AC.return @@ PP.(twice ampersand)
  | Or                   -> AC.return @@ PP.(twice bar)
  | EqualTo              -> AC.return @@ PP.equals
  | NotEqualTo           -> AC.return @@ PP.(bang ^^ equals)
  | LessThanOrEqualTo    -> AC.return @@ PP.(langle ^^ equals)
  | LessThan             -> AC.return @@ PP.langle
  | GreaterThanOrEqualTo -> AC.return @@ PP.(rangle ^^ equals)
  | GreaterThan          -> AC.return @@ PP.rangle
  | Pair                 -> AC.not_yet_implemented [%here] (* Should not occur *) (* use fail *)
  | Cons                 -> AC.not_yet_implemented [%here] (* Should not occur *)
  | Append               -> AC.not_yet_implemented [%here] (* Should not occur *)


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


let rec pp_expression (e : Ast.Expression.t) =
  let rec pp_exp_list expressions =
    match expressions with
    | []      -> AC.return @@ PP.string "nil"
    | x :: xs ->
      let* x'  = pp_par_expression x
      and* xs' = pp_exp_list xs
      in
      AC.return @@ PP.(parens @@ simple_app [string "cons"; x'; xs'])
  in
  let pp_exp_val (value : Ast.Value.t) =
    match value with
    | Bool true        -> AC.return @@ PP.string "exp_true"
    | Bool false       -> AC.return @@ PP.string "exp_false"
    | Int n            -> AC.return @@ PP.(simple_app [string "exp_int"; Coq.integer n])
    | String s         -> AC.return @@ PP.(simple_app [string "exp_string"; dquotes (string s)])
    | Unit             -> AC.return @@ PP.(simple_app [string "exp_val"; string "ty.unit"; string "tt"])
    | Prod (_, _) as v -> begin
        let* tuple_type' = Nanotype.pp_nanotype (ty_of_val v)
        in
        let value' = pp_value v
        in
        AC.return @@ PP.simple_app [
          PP.string "exp_val";
          tuple_type';
          value'
        ]
      end
  in
  let pp_exp_binop
      (binary_operator : Ast.BinaryOperator.t)
      (e1              : Ast.Expression.t    )
      (e2              : Ast.Expression.t    )
    =
    let* e1' = pp_par_expression e1
    and* e2' = pp_par_expression e2
    in
    match binary_operator with
    | Pair ->
       AC.return @@ PP.(simple_app [
         string "exp_binop";
         string "bop.pair";
         e1';
         e2'
       ])
    | Cons ->
       AC.return @@ PP.(simple_app [
         string "exp_binop";
         string "bop.cons";
         e1';
         e2'
       ])
    | Append ->
       AC.return @@ PP.(simple_app [
         string "exp_binop";
         string "bop.append";
         e1';
         e2'
       ])
    | _  ->
      let* binop' = pp_infix_binOp binary_operator
      in
      AC.return @@ PP.infix 2 1 binop' e1' e2'
  in
  match e with
  | Var v              -> AC.return PP.(simple_app [string "exp_var"; dquotes (pp_identifier v)])
  | Val v              -> pp_exp_val v
  | Neg e              -> let* e' = pp_par_expression e in AC.return @@ PP.(string "- " ^^ e')
  | Not e              -> let* e' = pp_par_expression e in AC.return @@ PP.(simple_app [string "exp_not"; e'])
  | Binop (bo, e1, e2) -> pp_exp_binop bo e1 e2
  | List lst           -> begin
      let* lst' =
        if
          Configuration.(get use_list_notations)
        then
          let* expressions = AC.map ~f:pp_expression lst
          in
          AC.return @@ Coq.list expressions
        else
          pp_exp_list lst
      in
      AC.return @@ PP.(simple_app [string "exp_list"; lst'])
    end
  | Record { type_identifier; variable_identifiers } -> begin
      AC.return @@ PP.(simple_app [
                       string "exp_record";
                       pp_identifier type_identifier;
                       Coq.list @@ List.map
                                     variable_identifiers
                                     ~f:(fun id -> simple_app [ string "exp_var"; pp_identifier id ])
                     ])
    end
  | Enum identifier -> AC.return @@ pp_identifier identifier

and pp_par_expression e =
  let* e' = pp_expression e
  in
  AC.return @@ PP.parens e'
