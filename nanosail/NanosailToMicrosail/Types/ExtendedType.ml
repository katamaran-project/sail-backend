open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module Big_int = Nat_big_num


module PPOutput = struct
  type t = PP.document

  let parenthesize = PP.parens
end

module Prec = struct
  include PrecedenceFormatter.Make(PPOutput)

  let addition =
    let pp x y =
      PP.(separate space [ x; string "+"; y ])
    in
    define_left_associative_binary_operator 10 pp

  let subtraction =
    let pp x y =
      PP.(separate space [ x; string "-"; y ])
    in
    define_left_associative_binary_operator 10 pp

  let multiplication =
    let pp x y =
      PP.(separate space [ x; string "*"; y ])
    in
    define_left_associative_binary_operator 20 pp

  let variable id =
    define_atom @@ PP.string @@ Printf.sprintf "$%d" id

  let constant k =
    define_atom @@ PP.string @@ Z.to_string k

  let negation =
    let pp x =
      PP.(string "-" ^^ x)
    in
    define_unary_prefix_operator 50 pp

  let conjunction =
    let pp x y =
      PP.(separate space [ x; string "&&"; y ])
    in
    define_left_associative_binary_operator 40 pp
end


let rec pp_extended_parameter_type (extended_type : Ast.ExtendedType.Parameter.t) : PP.document AC.t =
  let open Ast.ExtendedType.Parameter
  in
  match extended_type with
  | Int k        -> AC.return @@ PP.string @@ Printf.sprintf "int($%d)" k
  | Bool k       -> AC.return @@ PP.string @@ Printf.sprintf "bool($%d)" k
  | Other s      -> AC.return @@ PP.string s
  | Tuple ts     -> begin
      let* ts' = AC.map ~f:pp_extended_parameter_type ts (* add parentheses around each t of ts *)
      in
      AC.return @@ PP.(separate (string " * ") ts')
    end


let pp_int_expression (integer_expression : Ast.ExtendedType.IntExpression.t) : PP.document AC.t =
  let rec pp_int_expression integer_expression =
    match integer_expression with
    | Ast.ExtendedType.IntExpression.Var identifier    -> AC.return @@ Prec.variable identifier
    | Ast.ExtendedType.IntExpression.Constant k        -> AC.return @@ Prec.constant k
    | Ast.ExtendedType.IntExpression.Add (left, right) -> addition left right
    | Ast.ExtendedType.IntExpression.Sub (left, right) -> subtraction left right
    | Ast.ExtendedType.IntExpression.Mul (left, right) -> multiplication left right
    | Ast.ExtendedType.IntExpression.Neg operand       -> negation operand

  and unary_operation f operand =
    let* operand' = pp_int_expression operand
    in
    AC.return @@ f operand'

  and binary_operation f left right =
    let* left'  = pp_int_expression left
    and* right' = pp_int_expression right
    in
    AC.return @@ f left' right'

  and addition       l r = binary_operation Prec.addition l r
  and subtraction    l r = binary_operation Prec.subtraction l r
  and multiplication l r = binary_operation Prec.multiplication l r
  and negation       o   = unary_operation Prec.negation o

  in  
  let* result = pp_int_expression integer_expression
  in
  AC.return @@ Prec.output_of result


let pp_bool_expression (bool_expression : Ast.ExtendedType.BoolExpression.t) : PP.document AC.t =
  let rec conjunction left right =
    let* left'  = pp_bool_expression left
    and* right' = pp_bool_expression right
    in
    AC.return @@ Prec.conjunction left' right'

  and pp_bool_expression bool_expression =
    match bool_expression with
    | Ast.ExtendedType.BoolExpression.Var identifier    -> AC.return @@ Prec.variable identifier
    | Ast.ExtendedType.BoolExpression.And (left, right) -> conjunction left right

  in
  let* result = pp_bool_expression bool_expression
  in
  AC.return @@ Prec.output_of result


let pp_extended_return_value_type (extended_type : Ast.ExtendedType.ReturnValue.t) : PP.document AC.t =
  let open Ast.ExtendedType.ReturnValue
  in
  match extended_type with
  | Int int_expression   -> pp_int_expression int_expression
  | Bool bool_expression -> pp_bool_expression bool_expression

let pp_extended_function_type
      (ft  : Ast.function_type         )
      (eft : Ast.ExtendedFunctionType.t) : PP.document AC.t
  =
  let parameter_names =
    List.map ~f:(fun (id, _) -> Id.string_of id) ft.parameters
  and parameter_extended_types =
    eft.extended_parameter_types
  and return_extended_type =
    eft.extended_return_type
  in
  let* pp_parameter_names =
    AC.return @@ List.map ~f:(fun name -> PP.(string "parameter " ^^ PP.string name)) parameter_names
  and* pp_parameter_extended_types =
    AC.map ~f:pp_extended_parameter_type parameter_extended_types
  in
  let pp_parameter_pairs =
    List.zip_exn pp_parameter_names pp_parameter_extended_types
  in
  let* pp_return_value_pair =
    let* ret = pp_extended_return_value_type return_extended_type
    in
    AC.return (PP.string "return value", ret)
  in
  let pairs =
    List.append pp_parameter_pairs [pp_return_value_pair]
  in
  AC.return @@ PP.description_list pairs
