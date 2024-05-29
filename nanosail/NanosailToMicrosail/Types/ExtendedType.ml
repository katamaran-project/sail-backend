open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module Big_int = Nat_big_num


let rec pp_extended_parameter_type (extended_type : Ast.ExtendedType.Parameter.t) : PP.document AC.t =
  let open Ast.ExtendedType.Parameter
  in
  match extended_type with
  | Int k    -> AC.return @@ PP.(string "int" ^^ space ^^ string k)
  | Bool k   -> AC.return @@ PP.(string "bool" ^^ space ^^ string k)
  | Other s  -> AC.return @@ PP.string s
  | Tuple ts -> begin
      let* ts' = AC.map ~f:pp_extended_parameter_type ts (* add parentheses around each t of ts *)
      in
      AC.return @@ PP.(separate (string " * ") ts')
    end


let rec pp_int_expression (integer_expression : Ast.ExtendedType.IntExpression.t) : PP.document AC.t =
  let pp_unary_operation operator operand =
    let* operand' = pp_int_expression operand
    in
    AC.return @@ PP.(string operator ^^ parens operand')
  and pp_binary_operation left operator right =
    let* left'  = pp_int_expression left
    and* right' = pp_int_expression right
    in
    AC.return @@ PP.(separate space [parens left'; string operator; parens right'])
  in
  match integer_expression with
   | Ast.ExtendedType.IntExpression.Var identifier -> AC.return @@ PP.string identifier
   | Ast.ExtendedType.IntExpression.Constant k -> AC.return @@ PP.string @@ Z.to_string k
   | Ast.ExtendedType.IntExpression.Add (left, right) -> pp_binary_operation left "+" right
   | Ast.ExtendedType.IntExpression.Sub (left, right) -> pp_binary_operation left "-" right
   | Ast.ExtendedType.IntExpression.Mul (left, right) -> pp_binary_operation left "*" right
   | Ast.ExtendedType.IntExpression.Neg operand -> pp_unary_operation "-" operand


let pp_extended_return_value_type (extended_type : Ast.ExtendedType.ReturnValue.t) : PP.document AC.t =
  let open Ast.ExtendedType.ReturnValue
  in
  match extended_type with
  | Int expression -> pp_int_expression expression


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
