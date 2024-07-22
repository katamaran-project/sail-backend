open Base
open Monads.Notations.Star(AnnotationContext)

module AC      = AnnotationContext
module Big_int = Nat_big_num


module PPOutput = struct
  type t = PP.document

  let parenthesize = PP.parens
end


module Prec = struct
  include PrecedenceFormatter.Make(PPOutput)

  let pp_binary operator x y =
    PP.(separate space [ x; string operator; y ])


  let variable id =
    define_atom @@ PP.string @@ Printf.sprintf "$%d" id

  let constant k =
    define_atom @@ PP.string @@ Z.to_string k

  let negation =
    let pp x =
      PP.(string "-" ^^ x)
    in
    define_unary_prefix_operator 50 pp

  let addition                 = define_left_associative_binary_operator 10 @@ pp_binary "+"
  let subtraction              = define_left_associative_binary_operator 10 @@ pp_binary "-"
  let multiplication           = define_left_associative_binary_operator 20 @@ pp_binary "*"

  let conjunction              = define_left_associative_binary_operator 40 @@ pp_binary "&&"
  let disjunction              = define_left_associative_binary_operator 30 @@ pp_binary "||"

  let equality                 = define_left_associative_binary_operator 5  @@ pp_binary "="
  let inequality               = define_left_associative_binary_operator 5  @@ pp_binary "!="
  let less_than                = define_left_associative_binary_operator 5  @@ pp_binary "<"
  let greater_than             = define_left_associative_binary_operator 5  @@ pp_binary ">"
  let less_than_or_equal_to    = define_left_associative_binary_operator 5  @@ pp_binary "<="
  let greater_than_or_equal_to = define_left_associative_binary_operator 5  @@ pp_binary ">="
end


let ast_of_int_expression (integer_expression : Ast.ExtendedType.IntExpression.t) : Prec.ast AC.t =
  let rec ast_of_int_expression integer_expression =
    match integer_expression with
    | Ast.ExtendedType.IntExpression.Var identifier    -> AC.return @@ Prec.variable identifier
    | Ast.ExtendedType.IntExpression.Constant k        -> AC.return @@ Prec.constant k
    | Ast.ExtendedType.IntExpression.Add (left, right) -> addition left right
    | Ast.ExtendedType.IntExpression.Sub (left, right) -> subtraction left right
    | Ast.ExtendedType.IntExpression.Mul (left, right) -> multiplication left right
    | Ast.ExtendedType.IntExpression.Neg operand       -> negation operand

  and unary_operation f operand =
    let* operand' = ast_of_int_expression operand
    in
    AC.return @@ f operand'

  and binary_operation f left right =
    let* left'  = ast_of_int_expression left
    and* right' = ast_of_int_expression right
    in
    AC.return @@ f left' right'

  and addition       l r = binary_operation Prec.addition l r
  and subtraction    l r = binary_operation Prec.subtraction l r
  and multiplication l r = binary_operation Prec.multiplication l r
  and negation       o   = unary_operation Prec.negation o

  in
  ast_of_int_expression integer_expression


let rec pp_extended_parameter_type (extended_type : Ast.ExtendedType.Parameter.t) : PP.document AC.t =
  match extended_type with
  | Int k        -> AC.return @@ PP.string @@ Printf.sprintf "int($%d)" k
  | Bool k       -> AC.return @@ PP.string @@ Printf.sprintf "bool($%d)" k
  | Other s      -> AC.return @@ PP.string s
  | Tuple ts     -> begin
      let* ts' = AC.map ~f:pp_extended_parameter_type ts (* todo add parentheses around each t of ts *)
      in
      AC.return @@ PP.(separate (string " * ") ts')
    end
  | Unknown ud   -> begin
      let* annotation_index =
        let annotation_document =
          PP.lines [
              ud.annotation;
              Printf.sprintf "OCaml position: %s" @@ StringOf.OCaml.position ud.ocaml_location;
              Printf.sprintf "Sail position: %s" @@ StringOf.Sail.location ud.sail_location;
            ]
        in
        AC.create_annotation_from_document annotation_document
      in
      AC.return @@ PP.string @@ Printf.sprintf "?[%d]" annotation_index
    end


let pp_int_expression (integer_expression : Ast.ExtendedType.IntExpression.t) : PP.document AC.t =
  let* result = ast_of_int_expression integer_expression
  in
  AC.return @@ Prec.output_of result


let ast_of_bool_expression (bool_expression : Ast.ExtendedType.BoolExpression.t) : Prec.ast AC.t =
  let rec binary_operation f left right =
    let* left'  = ast_of_bool_expression left
    and* right' = ast_of_bool_expression right
    in
    AC.return @@ f left' right'

  and comparison f left right =
    let* left'  = ast_of_int_expression left
    and* right' = ast_of_int_expression right
    in
    AC.return @@ f left' right'

  and conjunction l r              = binary_operation Prec.conjunction              l r
  and disjunction l r              = binary_operation Prec.disjunction              l r
  and equality    l r              = comparison       Prec.equality                 l r
  and inequality  l r              = comparison       Prec.inequality               l r
  and less_than   l r              = comparison       Prec.less_than                l r
  and greater_than l r             = comparison       Prec.greater_than             l r
  and less_than_or_equal_to l r    = comparison       Prec.less_than_or_equal_to    l r
  and greater_than_or_equal_to l r = comparison       Prec.greater_than_or_equal_to l r

  and ast_of_bool_expression (bool_expression : Ast.ExtendedType.BoolExpression.t) =
    match bool_expression with
    | Var identifier                     -> AC.return @@ Prec.variable identifier
    | And (left, right)                  -> conjunction              left right
    | Or  (left, right)                  -> disjunction              left right
    | Equal (left, right)                -> equality                 left right
    | NotEqual (left, right)             -> inequality               left right
    | LessThan (left, right)             -> less_than                left right
    | GreaterThan (left, right)          -> greater_than             left right
    | LessThanOrEqualTo (left, right)    -> less_than_or_equal_to    left right
    | GreaterThanOrEqualTo (left, right) -> greater_than_or_equal_to left right
  in
  ast_of_bool_expression bool_expression


let pp_bool_expression (bool_expression : Ast.ExtendedType.BoolExpression.t) : PP.document AC.t =
  let* result = ast_of_bool_expression bool_expression
  in
  AC.return @@ Prec.output_of result


let pp_extended_return_value_type (extended_type : Ast.ExtendedType.ReturnValue.t) : PP.document AC.t =
  match extended_type with
  | Int int_expression   -> pp_int_expression int_expression
  | Bool bool_expression -> pp_bool_expression bool_expression
  | Other id             -> AC.return @@ PP.string id
  | Unknown unknown_data -> begin
      let* annotation_index =
        let annotation_document =
          PP.lines [
              unknown_data.annotation;
              Printf.sprintf "OCaml position: %s line %d" unknown_data.ocaml_location.pos_fname unknown_data.ocaml_location.pos_lnum;
              Printf.sprintf "Sail position: %s" @@ StringOf.Sail.location unknown_data.sail_location;
            ]
        in
        AC.create_annotation_from_document annotation_document
      in
      AC.return @@ PP.string @@ Printf.sprintf "?[%d]" annotation_index
    end


let pp_extended_function_type
      (ft  : Ast.Definition.FunctionType.t)
      (eft : Ast.ExtendedFunctionType.t   ) : PP.document AC.t
  =
  let parameter_names =
    List.map ~f:(fun (id, _) -> Ast.Identifier.string_of id) ft.parameters
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
