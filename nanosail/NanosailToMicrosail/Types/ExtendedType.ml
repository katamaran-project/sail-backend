open Base
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* Used as parameter for PrecedenceFormatter *)
module DocumentOutput : (PrecedenceFormatter.Output with type t = PP.document) = struct
  type t = PP.document

  let parenthesize = PP.(surround parens)
end


(*
  Module defining components for building expressions
*)
module Prec = struct
  include PrecedenceFormatter.Make(DocumentOutput)

  let pp_binary operator x y =
    PP.(separate_horizontally ~separator:space [ x; string operator; y ])

  let variable (id : int) : ast =
    define_atom @@ PP.string @@ Printf.sprintf "$%d" id

  let integer (k : Z.t) : ast =
    define_atom @@ PP.string @@ Z.to_string k

  let boolean (b : bool) : ast =
    define_atom @@ PP.string @@ Bool.to_string b

  let negation (ast : ast) : ast =
    let pp x =
      PP.(horizontal [string "-"; x])
    in
    define_unary_prefix_operator 50 pp ast

  and unknown
        (_ocaml_location : Lexing.position)
        (_sail_location  : Libsail.Ast.l  )
        (sail_type       : string         ) : ast
    =
    let document =
      PP.(surround dquotes @@ PP.string sail_type)
    in
    define_atom document


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


let ast_of_int_expression (integer_expression : Ast.ExtendedType.IntExpression.t) : Prec.ast GC.t =
  let rec ast_of_int_expression (integer_expression : Ast.ExtendedType.IntExpression.t) =
    match integer_expression with
    | Var identifier              -> GC.return @@ Prec.variable identifier
    | Constant k                  -> GC.return @@ Prec.integer k
    | Add (left, right)           -> addition left right
    | Sub (left, right)           -> subtraction left right
    | Mul (left, right)           -> multiplication left right
    | Neg operand                 -> negation operand
    | Unknown { ocaml_location;
                sail_location;
                sail_type }       -> GC.return @@ Prec.unknown ocaml_location sail_location sail_type

  and unary_operation f operand =
    let* operand' = ast_of_int_expression operand
    in
    GC.return @@ f operand'

  and binary_operation f left right =
    let* left'  = ast_of_int_expression left
    and* right' = ast_of_int_expression right
    in
    GC.return @@ f left' right'

  and addition       l r = binary_operation Prec.addition l r
  and subtraction    l r = binary_operation Prec.subtraction l r
  and multiplication l r = binary_operation Prec.multiplication l r
  and negation       o   = unary_operation Prec.negation o

  in
  ast_of_int_expression integer_expression


let rec pp_extended_parameter_type (extended_type : Ast.ExtendedType.Parameter.t) : PP.document GC.t =
  match extended_type with
  | Int k        -> GC.return @@ PP.string @@ Printf.sprintf "int($%d)" k
  | Bool k       -> GC.return @@ PP.string @@ Printf.sprintf "bool($%d)" k
  | Identifier s -> GC.return @@ PP.string s
  | Tuple ts     -> begin
      let* ts' = GC.map ~f:pp_extended_parameter_type ts (* todo add parentheses around each t of ts *)
      in
      GC.return @@ PP.(separate_horizontally ~separator:(string " * ") ts')
    end
  | Unknown ud   -> begin
      let* annotation_index =
        let annotation_document =
          PP.vertical @@ List.map ~f:PP.string [
              Printf.sprintf "OCaml position: %s" @@ StringOf.OCaml.position ud.ocaml_location;
              Printf.sprintf "Sail position: %s" @@ StringOf.Sail.location ud.sail_location;
            ]
        in
        GC.add_annotation annotation_document
      in
      GC.return @@ PP.string @@ Printf.sprintf "?[%d:%s]" annotation_index ud.sail_type
    end


let pp_int_expression (integer_expression : Ast.ExtendedType.IntExpression.t) : PP.document GC.t =
  let* result = ast_of_int_expression integer_expression
  in
  GC.return @@ Prec.output_of result


let ast_of_bool_expression (bool_expression : Ast.ExtendedType.BoolExpression.t) : Prec.ast GC.t =
  let rec binary_operation f left right =
    let* left'  = ast_of_bool_expression left
    and* right' = ast_of_bool_expression right
    in
    GC.return @@ f left' right'

  and comparison f left right =
    let* left'  = ast_of_int_expression left
    and* right' = ast_of_int_expression right
    in
    GC.return @@ f left' right'

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
    | Bool b                             -> GC.return @@ Prec.boolean b
    | Var identifier                     -> GC.return @@ Prec.variable identifier
    | And (left, right)                  -> conjunction              left right
    | Or  (left, right)                  -> disjunction              left right
    | Equal (left, right)                -> equality                 left right
    | NotEqual (left, right)             -> inequality               left right
    | LessThan (left, right)             -> less_than                left right
    | GreaterThan (left, right)          -> greater_than             left right
    | LessThanOrEqualTo (left, right)    -> less_than_or_equal_to    left right
    | GreaterThanOrEqualTo (left, right) -> greater_than_or_equal_to left right
    | Unknown { ocaml_location;
                sail_location ;
                sail_type     }          -> GC.return @@ Prec.unknown ocaml_location sail_location sail_type
  in
  ast_of_bool_expression bool_expression


let pp_bool_expression (bool_expression : Ast.ExtendedType.BoolExpression.t) : PP.document GC.t =
  let* result = ast_of_bool_expression bool_expression
  in
  GC.return @@ Prec.output_of result


let rec pp_extended_return_value_type (extended_type : Ast.ExtendedType.ReturnValue.t) : PP.document GC.t =
  match extended_type with
  | Int int_expression   -> pp_int_expression int_expression
  | Bool bool_expression -> pp_bool_expression bool_expression
  | Other id             -> GC.return @@ PP.string id
  | Tuple ts             -> pp_tuple ts
  | Unknown unknown_data -> begin
      let* annotation_index =
        let annotation_document =
          PP.vertical @@ List.map ~f:PP.string [
              Printf.sprintf "Sail type: %s" @@ unknown_data.sail_type;
              Printf.sprintf "OCaml position: %s line %d" unknown_data.ocaml_location.pos_fname unknown_data.ocaml_location.pos_lnum;
              Printf.sprintf "Sail position: %s" @@ StringOf.Sail.location unknown_data.sail_location;
            ]
        in
        GC.add_annotation annotation_document
      in
      GC.return @@ PP.string @@ Printf.sprintf "?[%d]" annotation_index
    end

and pp_tuple (extended_types : Ast.ExtendedType.ReturnValue.t list) : PP.document GC.t =
  let* pp_extended_types =
    GC.map ~f:(fun x -> GC.lift ~f:PP.(surround parens) @@ pp_extended_return_value_type x) extended_types
  in
  GC.return @@ PP.(surround parens) begin
    PP.(separate_horizontally ~separator:(string " * ") pp_extended_types)
  end


let pp_extended_function_type
      (ft  : Ast.Definition.FunctionType.t)
      (eft : Ast.ExtendedFunctionType.t   ) : PP.document GC.t
  =
  let parameter_names =
    List.map ~f:(fun (id, _) -> Ast.Identifier.to_string id) ft.parameters
  and parameter_extended_types =
    eft.extended_parameter_types
  and return_extended_type =
    eft.extended_return_type
  in
  let* pp_parameter_names =
    GC.return begin
        List.map ~f:(fun name -> PP.(horizontal [ string "parameter "; PP.string name ])) parameter_names
      end
  and* pp_parameter_extended_types =
    GC.map ~f:pp_extended_parameter_type parameter_extended_types
  in
  let* pp_parameter_pairs =
    match List.zip pp_parameter_names pp_parameter_extended_types with
    | List.Or_unequal_lengths.Ok result -> GC.return result
    | List.Or_unequal_lengths.Unequal_lengths -> begin
        let error_message =
          Printf.sprintf
            "number of parameters (%d) is different from number of number of extended types (%d)"
            (List.length pp_parameter_names)
            (List.length pp_parameter_extended_types)
        in
        GC.fail [%here] error_message
      end
  in
  let* pp_return_value_pair =
    let* ret = pp_extended_return_value_type return_extended_type
    in
    GC.return (PP.string "return value", ret)
  in
  let pairs =
    List.append pp_parameter_pairs [pp_return_value_pair]
  in
  GC.return @@ PP.description_list pairs
