(*
  Groups recursive modules
  Modules defined in here are aliased and should only be referred to by their alias.
*)

open! ExtBase


module NumericExpression = struct
  type t =
    | Constant        of Z.t
    | BinaryOperation of binop * t * t
    | Neg             of t
    | PowerOf2        of t
    | Id              of Identifier.t
    | Var             of Identifier.t

  and binop =
    | Add
    | Sub
    | Mul
    | Div
    | Mod

  (*
     Returns identifiers id appearing as (Id id) inside the numeric expression.
     Identifiers are return in sorted order without duplicates.
  *)
  let identifiers (numeric_expression : t) : Identifier.t list =
    let rec aux (numeric_expression : t) : Identifier.t list =
      match numeric_expression with
      | Constant _                       -> []
      | BinaryOperation (_, left, right) -> List.append (aux left) (aux right)
      | Neg operand                      -> aux operand
      | PowerOf2 operand                 -> aux operand
      | Id id                            -> [id]
      | Var _                            -> []
    in
    List.dedup_and_sort (aux numeric_expression) ~compare:Identifier.compare


  (*
     Replaces occurrences of Id id by the given numeric expression.
  *)
  let substitute_identifier
      (identifier         : Identifier.t)
      (replace_by         : t           )
      (numeric_expression : t           ) : t
    =
    let rec aux (numeric_expression : t) : t
      =
      match numeric_expression with
      | Constant _                        -> numeric_expression
      | BinaryOperation (op, left, right) -> BinaryOperation (op, aux left, aux right)
      | Neg operand                       -> Neg (aux operand)
      | PowerOf2 operand                  -> PowerOf2 (aux operand)
      | Var _                             -> numeric_expression
      | Id id                             -> begin
          if
            Identifier.equal id identifier
          then
            replace_by
          else
            numeric_expression
        end
    in
    aux numeric_expression


  let rec to_string (numeric_expression : t) =
    let string_of_binop op e1 e2 =
      let op_string =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "%"
      in
      Printf.sprintf "(%s %s %s)" (to_string e1) op_string (to_string e2)
    in
    match numeric_expression with
    | Constant n                   -> Z.to_string n
    | BinaryOperation (op, e1, e2) -> string_of_binop op e1 e2
    | Neg e                        -> Printf.sprintf "-%s" (to_string e)
    | PowerOf2 e                   -> Printf.sprintf "2^(%s)" (to_string e)
    | Id id                        -> Identifier.to_string id
    | Var id                       -> Identifier.to_string id


  let rec equal
      (left  : t)
      (right : t) : bool
    =
    match left with
    | Constant value_1 -> begin
        match right with
        | Constant value_2 -> begin
            Z.equal
              value_1
              value_2
          end
        | _ -> false
      end

    | BinaryOperation (operator_1, x_1, y_1) -> begin
        match right with
        | BinaryOperation (operator_2, x_2, y_2) -> begin
            let equal_binary_operator
                (operator_1 : binop)
                (operator_2 : binop) : bool
              =
              match operator_1, operator_2 with
              | Add, Add -> true
              | Add, _   -> false
              | Sub, Sub -> true
              | Sub, _   -> false
              | Mul, Mul -> true
              | Mul, _   -> false
              | Div, Div -> true
              | Div, _   -> false
              | Mod, Mod -> true
              | Mod, _   -> false
            in
            equal_binary_operator
              operator_1
              operator_2
            &&
            equal
              x_1
              x_2
            &&
            equal
              y_1
              y_2
          end
        | _ -> false
      end

    | Neg operand_1 -> begin
        match right with
        | Neg operand_2 -> begin
            equal
              operand_1
              operand_2
          end
        | _ -> false
      end

    | PowerOf2 operand_1 -> begin
        match right with
        | PowerOf2 operand_2 -> begin
            equal
              operand_1
              operand_2
          end
        | _ -> false
      end

    | Id identifier_1 -> begin
        match right with
        | Id identifier_2 -> begin
            Identifier.equal
              identifier_1
              identifier_2
          end
        | _ -> false
      end

    | Var identifier_1 -> begin
        match right with
        | Var identifier_2 -> begin
            Identifier.equal
              identifier_1
              identifier_2
          end
        | _ -> false
      end


  let rec to_fexpr (numeric_expression : t) : FExpr.t =
    let prefix head =
      String.append "NumExpr:" head
    in
    match numeric_expression with
    | Constant n -> FExpr.mk_int @@ Z.to_int n
    | BinaryOperation (operator, e1, e2) -> begin
        let positional =
          [
            (
              match operator with
              | Add -> FExpr.mk_string "Add"
              | Sub -> FExpr.mk_string "Sub"
              | Mul -> FExpr.mk_string "Mul"
              | Div -> FExpr.mk_string "Div"
              | Mod -> FExpr.mk_string "Mod"
            );
            to_fexpr e1;
            to_fexpr e2;
          ]
        in
        FExpr.mk_application ~positional @@ prefix "BinOp"
      end
    | Neg e          -> FExpr.mk_application ~positional:[to_fexpr e]                     @@ prefix "Neg"
    | PowerOf2 e     -> FExpr.mk_application ~positional:[to_fexpr e]                     @@ prefix "PowerOf2"
    | Id identifier  -> FExpr.mk_application ~positional:[Identifier.to_fexpr identifier] @@ prefix "Id"
    | Var identifier -> FExpr.mk_application ~positional:[Identifier.to_fexpr identifier] @@ prefix "Var"
end


(*
  Type, TypeArgument and NumericConstraint

  Put in same file because they are mutually recursive
*)

module rec Type : sig
  type t =
    | Int
    | Bool
    | String
    | Bit
    | List         of t
    | Sum          of t * t
    | Unit
    | Enum         of Identifier.t
    | Bitvector    of NumericExpression.t
    | Tuple        of t list
    | Variant      of Identifier.t
    | Record       of Identifier.t
    | Application  of t * TypeArgument.t list
    | Alias        of Identifier.t * t
    | Range        of NumericExpression.t * NumericExpression.t
    | Function     of { parameter_types : t list; result_type : t }
    | TypeVariable of Identifier.t
    | Nat
    | Vector       of t * NumericExpression.t
    | Implicit     of Identifier.t

  val to_string : t -> string
  val to_fexpr  : t -> FExpr.t
  val equal     : t -> t -> bool
end = struct
  type t =
    | Int
    | Bool
    | String
    | Bit
    | List         of t
    | Sum          of t * t
    | Unit
    | Enum         of Identifier.t
    | Bitvector    of NumericExpression.t
    | Tuple        of t list
    | Variant      of Identifier.t
    | Record       of Identifier.t
    | Application  of t * TypeArgument.t list
    | Alias        of Identifier.t * t
    | Range        of NumericExpression.t * NumericExpression.t
    | Function     of { parameter_types : t list; result_type : t }
    | TypeVariable of Identifier.t
    | Nat
    | Vector       of t * NumericExpression.t
    | Implicit     of Identifier.t

  let rec to_string (t : t) : string =
    let format fmt =
      Printf.ksprintf (fun s -> "Type." ^ s) fmt
    in
    match t with
    | Int                  -> format "Int"
    | Bool                 -> format "Bool"
    | String               -> format "String"
    | List _               -> format "List"
    | Bit                  -> format "Bit"
    | Nat                  -> format "Nat"
    | Sum (t1, t2)         -> Printf.sprintf "Sum(%s + %s)" (to_string t1) (to_string t2)
    | Unit                 -> format "Unit"
    | Bitvector numexp     -> format "Bitvector(%s)" (NumericExpression.to_string numexp)
    | Vector (typ, numexp) -> format "Vector(%s, %s)" (to_string typ) (NumericExpression.to_string numexp)
    | Enum id              -> format "Enum(%s)" (Identifier.to_string id)
    | Record id            -> format "Record(%s)" (Identifier.to_string id)
    | Variant id           -> format "Variant(%s)" (Identifier.to_string id)
    | Alias (id, _)        -> format "Alias(%s)" (Identifier.to_string id)
    | TypeVariable id      -> format "TypeVariable(%s)" (Identifier.to_string id)
    | Range (lower, upper) -> format "Range(%s, %s)" (NumericExpression.to_string lower) (NumericExpression.to_string upper)
    | Implicit id          -> format "Implicit(%s)" (Identifier.to_string id)
    | Application (constructor, targs) -> begin
        let constructor' = to_string constructor
        and targs' = List.map ~f:TypeArgument.to_string targs
        in
        Printf.sprintf "%s(%s)" constructor' (String.concat ~sep:"," targs')
      end
    | Tuple ts -> begin
        let ts' = List.map ~f:to_string ts
        in
        Printf.sprintf "(%s)" (String.concat ~sep:"," ts')
      end
    | Function { parameter_types; result_type } -> begin
        let parameter_type_string =
          String.concat ~sep:" * " @@ List.map ~f:to_string parameter_types
        and result_type_string =
          to_string result_type
        in
        Printf.sprintf "(%s) -> %s" parameter_type_string result_type_string
      end


  let rec to_fexpr (t : t) : FExpr.t =
    let prefix s =
      String.append "Type:" s
    in
    match t with
    | Int                   -> FExpr.mk_symbol @@ prefix "Int"
    | Bool                  -> FExpr.mk_symbol @@ prefix "Bool"
    | String                -> FExpr.mk_symbol @@ prefix "String"
    | Bit                   -> FExpr.mk_symbol @@ prefix "Bit"
    | Unit                  -> FExpr.mk_symbol @@ prefix "Unit"
    | Nat                   -> FExpr.mk_symbol @@ prefix "Nat"
    | List t                -> FExpr.mk_application ~positional:[to_fexpr t]                                       @@ prefix "List"
    | Sum (t1, t2)          -> FExpr.mk_application ~positional:[to_fexpr t1; to_fexpr t2]                         @@ prefix "Sum"
    | Enum id               -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]                           @@ prefix "Enum"
    | Bitvector numexpr     -> FExpr.mk_application ~positional:[NumericExpression.to_fexpr numexpr]               @@ prefix "Bitvector"
    | Vector (typ, numexpr) -> FExpr.mk_application ~positional:[to_fexpr typ; NumericExpression.to_fexpr numexpr] @@ prefix "Bitvector"
    | Tuple ts              -> FExpr.mk_application ~positional:(List.map ~f:to_fexpr ts)                          @@ prefix "Tuple"
    | Variant id            -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]                           @@ prefix "Variant"
    | Record id             -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]                           @@ prefix "Record"
    | TypeVariable id       -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]                           @@ prefix "TypeVariable"
    | Implicit id           -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]                           @@ prefix "Implicit"
    | Application (t, args) -> begin
        let positional =
          [
            to_fexpr t;
            FExpr.mk_list @@ List.map ~f:TypeArgument.to_fexpr args
          ]
        in
        FExpr.mk_application ~positional @@ prefix "Application"
      end
    | Alias (_, _)       -> FExpr.mk_application ~positional:[FExpr.mk_string "TODO"]             @@ prefix "Alias"
    | Range (a, b)       -> begin
        let positional =
          [
            NumericExpression.to_fexpr a;
            NumericExpression.to_fexpr b;
          ]
        in
        FExpr.mk_application ~positional @@ prefix "Range"
      end
    | Function { parameter_types; result_type } -> begin
        let keyword =
          [
            (
              "parameter_types",
              FExpr.mk_list @@ List.map ~f:to_fexpr parameter_types
            );
            (
              "result_type",
              to_fexpr result_type
            )
          ]
        in
        FExpr.mk_application ~keyword @@ prefix "Function"
      end


  let rec equal (t1 : t) (t2 : t) : bool =
    match t1 with
    | Int -> begin
        match t2 with
        | Int -> true
        | _   -> false
      end

    | Bool -> begin
        match t2 with
        | Bool -> true
        | _    -> false
      end

    | String -> begin
        match t2 with
        | String -> true
        | _      -> false
      end

    | Bit -> begin
        match t2 with
        | Bit -> true
        | _   -> false
      end

    | List x -> begin
        match t2 with
        | List x' -> equal x x'
        | _       -> false
      end

    | Sum (x, y) -> begin
        match t2 with
        | Sum (x', y') -> equal x x' && equal y y'
        | _            -> false
      end

    | Unit -> begin
        match t2 with
        | Unit -> true
        | _    -> false
      end

    | Enum x -> begin
        match t2 with
        | Enum x' -> Identifier.equal x x'
        | _       -> false
      end

    | Bitvector x -> begin
        match t2 with
        | Bitvector x' -> NumericExpression.equal x x'
        | _            -> false
      end

    | Tuple xs -> begin
        match t2 with
        | Tuple xs' -> List.equal equal xs xs'
        | _         -> false
      end

    | Variant x -> begin
        match t2 with
        | Variant x' -> Identifier.equal x x'
        | _          -> false
      end

    | Record x -> begin
        match t2 with
        | Record x' -> Identifier.equal x x'
        | _         -> false
      end

    | Application (x, ys) -> begin
        match t2 with
        | Application (x', ys') -> equal x x' && List.equal TypeArgument.equal ys ys'
        | _                     -> false
      end

    | Alias (x, y) -> begin
        match t2 with
        | Alias (x', y') -> Identifier.equal x x' && equal y y'
        | _              -> false
      end

    | Range (x, y) -> begin
        match t2 with
        | Range (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _              -> false
      end

    | Function { parameter_types = parameter_types_1; result_type = result_type_1 } -> begin
        match t2 with
        | Function { parameter_types = parameter_types_2; result_type = result_type_2 } -> begin
            List.equal equal
              parameter_types_1
              parameter_types_2
            &&
            equal
              result_type_1
              result_type_2
          end
        | _ -> false
      end

    | TypeVariable identifier_1 -> begin
        match t2 with
        | TypeVariable identifier_2 -> begin
            Identifier.equal
              identifier_1
              identifier_2
          end
        | _ -> false
      end

    | Nat -> begin
        match t2 with
        | Nat -> true
        | _   -> false
      end

    | Vector (type_1, numeric_expression_1) -> begin
        match t2 with
        | Vector (type_2, numeric_expression_2) -> begin
            equal
              type_1
              type_2
            &&
            NumericExpression.equal
              numeric_expression_1
              numeric_expression_2
          end
        | _ -> false
      end

    | Implicit identifier_1 -> begin
        match t2 with
        | Implicit identifier_2 -> begin
            Identifier.equal
              identifier_1
              identifier_2
          end
        | _ -> false
      end              
end

and TypeArgument : sig
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t

  val to_string : t -> string
  val to_fexpr  : t -> FExpr.t
  val equal     : t -> t -> bool
end = struct
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t

  let to_string (type_argument : t) : string =
    match type_argument with
    | Type t                   -> Type.to_string t
    | NumericExpression numexp -> NumericExpression.to_string numexp
    | Bool nc                  -> NumericConstraint.to_string nc


  let to_fexpr (type_argument : t) : FExpr.t =
    match type_argument with
    | Type typ -> begin
        let positional =
          [ Type.to_fexpr typ ]
        in
        FExpr.mk_application ~positional "TypeArg:Typ"
      end
    | NumericExpression expr -> begin
        let positional =
          [ NumericExpression.to_fexpr expr ]
        in
        FExpr.mk_application ~positional "TypeArg:NumericExpression"
      end
    | Bool b -> begin
        let positional =
          [ NumericConstraint.to_fexpr b ]
        in
        FExpr.mk_application ~positional "TypeArg:Bool"
      end


  let equal (t1 : t) (t2 : t) : bool =
    match t1 with
    | Type x -> begin
        match t2 with
        | Type x' -> Type.equal x x'
        | _       -> false
      end
    | NumericExpression x -> begin
        match t2 with
        | NumericExpression x' -> NumericExpression.equal x x'
        | _                    -> false
      end
    | Bool x -> begin
        match t2 with
        | Bool x' -> NumericConstraint.equal x x'
        | _       -> false
      end
end

and NumericConstraint : sig
  type t =
    | Equal                of TypeArgument.t      * TypeArgument.t
    | NotEqual             of TypeArgument.t      * TypeArgument.t
    | GreaterThanOrEqualTo of NumericExpression.t * NumericExpression.t
    | GreaterThan          of NumericExpression.t * NumericExpression.t
    | LessThanOrEqualTo    of NumericExpression.t * NumericExpression.t
    | LessThan             of NumericExpression.t * NumericExpression.t
    | Set                  of Identifier.t        * Z.t list
    | Or                   of t                   * t
    | And                  of t                   * t
    | App                  of Identifier.t        * TypeArgument.t list
    | Var                  of Identifier.t
    | True
    | False

  val to_string : t -> string
  val to_fexpr  : t -> FExpr.t
  val equal     : t -> t -> bool
end = struct
  type t =
    | Equal                of TypeArgument.t      * TypeArgument.t
    | NotEqual             of TypeArgument.t      * TypeArgument.t
    | GreaterThanOrEqualTo of NumericExpression.t * NumericExpression.t
    | GreaterThan          of NumericExpression.t * NumericExpression.t
    | LessThanOrEqualTo    of NumericExpression.t * NumericExpression.t
    | LessThan             of NumericExpression.t * NumericExpression.t
    | Set                  of Identifier.t        * Z.t list
    | Or                   of NumericConstraint.t * NumericConstraint.t
    | And                  of NumericConstraint.t * NumericConstraint.t
    | App                  of Identifier.t        * TypeArgument.t list
    | Var                  of Identifier.t
    | True
    | False


  let rec to_string (numeric_constraint : t) =
    let binop e1 op e2 =
      Printf.sprintf "(%s %s %s)" (NumericExpression.to_string e1) op (NumericExpression.to_string e2)
    in
    let eq e1 op e2 =
      Printf.sprintf "(%s %s %s)" (TypeArgument.to_string e1) op (TypeArgument.to_string e2)
    in
    match numeric_constraint with
    | Equal     (e1, e2)            -> eq e1 "==" e2
    | NotEqual  (e1, e2)            -> eq e1 "!=" e2
    | GreaterThanOrEqualTo (e1, e2) -> binop e1 ">=" e2
    | GreaterThan (e1, e2)          -> binop e1 ">"  e2
    | LessThanOrEqualTo (e1, e2)    -> binop e1 "<=" e2
    | LessThan (e1, e2)             -> binop e1 "<"  e2
    | Var id                        -> Identifier.to_string id
    | True                          -> "NC_true"
    | False                         -> "NC_false"
    | Set (_, _)                    -> failwith "Not yet implemented"
    | Or (c1, c2)                   -> Printf.sprintf "(%s || %s)" (to_string c1) (to_string c2)
    | And (c1, c2)                  -> Printf.sprintf "(%s && %s)" (to_string c1) (to_string c2)
    | App (_, _)                    -> failwith "Not yet imnplemented"


  let rec to_fexpr (numeric_constraint : t) : FExpr.t =
    let prefix string =
      String.append "NumConstr:" string
    in
    let binop head e1 e2 =
      let positional =
        [
          NumericExpression.to_fexpr e1;
          NumericExpression.to_fexpr e2;
        ]
      in
      FExpr.mk_application ~positional @@ prefix head

    and bincon head c1 c2 =
      let positional =
        [
          to_fexpr c1;
          to_fexpr c2;
        ]
      in
      FExpr.mk_application ~positional @@ prefix head

    and eq head e1 e2 =
      let positional =
        [
          TypeArgument.to_fexpr e1;
          TypeArgument.to_fexpr e2;
        ]
      in
      FExpr.mk_application ~positional @@ prefix head

    in
    match numeric_constraint with
     | Equal (e1, e2)                -> eq "Equal" e1 e2
     | NotEqual (e1, e2)             -> eq "NotEqual" e1 e2
     | GreaterThanOrEqualTo (e1, e2) -> binop "GreaterThanOrEqualTo" e1 e2
     | GreaterThan (e1, e2)          -> binop "GreaterThan" e1 e2
     | LessThanOrEqualTo (e1, e2)    -> binop "LessThanOrEqualTo" e1 e2
     | LessThan (e1, e2)             -> binop "LessThan" e1 e2
     | Or (c1, c2)                   -> bincon "Or" c1 c2
     | And (c1, c2)                  -> bincon "And" c1 c2
     | Set (identifier, numbers)     -> begin
         let positional =
           [
             Identifier.to_fexpr identifier;
             FExpr.mk_list @@ List.map ~f:(fun z -> FExpr.mk_int @@ Z.to_int z) numbers
           ]
         in
         FExpr.mk_application ~positional "Set"
       end
     | App (identifier, type_arguments) -> begin
         let positional =
           [
             Identifier.to_fexpr identifier;
             FExpr.mk_list @@ List.map ~f:TypeArgument.to_fexpr type_arguments
           ]
         in
         FExpr.mk_application ~positional @@ prefix "App"
       end
     | Var identifier -> begin
         let positional =
           [
             Identifier.to_fexpr identifier
           ]
         in
         FExpr.mk_application ~positional @@ prefix "Var"
       end
     | True -> begin
         FExpr.mk_symbol @@ prefix "True"
       end
     | False -> begin
         FExpr.mk_symbol @@ prefix "False"
       end


  let rec equal (t1 : t) (t2 : t) : bool =
    match t1 with
    | Equal (x, y) -> begin
        match t2 with
        | Equal (x', y') -> TypeArgument.equal x x' && TypeArgument.equal y y'
        | _              -> false
      end
    | NotEqual (x, y) -> begin
        match t2 with
        | NotEqual (x', y') -> TypeArgument.equal x x' && TypeArgument.equal y y'
        | _                 -> false
      end
    | GreaterThanOrEqualTo (x, y) -> begin
        match t2 with
        | GreaterThanOrEqualTo (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                             -> false
      end
    | GreaterThan (x, y) -> begin
        match t2 with
        | GreaterThan (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                    -> false
      end
    | LessThanOrEqualTo (x, y) -> begin
        match t2 with
        | LessThanOrEqualTo (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                          -> false
      end
    | LessThan (x, y) -> begin
        match t2 with
        | LessThan (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                 -> false
      end
    | Set (x, y) -> begin
        match t2 with
        | Set (x', y') -> Identifier.equal x x' && List.equal Z.equal y y'
        | _            -> false
      end
    | Or (x, y) -> begin
        match t2 with
        | Or (x', y') -> equal x x' && equal y y'
        | _           -> false
      end
    | And (x, y) -> begin
        match t2 with
        | And (x', y') -> equal x x' && equal y y'
        | _            -> false
      end
    | App (x, y) -> begin
        match t2 with
        | App (x', y') -> Identifier.equal x x' && List.equal TypeArgument.equal y y'
        | _            -> false
      end
    | Var x -> begin
        match t2 with
        | Var x' -> Identifier.equal x x'
        | _      -> false
      end
    | True -> begin
        match t2 with
        | True -> true
        | _    -> false
      end
    | False -> begin
        match t2 with
        | False -> true
        | _     -> false
      end
end
