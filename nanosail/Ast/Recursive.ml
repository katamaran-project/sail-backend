(*
  Groups recursive modules
  Modules defined in here are aliased and should only be referred to by their alias.
*)

open! ExtBase


(* todo make this local module redundant by either updating Sig.Monad or providing a ready made adapter module *)
module OptionLetSyntax = Monads.Notations.Star(
  struct
    type 'a t    = 'a Option.t
    let bind x f = Option.bind x ~f
    let return   = Option.return
  end)


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
      | Var _                            -> [] (* todo check if this needs fixing *)
    in
    List.dedup_and_sort (aux numeric_expression) ~compare:Identifier.compare


  (*
     Replaces occurrences of Id id by the given numeric expression.
  *)
  let substitute_identifier (* todo remove this and replace it by substitute *)
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


  let substitute_multiple_identifiers (* todo remove this *)
      (substitutions      : (Identifier.t * t) list)
      (numeric_expression : t                      ) : t
    =
    List.fold_left
      substitutions
      ~init:numeric_expression
      ~f:(fun nexp (id, ne) -> substitute_identifier id ne nexp)


  let substitute
      (subst              : Identifier.t -> t)
      (numeric_expression : t                ) : t
    =
    let rec aux (numeric_expression : t) : t
      =
      match numeric_expression with
      | Constant _                        -> numeric_expression
      | BinaryOperation (op, left, right) -> BinaryOperation (op, aux left, aux right)
      | Neg operand                       -> Neg (aux operand)
      | PowerOf2 operand                  -> PowerOf2 (aux operand)
      | Var id                            -> subst id
      | Id id                             -> subst id
    in
    aux numeric_expression


  (*
     Evaluates numeric expression to a Z.int, if possible (i.e., no unknowns appear in the numeric expression)
  *)
  let rec evaluate (numeric_expression : t) : Z.t option =
    let open OptionLetSyntax
    in
    match numeric_expression with
     | Constant n -> Some n
     | Neg n      -> let* n = evaluate n in Some (Z.neg n)
     | PowerOf2 n -> let* n = evaluate n in Some (Z.shift_left Z.one (Z.to_int n))
     | Id _       -> None
     | Var _      -> None
     | BinaryOperation (operator, left_operand, right_operand) -> begin
         let* left_operand  = evaluate left_operand
         and* right_operand = evaluate right_operand
         in
         match operator with
         | Add -> Some (Z.add left_operand right_operand)
         | Sub -> Some (Z.sub left_operand right_operand)
         | Mul -> Some (Z.mul left_operand right_operand)
         | Div -> Some (Z.div left_operand right_operand)
         | Mod -> Some (Z.rem left_operand right_operand)
       end

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
    | Int          of NumericExpression.t option
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

  val substitute_numeric_expression_identifier : (Identifier.t -> NumericExpression.t) -> t -> t
  val simplify                                 : t -> t
end = struct
  type t =
    | Int          of NumericExpression.t option
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


  let substitute_numeric_expression_identifier
      (substitution : Identifier.t -> NumericExpression.t)
      (typ          : t                                  ) : t
    =
    let rec subst (typ : t) : t =
      match (typ : t) with
      | Bool                                      -> typ
      | String                                    -> typ
      | Bit                                       -> typ
      | Unit                                      -> typ
      | Enum _                                    -> typ
      | Variant _                                 -> typ
      | Record _                                  -> typ
      | TypeVariable _                            -> typ
      | Nat                                       -> typ       
      | Implicit _                                -> typ (* todo it would make sense to remove implicits after substitution *)
      | Int numeric_expression                    -> Int (Option.map numeric_expression ~f:(NumericExpression.substitute substitution))
      | List element_type                         -> List (subst element_type)
      | Sum (left, right)                         -> Sum (subst left, subst right)
      | Bitvector numeric_expression              -> Bitvector (NumericExpression.substitute substitution numeric_expression)
      | Tuple element_types                       -> Tuple (List.map ~f:subst element_types)
      | Application (receiver, type_arguments)    -> Application (subst receiver, List.map type_arguments ~f:(TypeArgument.substitute_numeric_expression_identifier substitution))
      | Alias (identifier, t)                     -> Alias (identifier, subst t)
      | Range (lower, upper)                      -> Range (NumericExpression.substitute substitution lower, NumericExpression.substitute substitution upper)
      | Function { parameter_types; result_type } -> Function { parameter_types = List.map parameter_types ~f:subst; result_type = subst result_type }
      | Vector (element_type, numeric_expression) -> Vector (subst element_type, NumericExpression.substitute substitution numeric_expression)
    in
    subst typ


  let rec simplify (typ : t) : t =
    let eval (numeric_expression : NumericExpression.t) : NumericExpression.t =
      let open OptionLetSyntax
      in
      Option.value ~default:numeric_expression begin
        let* value = NumericExpression.evaluate numeric_expression
        in
        Some (NumericExpression.Constant value)
      end
    in
    match (typ : t) with
    | Int numeric_expression                    -> Int (Option.map ~f:eval numeric_expression)
    | Bool                                      -> typ
    | String                                    -> typ
    | Bit                                       -> typ
    | List element_type                         -> List (simplify element_type)
    | Sum (left, right)                         -> Sum (simplify left, simplify right)
    | Unit                                      -> typ
    | Enum _                                    -> typ
    | Bitvector numeric_expression              -> Bitvector (eval numeric_expression)
    | Tuple element_types                       -> Tuple (List.map ~f:simplify element_types)
    | Variant _                                 -> typ
    | Record _                                  -> typ
    | Application (receiver, type_arguments)    -> Application (simplify receiver, List.map ~f:TypeArgument.evaluate_numeric_expressions type_arguments)
    | Alias (identifier, typ)                   -> Alias (identifier, simplify typ)
    | Range (lower, upper)                      -> Range (eval lower, eval upper)
    | Function { parameter_types; result_type } -> Function { parameter_types = List.map ~f:simplify parameter_types;
                                                              result_type = simplify result_type }
    | TypeVariable _                            -> typ
    | Nat                                       -> typ
    | Vector (typ, numeric_expression)          -> Vector (simplify typ, eval numeric_expression)
    | Implicit _                                -> typ


  let rec to_string (t : t) : string =
    let format fmt =
      Printf.ksprintf (fun s -> "Type." ^ s) fmt
    in
    match t with
    | Int None             -> format "Int"
    | Int (Some numexp)    -> format "Int(%s)" (NumericExpression.to_string numexp)
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
    | Int None              -> FExpr.mk_symbol @@ prefix "Int"
    | Int Some (numexpr)    -> FExpr.mk_application ~positional:[NumericExpression.to_fexpr numexpr]               @@ prefix "Int"
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
    | Int numeric_expression_1 -> begin
        match t2 with
        | Int numeric_expression_2 -> begin
            Option.equal NumericExpression.equal
              numeric_expression_1
              numeric_expression_2
          end
        | _ -> false
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

  val substitute_numeric_expression_identifier : (Identifier.t -> NumericExpression.t) -> t -> t
  val evaluate_numeric_expressions             : t -> t
end = struct
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t


  let substitute_numeric_expression_identifier
      (substitution  : Identifier.t -> NumericExpression.t)
      (type_argument : t                                  ) : t
    =
    match type_argument with
    | Type typ                             -> Type (Type.substitute_numeric_expression_identifier substitution typ)
    | NumericExpression numeric_expression -> NumericExpression (NumericExpression.substitute substitution numeric_expression)
    | Bool numeric_constraint              -> Bool (NumericConstraint.substitute_numeric_expression_identifier substitution numeric_constraint)


  let evaluate_numeric_expressions (type_argument : t) : t =
    match type_argument with
    | Type typ                             -> Type (Type.simplify typ)
    | NumericExpression numeric_expression -> NumericExpression (Option.value ~default:numeric_expression @@ Option.map ~f:(fun n -> NumericExpression.Constant n) @@ NumericExpression.evaluate numeric_expression)
    | Bool numeric_constraint              -> Bool (NumericConstraint.evaluate_numeric_expressions numeric_constraint)
  
  
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

  val substitute_numeric_expression_identifier : (Identifier.t -> NumericExpression.t) -> t -> t
  val evaluate_numeric_expressions : t -> t
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


  let rec substitute_numeric_expression_identifier
      (substitution       : Identifier.t -> NumericExpression.t)
      (numeric_constraint : t                                  ) : t
    =
    match numeric_constraint with
    | Equal (left, right) -> begin
        Equal (
          TypeArgument.substitute_numeric_expression_identifier substitution left,
          TypeArgument.substitute_numeric_expression_identifier substitution right
        )
      end
    | NotEqual (left, right) -> begin
        NotEqual (
          TypeArgument.substitute_numeric_expression_identifier substitution left,
          TypeArgument.substitute_numeric_expression_identifier substitution right
        )
      end
    | GreaterThanOrEqualTo (left, right) -> begin
        GreaterThanOrEqualTo (
          NumericExpression.substitute substitution left,
          NumericExpression.substitute substitution right
        )
      end
    | GreaterThan (left, right) -> begin
        GreaterThan (
          NumericExpression.substitute substitution left,
          NumericExpression.substitute substitution right
        )
      end
    | LessThanOrEqualTo (left, right) -> begin
        LessThanOrEqualTo (
          NumericExpression.substitute substitution left,
          NumericExpression.substitute substitution right
        )
      end
    | LessThan (left, right) -> begin
        LessThan (
          NumericExpression.substitute substitution left,
          NumericExpression.substitute substitution right
        )
      end
    | Or (left, right) -> begin
        Or (
          substitute_numeric_expression_identifier substitution left,
          substitute_numeric_expression_identifier substitution right
        )
      end
    | And (left, right) -> begin
        And (
          substitute_numeric_expression_identifier substitution left,
          substitute_numeric_expression_identifier substitution right
        )
      end
    | App (receiver_identifier, type_arguments) -> begin
        App (
          receiver_identifier,
          List.map ~f:(TypeArgument.substitute_numeric_expression_identifier substitution) type_arguments
        )
      end
    | Var _ -> numeric_constraint
    | Set _ -> numeric_constraint
    | True  -> numeric_constraint
    | False -> numeric_constraint


  let rec evaluate_numeric_expressions (numeric_constraint : t) : t =
    let eval (numeric_expression : NumericExpression.t) : NumericExpression.t =
      Option.value ~default:numeric_expression begin
        Option.map ~f:(fun n -> NumericExpression.Constant n) begin
          NumericExpression.evaluate numeric_expression
        end
      end        
    in
    match (numeric_constraint : t) with
    | Equal (left, right)                -> Equal (TypeArgument.evaluate_numeric_expressions left, TypeArgument.evaluate_numeric_expressions right)
    | NotEqual (left, right)             -> NotEqual (TypeArgument.evaluate_numeric_expressions left, TypeArgument.evaluate_numeric_expressions right)
    | GreaterThanOrEqualTo (left, right) -> GreaterThanOrEqualTo (eval left, eval right)
    | GreaterThan (left, right)          -> GreaterThan (eval left, eval right)
    | LessThanOrEqualTo (left, right)    -> LessThanOrEqualTo (eval left, eval right)
    | LessThan (left, right)             -> LessThan (eval left, eval right)
    | Set (_, _)                         -> numeric_constraint
    | Or (left, right)                   -> Or (evaluate_numeric_expressions left, evaluate_numeric_expressions right)
    | And (left, right)                  -> And (evaluate_numeric_expressions left, evaluate_numeric_expressions right)
    | App (identifier, type_arguments)   -> App (identifier, List.map ~f:TypeArgument.evaluate_numeric_expressions type_arguments)
    | Var _                              -> numeric_constraint
    | True                               -> numeric_constraint
    | False                              -> numeric_constraint
  
  
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
