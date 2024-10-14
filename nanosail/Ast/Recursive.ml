(*
  Groups recursive modules
  Modules defined in here are aliased and should only be referred to by their alias.
*)

open Base


module NumericExpression = struct
  type t =
    | Constant of Z.t
    | Add      of t * t
    | Minus    of t * t
    | Times    of t * t
    | Neg      of t
    | Id       of Identifier.t
    | Var      of Identifier.t


  let rec to_string (numeric_expression : t) =
    match numeric_expression with
    | Constant n     -> Z.to_string n
    | Add   (e1, e2) -> Printf.sprintf "(%s + %s)" (to_string e1) (to_string e2)
    | Minus (e1, e2) -> Printf.sprintf "(%s - %s)" (to_string e1) (to_string e2)
    | Times (e1, e2) -> Printf.sprintf "(%s * %s)" (to_string e1) (to_string e2)
    | Neg e          -> Printf.sprintf "-%s" (to_string e)
    | Id id          -> Identifier.string_of id
    | Var id         -> Identifier.string_of id


  let rec equal (t1 : t) (t2 : t) : bool =
    match t1 with
    | Constant n1 -> begin
        match t2 with
        | Constant n2 -> Z.equal n1 n2
        | _           -> false
      end
    | Add (x, y) -> begin
        match t2 with
        | Add (x', y') -> equal x x' && equal y y'
        | _            -> false
      end
    | Minus (x, y) -> begin
        match t2 with
        | Minus (x', y') -> equal x x' && equal y y'
        | _              -> false
      end
    | Times (x, y) -> begin
        match t2 with
        | Times (x', y') -> equal x x' && equal y y'
        | _              -> false
      end
    | Neg x -> begin
        match t2 with
        | Neg x' -> equal x x'
        | _      -> false
      end
    | Id x -> begin
        match t2 with
        | Id x' -> Identifier.equal x x'
        | _     -> false
      end
    | Var x -> begin
        match t2 with
        | Var x' -> Identifier.equal x x'
        | _      -> false
      end
   

  let rec to_fexpr (numeric_expression : t) : FExpr.t =
    let prefix head =
      String.append "NumExpr:" head
    in
    match numeric_expression with
     | Constant n     -> FExpr.mk_int @@ Z.to_int n
     | Add (e1, e2)   -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2]       @@ prefix "Add"
     | Minus (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2]       @@ prefix "Minus"
     | Times (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2]       @@ prefix "Times"
     | Neg e          -> FExpr.mk_application ~positional:[to_fexpr e]                     @@ prefix "Neg"
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
    | List        of t
    | Product     of t * t
    | Sum         of t * t
    | Unit
    | Enum        of Identifier.t
    | Bitvector   of NumericExpression.t
    | Tuple       of t list
    | Variant     of Identifier.t
    | Record      of Identifier.t
    | Application of t * TypeArgument.t list
    | Alias       of Identifier.t * t

  val to_string : t -> string
  val to_fexpr  : t -> FExpr.t
  val equal     : t -> t -> bool
end = struct
(*
  should mirror

    Inductive Ty : Set :=
    | int
    | bool
    | string
    | list (σ : Ty)
    | prod (σ τ : Ty)
    | sum  (σ τ : Ty)
    | unit
    | enum (E : enumi)
    | bvec (n : nat)
    | tuple (σs : Ctx Ty)
    | union (U : unioni)
    | record (R : recordi)
    .

   defined in theories/Syntax/TypeDecl.v
 *)
  type t =
    | Int
    | Bool
    | String
    | Bit
    | List        of t
    | Product     of t * t
    | Sum         of t * t
    | Unit
    | Enum        of Identifier.t
    | Bitvector   of NumericExpression.t
    | Tuple       of t list
    | Variant     of Identifier.t
    | Record      of Identifier.t
    | Application of t * TypeArgument.t list
    | Alias       of Identifier.t * t

  
  let rec to_string (t : t) : string =
    match t with
    | Int              -> "Type.Int"
    | Bool             -> "Type.Bool"
    | String           -> "Type.String"
    | List _           -> "Type.List"
    | Bit              -> "Type.Bit"
    | Product (t1, t2) -> Printf.sprintf "(%s * %s)" (to_string t1) (to_string t2)
    | Sum (t1, t2)     -> Printf.sprintf "(%s + %s)" (to_string t1) (to_string t2)
    | Unit             -> "Type.Unit"
    | Bitvector numexp -> Printf.sprintf "Type.Bitvector(%s)" (NumericExpression.to_string numexp)
    | Enum id          -> Printf.sprintf "Type.Enum(%s)" (Identifier.string_of id)
    | Record id        -> Printf.sprintf "Type.Record(%s)" (Identifier.string_of id)
    | Variant id       -> Printf.sprintf "Type.Variant(%s)" (Identifier.string_of id)
    | Alias (id, _)    -> Printf.sprintf "Type.Alias(%s)" (Identifier.string_of id)
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

  
  let rec to_fexpr (t : t) : FExpr.t =
    let prefix s =
      String.append "Type:" s
    in
    match t with
    | Int                -> FExpr.mk_symbol @@ prefix "Int"
    | Bool               -> FExpr.mk_symbol @@ prefix "Bool"
    | String             -> FExpr.mk_symbol @@ prefix "String"
    | Bit                -> FExpr.mk_symbol @@ prefix "Bit"
    | Unit               -> FExpr.mk_symbol @@ prefix "Unit"
    | List t             -> FExpr.mk_application ~positional:[to_fexpr t]                         @@ prefix "List"
    | Product (t1, t2)   -> FExpr.mk_application ~positional:[to_fexpr t1; to_fexpr t2]           @@ prefix "Product"
    | Sum (t1, t2)       -> FExpr.mk_application ~positional:[to_fexpr t1; to_fexpr t2]           @@ prefix "Sum"
    | Enum id            -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]             @@ prefix "Enum"
    | Bitvector numexpr  -> FExpr.mk_application ~positional:[NumericExpression.to_fexpr numexpr] @@ prefix "Bitvector"
    | Tuple ts           -> FExpr.mk_application ~positional:(List.map ~f:to_fexpr ts)            @@ prefix "Tuple"
    | Variant id         -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]             @@ prefix "Variant"
    | Record id          -> FExpr.mk_application ~positional:[Identifier.to_fexpr id]             @@ prefix "Record"
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
    | Product (x, y) -> begin
        match t2 with
        | Product (x', y') -> equal x x' && equal y y'
        | _                -> false
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
    match t1, t2 with
    | Type t1                , Type t2                 -> Type.equal t1 t2
    | NumericExpression nexp1, NumericExpression nexp2 -> NumericExpression.equal nexp1 nexp2
    | Bool nconstr1          , Bool nconstr2           -> NumericConstraint.equal nconstr1 nconstr2
    | _                      , _                       -> false
end

and NumericConstraint : sig
  type t =
    | Equal      of NumericExpression.t * NumericExpression.t
    | BoundedGE  of NumericExpression.t * NumericExpression.t
    | BoundedGT  of NumericExpression.t * NumericExpression.t
    | BoundedLE  of NumericExpression.t * NumericExpression.t
    | BoundedLT  of NumericExpression.t * NumericExpression.t
    | NotEqual   of NumericExpression.t * NumericExpression.t
    | Set        of Identifier.t        * Z.t list
    | Or         of t                   * t
    | And        of t                   * t
    | App        of Identifier.t        * TypeArgument.t list
    | Var        of Identifier.t
    | True
    | False

  val to_string : t -> string
  val to_fexpr  : t -> FExpr.t
  val equal     : t -> t -> bool
end = struct
  type t =
    | Equal      of NumericExpression.t * NumericExpression.t
    | BoundedGE  of NumericExpression.t * NumericExpression.t
    | BoundedGT  of NumericExpression.t * NumericExpression.t
    | BoundedLE  of NumericExpression.t * NumericExpression.t
    | BoundedLT  of NumericExpression.t * NumericExpression.t
    | NotEqual   of NumericExpression.t * NumericExpression.t
    | Set        of Identifier.t        * Z.t list
    | Or         of NumericConstraint.t * NumericConstraint.t
    | And        of NumericConstraint.t * NumericConstraint.t
    | App        of Identifier.t        * TypeArgument.t list
    | Var        of Identifier.t
    | True
    | False


  let rec to_string (numeric_constraint : t) =
    let binop e1 op e2 =
      Printf.sprintf "(%s %s %s)" (NumericExpression.to_string e1) op (NumericExpression.to_string e2)
    in
    match numeric_constraint with
    | Equal     (e1, e2)  -> binop e1 "==" e2
    | BoundedGE (e1, e2)  -> binop e1 ">=" e2
    | BoundedGT (e1, e2)  -> binop e1 ">"  e2
    | BoundedLE (e1, e2)  -> binop e1 "<=" e2
    | BoundedLT (e1, e2)  -> binop e1 "<"  e2
    | NotEqual  (e1, e2)  -> binop e1 "!=" e2
    | Var id              -> Identifier.string_of id
    | True                -> "NC_true"
    | False               -> "NC_false"
    | Set (_, _)          -> failwith "Not yet implemented"
    | Or (c1, c2)         -> Printf.sprintf "(%s || %s)" (to_string c1) (to_string c2)
    | And (c1, c2)        -> Printf.sprintf "(%s && %s)" (to_string c1) (to_string c2)
    | App (_, _)          -> failwith "Not yet imnplemented"

  
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
    in
        
    match numeric_constraint with
     | Equal (e1, e2) -> binop "Equal" e1 e2
     | BoundedGE (e1, e2) -> binop "BoundedGE" e1 e2
     | BoundedGT (e1, e2) -> binop "BoundedGT" e1 e2
     | BoundedLE (e1, e2) -> binop "BoundedLE" e1 e2
     | BoundedLT (e1, e2) -> binop "BoundedLT" e1 e2
     | NotEqual (e1, e2) -> binop "NotEqual" e1 e2
     | Set (identifier, numbers) -> begin
         let positional =
           [
             Identifier.to_fexpr identifier;
             FExpr.mk_list @@ List.map ~f:(fun z -> FExpr.mk_int @@ Z.to_int z) numbers
           ]
         in
         FExpr.mk_application ~positional "Set"
       end
     | Or (c1, c2) -> bincon "Or" c1 c2
     | And (c1, c2) -> bincon "And" c1 c2
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
        | Equal (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _              -> false
      end
    | BoundedGE (x, y) -> begin
        match t2 with
        | BoundedGE (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                  -> false
      end
    | BoundedGT (x, y) -> begin
        match t2 with
        | BoundedGT (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                  -> false
      end
    | BoundedLE (x, y) -> begin
        match t2 with
        | BoundedLE (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                  -> false
      end
    | BoundedLT (x, y) -> begin
        match t2 with
        | BoundedLT (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
        | _                  -> false
      end
    | NotEqual (x, y) -> begin
        match t2 with
        | NotEqual (x', y') -> NumericExpression.equal x x' && NumericExpression.equal y y'
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
