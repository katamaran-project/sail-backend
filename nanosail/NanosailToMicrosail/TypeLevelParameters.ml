(*
   Type-level parameters of a function signature.

   muSail types are first-order, but Rocq can index a family of functions by a value:
   Katamaran's own RiscvPmp declares read_ram (bytes : nat) : FunX [...] (ty_bytes bytes).
   A Sail function or extern that is polymorphic in the width of a bitvector, such as

     val sign_extend : forall 'n 'm, 'm >= 'n. (int('m), bits('n)) -> bits('m)

   becomes one Fun (or FunX) constructor with the width as an implicit argument:

     | sign_extend {kid__m : nat} {kid__n : nat} : Fun [ "m" ∷ ty.int; "v" ∷ ty.bvec (kid__n) ] (ty.bvec (kid__m))

   and its definition takes the same implicit arguments, so that one definition (and one contract)
   covers all widths. Rocq infers the arguments at each call from the types of the arguments and
   of the result. Type variables (kind Type) become implicit arguments of type Ty in the same way.
   Sail's constraints on the variables ('m >= 'n) are not translated: a body that needs one to
   type-check (e.g., a zero extension needs 'n <= 'm) fails in Rocq.

   Integer types do not mention their identifiers: muSail's ty.int has no index, so 'n in int('n)
   needs no parameter.
*)
open ExtBase


(* The identifiers a numeric expression mentions (Id and Var alike: both are printed as identifiers) *)
let rec numeric_identifiers (numeric_expression : Ast.Numeric.Expression.t) : Ast.Identifier.t list =
  match numeric_expression with
  | Constant _                       -> []
  | BinaryOperation (_, left, right) -> List.append (numeric_identifiers left) (numeric_identifiers right)
  | Neg operand                      -> numeric_identifiers operand
  | PowerOf2 operand                 -> numeric_identifiers operand
  | Id identifier                    -> [ identifier ]
  | Var identifier                   -> [ identifier ]


(*
   The type-level identifiers a type mentions once printed as a muSail type:
   the identifiers in bitvector widths (kind Int) and type variables (kind Type).
*)
let rec of_type (typ : Ast.Type.t) : (Ast.Identifier.t * Ast.Kind.t) list =
  match typ with
  | Unit                       -> []
  | Bool                       -> []
  | Int _                      -> []
  | String                     -> []
  | Bit                        -> []
  | Nat                        -> []
  | Range (_, _)               -> []
  | Enum _                     -> []
  | Record _                   -> []
  | Variant _                  -> []
  | Alias (_, _)               -> []
  | Bitvector width            -> List.map ~f:(fun id -> (id, Ast.Kind.Int)) @@ numeric_identifiers width
  | TypeVariable identifier    -> [ (identifier, Ast.Kind.Type) ]
  | List element_type          -> of_type element_type
  | Tuple element_types        -> List.concat_map ~f:of_type element_types
  | Sum (left, right)          -> List.append (of_type left) (of_type right)
  | Application (_, arguments) -> List.concat_map ~f:of_type_argument arguments
  | Vector (element_type, _)   -> of_type element_type
  | Implicit _                 -> []
  | Function _                 -> []

and of_type_argument (type_argument : Ast.TypeArgument.t) : (Ast.Identifier.t * Ast.Kind.t) list =
  match type_argument with
  | Type typ             -> of_type typ
  | NumericExpression e  -> List.map ~f:(fun id -> (id, Ast.Kind.Int)) @@ numeric_identifiers e
  | Bool _               -> []


(* The type-level parameters of a signature, without duplicates, in order of appearance *)
let of_types (types : Ast.Type.t list) : (Ast.Identifier.t * Ast.Kind.t) list =
  List.concat_map ~f:of_type types
  |> List.fold ~init:[] ~f:(fun seen (identifier, kind) ->
      if List.exists seen ~f:(fun (identifier', _) -> Ast.Identifier.equal identifier identifier')
      then seen
      else (identifier, kind) :: seen
    )
  |> List.rev


(* The Rocq type of a parameter of the given kind *)
let pp_kind (kind : Ast.Kind.t) : PP.t =
  match kind with
  | Int  -> PP.string "nat"
  | Type -> PP.string "Ty"
  | Bool -> PP.string "bool"


(* {kid__n : nat} *)
let pp_binder ((identifier, kind) : Ast.Identifier.t * Ast.Kind.t) : PP.t =
  PP.(surround braces @@ separate_horizontally ~separator:space [ Identifier.pp identifier; colon; pp_kind kind ])


(* {kid__n : nat} {kid__m : nat}, the binders of a constructor *)
let pp_binders (parameters : (Ast.Identifier.t * Ast.Kind.t) list) : PP.t =
  PP.separate_horizontally ~separator:PP.space @@ List.map ~f:pp_binder parameters


(* The parameters as implicit parameters of Coq.pp_definition *)
let as_definition_parameters (parameters : (Ast.Identifier.t * Ast.Kind.t) list) : (PP.t * PP.t option) list =
  List.map parameters ~f:(fun (identifier, kind) -> (Identifier.pp identifier, Some (pp_kind kind)))


(* @name kid__n kid__m: the name applied to all its parameters explicitly, as a pattern or a term *)
let pp_explicit_application (name : PP.t) (parameters : (Ast.Identifier.t * Ast.Kind.t) list) : PP.t =
  match parameters with
  | []    -> name
  | _ :: _ -> PP.separate_horizontally ~separator:PP.space (PP.horizontal [ PP.string "@"; name ] :: List.map ~f:(Fn.compose Identifier.pp fst) parameters)


(*
   Sail's constraints on the size variables of a function become one implicit hypothesis
   (see case-studies/cheriot/docs/design.md):

     val store : forall 'n 'p, 0 < 'n <= 8 & 'p == 8 * 'n. (int('n), bits('p)) -> unit

     | store {kid__p : nat} {kid__n : nat}
         {H : IsTrue ((0 <? Z.of_nat kid__n) && (Z.of_nat kid__n <=? 8) && (Z.of_nat kid__p =? 8 * Z.of_nat kid__n))%Z} : Fun [...] ty.unit

   The constraint is a boolean over Z (Sail's integers), with Z.of_nat of each size variable.
   Rocq discharges it by type class search: Katamaran's IsTrue hint computes it at concrete widths,
   the hint sail_constraint (ProgramModule.FunDeclKit) proves it with lia from the hypotheses in scope.
*)
type signature =
  {
    parameters : (Ast.Identifier.t * Ast.Kind.t) list;   (* the implicit parameters, in order *)
    inferable  : Ast.Identifier.t list;                  (* those that the types mention, which Rocq infers *)
    hypotheses : Ast.Numeric.Constraint.t list;          (* conjuncts, all over parameters *)
  }


(* The size variables (Var, not Id: an Id is a constant) of a numeric expression *)
let rec numeric_variables (numeric_expression : Ast.Numeric.Expression.t) : Ast.Identifier.t list =
  match numeric_expression with
  | Constant _                       -> []
  | BinaryOperation (_, left, right) -> List.append (numeric_variables left) (numeric_variables right)
  | Neg operand                      -> numeric_variables operand
  | PowerOf2 operand                 -> numeric_variables operand
  | Id _                             -> []
  | Var identifier                   -> [ identifier ]


(* The size variables of a constraint, if Rocq can express it (see pp_constraint); None otherwise *)
let rec constraint_variables (numeric_constraint : Ast.Numeric.Constraint.t) : Ast.Identifier.t list option =
  match numeric_constraint with
  | True | False                                      -> Some []
  | Equal (NumericExpression left, NumericExpression right)
  | NotEqual (NumericExpression left, NumericExpression right)
  | GreaterThanOrEqualTo (left, right)
  | GreaterThan (left, right)
  | LessThanOrEqualTo (left, right)
  | LessThan (left, right)                            -> Some (List.append (numeric_variables left) (numeric_variables right))
  | Equal ((Type _ | Bool _ | NumericExpression _), _)
  | NotEqual ((Type _ | Bool _ | NumericExpression _), _) -> None
  | Set (identifier, _)                               -> Some [ identifier ]
  | Or (left, right) | And (left, right)              -> begin
      match constraint_variables left, constraint_variables right with
      | Some left, Some right -> Some (List.append left right)
      | None, _ | _, None     -> None
    end
  | App (_, _)                                        -> None
  | Var _                                             -> None


let of_signature
    ?(value_parameters : Ast.Identifier.t list = [] )
    ~(types       : Ast.Type.t list                )
    ~(constraints : Ast.Numeric.Constraint.t list  ) : signature
  =
  let from_types = of_types types
  in
  (* size variables only an int('n) parameter mentions, but that the body needs (see of_function) *)
  let from_values =
    List.filter_map value_parameters ~f:(fun identifier ->
        if List.exists from_types ~f:(fun (identifier', _) -> Ast.Identifier.equal identifier identifier')
        then None
        else Some (identifier, Ast.Kind.Int))
    |> List.dedup_and_sort ~compare:(fun (x, _) (y, _) -> Ast.Identifier.compare x y)
  in
  let expressible : (Ast.Numeric.Constraint.t * Ast.Identifier.t list) list =
    List.filter_map constraints ~f:(fun numeric_constraint ->
        Option.map (constraint_variables numeric_constraint) ~f:(fun variables -> (numeric_constraint, variables))
      )
  in
  let is_parameter (parameters : (Ast.Identifier.t * Ast.Kind.t) list) (identifier : Ast.Identifier.t) : bool =
    List.exists parameters ~f:(fun (identifier', _) -> Ast.Identifier.equal identifier identifier')
  in
  (* a constraint that mentions a parameter makes its other size variables parameters too, until nothing changes *)
  let rec close (parameters : (Ast.Identifier.t * Ast.Kind.t) list) : (Ast.Identifier.t * Ast.Kind.t) list =
    let added =
      List.concat_map expressible ~f:(fun (_, variables) ->
          if List.exists variables ~f:(is_parameter parameters)
          then List.filter variables ~f:(fun identifier -> not (is_parameter parameters identifier))
          else []
        )
      |> List.dedup_and_sort ~compare:Ast.Identifier.compare
    in
    if List.is_empty added
    then parameters
    else close (List.append parameters (List.map added ~f:(fun identifier -> (identifier, Ast.Kind.Int))))
  in
  let parameters = close (List.append from_types from_values)
  in
  let hypotheses =
    List.filter_map expressible ~f:(fun (numeric_constraint, variables) ->
        if not (List.is_empty variables) && List.for_all variables ~f:(is_parameter parameters)
        then Some numeric_constraint
        else None
      )
  in
  {
    parameters;
    inferable  = List.map ~f:fst from_types;
    hypotheses;
  }


(* The name of the hypothesis in binders and patterns *)
let hypothesis_name : PP.t = PP.string "H"


(* A numeric expression over Z: Z.of_nat kid__n for a size variable or a constant (both are nat in Rocq) *)
let rec pp_z_expression (numeric_expression : Ast.Numeric.Expression.t) : PP.t =
  let binary (operator : string) (left : Ast.Numeric.Expression.t) (right : Ast.Numeric.Expression.t) : PP.t =
    PP.(surround parens) @@ PP.separate_horizontally ~separator:PP.space [ pp_z_expression left; PP.string operator; pp_z_expression right ]
  in
  match numeric_expression with
  | Constant value                         -> if Z.lt value Z.zero then PP.(surround parens) @@ PP.string (Z.to_string value) else PP.string (Z.to_string value)
  | BinaryOperation (Add, left, right)     -> binary "+" left right
  | BinaryOperation (Sub, left, right)     -> binary "-" left right
  | BinaryOperation (Mul, left, right)     -> binary "*" left right
  | BinaryOperation (Div, left, right)     -> binary "/" left right
  | BinaryOperation (Mod, left, right)     -> binary "mod" left right
  | Neg operand                            -> PP.(surround parens) @@ PP.horizontal [ PP.string "- "; pp_z_expression operand ]
  | PowerOf2 operand                       -> PP.(surround parens) @@ PP.horizontal [ PP.string "2 ^ "; pp_z_expression operand ]
  | Id identifier | Var identifier         -> PP.horizontal [ PP.string "Z.of_nat "; Identifier.pp identifier ]


(* A constraint as a boolean over Z (inside %Z); only called on constraints whose constraint_variables is Some *)
let rec pp_constraint (numeric_constraint : Ast.Numeric.Constraint.t) : PP.t =
  let comparison (operator : string) (left : Ast.Numeric.Expression.t) (right : Ast.Numeric.Expression.t) : PP.t =
    PP.(surround parens) @@ PP.separate_horizontally ~separator:PP.space [ pp_z_expression left; PP.string operator; pp_z_expression right ]
  and connective (operator : string) (left : Ast.Numeric.Constraint.t) (right : Ast.Numeric.Constraint.t) : PP.t =
    PP.(surround parens) @@ PP.separate_horizontally ~separator:PP.space [ pp_constraint left; PP.string operator; pp_constraint right ]
  in
  match numeric_constraint with
  | True                                                       -> PP.string "true"
  | False                                                      -> PP.string "false"
  | Equal (NumericExpression left, NumericExpression right)    -> comparison "=?" left right
  | NotEqual (NumericExpression left, NumericExpression right) -> PP.(surround parens) @@ PP.horizontal [ PP.string "negb "; comparison "=?" left right ]
  | GreaterThanOrEqualTo (left, right)                         -> comparison "<=?" right left
  | GreaterThan (left, right)                                  -> comparison "<?" right left
  | LessThanOrEqualTo (left, right)                            -> comparison "<=?" left right
  | LessThan (left, right)                                     -> comparison "<?" left right
  | Set (identifier, [])                                       -> let _ = identifier in PP.string "false"
  | Set (identifier, values)                                   -> begin
      PP.(surround parens) @@ PP.separate_horizontally ~separator:(PP.string " || ") @@ List.map values ~f:(fun value ->
          comparison "=?" (Var identifier) (Constant value)
        )
    end
  | Or (left, right)                                           -> connective "||" left right
  | And (left, right)                                          -> connective "&&" left right
  | Equal ((Type _ | Bool _ | NumericExpression _), _)
  | NotEqual ((Type _ | Bool _ | NumericExpression _), _)
  | App (_, _)
  | Var _                                                      -> PP.string "true" (* excluded by constraint_variables *)


(* (c1 && c2 && ...)%Z; each conjunct is parenthesised *)
let pp_conjunction (constraints : Ast.Numeric.Constraint.t list) : PP.t =
  let conjunction =
    match constraints with
    | [ single ] -> pp_constraint single
    | _          -> PP.(surround parens) @@ PP.separate_horizontally ~separator:(PP.string " && ") @@ List.map ~f:pp_constraint constraints
  in
  PP.horizontal [ conjunction; PP.string "%Z" ]


(* IsTrue (c1 && c2 && ...)%Z *)
let pp_hypothesis_type (hypotheses : Ast.Numeric.Constraint.t list) : PP.t =
  PP.horizontal [ PP.string "IsTrue "; pp_conjunction hypotheses ]


(* {kid__n : nat} ... {H : IsTrue (...)%Z} *)
let pp_signature_binders (signature : signature) : PP.t =
  let hypothesis_binders =
    match signature.hypotheses with
    | []     -> []
    | _ :: _ -> [ PP.(surround braces @@ separate_horizontally ~separator:space [ hypothesis_name; colon; pp_hypothesis_type signature.hypotheses ]) ]
  in
  PP.separate_horizontally ~separator:PP.space (List.append (List.map ~f:pp_binder signature.parameters) hypothesis_binders)


(* The parameters and the hypothesis as implicit parameters of Coq.pp_definition *)
let signature_as_definition_parameters (signature : signature) : (PP.t * PP.t option) list =
  let hypothesis_parameters =
    match signature.hypotheses with
    | []     -> []
    | _ :: _ -> [ (hypothesis_name, Some (pp_hypothesis_type signature.hypotheses)) ]
  in
  List.append (as_definition_parameters signature.parameters) hypothesis_parameters


(* @name kid__n ... H *)
let pp_signature_explicit_application (name : PP.t) (signature : signature) : PP.t =
  let arguments =
    List.append
      (List.map ~f:(Fn.compose Identifier.pp fst) signature.parameters)
      (match signature.hypotheses with [] -> [] | _ :: _ -> [ hypothesis_name ])
  in
  match arguments with
  | []     -> name
  | _ :: _ -> PP.separate_horizontally ~separator:PP.space (PP.horizontal [ PP.string "@"; name ] :: arguments)


(*
   The signature of a function: the size variables of its parameter and result types, and
   those of its int('n) parameters that a type in its body mentions (bits(8 * 'n) for read(width)
   in read_is_val(width : int('n))), which callers pass by name; then those its constraints relate to them.
*)
let of_function (function_definition : Ast.Definition.Function.t) : signature =
  let parameter_types = List.map ~f:snd function_definition.function_type.parameters
  in
  let in_integer_parameters =
    List.concat_map parameter_types ~f:(fun typ ->
        match Ast.Type.strip_aliases typ with
        | Int (Some size) -> numeric_variables size
        | Int None | Bool | String | Bit | List _ | Unit | Enum _ | Bitvector _ | Tuple _ | Sum (_, _) | Variant _ | Record _
        | Application (_, _) | Alias (_, _) | Range (_, _) | Function _ | TypeVariable _ | Nat | Vector (_, _) | Implicit _ -> []
      )
  in
  let in_body =
    List.concat_map (Ast.Statement.types function_definition.function_body) ~f:(fun typ -> List.map ~f:fst (of_type typ))
  in
  of_signature
    ~value_parameters:(List.filter in_integer_parameters ~f:(fun identifier -> List.mem in_body identifier ~equal:Ast.Identifier.equal))
    ~types:(List.append parameter_types [ function_definition.function_type.return_type ])
    ~constraints:function_definition.type_constraints
