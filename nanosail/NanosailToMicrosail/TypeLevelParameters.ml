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
