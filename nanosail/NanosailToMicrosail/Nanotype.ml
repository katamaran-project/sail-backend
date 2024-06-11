open Base
open Numeric
open Monads.Notations.Star(AnnotationContext)
open Identifier

module AC = AnnotationContext


let rec pp_nanotype (typ : Ast.Type.t) =
  let pp_product x y =
    PP.parens @@ PP.simple_app [ pp_identifier @@ Ast.Identifier.mk "ty.prod"; x; y ]
  in
  
  let pp_tuple elts =
    let* elts' = AnnotationContext.map ~f:pp_nanotype elts
    in
    match Auxlib.split_last elts' with
    | Some (xs, last) -> AC.return @@ List.fold_right ~f:pp_product ~init:last xs
    | None            -> AC.not_yet_implemented [%here]
  in
  
  let pp_list element_type =
    let* element_type' = pp_nanotype element_type
    in
    AC.return @@ PP.parens @@ PP.simple_app [ pp_identifier @@ Ast.Identifier.mk "ty.list"; element_type' ]
  in
  
  let pp_application
      (constructor    : Ast.Type.t             )
      (type_arguments : Ast.TypeArgument.t list) : PP.document AC.t
    =
    let* constructor' = pp_nanotype constructor
    in
    let* type_arguments' =
      AC.map ~f:(AC.(compose (Fn.compose return PP.parens) pp_type_argument)) type_arguments
    in
    AC.return @@ PP.parens @@ PP.simple_app (constructor' :: type_arguments')
  in
  
  let pp_bitvector nexpr =
    let* nexpr' = pp_numeric_expression nexpr
    in
    AC.return @@ PP.simple_app [ pp_identifier @@ Ast.Identifier.mk "ty.bitvector"; nexpr' ]
  in
  match typ with
   | Unit                             -> AC.return @@ pp_identifier @@ Ast.Identifier.mk "ty.unit"
   | Bool                             -> AC.return @@ pp_identifier @@ Ast.Identifier.mk "ty.bool"
   | Int                              -> AC.return @@ pp_identifier @@ Ast.Identifier.mk "ty.int"
   | Nat                              -> AC.return @@ pp_identifier @@ Ast.Identifier.mk "ty.nat"
   | String                           -> AC.return @@ pp_identifier @@ Ast.Identifier.mk "ty.string"
   | Atom                             -> AC.return @@ pp_identifier @@ Ast.Identifier.mk "ty.atom"
   | Custom id                        -> AC.return @@ pp_identifier id
   | Record                           -> AC.not_yet_implemented [%here]
   | Enum id                          -> AC.return @@ pp_identifier @@ Ast.Identifier.mk @@ Printf.sprintf "ty.enum %s" @@ Ast.Identifier.string_of id
   | Product (_, _)                   -> AC.not_yet_implemented [%here]
   | Sum (_, _)                       -> AC.not_yet_implemented [%here]
   | Application (constructor, targs) -> pp_application constructor targs
   | List typ                         -> pp_list typ
   | Tuple ts                         -> pp_tuple ts
   | Bitvector nexpr                  -> pp_bitvector nexpr


and coq_type_of_nanotype (nanotype : Ast.Type.t) =
  let coq_type_of_bitvector_type n =
    let* n' = pp_numeric_expression n
    in
    AC.return @@ PP.(string "bv" ^^ space ^^ n')

  and coq_type_of_list_type t =
    let* t' = coq_type_of_nanotype t
    in
    AC.return @@ PP.(separate space [ string "list"; parens t' ])

  and coq_type_of_application t ts =
    let* t   = coq_type_of_nanotype t
    and* ts' = AC.map ~f:(Fn.compose (AC.lift ~f:PP.parens) pp_type_argument) ts
    in
    AC.return @@ PP.separate PP.space (t :: ts')

  in
  match nanotype with
  | Unit                -> AC.return @@ PP.string "Datatypes.unit"
  | Bool                -> AC.return @@ PP.string "Datatypes.bool"
  | Nat                 -> AC.return @@ PP.string "nat"
  | Int                 -> AC.return @@ PP.string "Z"
  | String              -> AC.return @@ PP.string "String.string"
  | Custom id           -> AC.return @@ pp_identifier id
  | Bitvector n         -> coq_type_of_bitvector_type n
  | List t              -> coq_type_of_list_type t
  | Application (t, ts) -> coq_type_of_application t ts
  | Tuple _ts           -> AC.not_yet_implemented [%here]
  | Atom                -> AC.not_yet_implemented [%here]
  | Record              -> AC.not_yet_implemented [%here]
  | Enum _id            -> AC.not_yet_implemented [%here] (* todo lookup Coq Inductive type corresponding to the enum named id *)
  | Product (_, _)      -> AC.not_yet_implemented [%here]
  | Sum (_, _)          -> AC.not_yet_implemented [%here]

and pp_type_argument (type_argument : Ast.TypeArgument.t) : PP.document AC.t =
  match type_argument with
  | TA_type t   -> pp_nanotype t
  | TA_numexp e -> pp_numeric_expression e
  | TA_bool nc  -> pp_numeric_constraint nc
