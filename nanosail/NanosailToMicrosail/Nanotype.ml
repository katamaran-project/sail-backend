open Base
open Ast
open Numeric
open Monads.Notations.Star(AnnotationContext)
open Identifier

module AC = AnnotationContext


let rec pp_nanotype (typ : nanotype) =
  let pp_product x y =
    PP.parens @@ PP.simple_app [ pp_identifier @@ Id.mk "ty.prod"; x; y ]
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
    AC.return @@ PP.parens @@ PP.simple_app [ pp_identifier @@ Id.mk "ty.list"; element_type' ]
  in
  
  let pp_application
      (constructor    : nanotype          )
      (type_arguments : type_argument list) : PP.document AC.t
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
    AC.return @@ PP.simple_app [ pp_identifier @@ Id.mk "ty.bitvector"; nexpr' ]
  in
  match typ with
   | Ty_unit                     -> AC.return @@ pp_identifier @@ Id.mk "ty.unit"
   | Ty_bool                     -> AC.return @@ pp_identifier @@ Id.mk "ty.bool"
   | Ty_int                      -> AC.return @@ pp_identifier @@ Id.mk "ty.int"
   | Ty_nat                      -> AC.return @@ pp_identifier @@ Id.mk "ty.nat"
   | Ty_string                   -> AC.return @@ pp_identifier @@ Id.mk "ty.string"
   | Ty_atom                     -> AC.return @@ pp_identifier @@ Id.mk "ty.atom"
   | Ty_custom id                -> AC.return @@ pp_identifier id
   | Ty_record                   -> AC.not_yet_implemented [%here]
   | Ty_prod (_, _)              -> AC.not_yet_implemented [%here]
   | Ty_sum (_, _)               -> AC.not_yet_implemented [%here]
   | Ty_app (constructor, targs) -> pp_application constructor targs
   | Ty_list typ                 -> pp_list typ
   | Ty_tuple ts                 -> pp_tuple ts
   | Ty_bitvector nexpr          -> pp_bitvector nexpr


and coq_type_of_nanotype (nanotype : nanotype) =
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
  | Ty_unit            -> AC.return @@ PP.string "Datatypes.unit"
  | Ty_bool            -> AC.return @@ PP.string "Datatypes.bool"
  | Ty_nat             -> AC.return @@ PP.string "nat"
  | Ty_int             -> AC.return @@ PP.string "Z"
  | Ty_string          -> AC.return @@ PP.string "String.string"
  | Ty_custom id       -> AC.return @@ pp_identifier id
  | Ty_bitvector n     -> coq_type_of_bitvector_type n
  | Ty_list t          -> coq_type_of_list_type t
  | Ty_app (t, ts)     -> coq_type_of_application t ts
  | Ty_tuple _ts       -> AC.not_yet_implemented [%here]
  | Ty_atom            -> AC.not_yet_implemented [%here]
  | Ty_record          -> AC.not_yet_implemented [%here]
  | Ty_prod (_, _)     -> AC.not_yet_implemented [%here]
  | Ty_sum (_, _)      -> AC.not_yet_implemented [%here]

and pp_type_argument (type_argument : type_argument) : PP.document AC.t =
  match type_argument with
  | TA_type t   -> pp_nanotype t
  | TA_numexp e -> pp_numeric_expression e
  | TA_bool nc  -> pp_numeric_constraint nc
