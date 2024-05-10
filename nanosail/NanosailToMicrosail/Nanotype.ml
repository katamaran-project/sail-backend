open Base
open PP
open Ast
open Numeric
open Monads.Notations.Star(AnnotationContext)
open Identifier

module AC = AnnotationContext
module PP = PPrint


let rec pp_nanotype (typ : nanotype) =
  let pp_product x y =
    parens @@ simple_app [ pp_identifier @@ Id.mk "ty.prod"; x; y ]
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
    AC.return @@ parens @@ simple_app [ pp_identifier @@ Id.mk "ty.list"; element_type' ]
  in
  
  let pp_application constructor type_arguments =
    let* constructor' = pp_nanotype constructor
    in
    let* type_arguments' =
      AC.map ~f:pp_type_argument type_arguments
    in
    AC.return @@ parens @@ simple_app (constructor' :: type_arguments')
  in
  
  let pp_bitvector nexpr =
    let* nexpr' = pp_numeric_expression nexpr
    in
    AC.return @@ simple_app [ pp_identifier @@ Id.mk "ty.bitvector"; nexpr' ]
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
  match nanotype with
  | Ty_unit            -> AC.return @@ string "Datatypes.unit"
  | Ty_bool            -> AC.return @@ string "Datatypes.bool"
  | Ty_nat             -> AC.return @@ string "nat"
  | Ty_int             -> AC.return @@ string "Z"
  | Ty_string          -> AC.return @@ string "String.string"
  | Ty_custom id       -> AC.return @@ pp_identifier id
  | Ty_bitvector n     -> begin
      let* n' = pp_numeric_expression n
      in
      AC.return @@ string "bv" ^^ space ^^ n'
    end
  | Ty_list t          -> begin
      let* t' = coq_type_of_nanotype t
      in
      AC.return @@ PP.(separate space [ string "list"; parens t' ])
    end
  | Ty_app (t, ts)     -> begin
      let* t   = coq_type_of_nanotype t
      and* ts' = AC.map ~f:(Fn.compose (AC.lift ~f:parens) pp_type_argument) ts
      in
      AC.return @@ separate space (t :: ts')
    end
  | Ty_tuple _ts       -> AC.not_yet_implemented [%here]
  | Ty_atom            -> AC.not_yet_implemented [%here]
  | Ty_record          -> AC.not_yet_implemented [%here]
  | Ty_prod (_, _)     -> AC.not_yet_implemented [%here]
  | Ty_sum (_, _)      -> AC.not_yet_implemented [%here]

and pp_type_argument (type_argument : type_argument) =
  match type_argument with
  | TA_type t   -> pp_nanotype t
  | TA_numexp e -> pp_numeric_expression e
  | TA_bool nc  -> pp_numeric_constraint nc
