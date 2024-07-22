open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let rec pp_nanotype (typ : Ast.Type.t) =
  let pp_tuple elts =
    let* elts' = AnnotationContext.map ~f:pp_nanotype elts
    in
    AC.return PP.(separate space [ string "ty.tuple"; Coq.list elts' ])

  and pp_list element_type =
    let* element_type' = pp_nanotype element_type
    in
    AC.return @@ PP.parens @@ PP.simple_app [ Identifier.pp_identifier @@ Ast.Identifier.mk "ty.list"; element_type' ]

  and pp_application
      (constructor    : Ast.Type.t             )
      (type_arguments : Ast.TypeArgument.t list) : PP.document AC.t
    =
    let* constructor' = pp_nanotype constructor
    in
    let* type_arguments' =
      AC.map ~f:(AC.(compose (Fn.compose return PP.parens) pp_type_argument)) type_arguments
    in
    AC.return @@ PP.parens @@ PP.simple_app (constructor' :: type_arguments')

  and pp_bitvector nexpr =
    let* nexpr' = Numeric.Expression.pp nexpr
    in
    AC.return @@ PP.simple_app [ Identifier.pp_identifier @@ Ast.Identifier.mk "ty.bitvector"; nexpr' ]

  and pp_enum identifier =
    let tag = TranslationSettings.convert_enum_name_to_tag identifier
    in
    AC.return @@ PP.string @@ Printf.sprintf "ty.enum %s" @@ Ast.Identifier.string_of tag

  and pp_product t1 t2 =
    let* t1' = pp_nanotype t1
    and* t2' = pp_nanotype t2
    in
    AC.return PP.(separate space [ string "ty.prod"; t1'; t2' ])

  and pp_record identifier =
    let tag = TranslationSettings.convert_record_name_to_tag identifier
    in
    AC.return @@ PP.string @@ Printf.sprintf "ty.record %s" @@ Ast.Identifier.string_of tag

  and ty s =
    AC.return @@ Identifier.pp_identifier @@ Ast.Identifier.mk @@ "ty." ^ s

  in
  match typ with
   | Unit                             -> ty "unit"
   | Bool                             -> ty "bool"
   | Int                              -> ty "int"
   | String                           -> ty "string"
   | Record id                        -> pp_record id
   | Product (t1, t2)                 -> pp_product t1 t2
   | Sum (_, _)                       -> AC.not_yet_implemented [%here]
   | Application (constructor, targs) -> pp_application constructor targs
   | Enum id                          -> pp_enum id
   | List typ                         -> pp_list typ
   | Tuple ts                         -> pp_tuple ts
   | Bitvector nexpr                  -> pp_bitvector nexpr


and coq_type_of_nanotype (nanotype : Ast.Type.t) =
  let coq_type_of_bitvector_type n =
    let* n' = Numeric.Expression.pp n
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
  | Int                 -> AC.return @@ PP.string "Z"
  | String              -> AC.return @@ PP.string "String.string"
  | Bitvector n         -> coq_type_of_bitvector_type n
  | List t              -> coq_type_of_list_type t
  | Application (t, ts) -> coq_type_of_application t ts
  | Tuple _ts           -> AC.not_yet_implemented [%here]
  | Record _id          -> AC.not_yet_implemented [%here]
  | Enum _id            -> AC.not_yet_implemented [%here] (* todo lookup Coq Inductive type corresponding to the enum named id *)
  | Product (_, _)      -> AC.not_yet_implemented [%here]
  | Sum (_, _)          -> AC.not_yet_implemented [%here]

and pp_type_argument (type_argument : Ast.TypeArgument.t) : PP.document AC.t =
  match type_argument with
  | Type t              -> pp_nanotype t
  | NumericExpression e -> Numeric.Expression.pp e
  | Bool nc             -> Numeric.Constraint.pp nc
