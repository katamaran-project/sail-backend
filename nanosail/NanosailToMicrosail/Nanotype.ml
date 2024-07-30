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
    AC.return @@ PP.parens @@ PP.simple_app [ Identifier.pp @@ Ast.Identifier.mk "ty.list"; element_type' ]

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
    AC.return @@ PP.simple_app [ Identifier.pp @@ Ast.Identifier.mk "ty.bitvector"; nexpr' ]

  and pp_enum identifier =
    let tag = TranslationSettings.convert_enum_name_to_tag identifier
    in
    AC.return @@ PP.string @@ Printf.sprintf "ty.enum %s" @@ Ast.Identifier.string_of tag

  and pp_record identifier =
    let tag = TranslationSettings.convert_record_name_to_tag identifier
    in
    AC.return @@ PP.string @@ Printf.sprintf "ty.record %s" @@ Ast.Identifier.string_of tag

  and pp_variant identifier =
    let tag = TranslationSettings.convert_variant_name_to_tag identifier
    in
    AC.return @@ PP.string @@ Printf.sprintf "ty.union %s" @@ Ast.Identifier.string_of tag

  and pp_product t1 t2 =
    let* t1' = pp_nanotype t1
    and* t2' = pp_nanotype t2
    in
    AC.return PP.(separate space [ string "ty.prod"; t1'; t2' ])

  and ty s =
    AC.return @@ Identifier.pp @@ Ast.Identifier.mk @@ "ty." ^ s

  in
  match typ with
   | Unit                             -> ty "unit"
   | Bool                             -> ty "bool"
   | Int                              -> ty "int"
   | String                           -> ty "string"
   | Record id                        -> pp_record id
   | Variant id                       -> pp_variant id
   | Product (t1, t2)                 -> pp_product t1 t2
   | Sum (_, _)                       -> AC.not_yet_implemented [%here]
   | Application (constructor, targs) -> pp_application constructor targs
   | Enum id                          -> pp_enum id
   | List typ                         -> pp_list typ
   | Tuple ts                         -> pp_tuple ts
   | Bitvector nexpr                  -> pp_bitvector nexpr
   | Alias _id                        -> AC.not_yet_implemented [%here]


and coq_type_of_nanotype (nanotype : Ast.Type.t) = (* todo check if this does what it's supposed to... also look for where it's being used *)
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

  and coq_type_of_tuple ts =
    let* ts' = AC.map ~f:coq_type_of_nanotype ts
    in
    AC.return @@ Coq.pp_tuple_type ts'

  and coq_type_of_product t1 t2 =
    let* t1' = coq_type_of_nanotype t1
    and* t2' = coq_type_of_nanotype t2
    in
    AC.return @@ Coq.pp_tuple_type [ t1'; t2' ]

  in
  match nanotype with
  | Unit                -> AC.return @@ PP.string "Datatypes.unit"
  | Bool                -> AC.return @@ PP.string "Datatypes.bool"
  | Int                 -> AC.return @@ PP.string "Z"
  | String              -> AC.return @@ PP.string "String.string"
  | Bitvector n         -> coq_type_of_bitvector_type n
  | List t              -> coq_type_of_list_type t
  | Application (t, ts) -> coq_type_of_application t ts
  | Tuple ts            -> coq_type_of_tuple ts
  | Record id           -> AC.return @@ Identifier.pp id
  | Enum id             -> AC.return @@ Identifier.pp id
  | Variant id          -> AC.return @@ Identifier.pp id
  | Product (t1, t2)    -> coq_type_of_product t1 t2
  | Sum (_, _)          -> AC.not_yet_implemented [%here]
  | Alias id            -> AC.return @@ Identifier.pp id

and pp_type_argument (type_argument : Ast.TypeArgument.t) : PP.document AC.t =
  match type_argument with
  | Type t              -> pp_nanotype t
  | NumericExpression e -> Numeric.Expression.pp e
  | Bool nc             -> Numeric.Constraint.pp nc
