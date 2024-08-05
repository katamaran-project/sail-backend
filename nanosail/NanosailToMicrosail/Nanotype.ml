open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let rec pp_nanotype (typ : Ast.Type.t) : PP.document GC.t =
  let pp_tuple elts =
    let* elts' = GC.map ~f:pp_nanotype elts
    in
    GC.return PP.(separate space [ string "ty.tuple"; Coq.list elts' ])

  and pp_list element_type =
    let* element_type' = pp_nanotype element_type
    in
    GC.return @@ PP.parens @@ PP.simple_app [ Identifier.pp @@ Ast.Identifier.mk "ty.list"; element_type' ]

  and pp_application
      (constructor    : Ast.Type.t             )
      (type_arguments : Ast.TypeArgument.t list) : PP.document GC.t
    =
    let* constructor' = pp_nanotype constructor
    in
    let* type_arguments' =
      GC.map ~f:(GC.(compose (Fn.compose return PP.parens) pp_type_argument)) type_arguments
    in
    GC.return @@ PP.parens @@ PP.simple_app (constructor' :: type_arguments')

  and pp_bitvector nexpr =
    let* nexpr' = Numeric.Expression.pp nexpr
    in
    GC.return @@ PP.simple_app [ Identifier.pp @@ Ast.Identifier.mk "ty.bitvector"; nexpr' ]

  and pp_enum identifier =
    let tag = Identifier.reified_enum_name identifier
    in
    GC.return @@ PP.string @@ Printf.sprintf "ty.enum %s" @@ Ast.Identifier.string_of tag

  and pp_record identifier =
    let tag = Identifier.reified_record_name identifier
    in
    GC.return @@ PP.string @@ Printf.sprintf "ty.record %s" @@ Ast.Identifier.string_of tag

  and pp_variant identifier =
    let tag = Identifier.reified_variant_name identifier
    in
    GC.return @@ PP.string @@ Printf.sprintf "ty.union %s" @@ Ast.Identifier.string_of tag

  and pp_product t1 t2 =
    let* t1' = pp_nanotype t1
    and* t2' = pp_nanotype t2
    in
    GC.return PP.(separate space [ string "ty.prod"; parens t1'; parens t2' ])

  and pp_alias id _typ =
    GC.return @@ Identifier.pp @@ Ast.Identifier.add_prefix "ty." id

  and ty s =
    GC.return @@ Identifier.pp @@ Ast.Identifier.mk @@ "ty." ^ s

  in
  match typ with
   | Unit                             -> ty "unit"
   | Bool                             -> ty "bool"
   | Int                              -> ty "int"
   | String                           -> ty "string"
   | Record id                        -> pp_record id
   | Variant id                       -> pp_variant id
   | Product (t1, t2)                 -> pp_product t1 t2
   | Sum (_, _)                       -> GC.not_yet_implemented [%here]
   | Application (constructor, targs) -> pp_application constructor targs
   | Enum id                          -> pp_enum id
   | List typ                         -> pp_list typ
   | Tuple ts                         -> pp_tuple ts
   | Bitvector nexpr                  -> pp_bitvector nexpr
   | Alias (id, typ)                  -> pp_alias id typ


and coq_type_of_nanotype (nanotype : Ast.Type.t) = (* todo check if this does what it's supposed to... also look for where it's being used *)
  let coq_type_of_bitvector_type n =
    let* n' = Numeric.Expression.pp n
    in
    GC.return @@ PP.(string "bv" ^^ space ^^ n')

  and coq_type_of_list_type t =
    let* t' = coq_type_of_nanotype t
    in
    GC.return @@ PP.(separate space [ string "list"; parens t' ])

  and coq_type_of_application t ts =
    let* t   = coq_type_of_nanotype t
    and* ts' = GC.map ~f:(Fn.compose (GC.lift ~f:PP.parens) pp_type_argument) ts
    in
    GC.return @@ PP.separate PP.space (t :: ts')

  and coq_type_of_tuple ts =
    let* ts' = GC.map ~f:coq_type_of_nanotype ts
    in
    GC.return @@ Coq.pp_tuple_type ts'

  and coq_type_of_product t1 t2 =
    let* t1' = coq_type_of_nanotype t1
    and* t2' = coq_type_of_nanotype t2
    in
    GC.return @@ Coq.pp_tuple_type [ t1'; t2' ]

  in
  match nanotype with
  | Unit                -> GC.return @@ PP.string "Datatypes.unit"
  | Bool                -> GC.return @@ PP.string "Datatypes.bool"
  | Int                 -> GC.return @@ PP.string "Z"
  | String              -> GC.return @@ PP.string "String.string"
  | Bitvector n         -> coq_type_of_bitvector_type n
  | List t              -> coq_type_of_list_type t
  | Application (t, ts) -> coq_type_of_application t ts
  | Tuple ts            -> coq_type_of_tuple ts
  | Record id           -> GC.return @@ Identifier.pp id
  | Enum id             -> GC.return @@ Identifier.pp id
  | Variant id          -> GC.return @@ Identifier.pp id
  | Product (t1, t2)    -> coq_type_of_product t1 t2
  | Sum (_, _)          -> GC.not_yet_implemented [%here]
  | Alias (id, _)       -> GC.return @@ Identifier.pp id

and pp_type_argument (type_argument : Ast.TypeArgument.t) : PP.document GC.t =
  match type_argument with
  | Type t              -> pp_nanotype t
  | NumericExpression e -> Numeric.Expression.pp e
  | Bool nc             -> Numeric.Constraint.pp nc
