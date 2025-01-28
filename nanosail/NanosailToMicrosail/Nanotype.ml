open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let rec pp_nanotype (typ : Ast.Type.t) : PP.document GC.t =
  (* Tuple of two elements get represented as products *)
  let pp_tuple (subtypes : Ast.Type.t list) =
    let pp_product
        (t1 : Ast.Type.t)
        (t2 : Ast.Type.t) : PP.document GC.t
      =
      let* t1' =
        GC.pp_annotate [%here] @@ pp_nanotype t1
      and* t2' =
        GC.pp_annotate [%here] @@ pp_nanotype t2
      in
      GC.return begin
        PP.annotate [%here] begin
          Coq.pp_application (PP.string "ty.prod") [
            PP.(surround parens) t1';
            PP.(surround parens) t2';
          ]
        end
      end
    in
    match subtypes with
    | []       -> GC.fail [%here] "should not occur"
    | [_]      -> GC.fail [%here] "should not occur"
    | [t1; t2] -> pp_product t1 t2
    | _        -> begin
        let* pp_elts = GC.map ~f:pp_nanotype subtypes
        in
        GC.return begin
          PP.annotate [%here] begin
            Coq.pp_application
              (PP.string "ty.tuple")
              [ Coq.pp_list pp_elts ]
          end
        end
      end

  and pp_list element_type =
    let* pp_element_type = pp_nanotype element_type
    in
    GC.return begin
      PP.annotate [%here] begin
        PP.(surround parens) begin
          Coq.pp_application
            (Identifier.pp @@ Ast.Identifier.mk "ty.list")
            [ pp_element_type ]
        end
      end
    end

  and pp_application
      (constructor    : Ast.Type.t             )
      (type_arguments : Ast.TypeArgument.t list) : PP.document GC.t
    =
    let* pp_constructor =
      GC.pp_annotate [%here] @@ pp_nanotype constructor
    and* pp_type_arguments =
      GC.map ~f:(GC.(compose (Fn.compose return PP.(surround parens)) pp_type_argument)) type_arguments
    in
    GC.return begin
      PP.annotate [%here] begin
        PP.(surround parens) begin
          PP.separate_horizontally ~separator:PP.space (pp_constructor :: pp_type_arguments)
        end
      end
    end

  and pp_bitvector (nexpr : Ast.Numeric.Expression.t) : PP.document GC.t =
    let* pp_nexpr =
      GC.pp_annotate [%here] @@ Numeric.Expression.pp nexpr
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application
          (Identifier.pp @@ Ast.Identifier.mk "ty.bvec")
          [ PP.(surround parens) pp_nexpr ]
      end
    end

  and pp_enum (identifier : Ast.Identifier.t) : PP.document GC.t =
    let tag =
      Identifier.reified_enum_name identifier
    in
    GC.return begin
      PP.annotate [%here] begin
        PP.string @@ Printf.sprintf "ty.enum %s" @@ Ast.Identifier.to_string tag
      end
    end

  and pp_record (identifier : Ast.Identifier.t) : PP.document GC.t =
    let tag =
      Identifier.reified_record_name identifier
    in
    GC.return begin
      PP.annotate [%here] begin
        PP.string @@ Printf.sprintf "ty.record %s" @@ Ast.Identifier.to_string tag
      end
    end

  and pp_variant (identifier : Ast.Identifier.t) : PP.document GC.t =
    let tag =
      Identifier.reified_variant_name identifier
    in
    GC.return begin
      PP.annotate [%here] begin
        PP.string @@ Printf.sprintf "ty.union %s" @@ Ast.Identifier.to_string tag
      end
    end

  and pp_alias id _typ =
    GC.return @@ PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.add_prefix "ty." id

  and ty s =
    GC.return @@ PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.mk @@ "ty." ^ s

  in
  match typ with
  | Unit                             -> GC.pp_annotate [%here] @@ ty "unit"
  | Bool                             -> GC.pp_annotate [%here] @@ ty "bool"
  | Int                              -> GC.pp_annotate [%here] @@ ty "int"
  | String                           -> GC.pp_annotate [%here] @@ ty "string"
  | Bit                              -> GC.pp_annotate [%here] @@ ty "bool"
  | Record id                        -> GC.pp_annotate [%here] @@ pp_record id
  | Variant id                       -> GC.pp_annotate [%here] @@ pp_variant id
  | Application (constructor, targs) -> GC.pp_annotate [%here] @@ pp_application constructor targs
  | Enum id                          -> GC.pp_annotate [%here] @@ pp_enum id
  | List typ                         -> GC.pp_annotate [%here] @@ pp_list typ
  | Tuple ts                         -> GC.pp_annotate [%here] @@ pp_tuple ts
  | Bitvector nexpr                  -> GC.pp_annotate [%here] @@ pp_bitvector nexpr
  | Alias (id, typ)                  -> GC.pp_annotate [%here] @@ pp_alias id typ
  | Sum (_, _)                       -> GC.not_yet_implemented [%here]
  | Range (_, _)                     -> GC.pp_annotate [%here] @@ ty "int"


and coq_type_of_nanotype (nanotype : Ast.Type.t) =
  let coq_type_of_bitvector_type n =
    let* n' =
      GC.pp_annotate [%here] @@ Numeric.Expression.pp n
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application
          (PP.string "bv")
          [ n' ]
      end
    end

  and coq_type_of_list_type t =
    let* t' =
      GC.pp_annotate [%here] @@ coq_type_of_nanotype t
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application
          (PP.string "list")
          [ PP.(surround parens) t' ]
      end
    end

  and coq_type_of_application t ts =
    let* t   = GC.pp_annotate [%here] @@ coq_type_of_nanotype t
    and* ts' =
      let aux (type_argument : Ast.TypeArgument.t) : PP.document GC.t =
        GC.pp_annotate [%here] @@ GC.lift ~f:PP.(surround parens) @@ pp_type_argument type_argument
      in
      GC.map ~f:aux ts
    in
    GC.return @@ PP.annotate [%here] @@ Coq.pp_application t ts'

  and coq_type_of_tuple ts =
    let* ts' = GC.map ~f:coq_type_of_nanotype ts
    in
    match ts' with
    | []       -> GC.fail [%here] "should not occur"
    | [_]      -> GC.fail [%here] "should not occur"
    | [t1; t2] -> GC.return @@ PP.annotate [%here] @@ Coq.pp_tuple_type [ t1; t2 ]
    | _        -> GC.return @@ PP.annotate [%here] @@ Coq.pp_tuple_type ts'



  in
  match nanotype with
  | Unit                -> GC.return @@ PP.annotate [%here] @@ PP.string "Datatypes.unit"
  | Bool                -> GC.return @@ PP.annotate [%here] @@ PP.string "Datatypes.bool"
  | Int                 -> GC.return @@ PP.annotate [%here] @@ PP.string "Z"
  | String              -> GC.return @@ PP.annotate [%here] @@ PP.string "String.string"
  | Record id           -> GC.return @@ PP.annotate [%here] @@ Identifier.pp id
  | Enum id             -> GC.return @@ PP.annotate [%here] @@ Identifier.pp id
  | Variant id          -> GC.return @@ PP.annotate [%here] @@ Identifier.pp id
  | Alias (id, _)       -> GC.return @@ PP.annotate [%here] @@ Identifier.pp id
  | Bitvector n         -> GC.pp_annotate [%here] @@ coq_type_of_bitvector_type n
  | List t              -> GC.pp_annotate [%here] @@ coq_type_of_list_type t
  | Application (t, ts) -> GC.pp_annotate [%here] @@ coq_type_of_application t ts
  | Tuple ts            -> GC.pp_annotate [%here] @@ coq_type_of_tuple ts
  | Bit                 -> GC.not_yet_implemented [%here]
  | Sum (_, _)          -> GC.not_yet_implemented [%here]
  | Range (_, _)        -> GC.not_yet_implemented [%here]

and pp_type_argument (type_argument : Ast.TypeArgument.t) : PP.document GC.t =
  match type_argument with
  | Type t              -> GC.pp_annotate [%here] @@ pp_nanotype t
  | NumericExpression e -> GC.pp_annotate [%here] @@ Numeric.Expression.pp e
  | Bool nc             -> GC.pp_annotate [%here] @@ Numeric.Constraint.pp nc
