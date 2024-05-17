open Base
open PPrint
open Ast
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint


let generate_inductive_type (variant_definition : variant_definition) : PP.document AC.t =
  let { identifier; type_quantifier; constructors } = variant_definition
  in
  let inductive_type =
    let identifier' =
      pp_identifier identifier
    in
    let pp_constructor_types (field_nanotypes : nanotype list) =
      let* ts = AC.map ~f:Nanotype.coq_type_of_nanotype field_nanotypes
      in
      let ts = ts @ [ identifier' ]
      in
      AC.return @@ separate (string " -> ") ts
    in
    let* type_quantifier' =
      AC.map type_quantifier ~f:(fun (id, kind) ->
          let* kind' = Sail.pp_kind kind
          in
          AC.return (pp_identifier id, kind')
        )
    in
    Coq.build_inductive_type
      identifier'
      ~parameters: type_quantifier'
      (string "Set")
      begin
        fun add_constructor ->
        AC.iter constructors ~f:(fun (constructor, typ) ->
            let* typ' = pp_constructor_types typ
            in
            add_constructor ~typ:typ' (pp_identifier constructor))
      end
  in
  inductive_type

let generate_constructors_inductive_type (variant_definition  : variant_definition) =
  let identifier = pp_identifier @@ Id.add_suffix "Constructor" variant_definition.identifier
  and typ = pp_identifier @@ Id.mk "Set"
  and constructor_names = List.map ~f:fst variant_definition.constructors
  in
  Coq.build_inductive_type identifier typ (fun add_constructor ->
      AC.iter constructor_names
        ~f:(fun case -> add_constructor @@ pp_identifier @@ Id.add_prefix "K" case)
    )

let generate (variant_definition : variant_definition) =
  let* inductive_type = generate_inductive_type variant_definition
  and* constructors_inductive_type = generate_constructors_inductive_type variant_definition
  in
  AC.return @@ PP.separate (twice hardline) [
                   inductive_type;
                   constructors_inductive_type
                 ]

