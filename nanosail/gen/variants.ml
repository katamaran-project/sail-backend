open PPrint
open Ast
open Annotation_monad
open Monads.Notations.Star(Annotation_monad)


let generate_inductive_type
      (sail_definition                               : sail_definition   )
      ({ identifier; type_quantifier; constructors } : variant_definition)
  =
  let inductive_type =
    let identifier' =
      Sail.pp_identifier identifier
    in
    let pp_constructor_type (nanotype : nanotype) =
      let* ts = map Sail.pp_nanotype @@ Ast.tuple_to_list nanotype
      in
      let ts = ts @ [ identifier' ]
      in
      return @@ separate (string " -> ") ts
    in
    let* type_quantifier' =
      map (fun (id, kind) ->
          let* kind' = Sail.pp_kind kind
          in
          return (Sail.pp_identifier id, kind')
        ) type_quantifier
    in
    Coq.mbuild_inductive_type
      identifier'
      ~parameters: type_quantifier'
      (string "Set")
      (fun add_constructor ->
         iter
           (fun (constructor, typ) ->
              let* typ' = pp_constructor_type typ
              in
              add_constructor ~typ:typ' (string constructor))
           constructors
      )
  in
  Coq.annotate_with_original_definition sail_definition @@ Coq.annotate inductive_type
