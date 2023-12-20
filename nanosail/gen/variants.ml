open PPrint
open Ast
open Monad


let generate_inductive_type sail_definition ({ identifier; type_quantifier; constructors } : variant_definition) =
  let identifier' =
    Sail.pp_identifier identifier
  in
  let pp_constructor_type (nanotype : nanotype) =
    let* ts = map Sail.pp_nanotype @@ Ast.tuple_to_list nanotype
    in
    let ts = ts @ [ identifier' ]
    in
    generate @@ separate (string " -> ") ts
  in
  let inductive_type =
    let* type_quantifier' =
      map (fun (id, kind) ->
          let* kind' = Sail.pp_kind kind
          in
          generate (Sail.pp_identifier id, kind')
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
