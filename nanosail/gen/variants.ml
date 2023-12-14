open PPrint
open Ast
open Monad


let generate_inductive_type sail_definition (variant_definition : variant_definition) =
  match variant_definition with
  | { identifier; constructors } ->
     let inductive_type =
       Coq.mbuild_inductive_type
         (string identifier)
         (string "Set")
         (fun add_constructor ->
           iter
             (fun (constructor, typ) ->
               let* typ' = Sail.pp_nanotype typ
               in
               add_constructor ~typ:typ' (string constructor))
             constructors
         )
     in
     Coq.annotate_with_original_definition sail_definition (Coq.annotate inductive_type)


let generate (variant_definitions : (sail_definition * variant_definition) list) =
  if
    List.is_empty variant_definitions
  then
    []
  else
    List.map (Auxlib.uncurry generate_inductive_type) variant_definitions
