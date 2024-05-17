open Base
open PP
open Ast
open Monads.Notations.Star(AnnotationContext)
open Identifier

module AC = AnnotationContext


let generate function_definitions =
  let pp_function_declaration function_definition =
    let name = pp_identifier function_definition.function_name in
    let* function_type =
      let* parameter_types =
        let* ps = AC.map ~f:Sail.pp_bind function_definition.function_type.arg_types
        in
        AC.return @@ Coq.list ps
      in
      let* return_type = Nanotype.pp_nanotype function_definition.function_type.ret_type
      in
      AC.return @@
          concat [
              string "Fun";
              space;
              align (
                  group (
                      concat [
                          parameter_types;
                          break 1;
                          return_type
                        ]
                    )
                )
            ]
    in
    AC.return (name, function_type)
  in
  let inductive_type_declaration =
    let name = string "Fun"
    and typ = string "PCtx -> Ty -> Set"
    in
    Coq.build_inductive_type name typ @@ fun add_constructor -> begin
        AC.iter function_definitions ~f:(fun function_definition ->
            let* name, function_type = pp_function_declaration function_definition
            in
            add_constructor ~typ:function_type name
          )
      end
  in
  let contents =
    separate small_step [
        Coq.annotate inductive_type_declaration;
        separate_map hardline utf8string [
            "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
            "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
            "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
          ]
      ]
  in
  Coq.section (Id.mk "FunDeclKit") contents
