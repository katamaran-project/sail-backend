open Base
open PP
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let generate (function_definitions : Ast.Definition.Function.t list) =
  let pp_function_declaration (function_definition : Ast.Definition.Function.t) =
    let name = pp_identifier function_definition.function_name in
    let* function_type =
      let* parameter_bindings =
        let* pp_parameter_bindings =
          let pp (id : Ast.Identifier.t) (t : Ast.Type.t) =
            let* t' = Nanotype.pp_nanotype t
            in
            AC.return (id, t')
          in
          AC.map ~f:(Auxlib.uncurry pp) function_definition.function_type.parameters
        in
        let* ps = AC.map ~f:PPSail.pp_bind pp_parameter_bindings
        in
        AC.return @@ Coq.list ps
      in
      let* return_type = Nanotype.pp_nanotype function_definition.function_type.return_type
      in
      AC.return @@
          concat [
              string "Fun";
              space;
              align (
                  group (
                      concat [
                          parameter_bindings;
                          break 1;
                          parens return_type
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
  Coq.section (Ast.Identifier.mk "FunDeclKit") contents
