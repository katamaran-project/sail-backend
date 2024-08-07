open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let generate (function_definitions : Ast.Definition.Function.t list) =
  let pp_function_declaration (function_definition : Ast.Definition.Function.t) =
    let name = Identifier.pp function_definition.function_name in
    let* function_type =
      let* parameter_bindings =
        let* pp_parameter_bindings =
          let pp (id : Ast.Identifier.t) (t : Ast.Type.t) =
            let* t' = Nanotype.pp_nanotype t
            in
            GC.return (id, t')
          in
          GC.map ~f:(Auxlib.uncurry pp) function_definition.function_type.parameters
        in
        let ps = List.map ~f:PPSail.pp_bind pp_parameter_bindings
        in
        GC.return @@ Coq.pp_list ps
      in
      let* return_type =
        Nanotype.pp_nanotype function_definition.function_type.return_type
      in
      GC.return PP.(
          horizontal [
            string "Fun";
            align @@ horizontal_or_vertical [ parameter_bindings; parens return_type ] (* todo check this *)
          ]
        )
    in
    GC.return (name, function_type)
  in
  let* inductive_type_declaration =
    GC.block begin
      let name = PP.string "Fun"
      and typ = PP.string "PCtx -> Ty -> Set"
      in
      GC.pp_inductive_type name typ @@ fun add_constructor -> begin
        GC.iter function_definitions ~f:(fun function_definition ->
            let* name, function_type = pp_function_declaration function_definition
            in
            add_constructor ~typ:function_type name
          )
      end
    end
  in
  let contents =
    PP.vertical ~separator:PP.hardline [
        inductive_type_declaration;
        PP.vertical_strings [
            "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
            "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
            "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
          ]
      ]
  in
  GC.return @@ Coq.pp_section (Ast.Identifier.mk "FunDeclKit") contents
