open! ExtBase
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let genblock loc label (doc : PP.document GC.t) =
  GC.generation_block loc label doc


let generate (function_definitions : Ast.Definition.Function.t list) =
  genblock [%here] "FunDeclKit" begin
    let pp_function_declaration (function_definition : Ast.Definition.Function.t) =
      let name =
        PP.annotate [%here] @@ Identifier.pp function_definition.function_name
      in
      let* function_type =
        let* parameter_bindings =
          let* pp_parameter_bindings =
            let pp (id : Ast.Identifier.t) (typ : Ast.Type.t) =
              let pp_id =
                PP.annotate [%here] @@ Identifier.pp id
              in
              let* pp_typ =
                GC.pp_annotate [%here] @@ Nanotype.pp_nanotype typ
              in
              GC.return (pp_id, pp_typ)
            in
            GC.map ~f:(Fn.uncurry pp) function_definition.function_type.parameters
          in
          let ps =
            List.map ~f:(Fn.uncurry MuSail.pp_bind) pp_parameter_bindings
          in
          GC.return @@ PP.annotate [%here] @@ Coq.pp_list ps
        in
        let* return_type =
          GC.pp_annotate [%here] @@ Nanotype.pp_nanotype function_definition.function_type.return_type
        in
        GC.return begin
            PP.annotate [%here] begin
                PP.(
                horizontal
                  [
                    string "Fun";
                    horizontal [ parameter_bindings; surround parens return_type ] (* todo check this *)
                  ]
                )
              end
          end
      in
      GC.return (name, function_type)
    in
    let* inductive_type_declaration =
      GC.block begin
        let name = PP.string "Fun"
        and typ  = PP.string "PCtx -> Ty -> Set"
        in
        GC.pp_inductive_type name typ @@ fun add_constructor -> begin
          GC.iter function_definitions ~f:(fun (function_definition : Ast.Definition.Function.t) ->
              if
                function_definition.polymorphic
              then
                (* function is polymorhic; add all monomorphs associated with it *)
                GC.iter function_definition.monomorphs ~f:(fun (monomorph_definition : Ast.Definition.Function.t) ->
                    let* name, function_type = pp_function_declaration monomorph_definition
                    in
                    add_constructor ~typ:function_type name
                  )
              else
                let* name, function_type = pp_function_declaration function_definition
                in
                add_constructor ~typ:function_type name
            )
        end
      end
    in
    let contents =
      PP.annotate [%here] begin
          PP.vertical [
              inductive_type_declaration;
              PP.vertical @@ List.map ~f:PP.string [
                                 "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
                                 "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
                                 "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
                               ]
            ]
        end
    in
    GC.return @@ PP.annotate [%here] @@ Coq.pp_section (Ast.Identifier.mk "FunDeclKit") contents
  end
