open! ExtBase
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_fun_inductive_type (function_definitions : Ast.Definition.Function.t list) : PP.t GC.t =
  let pp_function_declaration (function_definition : Ast.Definition.Function.t) : (PP.t * PP.t) GC.t =
    let name =
      Identifier.pp function_definition.function_name
    in
    let* function_type =
      let* parameter_bindings =
        let* pp_parameter_bindings =
          let pp (id : Ast.Identifier.t) (typ : Ast.Type.t) =
            let pp_id =
              Identifier.pp id
            in
            let* pp_typ =
              Nanotype.pp_nanotype typ
            in
            GC.return (pp_id, pp_typ)
          in
          GC.map ~f:(Fn.uncurry pp) function_definition.function_type.parameters
        in
        let ps =
          List.map ~f:(Fn.uncurry MuSail.pp_bind) pp_parameter_bindings
        in
        GC.return @@ Coq.pp_list ps
      in
      let* return_type =
        Nanotype.pp_nanotype function_definition.function_type.return_type
      in
      GC.return begin
        PP.(
          horizontal
            [
              string "Fun";
              horizontal [ parameter_bindings; surround parens return_type ] (* todo check this *)
            ]
        )
      end
    in
    GC.return (
      PP.annotate [%here] name,
      PP.annotate [%here] function_type
    )
  in
  GC.block begin
    let name = PP.string "Fun"
    and typ  = PP.string "PCtx -> Ty -> Set"
    in
    GC.pp_inductive_type name typ @@ fun add_constructor -> begin
      GC.iter function_definitions ~f:(fun (function_definition : Ast.Definition.Function.t) ->
          if
            function_definition.polymorphic && not (List.is_empty function_definition.monomorphs)
          then
            (* function is polymorhic and has monomorphs; add all monomorphs associated with it *)
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


let generate (function_definitions : Ast.Definition.Function.t list) : PP.t GC.t =
  GC.generation_block [%here] "FunDeclKit" begin
    let* pp_fun_inductive_type = pp_fun_inductive_type function_definitions
    in
    let pp_definitions : PP.t =
      PP.vertical @@ List.map ~f:PP.string [
        "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
        "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
        "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
      ]
    in      
    let contents =
      PP.vertical [
        pp_fun_inductive_type;
        pp_definitions
      ]
    in
    GC.return begin
      Coq.pp_section (PP.string "FunDeclKit") contents
    end
  end
