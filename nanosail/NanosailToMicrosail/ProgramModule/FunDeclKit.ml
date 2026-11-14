open ExtBase
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
              Type.pp_type typ
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
        Type.pp_type function_definition.function_type.return_type
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


(*
   The foreign functions. For now, there is only one family:

     unconstrained σ

   which produces an unconstrained value of type σ. The uses of definitions that the configuration
   excludes become calls to it (Configuration.FunctionExclusion.Unconstrained, Configuration.is_excluded_register).
   Its semantics is in ForeignKit. Its implicit argument, a value of σ, only shows that σ has a value
   (ForeignProgress needs one); see pp_unconstrained_inhabitant.
*)
let pp_foreign_function_inductive_type () : PP.t =
  PP.vertical @@ List.map ~f:PP.string [
    "Inductive FunX : PCtx -> Ty -> Set :=";
    "| unconstrained (σ : Ty) {inhabitant : Unconstrained σ} : FunX ctx.nil σ.";
  ]


(*
   Rocq finds the inhabitant of (unconstrained σ) by type class search: the hint below builds
   a value of a concrete type σ with the first constructor of each inductive type it meets
   (bv.zero for bitvectors). The value plays no part in the semantics.
   This is outside the FunDeclKit section, so that the hint holds in the rest of the module.
*)
let pp_unconstrained_inhabitant () : PP.t =
  PP.vertical @@ List.map ~f:PP.string [
    "Class Unconstrained (σ : Ty) : Set := unconstrained_inhabitant : Val σ.";
    "";
    "Ltac construct_inhabitant :=";
    "  cbn; first [ exact bv.zero | constructor; construct_inhabitant ].";
    "";
    "#[export] Hint Extern 1 (Unconstrained _) => unfold Unconstrained; construct_inhabitant : typeclass_instances.";
  ]


let generate (function_definitions : Ast.Definition.Function.t list) : PP.t GC.t =
  GC.generation_block [%here] "FunDeclKit" begin
    let* pp_fun_inductive_type = pp_fun_inductive_type function_definitions
    in
    let pp_definitions : PP.t =
      PP.vertical @@ List.map ~f:PP.string [
        "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
        "Definition 𝑭𝑿 : PCtx -> Ty -> Set := FunX.";
        "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
      ]
    in
    let contents =
      PP.vertical [
        pp_fun_inductive_type;
        pp_foreign_function_inductive_type ();
        pp_definitions
      ]
    in
    GC.return begin
      PP.paragraphs [
        pp_unconstrained_inhabitant ();
        Coq.pp_section (PP.string "FunDeclKit") contents
      ]
    end
  end
