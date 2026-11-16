open ExtBase
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* The type-level parameters and hypothesis of a function's signature (see TypeLevelParameters) *)
let signature (function_definition : Ast.Definition.Function.t) : TypeLevelParameters.signature =
  TypeLevelParameters.of_function function_definition


let type_level_parameters (function_definition : Ast.Definition.Function.t) : (Ast.Identifier.t * Ast.Kind.t) list =
  (signature function_definition).parameters


let pp_fun_inductive_type (function_definitions : Ast.Definition.Function.t list) : PP.t GC.t =
  let pp_function_declaration (function_definition : Ast.Definition.Function.t) : (PP.t * PP.t * PP.t) GC.t =
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
    (* widths and type variables of the signature become implicit arguments (see TypeLevelParameters) *)
    let pp_parameters =
      TypeLevelParameters.pp_signature_binders @@ signature function_definition
    in
    GC.return (
      PP.annotate [%here] name,
      PP.annotate [%here] pp_parameters,
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
                let* name, parameters, function_type = pp_function_declaration monomorph_definition
                in
                add_constructor ~parameters ~typ:function_type name
              )
          else
            let* name, parameters, function_type = pp_function_declaration function_definition
            in
            add_constructor ~parameters ~typ:function_type name
        )
    end
  end


(*
   The foreign functions:

     unconstrained σ

   produces an unconstrained value of type σ. The uses of definitions that the configuration
   excludes become calls to it (Configuration.FunctionExclusion.Unconstrained, Configuration.is_excluded_register).
   Its implicit argument, a value of σ, only shows that σ has a value
   (ForeignProgress needs one); see pp_unconstrained_inhabitant.

   Then one constructor for each extern that a call was translated into a foreign call to
   (see ForeignFunctions), with the extern's signature; for example

     | popcount {kid__n : nat} : FunX [ "arg1" ∷ ty.bvec (kid__n) ] (ty.int)

   Their semantics is in ForeignKit.
*)
let pp_foreign_function_inductive_type () : PP.t GC.t =
  let* foreign_functions = ForeignFunctions.used ()
  in
  GC.pp_inductive_type (PP.string "FunX") (PP.string "PCtx -> Ty -> Set") @@ fun add_constructor -> begin
    let* () =
      add_constructor ~typ:(PP.string "FunX ctx.nil σ") ~parameters:(PP.string "(σ : Ty) {inhabitant : Unconstrained σ}") (PP.string "unconstrained")
    in
    GC.iter foreign_functions ~f:(fun (foreign_function : ForeignFunctions.t) ->
        let* name, parameters, typ = ForeignFunctions.pp_constructor foreign_function
        in
        add_constructor ~parameters ~typ name
      )
  end


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


(*
   For functions with hypotheses (Sail's constraints on their size variables, see TypeLevelParameters)
   and branches that Sail's flow typing specialises (Ast.Statement.Specialize):

   - sail_constraint proves an IsTrue side condition from the IsTrue hypotheses and equalities in scope,
     by lia; Katamaran's own hint only computes closed booleans. Both run during type class search,
     for the hypotheses of a callee and for side conditions such as uop.vector_subrange's.
   - sail_cast casts an expression along an equality of size variables (F is the type as a function of it).
   - sail_decide decides a branch fact (a constraint that Sail's flow typing adds in a branch).
*)
let pp_hypothesis_support () : PP.t =
  PP.vertical @@ List.map ~f:PP.string [
    "Ltac sail_constraint :=";
    "  repeat match goal with H : IsTrue _ |- _ => apply IsTrue.from, Is_true_eq_true in H end;";
    "  constructor; apply Is_true_eq_left;";
    "  (* unification can leave widths unfolded (8 * n as a chain of fix add): refold them for lia *)";
    "  change (fix add (n m : nat) {struct n} : nat := match n with | Datatypes.O => m | Datatypes.S p => Datatypes.S (add p m) end) with Nat.add in *;";
    "  try change (fix mul (n m : nat) {struct n} : nat := match n with | Datatypes.O => Datatypes.O | Datatypes.S p => Nat.add m (mul p m) end) with Nat.mul in *;";
    "  rewrite ?Nat2Z.inj_div, ?Nat2Z.inj_mod in *;";
    "  repeat rewrite ?Bool.andb_true_iff, ?Bool.orb_true_iff, ?Bool.negb_true_iff, ?Z.leb_le, ?Z.ltb_lt, ?Z.eqb_eq, ?Z.eqb_neq,";
    "                 ?Nat.leb_le, ?Nat.ltb_lt, ?Nat.eqb_eq, ?Nat.eqb_neq in *;";
    "  Z.div_mod_to_equations; lia.";
    "";
    "#[export] Hint Extern 20 (IsTrue _) => sail_constraint : typeclass_instances.";
    "";
    "Definition sail_cast {Γ : LCtx} (F : nat -> Ty) {m n : nat} (H : m = n) (e : Exp Γ (F m)) : Exp Γ (F n) :=";
    "  match H with eq_refl => e end.";
    "";
    "Definition sail_decide (b : bool) : {IsTrue b} + {~ IsTrue b} :=";
    "  match b return {IsTrue b} + {~ IsTrue b} with";
    "  | true  => left (@IsTrue.mk true I)";
    "  | false => right (fun H => IsTrue.from H)";
    "  end.";
  ]


let needs_hypothesis_support (function_definitions : Ast.Definition.Function.t list) : bool =
  List.exists function_definitions ~f:(fun (function_definition : Ast.Definition.Function.t) ->
      not (List.is_empty (signature function_definition).hypotheses)
      || Ast.Statement.contains_specialize function_definition.function_body (* sail_cast, sail_decide *)
    )


let generate (function_definitions : Ast.Definition.Function.t list) : PP.t GC.t =
  GC.generation_block [%here] "FunDeclKit" begin
    let* pp_fun_inductive_type = pp_fun_inductive_type function_definitions
    in
    let* pp_foreign_function_inductive_type = pp_foreign_function_inductive_type ()
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
        pp_foreign_function_inductive_type;
        pp_definitions
      ]
    in
    GC.return begin
      PP.paragraphs begin
        List.build_list (fun { add; _ } ->
            add @@ pp_unconstrained_inhabitant ();
            if needs_hypothesis_support function_definitions then add @@ pp_hypothesis_support ();
            add @@ Coq.pp_section (PP.string "FunDeclKit") contents
          )
      end
    end
  end
