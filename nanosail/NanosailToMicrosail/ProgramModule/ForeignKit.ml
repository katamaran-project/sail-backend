open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(*
   The semantics of the foreign functions (FunX, see FunDeclKit and ForeignFunctions):

   * (unconstrained σ) returns any value of σ, never fails, and changes nothing;
   * each extern that is a foreign function never fails and leaves the registers unchanged;
     a pure one leaves the memory unchanged, a monadic one may change it in any way;
     its result is given by a Rocq definition (semantics_<name>) or else unconstrained.

   For example

     Definition semantics_gcd_int (arg1 : Val (ty.int)) (arg2 : Val (ty.int)) : Val (ty.int) :=
       Z.gcd arg1 arg2.

     Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)
       (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop :=
       match f in FunX σs σ return NamedEnv Val σs -> string + Val σ -> Prop with
       | @unconstrained _ _ => fun _ res => γ' = γ /\ μ' = μ /\ exists v, res = inr v
       | @gcd_int => fun args res => γ' = γ /\ μ' = μ /\ res = inr (semantics_gcd_int (env.head (env.tail args)) (env.head args))
       | @read_device => fun args res => γ' = γ /\ exists v, res = inr v
       end args res.
*)
let pp_foreign_kit () : PP.t GC.t =
  GC.generation_block [%here] "Foreign Kit" begin
    let* foreign_functions = ForeignFunctions.used ()
    in
    let* semantics_definitions =
      GC.filter_map foreign_functions ~f:(fun (foreign_function : ForeignFunctions.t) ->
          match ForeignFunctions.semantics foreign_function with
          | Given term    -> GC.lift ~f:Option.some @@ ForeignFunctions.pp_semantics_definition foreign_function term
          | Unconstrained -> GC.return None
        )
    in
    let foreign_call =
      PP.vertical @@ List.concat [
        List.map ~f:PP.string [
          "Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)";
          "  (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop :=";
          "  match f in FunX σs σ return NamedEnv Val σs -> string + Val σ -> Prop with";
          "  | @unconstrained _ _ => fun _ res => γ' = γ /\\ μ' = μ /\\ exists v, res = inr v";
        ];
        List.map ~f:(fun foreign_function -> PP.indent @@ ForeignFunctions.pp_foreign_call_case foreign_function) foreign_functions;
        [ PP.string "  end args res." ];
      ]
    in
    let foreign_progress =
      let destruct_pattern =
        String.concat ~sep:"" @@ "[σ' inhabitant" :: List.map ~f:(Fn.const " |") foreign_functions @ [ "]" ]
      in
      PP.vertical @@ List.concat [
        List.map ~f:PP.string [
          "Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :";
          "  exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.";
          "Proof.";
          "  destruct f as " ^ destruct_pattern ^ ".";
          "  - exists γ, μ, (inr inhabitant). cbn. eauto.";
        ];
        List.map ~f:(fun foreign_function -> PP.indent @@ ForeignFunctions.pp_foreign_progress_case foreign_function) foreign_functions;
        [ PP.string "Qed." ];
      ]
    in
    let title =
      PP.string "ForeignKit"
    and contents =
      PP.paragraphs @@ List.concat [
        [
          PP.string "(* (unconstrained σ) returns any value of σ, never fails, and changes nothing; for the externs, see ForeignFunctions in the translator *)";
          PP.string "Definition bv_width {n} (x : bv n) : nat := n.";
        ];
        semantics_definitions;
        [
          foreign_call;
          foreign_progress;
        ]
      ]
    in
    GC.return @@ Coq.pp_section title contents
  end
