open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let pp_foreign_kit () : PP.t GC.t =
  GC.generation_block [%here] "Foreign Kit" begin
    let title =
      PP.string "ForeignKit"
    and contents =
      PP.vertical @@ List.map ~f:PP.string [
        "(* (unconstrained σ) returns any value of σ, never fails, and changes nothing *)";
        "Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)";
        "  (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop :=";
        "  match f with";
        "  | unconstrained _ => γ' = γ /\\ μ' = μ /\\ exists v, res = inr v";
        "  end.";
        "Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :";
        "  exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.";
        "Proof. destruct f as [σ' inhabitant]. exists γ, μ, (inr inhabitant). cbn. eauto. Qed."
      ]
    in
    GC.return @@ Coq.pp_section title contents
  end
