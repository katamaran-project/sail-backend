open! ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let pp_foreign_kit () : PP.t GC.t =
  GC.generation_block [%here] "Foreign Kit" begin
    let title =
      PP.string "ForeignKit"
    and contents =
      PP.vertical @@ List.map ~f:PP.string [
        "Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)";
        "  (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop := False.";
        "Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :";
        "  exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.";
        "Proof. destruct f. Qed."
      ]
    in
    GC.return @@ Coq.pp_section title contents
  end
