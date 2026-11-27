Require Export base.

Require Export machine_decl.
Require Machine.tt_registers
        Machine.I.tt_steps
        Machine.model.

From Stdlib Require Import Classes.EquivDec
                           Strings.String
                           ZArith.BinInt.

From Katamaran Require Import Semantics.Registers
                              Bitvector
                              Program.

From stdpp Require Import finite.

From Equations Require Import Equations.

Import ctx.notations
       ctx.resolution
       env.notations
       bv.notations.

Local Open Scope string_scope.
Local Open Scope list_scope.

Import UntitledBase.

Import ModelProgramDecl.

Module Import ModelProgram <: Program UntitledBase.
  Include ModelProgramDecl.
  
  Definition FunDef {Δ}
                    {τ}
                    (f : Fun Δ τ) : Stm Δ τ :=
    match f in Fun Δ τ return Stm Δ τ with
    | toggle             => Machine.tt_registers.fun_toggle
    | Mk_flags           => Machine.tt_registers.fun_Mk_flags
    | _get_flags_bits    => Machine.tt_registers.fun__get_flags_bits
    | _update_flags_bits => Machine.tt_registers.fun__update_flags_bits
    | _get_flags_C       => Machine.tt_registers.fun__get_flags_C
    | _update_flags_C    => Machine.tt_registers.fun__update_flags_C
    | _get_flags_Z       => Machine.tt_registers.fun__get_flags_Z
    | _update_flags_Z    => Machine.tt_registers.fun__update_flags_Z
    | step               => Machine.I.tt_steps.fun_step
    | main               => Machine.model.fun_main
    end.
  
  Include DefaultRegStoreKit UntitledBase.
  
  Section ForeignKit.
    (* (unconstrained σ) returns any value of σ, never fails, and changes nothing; for the externs, see ForeignFunctions in the translator *)
    
    Definition bv_width {n} (x : bv n) : nat := n.
    
    Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)
      (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop :=
      match f in FunX σs σ return NamedEnv Val σs -> string + Val σ -> Prop with
      | @unconstrained _ _ => fun _ res => γ' = γ /\ μ' = μ /\ exists v, res = inr v
      end args res.
    
    Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :
      exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.
    Proof.
      destruct f as [σ' inhabitant].
      - exists γ, μ, (inr inhabitant). cbn. eauto.
    Qed.
  End ForeignKit.
  
  Include ProgramMixin UntitledBase.
  
  Import callgraph.
  
    Lemma fundef_bindfree (Δ : PCtx) (τ : Ty) (f : Fun Δ τ) :
      stm_bindfree (FunDef f).
    Proof. destruct f; now vm_compute. Qed.
  
    Definition 𝑭_call_graph := generic_call_graph.
    Lemma 𝑭_call_graph_wellformed : CallGraphWellFormed 𝑭_call_graph.
    Proof. apply generic_call_graph_wellformed, fundef_bindfree. Qed.
  
    Definition 𝑭_accessible {Δ τ} (f : 𝑭 Δ τ) : option (Accessible 𝑭_call_graph f) :=
      None.
End ModelProgram.
