(* Fuel-based interpreter for µSail statements, generic over Katamaran programs.
   See validation/docs/validate.md. *)

From Stdlib Require Import
     Strings.String.
From Katamaran Require Import
     Base
     Program
     Semantics.

Import ctx.notations.
Import env.notations.

Set Implicit Arguments.

Module Type InterpOn (Import B : Base) (Import P : Program B).

  (* Machine state: the two global components of Katamaran's [Step]. *)
  Record State : Type :=
    mkState { st_regs : RegStore;
              st_mem  : Memory }.

  (* Outcome of a statement, as in [ForeignCall]: [inl msg] is [stm_fail msg],
     [inr v] is [stm_val v]. *)
  Definition Result (τ : Ty) : Type := (string + Val τ)%type.

  (* Answers foreign calls.  [None]: no answer (the run stops with [NoAnswer f]). *)
  Definition Oracle : Type :=
    forall Δ σ (f : 𝑭𝑿 Δ σ) (args : CStore Δ) (st : State),
      option (Result σ * State).

  (* Outcome of a run.  [Done]: finished, with a value or a µSail failure
     (inside [Result]); [OutOfFuel]; [NoAnswer f]: the oracle gave no answer
     for foreign function [f]. *)
  Inductive Outcome (A : Type) : Type :=
  | Done (a : A)
  | OutOfFuel
  | NoAnswer Δ σ (f : 𝑭𝑿 Δ σ).

  Arguments Done {A} a.
  Arguments OutOfFuel {A}.
  Arguments NoAnswer {A Δ σ} f.

  Definition obind {A B} (o : Outcome A) (k : A -> Outcome B) : Outcome B :=
    match o with
    | Done a => k a
    | OutOfFuel => OutOfFuel
    | NoAnswer f => NoAnswer f
    end.

  Definition omap {A B} (g : A -> B) (o : Outcome A) : Outcome B :=
    obind o (fun a => Done (g a)).

  Section WithOracle.
    Context (oracle : Oracle).

    (* Big-step evaluation.  Fuel bounds the recursion depth: every recursive
       call (sub-statement, continuation, function body) consumes one unit.
       Also returns the final local store. *)
    Fixpoint exec_aux (fuel : nat) {Γ τ} (s : Stm Γ τ) (st : State) (δ : CStore Γ)
      {struct fuel} : Outcome (Result τ * State * CStore Γ) :=
      match fuel with
      | O => OutOfFuel
      | S n =>
        match s with
        | stm_val _ v => Done (inr v, st, δ)
        | stm_exp e => Done (inr (eval e δ), st, δ)
        | stm_let x σ s1 k =>
            obind (exec_aux n s1 st δ) (fun '(r1, st1, δ1) =>
              match r1 with
              | inr v =>
                  omap (fun '(r, st2, δ2) => (r, st2, env.tail δ2))
                    (exec_aux n k st1 (env.snoc δ1 (x∷σ) v))
              | inl m => Done (inl m, st1, δ1)
              end)
        | @stm_block _ _ Δ δΔ k =>
            omap (fun '(r, st1, δ1) => (r, st1, env.drop Δ δ1))
              (exec_aux n k st (δ ►► δΔ))
        | @stm_assign _ _ x xIn s1 =>
            omap (fun '(r, st1, δ1) =>
                    match r with
                    | inr v => (inr v, st1, env.update δ1 xIn v)
                    | inl m => (inl m, st1, δ1)
                    end)
              (exec_aux n s1 st δ)
        | stm_call f es =>
            omap (fun '(r, st1, _) => (r, st1, δ))
              (exec_aux n (FunDef f) st (evals es δ))
        | stm_call_frame δΔ s1 =>
            omap (fun '(r, st1, _) => (r, st1, δ)) (exec_aux n s1 st δΔ)
        | stm_foreign f es =>
            match oracle f (evals es δ) st with
            | Some (r, st1) => Done (r, st1, δ)
            | None => NoAnswer f
            end
        | stm_lemmak _ _ k => exec_aux n k st δ
        | stm_seq s1 k =>
            obind (exec_aux n s1 st δ) (fun '(r1, st1, δ1) =>
              match r1 with
              | inr _ => exec_aux n k st1 δ1
              | inl m => Done (inl m, st1, δ1)
              end)
        | stm_assertk e1 e2 k =>
            if eval e1 δ then exec_aux n k st δ
            else Done (inl (eval e2 δ), st, δ)
        | stm_fail _ m => Done (inl m, st, δ)
        | stm_pattern_match s1 pat rhs =>
            obind (exec_aux n s1 st δ) (fun '(r1, st1, δ1) =>
              match r1 with
              | inr v =>
                  match pattern_match_val pat v with
                  | existT pc δpc =>
                      omap (fun '(r, st2, δ2) => (r, st2, env.drop _ δ2))
                        (exec_aux n (rhs pc) st1 (δ1 ►► δpc))
                  end
              | inl m => Done (inl m, st1, δ1)
              end)
        | stm_read_register r =>
            Done (inr (read_register (st_regs st) r), st, δ)
        | stm_write_register r e =>
            let v := eval e δ in
            Done (inr v, mkState (write_register (st_regs st) r v) (st_mem st), δ)
        | stm_bind s1 k =>
            obind (exec_aux n s1 st δ) (fun '(r1, st1, δ1) =>
              match r1 with
              | inr v => exec_aux n (k v) st1 δ1
              | inl m => Done (inl m, st1, δ1)
              end)
        | stm_debugk k => exec_aux n k st δ
        end
      end.

    (* The interface of the design document: final local store dropped. *)
    Definition exec (fuel : nat) {Γ τ} (s : Stm Γ τ) (st : State) (δ : CStore Γ) :
      Outcome (Result τ * State) :=
      omap (fun '(r, st1, _) => (r, st1)) (exec_aux fuel s st δ).

    (* Run a function body on argument values. *)
    Definition exec_call (fuel : nat) {Δ τ} (f : 𝑭 Δ τ) (args : CStore Δ) (st : State) :
      Outcome (Result τ * State) :=
      exec fuel (FunDef f) st args.

  End WithOracle.

End InterpOn.

Module MakeInterp (B : Base) (P : Program B) <: InterpOn B P.
  Include InterpOn B P.
End MakeInterp.

(* Soundness w.r.t. Katamaran's small-step semantics.  STATED ONLY, NOT
   PROVED: [exec_aux_sound] is a module-type parameter; nothing instantiates
   this module type. *)
Module Type InterpSoundOn (Import B : Base) (Import P : Program B)
  (Import SEM : Semantics B P) (Import I : InterpOn B P).

  Import SmallStepNotations.

  (* Every oracle answer is allowed by [ForeignCall]. *)
  Definition OracleSound (oracle : Oracle) : Prop :=
    forall Δ σ (f : 𝑭𝑿 Δ σ) (args : CStore Δ) (st st' : State) (r : Result σ),
      oracle _ _ f args st = Some (r, st') ->
      ForeignCall f args r (st_regs st) (st_regs st') (st_mem st) (st_mem st').

  Definition result_stm {Γ τ} (r : Result τ) : Stm Γ τ :=
    match r with
    | inl m => stm_fail τ m
    | inr v => stm_val τ v
    end.

  Parameter exec_aux_sound :
    forall (oracle : Oracle), OracleSound oracle ->
    forall fuel Γ τ (s : Stm Γ τ) (st st' : State) (δ δ' : CStore Γ) (r : Result τ),
      exec_aux oracle fuel s st δ = Done (r, st', δ') ->
      ⟨ st_regs st, st_mem st, δ, s ⟩ --->* ⟨ st_regs st', st_mem st', δ', result_stm r ⟩.

End InterpSoundOn.
