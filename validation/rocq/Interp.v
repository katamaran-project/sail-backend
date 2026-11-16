(* Fuel-based interpreter for µSail statements, generic over Katamaran programs.
   See validation/docs/interp.md. *)

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

  (* Answers foreign calls.  [None]: no answer (the run stops with [None]). *)
  Definition Oracle : Type :=
    forall Δ σ (f : 𝑭𝑿 Δ σ) (args : CStore Δ) (st : State),
      option (Result σ * State).

  Section WithOracle.
    Context (oracle : Oracle).

    (* Big-step evaluation.  Fuel bounds the recursion depth: every recursive
       call (sub-statement, continuation, function body) consumes one unit.
       [None]: out of fuel, or the oracle gave no answer.  Also returns the
       final local store. *)
    Fixpoint exec_aux (fuel : nat) {Γ τ} (s : Stm Γ τ) (st : State) (δ : CStore Γ)
      {struct fuel} : option (Result τ * State * CStore Γ) :=
      match fuel with
      | O => None
      | S n =>
        match s with
        | stm_val _ v => Some (inr v, st, δ)
        | stm_exp e => Some (inr (eval e δ), st, δ)
        | stm_let x σ s1 k =>
            match exec_aux n s1 st δ with
            | Some (inr v, st1, δ1) =>
                match exec_aux n k st1 (env.snoc δ1 (x∷σ) v) with
                | Some (r, st2, δ2) => Some (r, st2, env.tail δ2)
                | None => None
                end
            | Some (inl m, st1, δ1) => Some (inl m, st1, δ1)
            | None => None
            end
        | @stm_block _ _ Δ δΔ k =>
            match exec_aux n k st (δ ►► δΔ) with
            | Some (r, st1, δ1) => Some (r, st1, env.drop Δ δ1)
            | None => None
            end
        | @stm_assign _ _ x xIn s1 =>
            match exec_aux n s1 st δ with
            | Some (inr v, st1, δ1) => Some (inr v, st1, env.update δ1 xIn v)
            | Some (inl m, st1, δ1) => Some (inl m, st1, δ1)
            | None => None
            end
        | stm_call f es =>
            match exec_aux n (FunDef f) st (evals es δ) with
            | Some (r, st1, _) => Some (r, st1, δ)
            | None => None
            end
        | stm_call_frame δΔ s1 =>
            match exec_aux n s1 st δΔ with
            | Some (r, st1, _) => Some (r, st1, δ)
            | None => None
            end
        | stm_foreign f es =>
            match oracle f (evals es δ) st with
            | Some (r, st1) => Some (r, st1, δ)
            | None => None
            end
        | stm_lemmak _ _ k => exec_aux n k st δ
        | stm_seq s1 k =>
            match exec_aux n s1 st δ with
            | Some (inr _, st1, δ1) => exec_aux n k st1 δ1
            | Some (inl m, st1, δ1) => Some (inl m, st1, δ1)
            | None => None
            end
        | stm_assertk e1 e2 k =>
            if eval e1 δ then exec_aux n k st δ
            else Some (inl (eval e2 δ), st, δ)
        | stm_fail _ m => Some (inl m, st, δ)
        | stm_pattern_match s1 pat rhs =>
            match exec_aux n s1 st δ with
            | Some (inr v, st1, δ1) =>
                match pattern_match_val pat v with
                | existT pc δpc =>
                    match exec_aux n (rhs pc) st1 (δ1 ►► δpc) with
                    | Some (r, st2, δ2) => Some (r, st2, env.drop _ δ2)
                    | None => None
                    end
                end
            | Some (inl m, st1, δ1) => Some (inl m, st1, δ1)
            | None => None
            end
        | stm_read_register r =>
            Some (inr (read_register (st_regs st) r), st, δ)
        | stm_write_register r e =>
            let v := eval e δ in
            Some (inr v, mkState (write_register (st_regs st) r v) (st_mem st), δ)
        | stm_bind s1 k =>
            match exec_aux n s1 st δ with
            | Some (inr v, st1, δ1) => exec_aux n (k v) st1 δ1
            | Some (inl m, st1, δ1) => Some (inl m, st1, δ1)
            | None => None
            end
        | stm_debugk k => exec_aux n k st δ
        end
      end.

    (* The interface of the design document: final local store dropped. *)
    Definition exec (fuel : nat) {Γ τ} (s : Stm Γ τ) (st : State) (δ : CStore Γ) :
      option (Result τ * State) :=
      match exec_aux fuel s st δ with
      | Some (r, st1, _) => Some (r, st1)
      | None => None
      end.

    (* Run a function body on argument values. *)
    Definition exec_call (fuel : nat) {Δ τ} (f : 𝑭 Δ τ) (args : CStore Δ) (st : State) :
      option (Result τ * State) :=
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
      exec_aux oracle fuel s st δ = Some (r, st', δ') ->
      ⟨ st_regs st, st_mem st, δ, s ⟩ --->* ⟨ st_regs st', st_mem st', δ', result_stm r ⟩.

End InterpSoundOn.
