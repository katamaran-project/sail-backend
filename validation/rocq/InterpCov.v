(* Interp.v's interpreter, also counting the functions it calls: for the
   coverage of a validation run (validation/docs/validate.md, "Coverage").
   Generic over Katamaran programs, like Interp.v.  [Interp.exec] stays as it
   is (its soundness statement refers to it); this is a copy with one more
   component, the counts, threaded through.  The values, states and stores
   are those of [exec_aux]; coverage.py checks that each run ends as the
   validation run did. *)

From Stdlib Require Import
     Strings.String
     NArith.NArith
     PArith.PArith
     FSets.FMapPositive.
From Katamaran Require Import
     Base
     Program.
From Validation Require Import Interp.

Import ctx.notations.
Import env.notations.

Set Implicit Arguments.

Module Type InterpCovOn (Import B : Base) (Import P : Program B) (Import I : InterpOn B P).

  (* Calls per function: key -> (name, number of calls). *)
  Definition Counts : Type := PositiveMap.t (string * N).

  (* [h_calls]: µSail functions ([stm_call], and the function a run starts
     with); [h_foreign]: foreign functions ([stm_foreign]). *)
  Record Hits : Type :=
    mkHits { h_calls   : Counts;
             h_foreign : Counts }.

  Definition hits0 : Hits := mkHits (PositiveMap.empty _) (PositiveMap.empty _).

  Definition count (k : positive) (name : string) (c : Counts) : Counts :=
    match PositiveMap.find k c with
    | Some (_, n) => PositiveMap.add k (name, N.succ n) c
    | None => PositiveMap.add k (name, 1%N) c
    end.

  Section WithOracle.
    Context (oracle : Oracle)
            (fkey  : forall Δ τ, 𝑭 Δ τ -> positive)
            (fname : forall Δ τ, 𝑭 Δ τ -> string)
            (xkey  : forall Δ τ, 𝑭𝑿 Δ τ -> positive)
            (xname : forall Δ τ, 𝑭𝑿 Δ τ -> string).

    Definition hit_call {Δ τ} (f : 𝑭 Δ τ) (h : Hits) : Hits :=
      mkHits (count (fkey f) (fname f) (h_calls h)) (h_foreign h).

    Definition hit_foreign {Δ τ} (f : 𝑭𝑿 Δ τ) (h : Hits) : Hits :=
      mkHits (h_calls h) (count (xkey f) (xname f) (h_foreign h)).

    (* As [exec_aux], with the counts [h]. *)
    Fixpoint exec_cov_aux (fuel : nat) {Γ τ} (s : Stm Γ τ) (st : State) (δ : CStore Γ) (h : Hits)
      {struct fuel} : Outcome (Result τ * State * CStore Γ * Hits) :=
      match fuel with
      | O => OutOfFuel
      | S n =>
        match s with
        | stm_val _ v => Done (inr v, st, δ, h)
        | stm_exp e => Done (inr (eval e δ), st, δ, h)
        | stm_let x σ s1 k =>
            obind (exec_cov_aux n s1 st δ h) (fun '(r1, st1, δ1, h1) =>
              match r1 with
              | inr v =>
                  omap (fun '(r, st2, δ2, h2) => (r, st2, env.tail δ2, h2))
                    (exec_cov_aux n k st1 (env.snoc δ1 (x∷σ) v) h1)
              | inl m => Done (inl m, st1, δ1, h1)
              end)
        | @stm_block _ _ Δ δΔ k =>
            omap (fun '(r, st1, δ1, h1) => (r, st1, env.drop Δ δ1, h1))
              (exec_cov_aux n k st (δ ►► δΔ) h)
        | @stm_assign _ _ x xIn s1 =>
            omap (fun '(r, st1, δ1, h1) =>
                    match r with
                    | inr v => (inr v, st1, env.update δ1 xIn v, h1)
                    | inl m => (inl m, st1, δ1, h1)
                    end)
              (exec_cov_aux n s1 st δ h)
        | stm_call f es =>
            omap (fun '(r, st1, _, h1) => (r, st1, δ, h1))
              (exec_cov_aux n (FunDef f) st (evals es δ) (hit_call f h))
        | stm_call_frame δΔ s1 =>
            omap (fun '(r, st1, _, h1) => (r, st1, δ, h1)) (exec_cov_aux n s1 st δΔ h)
        | stm_foreign f es =>
            match oracle f (evals es δ) st with
            | Some (r, st1) => Done (r, st1, δ, hit_foreign f h)
            | None => NoAnswer f
            end
        | stm_lemmak _ _ k => exec_cov_aux n k st δ h
        | stm_seq s1 k =>
            obind (exec_cov_aux n s1 st δ h) (fun '(r1, st1, δ1, h1) =>
              match r1 with
              | inr _ => exec_cov_aux n k st1 δ1 h1
              | inl m => Done (inl m, st1, δ1, h1)
              end)
        | stm_assertk e1 e2 k =>
            if eval e1 δ then exec_cov_aux n k st δ h
            else Done (inl (eval e2 δ), st, δ, h)
        | stm_fail _ m => Done (inl m, st, δ, h)
        | stm_pattern_match s1 pat rhs =>
            obind (exec_cov_aux n s1 st δ h) (fun '(r1, st1, δ1, h1) =>
              match r1 with
              | inr v =>
                  match pattern_match_val pat v with
                  | existT pc δpc =>
                      omap (fun '(r, st2, δ2, h2) => (r, st2, env.drop _ δ2, h2))
                        (exec_cov_aux n (rhs pc) st1 (δ1 ►► δpc) h1)
                  end
              | inl m => Done (inl m, st1, δ1, h1)
              end)
        | stm_read_register r =>
            Done (inr (read_register (st_regs st) r), st, δ, h)
        | stm_write_register r e =>
            let v := eval e δ in
            Done (inr v, mkState (write_register (st_regs st) r v) (st_mem st), δ, h)
        | stm_bind s1 k =>
            obind (exec_cov_aux n s1 st δ h) (fun '(r1, st1, δ1, h1) =>
              match r1 with
              | inr v => exec_cov_aux n (k v) st1 δ1 h1
              | inl m => Done (inl m, st1, δ1, h1)
              end)
        | stm_debugk k => exec_cov_aux n k st δ h
        end
      end.

    (* As [exec_call]; the call of [f] itself is counted. *)
    Definition exec_call_cov (fuel : nat) {Δ τ} (f : 𝑭 Δ τ) (args : CStore Δ) (st : State) (h : Hits) :
      Outcome (Result τ * State * Hits) :=
      omap (fun '(r, st1, _, h1) => (r, st1, h1)) (exec_cov_aux fuel (FunDef f) st args (hit_call f h)).

  End WithOracle.

End InterpCovOn.

Module MakeInterpCov (B : Base) (P : Program B) (I : InterpOn B P) <: InterpCovOn B P I.
  Include InterpCovOn B P I.
End MakeInterpCov.
