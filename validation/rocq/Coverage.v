(* Coverage of a validation run (validation/docs/validate.md, "Coverage"): the
   run of Validate.v (reset, then up to a step limit), with InterpCov.v's
   interpreter, printing the µSail functions and foreign functions called and
   how often.  A generated Cov.v per case calls [cov_lines]. *)

From Stdlib Require Import
     Strings.Ascii
     Strings.String
     Lists.List
     ZArith.ZArith
     NArith.NArith
     PArith.PArith
     FSets.FMapPositive.
From Ltac2 Require Import Ltac2.
From Katamaran Require Import
     Base
     Bitvector
     Program.
From CheriotModel Require Import
     base
     machine.
From Validation Require Import
     Interp
     InterpCov
     Harness
     Names.

Import ctx.notations.
Import env.notations.
Import ListNotations.
Import UntitledBase.
Import ModelProgram.
Import Harness.I.

Set Default Proof Mode "Classic".
Open Scope string_scope.
Open Scope Z_scope.

Module Import C := MakeInterpCov UntitledBase ModelProgram Harness.I.

(*** Keys and names of functions, from the inductive types ***)

(* The goal [keyed a]: a key for [a]'s head constructor.  Reduces to [positive]. *)
Definition keyed {A : Type} (a : A) : Set := positive.

Ltac2 rec pos_of_int (n : int) : constr :=
  if Int.equal n 1 then constr:(xH)
  else
    let p := pos_of_int (Int.lsr n 1) in
    if Int.equal (Int.land n 1) 1 then constr:(xI $p) else constr:(xO $p).

(* Solves the goals [keyed _] with 1, 2, ..., in order: after [destruct], one
   key per constructor. *)
Ltac2 key_goals () :=
  let next := Ref.ref 1 in
  Control.enter (fun () =>
    let k := pos_of_int (Ref.get next) in
    Ref.set next (Int.add (Ref.get next) 1);
    exact $k).

Definition fun_key : forall Δ τ (f : 𝑭 Δ τ), keyed f.
Proof. intros Δ τ f; destruct f; ltac2:(key_goals ()). Defined.

Definition fun_name : forall Δ τ (f : 𝑭 Δ τ), named f.
Proof. intros Δ τ f; destruct f; ltac2:(name_goal ()). Defined.

Definition funx_key : forall Δ τ (f : 𝑭𝑿 Δ τ), keyed f.
Proof. intros Δ τ f; destruct f; ltac2:(key_goals ()). Defined.

Definition funx_name : forall Δ τ (f : 𝑭𝑿 Δ τ), named f.
Proof. intros Δ τ f; destruct f; ltac2:(name_goal ()). Defined.

(*** Runs ***)

(* As Validate.fuel. *)
Definition fuel : nat := 1000.

Definition call_cov {Δ τ} (f : 𝑭 Δ τ) (args : CStore Δ) (st : State) (h : Hits) :
  Outcome (Result τ * State * Hits) :=
  exec_call_cov oracle fun_key fun_name funx_key funx_name fuel f args st h.

(* As Harness.reset. *)
Definition reset_cov (m : Memory) : Outcome (Result ty.unit * State * Hits) :=
  omap (fun '(r, st, h) =>
          match r with
          | inr _ => (r, set_pc st c_entry, h)
          | inl _ => (r, st, h)
          end)
    (call_cov init_model (env.snoc env.nil (_∷_) tt) (state0 m) hits0).

(* As Validate.steps_aux: the counts, why the run ended, the steps run.  The
   counts of a step that runs out of fuel or has no answer are dropped. *)
Fixpoint steps_cov (n : nat) (k : Z) (st : State) (h : Hits) (done : nat) : Hits * string * nat :=
  match n with
  | O => (h, "StepLimit", done)
  | S n' =>
      match call_cov step (env.snoc env.nil (_∷_) k) st h with
      | OutOfFuel => (h, "OutOfFuel", done)
      | NoAnswer _ => (h, "NoAnswer", done)
      | Done (r, st', h') =>
          match r with
          | inl _ => (h', "Failed", S done)
          | inr b =>
              if htif_halted st' then (h', "Halted", S done)
              else steps_cov n' (if b then k + 1 else k) st' h' (S done)
          end
      end
  end.

(* As Validate.run_case. *)
Definition run_cov (n : nat) (prog : list Z) : Hits * string * nat :=
  match reset_cov (load_words mem_empty c_entry prog) with
  | OutOfFuel => (hits0, "OutOfFuel", O)
  | NoAnswer _ => (hits0, "NoAnswer", O)
  | Done (r0, st0, h0) =>
      match r0 with
      | inl _ => (h0, "Failed", O)
      | inr _ => steps_cov n 0 st0 h0 O
      end
  end.

(*** Output ***)

Definition digit (d : N) : ascii := ascii_of_N (48 + d).

Fixpoint dec_aux (k : nat) (n : N) (acc : string) : string :=
  match k with
  | O => acc
  | S k' =>
      let acc' := String (digit (N.modulo n 10)) acc in
      if (n <? 10)%N then acc' else dec_aux k' (N.div n 10) acc'
  end.

Definition dec (n : N) : string := dec_aux (S (N.to_nat (N.log2 n))) n "".

Definition count_lines (kind : string) (c : Counts) : list string :=
  map (fun '(_, (name, n)) => kind ++ " " ++ name ++ " " ++ dec n) (PositiveMap.elements c).

(* "call <function> <calls>" per µSail function called, "foreign <function>
   <calls>" per foreign function, then "end <why> <steps>" (why: as
   Validate.case_lines). *)
Definition cov_lines (n : nat) (prog : list Z) : list string :=
  let '(h, why, steps) := run_cov n prog in
  count_lines "call" (h_calls h) ++ count_lines "foreign" (h_foreign h)
    ++ ["end " ++ why ++ " " ++ dec (N.of_nat steps)].
