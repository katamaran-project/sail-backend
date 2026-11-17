(* Runner for gmake validate (validation/docs/validate.md): a program from
   reset, up to a step limit; the state after reset and after every step, in
   the run-file format (state-format.md), then one line "end <why>".
   A generated Run.v per case calls [case_lines] and prints the result. *)

From Stdlib Require Import
     Strings.String
     Lists.List
     ZArith.ZArith.
From Katamaran Require Import
     Base
     Bitvector
     Program.
From CheriotModel Require Import
     base
     machine.
From Validation Require Import
     Interp
     Harness
     Names
     Dump.

Import ListNotations.
Import UntitledBase.
Import ModelProgram.
Import Harness.I.

Open Scope string_scope.
Open Scope Z_scope.

(* Fuel per call (depth, not steps; harness.md).  Generous: an instruction
   needing more shows up as OutOfFuel in the table. *)
Definition fuel : nat := 1000.

(* As Harness.run_steps, but keeps the states before an OutOfFuel or
   NoAnswer, so the comparison still covers them.  Why the run ended:
   Halted, Failed, StepLimit (as Harness.Stop), OutOfFuel, NoAnswer. *)
Fixpoint steps_aux (fuel n : nat) (k : Z) (st : State) (acc : list Dumped) : list Dumped * string :=
  match n with
  | O => (rev acc, "StepLimit")
  | S n' =>
      match run_step_at fuel k st with
      | OutOfFuel => (rev acc, "OutOfFuel")
      | NoAnswer _ => (rev acc, "NoAnswer")
      | Done (r, st') =>
          let acc' := dumped_step r st' :: acc in
          match r with
          | inl _ => (rev acc', "Failed")
          | inr b =>
              if htif_halted st' then (rev acc', "Halted")
              else steps_aux fuel n' (if b then k + 1 else k) st' acc'
          end
      end
  end.

Definition run_case (fuel n : nat) (prog : list Z) : list Dumped * string :=
  match reset fuel (load_words mem_empty c_entry prog) with
  | OutOfFuel => ([], "OutOfFuel")
  | NoAnswer _ => ([], "NoAnswer")
  | Done (r0, st0) =>
      let d0 := dumped_step r0 st0 in
      match r0 with
      | inl _ => ([d0], "Failed")
      | inr _ => steps_aux fuel n 0 st0 [d0]
      end
  end.

(* The run file's lines, then "end <why>". *)
Definition case_lines (fuel n : nat) (prog : list Z) : list string :=
  let '(ds, why) := run_case fuel n prog in
  dump_run ds ++ ["end " ++ why].
