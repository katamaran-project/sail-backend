(* Runner of gmake validate-sweep (validation/docs/validate.md, "All 16-bit
   parcels"): a setup program from reset to a fixed state, then, for each
   parcel, the parcel written at PC (not logged) and one step from the fixed
   state.  Lines: "fixed <line>" (the fixed state in full), then per parcel
   "<parcel> <line>" for each line of its state that is not in the fixed
   state, then "<parcel> end"; or "<parcel> stop <why>" (OutOfFuel,
   NoAnswer).  A generated Batch.v prints [sweep_output] with [print_plines]. *)

From Stdlib Require
     Strings.PrimString.
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

Definition fuel : nat := 1000.

(* [n] steps, each returning a value, none halting: the step counter (steps
   that returned true) and the last step's result and state. *)
Fixpoint setup (n : nat) (k : Z) (r : Result ty.bool) (st : State) : option (Z * Result ty.bool * State) :=
  match n with
  | O => Some (k, r, st)
  | S n' =>
      match run_step_at fuel k st with
      | Done (inr b, st') =>
          if htif_halted st' then None
          else setup n' (if b then k + 1 else k) (inr b) st'
      | _ => None
      end
  end.

Definition parcel_lines (k pc : Z) (st : State) (d : Dumped) (p : Z) : list string :=
  let name := hex 4 p in
  let st1 := mkState (st_regs st) (write_bytes (st_mem st) pc 2 p) in
  match run_step_at fuel k st1 with
  | OutOfFuel => [name ++ " stop OutOfFuel"]
  | NoAnswer _ => [name ++ " stop NoAnswer"]
  | Done (r, st') => map (fun l => name ++ " " ++ l) (delta_of d (dumped_step r st')) ++ [name ++ " end"]
  end.

Definition sweep_lines (n : nat) (prog parcels : list Z) : list string :=
  match reset fuel (load_words mem_empty c_entry prog) with
  | Done (inr _, st0) =>
      match setup n 0 (inr true) st0 with
      | Some (k, r, st) =>
          let d := dumped_step r st in
          let pc := bv.unsigned (read_register (st_regs st) PC) in
          map (fun l => "fixed " ++ l) (map line (items d))
            ++ flat_map (parcel_lines k pc st d) parcels
      | None => ["error setup"]
      end
  | _ => ["error reset"]
  end.

Definition sweep_output (n : nat) (prog parcels : list Z) : list PrimString.string :=
  map pstring_of_string (sweep_lines n prog parcels).
