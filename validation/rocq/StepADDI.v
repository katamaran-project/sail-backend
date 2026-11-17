(* One step of the generated CHERIoT model from reset on ADDI x1, x0, 5.
   See validation/docs/validate.md. *)

From Stdlib Require Import
     Strings.String
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
     Harness.

Import UntitledBase.
Import ModelProgram.
Import Harness.I.

Open Scope Z_scope.

(* ADDI x1, x0, 5: imm 5, rs1 0, funct3 0, rd 1, opcode 0x13. *)
Definition addi_x1_x0_5 : Z := 0x00500093.

Definition mem_addi : Memory := load_word mem_empty c_entry addi_x1_x0_5.

(* Least fuel (nesting depth) that init_model and step need. *)
Definition fuel_reset : nat := 65.
Definition fuel_step  : nat := 50.

(* What the check looks at: step's result, PC, x1's address and tag, minstret. *)
Definition observe (o : Outcome (Result ty.bool * State)) :
  Outcome (Result ty.bool * Z * Z * bool * Z) :=
  omap (fun '(r, st) =>
          let x := read_register (st_regs st) x1 in
          (r,
           bv.unsigned (read_register (st_regs st) PC),
           bv.unsigned (Capability_address x),
           Capability_tag x,
           bv.unsigned (read_register (st_regs st) minstret)))
    o.

(* reset, then step; a failure of init_model is the run's failure. *)
Definition run (fr fs : nat) : Outcome (Result ty.bool * State) :=
  obind (reset fr mem_addi) (fun '(r, st) =>
    match r with
    | inr _ => run_step fs st
    | inl m => Done (inl m, st)
    end).

(* Reset: PC at the entry. *)
Example reset_pc :
  omap (fun '(r, st) => (r, bv.unsigned (read_register (st_regs st) PC))) (reset fuel_reset mem_addi)
  = Done (inr tt, c_entry).
Proof. Time vm_compute. reflexivity. Qed.

(* The step: retires, PC + 4, x1 = 5 (untagged), minstret = 1. *)
Example step_addi :
  observe (run fuel_reset fuel_step) = Done (inr true, c_entry + 4, 5, false, 1).
Proof. Time vm_compute. reflexivity. Qed.

(* The fuel is the least that works: one less runs out of fuel. *)
Example reset_fuel_least :
  omap fst (reset (fuel_reset - 1) mem_addi) = OutOfFuel.
Proof. Time vm_compute. reflexivity. Qed.

Example step_fuel_least : observe (run fuel_reset (fuel_step - 1)) = OutOfFuel.
Proof. Time vm_compute. reflexivity. Qed.

(*** Runs: how they stop ***)

Definition run_fuel : nat := 300.

(* Per step: result, PC, mcause. *)
Definition observe_run (o : Outcome (list (Result ty.bool * State) * Stop)) :
  Outcome (list (Result ty.bool * Z * Z) * Stop) :=
  omap (fun '(rs, s) =>
          (List.map (fun '(r, st) =>
                       (r, bv.unsigned (read_register (st_regs st) PC),
                        bv.unsigned (read_register (st_regs st) mcause))) rs, s))
    o.

Definition run_from (m : Memory) (n : nat) : Outcome (list (Result ty.bool * State) * Stop) :=
  obind (reset run_fuel m) (fun '(_, st) => run_steps run_fuel n st).

(* Encoding 0 traps (illegal, mcause 2; step true: an instruction was
   decoded) to MTCC = 0, outside memory; from
   then on every fetch faults (mcause 1) and step returns false.  The step
   limit ends the run (the emulator would loop forever). *)
Definition mem_trap : Memory := load_word mem_empty c_entry 0.

Example trap_loop :
  observe_run (run_from mem_trap 3) =
    Done (cons (inr true, 0, 2) (cons (inr false, 0, 1) (cons (inr false, 0, 1) nil)), StepLimit).
Proof. Time vm_compute. reflexivity. Qed.

(* validation/scripts/examples/addi-add-sw.s: ends via htif after 9 steps,
   before the limit (20). *)
Definition addi_add_sw : list Z :=
  cons 0x00500593 (cons 0x00b58633 (cons 0x03d0055b (cons 0x800016b7 (cons 0x20d5055b
  (cons 0x10c52023 (cons 0x00100713 (cons 0x00e52023 (cons 0x00052223 nil)))))))).

Definition mem_addi_add_sw : Memory := load_words mem_empty c_entry addi_add_sw.

Example addi_add_sw_halts :
  omap (fun '(rs, s) => (List.length rs, s)) (run_from mem_addi_add_sw 20) = Done (9%nat, Halted).
Proof. Time vm_compute. reflexivity. Qed.
