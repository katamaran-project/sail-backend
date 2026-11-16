(* One step of the generated CHERIoT model from reset on ADDI x1, x0, 5.
   See validation/docs/harness.md. *)

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
Definition observe (o : option (Result ty.bool * State)) :
  option (Result ty.bool * Z * Z * bool * Z) :=
  match o with
  | Some (r, st) =>
      let x := read_register (st_regs st) x1 in
      Some (r,
            bv.unsigned (read_register (st_regs st) PC),
            bv.unsigned (Capability_address x),
            Capability_tag x,
            bv.unsigned (read_register (st_regs st) minstret))
  | None => None
  end.

Definition run (fr fs : nat) : option (Result ty.bool * State) :=
  match reset fr mem_addi with
  | Some st => run_step fs st
  | None => None
  end.

(* Reset: PC at the entry. *)
Example reset_pc :
  option_map (fun st => bv.unsigned (read_register (st_regs st) PC)) (reset fuel_reset mem_addi)
  = Some c_entry.
Proof. Time vm_compute. reflexivity. Qed.

(* The step: retires, PC + 4, x1 = 5 (untagged), minstret = 1. *)
Example step_addi :
  observe (run fuel_reset fuel_step) = Some (inr true, c_entry + 4, 5, false, 1).
Proof. Time vm_compute. reflexivity. Qed.

(* The fuel is the least that works. *)
Example reset_fuel_least : reset (fuel_reset - 1) mem_addi = None.
Proof. Time vm_compute. reflexivity. Qed.

Example step_fuel_least : observe (run fuel_reset (fuel_step - 1)) = None.
Proof. Time vm_compute. reflexivity. Qed.
