(* Dumps of ADDI x1, x0, 5 (StepADDI.v): after reset, after one step, and the
   delta; and the run of addi-add-sw (StepADDI.v).  Checked here, written to
   ../build/dumps/ by Redirect (gmake dumps).  Format: validation/docs/state-format.md. *)

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
     Dump
     StepADDI.

Import ListNotations.
Import UntitledBase.
Import ModelProgram.
Import Harness.I.

Set Default Proof Mode "Classic".
Open Scope string_scope.

(* Generous fuel: the cost is depth, not steps (validate.md). *)
Definition fuel : nat := 300.

Definition run_addi : Outcome (list Dumped * Stop) := run_dumps fuel 1 mem_addi.

(* The dumps of a run, or the outcome when the run did not finish. *)
Definition or_outcome {A} (o : Outcome A) (f : A -> list string) : list string :=
  match o with
  | Done a => f a
  | OutOfFuel => ["OutOfFuel"]
  | NoAnswer _ => ["NoAnswer"]
  end.

Definition nth_dump (ds : list Dumped * Stop) (i : nat) : list string :=
  match nth_error (fst ds) i with Some d => map line (items d) | None => ["no state"] end.

Definition reset_dump : list string := or_outcome run_addi (fun ds => nth_dump ds 0).
Definition step1_dump : list string := or_outcome run_addi (fun ds => nth_dump ds 1).
Definition step1_delta : list string :=
  or_outcome run_addi (fun ds =>
    match fst ds with
    | [d0; d1] => delta_of d0 d1
    | _ => ["not two states"]
    end).
Definition run_dump : list string := or_outcome run_addi (fun ds => dump_run (fst ds)).

(* addi-add-sw: up to 20 steps; ends via htif after 9. *)
Definition run_aas : Outcome (list Dumped * Stop) := run_dumps fuel 20 mem_addi_add_sw.
Definition aas_dump : list string := or_outcome run_aas (fun ds => dump_run (fst ds)).

(*** Checks ***)

(* The step's delta: PC and nextPC + 4, the instruction bits, minstret + 1,
   x1's address = 5, step's result (after reset: ()).  Nothing else changes;
   no memory writes. *)
Example addi_delta :
  step1_delta =
    [ "PC 0x80000004";
      "instbits 0x00500093";
      "minstret 0x0000000000000001";
      "nextPC 0x80000004";
      "step_result true";
      "x1.address 0x00000005" ].
Proof. Time vm_compute. reflexivity. Qed.

(* One line per register item: 86 registers (118 - 32 excluded), 21 of them
   capabilities (19 fields each): 65 + 21 * 19 = 464; and step_result.
   No memory written. *)
Example reset_lines : length reset_dump = 465%nat.
Proof. Time vm_compute. reflexivity. Qed.

Example step1_lines : length step1_dump = 465%nat.
Proof. Time vm_compute. reflexivity. Qed.

(* Register items whose value after reset depends on the start store
   (Dump.reg_alt vs Harness.reg_default): not set by init_model.
   See state-format.md, "Coverage". *)
Example start_dependent_regs :
  start_dependent fuel mem_addi =
    Done [ "MSHWM"; "MSHWMB"; "UART_DLAB"; "cur_inst"; "instbits"; "marchid"; "mccsr";
           "mcountinhibit"; "mimpid"; "misa"; "mstatus"; "mtimecmp"; "mvendorid"; "nextPC";
           "satp"; "scause"; "sccsr"; "scounteren"; "sedeleg"; "sepc"; "sideleg"; "sscratch";
           "stval"; "stvec"; "tselect"; "ucause"; "uccsr"; "uepc"; "uscratch"; "utval"; "utvec" ].
Proof. Time vm_compute. reflexivity. Qed.

(* Memory and tag writes go through the log: __write_mem of 2 bytes at
   0x80000100, then MEMw_tag on tag address 0x10000020 (granule 0x80000100),
   then the same bytes again (one line each). *)
Definition write_args (a : Z) : CStore _ :=
  env.snoc (env.snoc (env.snoc (env.snoc (env.snoc env.nil (MkB "arg1" (ty.enum Ewrite_kind)) Write_plain)
    (MkB "arg2" ty.int) 32) (MkB "arg3" (ty.bvec 32)) (bv.of_Z a)) (MkB "arg4" ty.int) 2)
    (MkB "arg5" (ty.bvec 16)) (bv.of_Z 0xbeef).

Definition tag_args : CStore _ :=
  env.snoc (env.snoc env.nil (MkB "arg1" (ty.bvec 64)) (bv.of_Z 0x10000020)) (MkB "arg2" ty.bool) true.

Definition log_call {Δ σ} (f : 𝑭𝑿 Δ σ) (args : CStore Δ) (o : option State) : option State :=
  match o with
  | Some st => option_map snd (oracle Δ σ f args st)
  | None => None
  end.

Definition after_writes : option State :=
  log_call (@__write_mem 32 16) (write_args 0x80000100)
    (log_call MEMw_tag tag_args
       (log_call (@__write_mem 32 16) (write_args 0x80000100) (Some (state0 mem_addi)))).

(* The program image is not logged: only the writes. *)
Example mem_lines :
  option_map (fun st => let '(ms, ts) := mem_items (st_mem st) in map line (app ms ts)) after_writes =
    Some [ "mem 0x80000100 0xef"; "mem 0x80000101 0xbe"; "tag 0x80000100 true" ].
Proof. Time vm_compute. reflexivity. Qed.

(* Types no register has yet: union, sum, product, list, tuple, int. *)
Example render_other_types :
  map line (concat [
    render render_depth (ty.union Ufetchresult) "f" (F_RVC (bv.of_Z 0x1234));
    render render_depth (ty.sum (ty.bvec 8) ty.unit) "o" (inl (bv.of_Z 5));
    render render_depth (ty.sum (ty.bvec 8) ty.unit) "n" (inr tt);
    render render_depth (ty.prod ty.int ty.bool) "p" (-12, true)%Z;
    render render_depth (ty.list ty.int) "l" [0; 1234]%Z;
    render render_depth (ty.tuple (ctx.snoc (ctx.snoc ctx.nil ty.int) ty.bool)) "t" (tt, 7%Z, false)]) =
  [ "f F_RVC"; "f.F_RVC 0x1234";
    "o inl"; "o.inl 0x05";
    "n inr";
    "p.0 -12"; "p.1 true";
    "l.len 2"; "l.0 0"; "l.1 1234";
    "t.0 7"; "t.1 false" ].
Proof. Time vm_compute. reflexivity. Qed.

(*** Output (gmake dumps) ***)

Goal True.
  Time Redirect "addi/reset.state" ltac2:(print_lines constr:(reset_dump)).
  Redirect "addi/step1.state" ltac2:(print_lines constr:(step1_dump)).
  Redirect "addi/step1.delta" ltac2:(print_lines constr:(step1_delta)).
  Redirect "addi/run.states" ltac2:(print_lines constr:(run_dump)).
  Redirect "addi-add-sw/run.states" ltac2:(print_lines constr:(aas_dump)).
  Redirect "excluded" ltac2:(print_lines constr:(excluded_lines)).
  exact I.
Qed.
