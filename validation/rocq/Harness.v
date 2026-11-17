(* Harness for the generated CHERIoT µSail model: an oracle for its foreign
   functions and an initial state.  See validation/docs/harness.md. *)

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
From Validation Require Import Interp.

Import ctx.notations.
Import env.notations.
Import UntitledBase.
Import ModelProgram.

Open Scope Z_scope.

Module Import I := MakeInterp UntitledBase ModelProgram.

(*** Memory ***)

(* The model's [Memory] is [Z -> Z].  Bytes: key = physical address (>= 0),
   value in [0, 256).  Tags: key = [-1 - tag address] (< 0), value 0 or 1.
   Unwritten memory reads 0 (bytes) and false (tags), as the emulator. *)

Definition mem_empty : Memory := fun _ => 0.

(* Little-endian read of [w] bytes at [a]. *)
Fixpoint read_bytes (m : Memory) (a : Z) (w : nat) : Z :=
  match w with
  | O => 0
  | S w' => m a + 256 * read_bytes m (a + 1) w'
  end.

(* Little-endian write of the [w] low bytes of [v] at [a]. *)
Definition write_bytes (m : Memory) (a : Z) (w : Z) (v : Z) : Memory :=
  fun k => if ((a <=? k) && (k <? a + w))%bool
           then Z.land (Z.shiftr v (8 * (k - a))) 255
           else m k.

Definition tag_key (a : Z) : Z := -1 - a.

Definition read_tag (m : Memory) (a : Z) : bool := negb (m (tag_key a) =? 0).

Definition write_tag (m : Memory) (a : Z) (t : bool) : Memory :=
  fun k => if k =? tag_key a then (if t then 1 else 0) else m k.

(* Write log: the memory is a function, so it cannot be listed.  The oracle
   records each written key in the memory itself, above every physical
   address: key [log_base] = count, key [log_base + 1 + i] = i-th written key
   (byte address >= 0, or tag key < 0). *)
Definition log_base : Z := 2 ^ 40.

Definition log_push (m : Memory) (key : Z) : Memory :=
  let n := m log_base in
  fun j => if j =? log_base then n + 1 else if j =? log_base + 1 + n then key else m j.

Definition log_keys (m : Memory) : list Z :=
  map (fun i => m (log_base + 1 + Z.of_nat i)) (seq 0 (Z.to_nat (m log_base))).

(* Writes with logging. *)
Definition store_bytes (m : Memory) (a w v : Z) : Memory :=
  fold_left log_push (map (fun i => a + Z.of_nat i) (seq 0 (Z.to_nat w))) (write_bytes m a w v).

Definition store_tag (m : Memory) (a : Z) (t : bool) : Memory :=
  log_push (write_tag m a t) (tag_key a).

(*** Platform constants ***)

(* As the validation emulator runs: cheriot_sim_nozb --disable-compressed -F -W
   (sail-riscv/c_emulator/riscv_platform_impl.c defaults, riscv_sim.c flags);
   ELF layout of validation/scripts/mk-test-elf.py (entry = RAM base, tohost).
   Table with source lines: validation/docs/harness.md, "Constants". *)
Definition c_ram_base  : Z := 0x80000000.
Definition c_ram_size  : Z := 0x4000000.
Definition c_rom_base  : Z := 0x1000.
Definition c_rom_size  : Z := 0x100.
Definition c_clint_base : Z := 0x2000000.
Definition c_clint_size : Z := 0xc0000.
Definition c_uart_base : Z := 0x10000000.
Definition c_uart_size : Z := 0x100.
Definition c_tohost    : Z := 0x80001000.
Definition c_entry     : Z := c_ram_base.

Definition c_enable_writable_misa  : bool := true.
Definition c_enable_fdext          : bool := false.  (* -F *)
Definition c_enable_zfinx          : bool := false.
Definition c_enable_writable_fiom  : bool := true.
Definition c_enable_vext           : bool := false.  (* -W *)
Definition c_enable_misaligned     : bool := true.
Definition c_mtval_has_illegal_inst_bits : bool := false.

(*** Oracle ***)

Definition bitvector_update_val {n} (x : bv n) (i : Z) (b : bv 1) : bv n :=
  bv.of_Z (if bv.unsigned b =? 0
           then Z.land (bv.unsigned x) (Z.lnot (Z.shiftl 1 i))
           else Z.lor (bv.unsigned x) (Z.shiftl 1 i)).

Definition oracle : Oracle :=
  fun Δ σ f args st =>
  let ok {τ} (v : Val τ) : option (Result τ * State) := Some (inr v, st) in
  let m := st_mem st in
  match f in FunX Δ σ return CStore Δ -> option (Result σ * State) with
  | @unconstrained _ i     => fun _ => ok i
  | emod_int               => fun a => ok (semantics_emod_int (env.head (env.tail a)) (env.head a))
  | pow2                   => fun a => ok (semantics_pow2 (env.head a))
  | min_int                => fun a => ok (semantics_min_int (env.head (env.tail a)) (env.head a))
  | count_leading_zeros    => fun a => ok (semantics_count_leading_zeros (env.head a))
  | count_trailing_zeros   => fun a => ok (semantics_count_trailing_zeros (env.head a))
  | sail_sign_extend       => fun a => ok (semantics_sail_sign_extend (env.head (env.tail a)) (env.head a))
  | sail_zero_extend       => fun a => ok (semantics_sail_zero_extend (env.head (env.tail a)) (env.head a))
  | truncateLSB            => fun a => ok (semantics_truncateLSB (env.head (env.tail a)) (env.head a))
  | bitvector_access       => fun a => ok (semantics_bitvector_access (env.head (env.tail a)) (env.head a))
  | bitvector_update       => fun a =>
      @ok (ty.bvec _) (bitvector_update_val (env.head (env.tail (env.tail a))) (env.head (env.tail a)) (env.head a))
  | sail_zeros             => fun a => ok (semantics_sail_zeros (env.head a))
  | get_slice_int          => fun a =>
      ok (semantics_get_slice_int (env.head (env.tail (env.tail a))) (env.head (env.tail a)) (env.head a))
  | quot_round_zero        => fun a => ok (semantics_quot_round_zero (env.head (env.tail a)) (env.head a))
  | rem_round_zero         => fun a => ok (semantics_rem_round_zero (env.head (env.tail a)) (env.head a))
  (* __read_mem(kind, xlen, addr, width) *)
  | __read_mem             => fun a =>
      @ok (ty.bvec _) (bv.of_Z (read_bytes m (bv.unsigned (env.head (env.tail a))) (Z.to_nat (env.head a))))
  | __write_mem_ea         => fun _ => @ok ty.unit tt
  (* __write_mem(kind, xlen, addr, width, data) *)
  | __write_mem            => fun a =>
      Some (inr true,
            mkState (st_regs st)
              (store_bytes m (bv.unsigned (env.head (env.tail (env.tail a))))
                 (env.head (env.tail a)) (bv.unsigned (env.head a))))
  | sail_barrier           => fun _ => @ok ty.unit tt
  | MEMr_tag               => fun a => @ok ty.bool (read_tag m (bv.unsigned (env.head a)))
  | MEMw_tag               => fun a =>
      Some (inr tt, mkState (st_regs st)
                      (store_tag m (bv.unsigned (env.head (env.tail a))) (env.head a)))
  | sys_enable_writable_misa => fun _ => @ok ty.bool c_enable_writable_misa
  | sys_enable_fdext       => fun _ => @ok ty.bool c_enable_fdext
  | sys_enable_zfinx       => fun _ => @ok ty.bool c_enable_zfinx
  | sys_enable_writable_fiom => fun _ => @ok ty.bool c_enable_writable_fiom
  | sys_enable_vext        => fun _ => @ok ty.bool c_enable_vext
  | get_16_random_bits     => fun _ => @ok (ty.bvec 16) bv.zero
  | cancel_reservation     => fun _ => @ok ty.unit tt
  | elf_tohost             => fun _ => @ok ty.int c_tohost
  | plat_ram_base          => fun _ => @ok (ty.bvec 32) (bv.of_Z c_ram_base)
  | plat_ram_size          => fun _ => @ok (ty.bvec 32) (bv.of_Z c_ram_size)
  | plat_enable_misaligned_access => fun _ => @ok ty.bool c_enable_misaligned
  | plat_mtval_has_illegal_inst_bits => fun _ => @ok ty.bool c_mtval_has_illegal_inst_bits
  | plat_rom_base          => fun _ => @ok (ty.bvec 32) (bv.of_Z c_rom_base)
  | plat_rom_size          => fun _ => @ok (ty.bvec 32) (bv.of_Z c_rom_size)
  | plat_clint_base        => fun _ => @ok (ty.bvec 32) (bv.of_Z c_clint_base)
  | plat_clint_size        => fun _ => @ok (ty.bvec 32) (bv.of_Z c_clint_size)
  | plat_uart_base         => fun _ => @ok (ty.bvec 32) (bv.of_Z c_uart_base)
  | plat_uart_size         => fun _ => @ok (ty.bvec 32) (bv.of_Z c_uart_size)
  | plat_term_write        => fun _ => @ok ty.unit tt
  end args.

(*** Initial state ***)

(* The emulator's registers before init_model (Sail C, initialize_registers):
   bitvectors 0, bools false, enums the first constructor, records field by
   field.  [exact bv.zero] first: [cbn] on the 65536-bit vregtype is slow. *)
Ltac zero_val :=
  first [ exact bv.zero | exact false | exact 0%Z | cbn; constructor; zero_val ].

Definition reg_default : RegStore.
Proof. intros σ r. destruct r; zero_val. Defined.

Definition state0 (m : Memory) : State := mkState reg_default m.

(* Program loading: not logged (both sides load the same ELF). *)
Definition load_word (m : Memory) (a w : Z) : Memory := write_bytes m a 4 w.

(* Consecutive words from [a]. *)
Fixpoint load_words (m : Memory) (a : Z) (ws : list Z) : Memory :=
  match ws with
  | nil => m
  | cons w ws' => load_words (load_word m a w) (a + 4) ws'
  end.

Definition set_pc (st : State) (pc : Z) : State :=
  mkState (write_register (st_regs st) PC (bv.of_Z pc)) (st_mem st).

(* As the emulator (riscv_sim.c, init_sail with NO_BOOT_ROM): init_model,
   then PC := ELF entry (only if init_model did not fail). *)
Definition reset_from (fuel : nat) (st0 : State) : Outcome (Result ty.unit * State) :=
  omap (fun '(r, st) =>
          match r with
          | inr _ => (r, set_pc st c_entry)
          | inl _ => (r, st)
          end)
    (exec_call oracle fuel init_model (env.snoc env.nil (_∷_) tt) st0).

Definition reset (fuel : nat) (m : Memory) : Outcome (Result ty.unit * State) :=
  reset_from fuel (state0 m).

(* step(k); the emulator passes the number of earlier steps that returned
   true (only printed). *)
Definition run_step_at (fuel : nat) (k : Z) (st : State) : Outcome (Result ty.bool * State) :=
  exec_call oracle fuel step (env.snoc env.nil (_∷_) k) st.

Definition run_step (fuel : nat) (st : State) : Outcome (Result ty.bool * State) :=
  run_step_at fuel 0 st.

(*** Runs ***)

(* Why a run stopped. *)
Inductive Stop : Set :=
| Halted       (* htif_done after a step: the program wrote tohost (emulator: exit) *)
| Failed       (* a step returned inl (stm_fail) *)
| StepLimit.   (* the step limit; e.g. a trap to MTCC = 0 fetch-faults forever *)

Definition htif_halted (st : State) : bool := read_register (st_regs st) htif_done.

(* Up to [n] steps from [st] (emulator loop: while !htif_done; no
   tick_clock).  Every step is counted, also those returning false (the
   emulator's --inst-limit counts only true ones, so it never stops a fetch
   fault loop).  Fuel per step: depth, so a run always ends. *)
Fixpoint run_steps_aux (fuel n : nat) (k : Z) (st : State) :
  Outcome (list (Result ty.bool * State) * Stop) :=
  match n with
  | O => Done (nil, StepLimit)
  | S n' =>
      obind (run_step_at fuel k st) (fun '(r, st') =>
        match r with
        | inl _ => Done (cons (r, st') nil, Failed)
        | inr b =>
            if htif_halted st' then Done (cons (r, st') nil, Halted)
            else omap (fun (p : list (Result ty.bool * State) * Stop) =>
                         let (rs, s) := p in (cons (r, st') rs, s))
                   (run_steps_aux fuel n' (if b then k + 1 else k) st')
        end)
  end.

Definition run_steps (fuel n : nat) (st : State) : Outcome (list (Result ty.bool * State) * Stop) :=
  run_steps_aux fuel n 0 st.
