(* Harness for the generated CHERIoT µSail model: an oracle for its foreign
   functions and an initial state.  See validation/docs/harness.md. *)

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

(*** Platform constants ***)

(* sail-riscv/c_emulator/riscv_platform_impl.c defaults; ELF layout of
   validation/scripts/mk-test-elf.py (entry = RAM base, tohost). *)
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
Definition c_enable_fdext          : bool := true.
Definition c_enable_zfinx          : bool := false.
Definition c_enable_writable_fiom  : bool := true.
Definition c_enable_vext           : bool := true.
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
              (write_bytes m (bv.unsigned (env.head (env.tail (env.tail a))))
                 (env.head (env.tail a)) (bv.unsigned (env.head a))))
  | sail_barrier           => fun _ => @ok ty.unit tt
  | MEMr_tag               => fun a => @ok ty.bool (read_tag m (bv.unsigned (env.head a)))
  | MEMw_tag               => fun a =>
      Some (inr tt, mkState (st_regs st)
                      (write_tag m (bv.unsigned (env.head (env.tail a))) (env.head a)))
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

(* Every register starts as the type's [Unconstrained] inhabitant (zeros,
   first constructors), as the emulator's zero-initialised globals; init_model
   then resets the architectural state. *)
Definition reg_default : RegStore.
Proof. intros σ r. destruct r; first [exact bv.zero | exact unconstrained_inhabitant]. Defined.

Definition state0 (m : Memory) : State := mkState reg_default m.

Definition load_word (m : Memory) (a w : Z) : Memory := write_bytes m a 4 w.

Definition set_pc (st : State) (pc : Z) : State :=
  mkState (write_register (st_regs st) PC (bv.of_Z pc)) (st_mem st).

(* As the emulator (riscv_sim.c, init_sail with NO_BOOT_ROM): init_model,
   then PC := ELF entry (only if init_model did not fail). *)
Definition reset (fuel : nat) (m : Memory) : Outcome (Result ty.unit * State) :=
  omap (fun '(r, st) =>
          match r with
          | inr _ => (r, set_pc st c_entry)
          | inl _ => (r, st)
          end)
    (exec_call oracle fuel init_model (env.snoc env.nil (_∷_) tt) (state0 m)).

Definition run_step (fuel : nat) (st : State) : Outcome (Result ty.bool * State) :=
  exec_call oracle fuel step (env.snoc env.nil (_∷_) 0) st.
