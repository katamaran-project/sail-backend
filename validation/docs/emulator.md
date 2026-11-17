# CHERIoT C emulator

Sail's C emulator for `cheriot-sail`: build, configuration, test ELFs, trace contents.
Design: [`design.md`](design.md).

## Build

- Local clone, no network (the main checkout stays untouched):
  ```sh
  W=~/phd/PROJECTS/worktrees/validation
  git clone ~/phd/PROJECTS/cheriot-sail $W/cheriot-sail
  cd $W/cheriot-sail
  git config submodule.sail-riscv.url ~/phd/PROJECTS/cheriot-sail/sail-riscv
  git -c protocol.file.allow=always submodule update --init
  ```
  - `protocol.file.allow=always`: git refuses `file` transport for submodules by default.
  - Submodule `sail-riscv` at `30317c5a`, as in the main checkout.
- Build (Sail 0.20.3 works unchanged):
  ```sh
  opam exec --switch=sail-backend -- gmake csim C_OPT_FLAGS=-O1
  ```
  - Output: `c_emulator/cheriot_sim` (copy of `c_emulator/cheri_riscv_sim_RV32`).
  - `C_OPT_FLAGS=-O1` overrides the Makefile's `-O2 -flto=auto`: with LTO, Apple `ld` ran > 30 min single-threaded (stopped).
  - Time at `-O1`: Sail → C ~5 s (5.3 MB `riscv_model_RV32.c`), `cc` + link ~13 min; total 793 s.
  - Binary built with `-DNO_BOOT_ROM=1`: no reset-vector ROM, PC starts at the ELF entry.
- Without Zb\*/Zbk\* (see Configuration): same, with the Zb\* files left out of `SAIL_DEFAULT_INST`:
  ```sh
  rm -f c_emulator/cheri_riscv_sim_RV32
  opam exec --switch=sail-backend -- gmake c_emulator/cheri_riscv_sim_RV32 C_OPT_FLAGS=-O1 \
    SAIL_DEFAULT_INST="sail-riscv/model/riscv_insts_base.sail sail-riscv/model/riscv_insts_cext.sail \
      sail-riscv/model/riscv_insts_mext.sail sail-riscv/model/riscv_insts_zicsr.sail \
      sail-riscv/model/riscv_insts_hints.sail src/cheri_insts_begin.sail src/cheri_insts.sail \
      src/cheri_insts_cext.sail src/cheri_insts_end.sail"
  cp c_emulator/cheri_riscv_sim_RV32 c_emulator/cheriot_sim_nozb
  ```
  - Built binaries in the clone: `cheriot_sim_zb` (upstream instruction set), `cheriot_sim_nozb` (validation).
  - Time: 621 s (Sail → C included). Checked: `andn` decodes as `illegal` and traps; with `cheriot_sim_zb` it executes.
  - `cheriot_sim` (Makefile output) = `cheriot_sim_zb`; `cheri_riscv_sim_RV32` = the last build (no Zb).
  - Build logs: `cheriot-sail/build-logs/` in the workspace.

## Configuration

| Feature | scope.toml | Emulator | How |
|---|---|---|---|
| Compressed (C) | `sys_enable_rvc = false` | runtime | `--disable-compressed` (`-C`): `misa.C = 0`; a 16-bit parcel traps illegal |
| PMP | `sys_pmp_count = 0` | runtime | `--pmp-count 0` (the default) |
| Zb\*, Zbk\* | `haveZb* = false` | build time only | `haveZb*()` are hard-coded `true` (`riscv_sys_regs.sail`); drop the 6 `riscv_insts_zb*.sail` files (above). Nothing else refers to their constructors |
| F/D, V | not pinned | runtime | `-F` (`--disable-fdext`), `-W` (no long option): only `misa.F/D/V` |

- Validation runs: `cheriot_sim_nozb --disable-compressed -F -W`.
- `misa` with these flags: `0x40801012` (MXL=1, X, M, E, B). `misa.B = 1` is set by CHERIoT's `ext_init_regs` (`src/cheri_regs.sail`), whatever the instruction set; µSail runs the same code.
- Other platform constants (C defaults, `sail-riscv/c_emulator/riscv_platform_impl.c`), for the harness model:
  - RAM `0x80000000`, size `0x4000000` (`--ram-size MB`); ROM `0x1000`, size `0x100`; CLINT `0x2000000`/`0xc0000`.
  - Misaligned accesses enabled (`--disable-misaligned` to turn off); writable `misa`; writable FIOM; no dirty update; `mtval` without illegal instruction bits (`-i` to set).
  - `tohost`: from the ELF symbol (required).
  - Every 100 retired steps the C loop calls `tick_clock` (`mcycle`, `mtime` + 1) and `tick_platform`: outside `step`, so the harness must do the same (or keep programs < 100 instructions).

## Test ELF

- No cross toolchain needed: [`validation/scripts/mk-test-elf.py`](../scripts/mk-test-elf.py) assembles a small subset (RV32I ALU/load/store, `lui`, `cspecialr`, `csetaddr`, `cincaddr[imm]`, `.word`) into an ELF32 with one `PT_LOAD` at `0x80000000` (entry) and a `tohost` symbol (`0x80001000`).
  - Also available locally, if wanted later: Homebrew LLVM 22 (`clang --target=riscv32`, `llvm-mc`, `ld.lld`) and `riscv64-elf-gcc`/binutils; neither knows CHERIoT encodings (use `.insn`/`.word`).
- Reset state: all GPRs null; `PCC`, `MTCC`, `MEPCC` = root executable cap, `MTDC` = root memory cap, `MScratchC` = root sealing cap; all at address 0. A store needs a tagged cap: `cspecialr cX, mtdc` + `csetaddr`.
- End of run: htif. Store 1 to `tohost`, then 0 to `tohost+4` (one 4-byte store does not complete a command); exit code 0, `SUCCESS`.
- A trap goes to `MTCC` (address 0, outside RAM): fetch faults forever, and `--inst-limit` does not stop it (fetch faults are not counted). Programs that trap must install a handler in `MTCC` that writes `tohost`; until then use `gtimeout`.
- Examples: `validation/scripts/examples/*.s` (`addi-add-sw`, `trap-null-store`, `zb-andn`, `rvc-addi`).
  ```sh
  validation/scripts/mk-test-elf.py validation/scripts/examples/addi-add-sw.s /tmp/t.elf
  $W/cheriot-sail/c_emulator/cheriot_sim_nozb --disable-compressed -F -W --trace /tmp/t.elf
  ```

## Trace

- `--trace[=instr|reg|mem|platform|exception|all]` (default `all`), `--no-trace=...`, `--trace-output FILE`.
- `addi-add-sw` (fetch reads `mem[X,...]` left out):
  ```text
  CSR mstatus <- 0x00001800 (input: 0x00000000)
  [0] [M]: 0x80000000 (0x00500593) addi a1, zero, 0x5
  x11 <- 0x00000005 (v:0 0x00000000-0x000000000 l:0x000000000 o:0x0 p:- ------ -- ---)
  [1] [M]: 0x80000004 (0x00B58633) add a2, a1, a1
  x12 <- 0x0000000A (v:0 0x00000000-0x000000000 l:0x000000000 o:0x0 p:- ------ -- ---)
  [2] [M]: 0x80000008 (0x03D0055B) cspecialrw ca0, mtdc, cnull
  x10 <- 0x00000000 (v:1 0x00000000-0x100000000 l:0x100000000 o:0x0 p:G RWcmgl -- ---)
  [3] [M]: 0x8000000C (0x800016B7) lui a3, 0x80001
  x13 <- 0x80001000 (v:0 0x80001000-0x080001000 l:0x000000000 o:0x0 p:- ------ -- ---)
  [4] [M]: 0x80000010 (0x20D5055B) csetaddr ca0, ca0, a3
  x10 <- 0x80001000 (v:1 0x00000000-0x100000000 l:0x100000000 o:0x0 p:G RWcmgl -- ---)
  [5] [M]: 0x80000014 (0x10C52023) sw a2, 0x100(a0)
  tag[0x80001100] <- 0
  mem[0x80001100] <- 0x0000000A
  [6] [M]: 0x80000018 (0x00100713) addi a4, zero, 0x1
  x14 <- 0x00000001 (v:0 0x00000000-0x000000000 l:0x000000000 o:0x0 p:- ------ -- ---)
  [7] [M]: 0x8000001C (0x00E52023) sw a4, 0x0(a0)
  htif[0x80001000] <- 0x00000001
  [8] [M]: 0x80000020 (0x00052223) sw zero, 0x4(a0)
  htif[0x80001004] <- 0x00000000
  htif-syscall-proxy cmd: 0x000000000001
  SUCCESS
  ```
- A trap (`trap-null-store`): no register lines for `mcause`, `mtval`, `MEPCC`, `PCC`:
  ```text
  [1] [M]: 0x80000004 (0x00B52023) sw a1, 0x0(a0)
  CHERI TagViolation Reg=c10/ca0 PC=0x80000004
  trapping from M to M to handle cheri
  handling exc#0x1C at priv M with tval 0x00000142
  CSR mstatus <- 0x00001800
  ```

### What the trace shows

- Per step: step number, privilege, PC (address only), instruction word, disassembly.
- GPR writes (`x1`–`x15`): the full capability *as decoded*: address, tag, base–top, length, otype, permissions. Not the encoded fields `E`, `B`, `T`, `reserved`.
- Memory: every read (incl. fetch, 16-bit parcels) and write with physical address and data; tag writes (`tag[a] <- 0/1`) and tag reads. htif stores print as `htif[...]`, not memory.
- CSRs: Zicsr reads and writes (`CSR mscratch <- ...`); `mstatus` on traps and at reset; `MSHWM`.

### What it does not show

- `PCC` (bounds, perms) and `nextPCC`; only the PC address.
- Special capability registers `MTCC`, `MTDC`, `MScratchC`, `MEPCC` (`CSpecialRW` writes, trap writes of `MEPCC`).
- Trap CSRs `mcause`, `mtval` as register writes (only `handling exc#.. with tval ..`, platform category); `cur_privilege` only in the next instruction line.
- Implicit counters: `minstret` per retired step, `mcycle`/`mtime` on ticks.
- Raw capability encoding (`E`, `B`, `T`, `reserved`): two caps with the same decoded bounds can differ there.
- No option dumps the full state (start, end, or per step). RVFI (`cheri_riscv_rvfi_RV32`) is weaker: `rvfi_wX` reports only the address of a written capability.

## Open

- **Full state per step.** The trace misses `PCC`, the SCRs, trap CSRs, counters and raw capability fields.
  - Options:
    1. Reconstruct from the trace plus re-execution knowledge: fragile, misses raw cap fields.
    2. A small dump: an extra Sail file appended to `SAIL_SRCS` (no edit of cheriot-sail) with `dump_state : unit -> unit` printing every register in scope (caps as tag + `capToBits`, CSRs, `PCC`, SCRs, `cur_privilege`, `minstret`), kept with `-c_preserve dump_state`; plus a copy of `riscv_sim.c` under `validation/` that calls `zdump_state(UNIT)` after each `zstep` when `--dump-state` is given. Built by `validation/`'s own make rule.
    3. Dump only at the end of each program: cheaper output, but loses per-step localisation (the design compares per step).
  - Recommendation: 2. The register list can come from the generated µSail `Reg` (same names), so both sides print the same fields.
- **F/D, V platform flags** (`sys_enable_fdext`, `sys_enable_vext`) are not pinned in `scope.toml`; they set `misa.F/D/V`. Options: add value exclusions `= false` in `scope.toml`, or let the harness model answer `false`. Recommendation: the harness answers `false` (emulator run with `-F -W`); no change to the translation needed.
- **Trap handler for trapping tests**: the harness programs need a prologue that sets `MTCC` to a handler ending the run via `tohost` (task 4).
