# State format

Canonical text form of a CHERIoT processor state, produced by both sides (µSail dump in Rocq; emulator traces, task 4) and compared with `diff -u`.

- `validation/rocq/Names.v`: constructor names as strings (Ltac2), raw printing of `list string`. Generic.
- `validation/rocq/Dump.v`: rendering of a harness `State`, `step_result`, delta, run dumps (`run_dumps`), start-value check.
- `validation/rocq/DumpADDI.v`: ADDI demo and the addi-add-sw run (`StepADDI.v`'s programs); checks and writes the files below.
- `validation/compare/statefile.py`: `check` (grammar, order) and `delta` on text files.

## Build

```sh
opam exec --switch=sail-backend -- gmake -C validation/rocq cheriot   # everything CHERIoT, incl. the dumps
opam exec --switch=sail-backend -- gmake -C validation/rocq dumps     # only DumpADDI.v and the files
```

Output in `validation/build/dumps/` (gitignored):

| file | content |
|---|---|
| `addi/reset.state` | state after reset |
| `addi/step1.state` | state after one `step` |
| `addi/step1.delta` | items of `step1.state` that differ from `reset.state` |
| `addi/run.states` | the run: both states, step-prefixed |
| `addi-add-sw/run.states` | `validation/scripts/examples/addi-add-sw.s`: reset + 9 steps, ends via htif |
| `excluded` | registers left out, with the reason |

- `DumpADDI.v` writes them with `Redirect` (`<name>.out`, renamed by the Makefile); Ltac2 prints the strings unquoted, so no quote-stripping script.
- Then the rule runs `statefile.py check` on every file, and `statefile.py delta reset.state step1.state | diff -u step1.delta -` (Rocq's delta = the script's).

## Grammar

```
state     ::= reg-line* mem-line* tag-line*
reg-line  ::= key SP value LF
mem-line  ::= "mem" SP addr SP byte LF
tag-line  ::= "tag" SP addr SP bool LF
key       ::= seg ("." seg)*                 seg ::= [A-Za-z0-9_]+
addr      ::= "0x" hexdigit{8,}              minimal digits, at least 8
byte      ::= "0x" hexdigit{2}
hexdigit  ::= [0-9a-f]
delta     ::= state                          (subset of the "after" state)
run       ::= (step SP line)*                step ::= "0" | [1-9][0-9]*
```

- Order: register lines sorted bytewise by key (`LC_ALL=C sort`; uppercase first), then `mem` by address, then `tag` by address. Each key once.
- Run file: step 0 = after reset, step i = after the i-th `step`; each step's lines form a state, steps ascending.
- No comments, no blank lines; LF at the end of every line.

### Values

| Sail / µSail type | line(s) | example |
|---|---|---|
| `bits(n)` (`ty.bvec n`) | `0x` + exactly ceil(n/4) digits (min 1) | `minstret 0x0000000000000001`, `UART_DLAB 0x0` |
| `bool` | `true` / `false` | `htif_done false` |
| `int` | decimal, `-` if negative | |
| enum | constructor name | `cur_privilege Machine` |
| struct (record) | one line per field: `key.field` | `x1.address 0x00000005` |
| union | `key Ctor`, then the payload under `key.Ctor` (none for `unit`) | |
| option / sum | `key inl` / `key inr`, payload under `key.inl` / `key.inr` | |
| tuple, product | `key.0`, `key.1`, ... | |
| list | `key.len n`, then `key.0`, ... | |
| `string` | `"..."` | |
| `unit` | `()` | |

- `<depth>`: value nested deeper than 16 levels (none in this model).
- Capabilities (`Capability` record, 19 fields): `tag`, the 12 permission bools, `reserved`, `E`, `B`, `T`, `otype`, `address`. The decoded register form, not the 64-bit memory encoding.
- Memory: one `mem` line per byte written since the start (last value), one `tag` line per tag written. `tag` address: 8 × the `MEMw_tag` argument (byte address of the 8-byte granule).
- Program image: not listed (both sides load the same ELF).
- Emitting from Python: `"0x%0*x" % (max(1, (n + 3) // 4), v)`, addresses `"0x%08x"`, bytes `"0x%02x"`.

Example (`addi/step1.delta`):

```
PC 0x80000004
instbits 0x00500093
minstret 0x0000000000000001
nextPC 0x80000004
step_result true
x1.address 0x00000005
```

### `step_result`

- Pseudo-item, sorted in with the registers (bytewise: between `sscratch` and `stval`). Every state has it.
- Value: the result of the call that produced the state.

| state | value |
|---|---|
| step 0 (after reset) | `()` (`init_model` returned) |
| step i ≥ 1 | `true` / `false` (`step`'s bool) |
| a failure (`stm_fail`) | `"<message>"` (string; `"` in the message becomes `'`) |

- `false`: the step did not execute an instruction (fetch fault, interrupt taken, `WFI`); a decoded instruction that traps gives `true` (as the emulator's `stepped`).
- A failure is the last state of a run (`Harness.Stop` = `Failed`). The emulator has no failure value (it aborts); a failure line is a divergence by itself.
- Emulator side: `step_result ()` at step 0, then the C `zstep` return value.

## Delta

- `delta before after`: lines of `after` whose key is new or whose value differs; same grammar and order.
- Registers never disappear; memory lines only accumulate. So the delta says everything that changed.
- Rocq: `Dump.delta`; text files: `statefile.py delta BEFORE AFTER` (same output).

## Names

- Taken: (c) a Ltac2 tactic builds the name functions from the inductive types:
  ```coq
  Definition reg_name_dep : forall σ (r : 𝑹𝑬𝑮 σ), named r.
  Proof. intros σ r; destruct r; ltac2:(name_goal ()). Defined.
  ```
  - `named r` reduces to `string`; after `destruct`, each goal shows its constructor, and `name_goal` fills in its name as a string literal.
  - Same for enums (`enumt E`) and union values (`uniont U`, the value's constructor, e.g. `F_RVC`, not the `unionk` tag `Kf_rvc`).
  - Record field names: already strings (`recordf_ty`).
- Names = Rocq constructor names = Sail names (unless renamed by the translator's configuration).
- Coverage: the dump iterates over `enum (sigT 𝑹𝑬𝑮)` (`𝑹𝑬𝑮_finite`; local to its section, so passed explicitly). A new register appears with its name, no list to edit.
- Considered:
  - (a) script generating a names file from `base.v`: extra build step, parses generated Rocq.
  - (b) translator emits `Reg -> string`: clean, but needs a translator change; unnecessary given (c).

## Not compared

- `vr0` ... `vr31`: vector registers. Vector extension out of scope; 65536 bits each. Listed in `Dump.excluded` (and the `excluded` file); every other register is dumped.
- Vector CSRs (`elen`, `vlen`, `vl`, `vlenb`, `vstart`, `vtype`, `vxrm`, `vxsat`, `vcsr`): dumped; `init_model` sets them (the emulator enables V by default).

## Start values

- Registers start as `Harness.reg_default` = the emulator's registers before `init_model` (Sail C zero-initialisation: bitvectors 0, bools `false`, enums first constructor; `harness.md`, "Initial state"). Every model register matches.
- `DumpADDI.start_dependent_regs` pins the items whose value after reset depends on the start store (reset from `reg_default` vs `Dump.reg_alt`: bitvectors inverted, bools negated, ints + 1, enums next):

  `MSHWM`, `MSHWMB`, `UART_DLAB`, `cur_inst`, `instbits`, `marchid`, `mccsr`, `mcountinhibit`, `mimpid`, `misa`, `mstatus`, `mtimecmp`, `mvendorid`, `nextPC`, `satp`, `scause`, `sccsr`, `scounteren`, `sedeleg`, `sepc`, `sideleg`, `sscratch`, `stval`, `stvec`, `tselect`, `ucause`, `uccsr`, `uepc`, `uscratch`, `utval`, `utvec`.

- All are bitvectors (bools and capabilities are all set by `init_model`); `misa`, `mstatus`, `mccsr`, ... are set only in part.
- Compared: both sides start them at 0. A difference at step 0 in one of these means the start values differ after all, not a translation bug.
- Unchanged by the F/D/V constants and the bool start value (checked).
- A translation change that alters this set fails `DumpADDI.v`; update the list here.
- Excluded or start-dependent beyond these: none.
## ADDI demo

`ADDI x1, x0, 5` at `0x80000000`; reset, then one `step` (fuel 300).

| key | reset | step 1 | why |
|---|---|---|---|
| `PC` | `0x80000000` | `0x80000004` | next instruction |
| `nextPC` | `0x00000000` | `0x80000004` | `PC + 4` |
| `instbits` | `0x00000000` | `0x00500093` | the fetched instruction |
| `minstret` | `0x0…0` | `0x0…01` | retired |
| `step_result` | `()` | `true` | reset, then `step` |
| `x1.address` | `0x00000000` | `0x00000005` | the result; `x1.tag` stays `false` |

- Nothing else changes: no memory writes, `mcycle`/`mtime` unchanged (`tick_clock` is the emulator loop's, not `step`'s), `PCC.address` stays `0x00000000` (CHERIoT keeps the PC in `PC`).
- `misa` after reset: `0x40801012` (as the emulator with `-F -W`).
- Checked in `DumpADDI.v`: the exact delta, 465 lines per state (86 registers, 21 of them capabilities: 65 + 21 × 19; `step_result`), the start-dependent set, and memory/tag lines through the oracle's log (`mem_lines`).
- `addi-add-sw/run.states`: last step adds `mem 0x80001100..03` (`0x0a 00 00 00`), `tag 0x80001100 false` (a data store clears the tag), `htif_done true`.

## Memory write log

- The harness memory is `Z -> Z` (model's `Memory`): it cannot be listed.
- `Harness.oracle` logs every key `__write_mem` and `MEMw_tag` write, in the memory itself (`harness.md`, "Memory"); `Dump.mem_items` reads `Harness.log_keys`.
- So `Harness.reset`/`run_step`/`run_steps` give dumpable states directly; `Dump.run_dumps fuel n m` = reset + `run_steps` + a `Dumped` per state (with `step_result`) + `Stop`.
- Program image: written with `load_word(s)`, not logged.
## Timings

| what | time |
|---|---|
| `coqc Names.v` / `Dump.v` | 0.2 s / 1.5 s |
| `gmake dumps` (`DumpADDI.v` + checks) | 2.9 s |
| `vm_compute`: first reset + step + dump in a file (VM compiles the model) | 0.2 s |
| then reset + step + two dumps + delta | 0.03 s |
| reset from two start stores, both dumped, compared | 0.07 s |
| Ltac2 printing of a 465-line state | 0.3 s |
| `gmake dumps` with `addi-add-sw` (10 states) | 6.5 s |

- Register lines sorted with a merge sort; delta by merge on sorted keys (insertion sort and a quadratic delta: 0.7 s).

## For task 4 (emulator side)

- Expected state per step = reset state + the writes `--trace` prints, rendered as above; sorted with `LC_ALL=C sort` per section.
- `step_result`: `()` at step 0, then `zstep`'s return value.
- Direct comparison target: `addi-add-sw/run.states` (same program as `validation/scripts/examples/addi-add-sw.s`).
- Leave out the registers in `excluded`.
- Delta of emulator states: `statefile.py delta`.
- Validate files with `statefile.py check [--run]`.

## Open

- **Memory log in the harness.** Options: (a) `oracle_log` wrapper in `Dump.v`; (b) the log in `Harness.oracle` (one oracle, `Harness.reset` reused); (c) a `Memory` record of bytes, tags and log when the memory model is written (Katamaran-side). Recommendation: (b) now, (c) later. Taken: (b).
- **Start values.** Options: (a) compare the start-dependent registers (start 0); (b) leave them out; (c) start µSail from the emulator's register values after its reset. Taken: (a); checked in the generated C that the emulator starts them at 0 too (`harness.md`, "Initial state").
- **Clock ticks.** The emulator calls `tick_clock`/`tick_platform` every `rv_insns_per_tick` (default 100) steps, outside `step`; not in the model. Runs over 100 steps differ in `mcycle`/`mtime`/`mip`: expected divergence, or a larger tick interval in the emulator.
- **Capabilities in the trace.** `--trace` prints a capability as `v:1 base-top l:len o:otype p:perms` (emulator.md), not the record fields `E`/`B`/`T`. Options: (a) task 4 encodes base/top into `E`/`B`/`T` (CHERIoT bounds encoding in Python); (b) the Rocq dump adds derived lines (`x1.base`, `x1.top`) computed by the model's own bounds function through the interpreter, and the comparison uses those; (c) a register dump in the emulator. Recommendation: (b) (the model computes them; no re-implementation). Not done here.
- **`step_result` at step 0.** Options: (a) `()` (every state has the same keys); (b) no `step_result` at step 0. Recommendation: (a). Taken: (a).
