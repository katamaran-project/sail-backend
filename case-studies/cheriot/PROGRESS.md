# CHERIoT translation progress

How much of the CHERIoT core translates to µSail and compiles in Rocq.
The core is the scope in [`tests/cheriot/scope.toml`](../../tests/cheriot/scope.toml): the base integer, M, Zicsr and CHERIoT capability instructions (61), and everything they reach.
The numbers come from the loop ([`docs/cheriot-loop.md`](../../docs/cheriot-loop.md)):

```sh
gmake cheriot CHERIOT_SAIL_DIR=<cheriot-sail>
```

**Updating this file.** When a commit changes the loop's numbers, update *Now* and *Blockers*, and add a row to *History*, in the same commit
(usually the commit that runs `gmake cheriot CHERIOT_ARGS=--update-baseline`).
The values are in the run's report, `metrics.json` and `progress.json`; `tests/cheriot/history.csv` has a line per run.
An instruction is *clean* when everything it reaches translates without holes and calls no missing function: it is ready for Rocq.

## Now

14 Nov 2026, measured on `15c31cfc` plus the Sv32 TLB types ignored by configuration; cheriot-sail `518813e`, Katamaran `1688c950`.

| | core |
|---|---|
| instructions clean | **1 of 61** (`FENCEI`) |
| instructions generated | 53 of 61 |
| functions generated | 565 of 647 |
| untranslated definitions | 105 |
| `NYI` holes | 23 |
| hard failures | 0 |
| calls to missing functions | 1494: extern 1283, untranslated 109, outside the scope 86 (from `execute`), skipped by configuration 14, unknown 2 |
| Rocq, plain | `base.v:12184`, in `Inductive Reg`: the `NYI` in the types of the PMP register arrays `pmpcfg_n`, `pmpaddr_n` (P10); 1053 of 1106 definitions before it; 5 s |
| Rocq, probe (`NYI` as an axiom) | all of `base.v` compiles (7 s); `machine.v:54`, in `Inductive Fun`: `The reference kid__n was not found` (a size-polymorphic function signature, P13), before any of its 576 definitions |

The baseline (`tests/cheriot/baseline.json`) holds these numbers and the status of all definitions in scope.

## Blockers

**What keeps instructions from being clean.** Each of these blocks 58 of the 61 instructions, because the code they share reaches it:
externs of the tracing and printing code (`print_reg`, `bits_str`, `concat_str`; the policy exclusion of `docs/cheriot-loop.md` §4.2 would remove them),
arithmetic externs (`emod_int`, `get_slice_int`, `shiftr`; P16, externs as foreign functions),
a call to the Sail builtin `size_itself_int` in the size-polymorphic `to_bits` (P13), and calls to `zeros_implicit`,
which the configuration skips (P14; it could be translated like `sail_zeros`).
`getCapBoundsBits` no longer has holes: `truncate(X @ zeros(E), w)` with a run-time `E` is rewritten to `sail_shiftleft(truncate(X, w), E)`
before translation (so no bitvector has a run-time width), shifts by a run-time amount are supported, and Sail's `truncate` is µSail's `uop.truncate`.
All computed top-level values (`cap_len_width`, `null_cap`, `zero_reg`, the root capabilities, ...; P08) now translate:
they are evaluated at translation time and become Rocq definitions (`Definition null_cap : Capability := {| ... |}`), used as `exp_val ty null_cap`.
Vector literals of single bits (`[bool_to_bit(x), ...]`, P04) now translate as a `bop.bvapp` chain: `FENCE`, `FENCE_TSO`, `getCapPerms`, `bool_to_bits`, `readCSR` and the compressed decoders are generated.
The main decoder `encdec_backwards` now stops at a `match` on a `bool` (`Match.ml:3068`, from `bool_not_bits(s)` in the `DIV` encoding), and `writeCSR` has 4 holes (P01: widths and slice indices not known at compile time).
Integer types whose numeric expression contains an `if` (`CGetLen`, `CGetTop`: `to_bits(xlen, if len > cap_max_addr then cap_max_addr else len)`, P11) are erased to `int`.
The Sv32 TLB code (`make_TLB_Entry`, `match_TLB_Entry`, `flush_TLB_Entry`, `translate32`) is out of scope: virtual memory is not part of the CHERIoT target (`cheriot-inventory/PROJECT-STATUS.md` §1a). In-scope code still reaches it, so it is dead code for the configuration exclusion (`docs/cheriot-loop.md` §4.2), not a blocker; it will not be implemented.
The run's report ranks all blockers; `progress.json` lists them per instruction.

**Instructions not generated (8)**, by pile ([inventory](../../cheriot-inventory/20261113-1408/INVENTORY.md)):

| pile | instructions | cause |
|---|---|---|
| P03 | `BTYPE`, `RISCV_JAL`, `RISCV_JALR`, `LOAD`, `STORE` | a pattern that binds all fields of a constructor with several fields to one variable (`Ext_ControlAddr_Error(e)`, `TR_Failure(e)`, `MemException(e)`; `Match.ml`, "unsupported binder at this location"); before P02 these stopped at the union with type parameters |
| P06 | `CSpecialRW`, `LoadCapImm` | assignment to a mutable local variable (`var`) |
| P07 | `CJALR` | register reference |

**Rocq:** the Sv32 types `TLB_Entry` (a struct with `Int` parameters, where `base.v` used to stop) and `TLB32_Entry` are ignored by configuration (virtual memory is out of scope),
so `lookup_TLB32`, `add_to_TLB32`, `write_TLB32`, `flush_TLB32` and `init_vmem_sv32` are now untranslated (dead code, accepted as the baseline).
The plain `base.v` now stops only at the `NYI` types of the PMP register arrays (P10, deferred: the configuration feature that removes a definition with its uses is planned for them);
the probe compiles all of `base.v` (the probe leaves out the template's marker for untranslated definitions) and stops at the start of `machine.v`, at `Inductive Fun`, whose signatures still mention size variables (P13).
`EqDec` instances are no longer derived with Equations for enums, unions and registers, but built linearly (`nanosail/NanosailToMicrosail/EqDec.ml`):
enums by numbering their values, unions through their constructor tag and the tuple of the fields, `sigT Reg` by numbering the registers.
`NoConfusion` is only derived for records (their `Derive EqDec` needs it). Measured on `base.v` with the TLB types ignored, `coqc base.v` went from 591 s
(`Derive EqDec` for `astConstructor` 210 s, `ast` 149 s, `RegName` 98 s, `Enums` 55 s; `NoConfusion` for `ast` 31 s) to 13 s, and the probe, which never finished the
`depelim` proof of the old `𝑹𝑬𝑮_eq_dec` (a match whose wildcard expands into about 14,000 obligations for 120 registers), to 17 s.
Type abbreviations of tuples are now `(CapEx * capreg_idx)%type` (Rocq read `*` as multiplication of `nat`s).
CHERIoT's constants defined both as a type and as a value (`type cap_addr_width : Int = xlen`, `let cap_addr_width = sizeof(cap_addr_width)`; 10 names) no longer clash:
the type keeps its name, which Rocq types use (`bv (cap_addr_width + 1)`), and the value's definition is `cap_addr_width_value`, which µSail code uses (`exp_val ty.int cap_addr_width_value`).
Unions with type parameters (P02) are now translated to one monomorphic union for each closed type argument that the translated code uses (15 copies):
`AccessType`, `Ext_FetchAddr_Check`, `Ext_ControlAddr_Check`, `Ext_DataAddr_Check` and `PTW_Result` are used at one type and keep Sail's names;
`MemoryOpResult` (8 copies, e.g. `MemoryOpResult_bool`, `MemoryOpResult_bits_32`) and `TR_Result` (2) get the type argument as suffix of the union and constructor names (`MemValue_bool`).
A copy is emitted after the definitions of its type arguments (`AccessType(ext_access_type)` after `ext_access_type`); functions are not copied.
What uses such a union at a type with type variables stays untranslated: the polymorphic helpers `MemoryOpResult_add_meta`/`drop_meta` and the size-polymorphic memory reads
(`mem_read`, `mem_read_priv`, `mem_read_meta`, `phys_mem_read`, `pmp_mem_read`, ..., at `MemoryOpResult(bits(8 * 'n))`; P13). These 7 functions used to count as generated, but their µSail was ill-typed
(`ty.union Umemoryopresult (ty.bvec (8 * kid__p8_times_n / 8))`), so the run that introduced P02 counts them as worse and was accepted as the baseline.
All records now compile: Rocq record projections are now named `<record>_<field>` (for example `Capability_address`), because Rocq projections are global and Sail structs may share field names.
The concurrency-interface types (`Access_kind`, `Mem_*_request`, ...) and the unused `regfp.sail` types are ignored by configuration; see `configuration.lisp`.
The range abbreviations `CapAddrInt` and `CapLen` are no longer ignored: `range` types are now `Z` in Rocq (P11).

**All piles in scope** (before P02), by definitions affected: P13 size-polymorphic functions 55, P02 31, P15 casts 24, P09 mappings 16, P07 9, P01 6, P11 4, P14 6, P05 5, P06 5, P10 4, P03 3; and 1 not in a pile (`match` on `bool`).


## History

| date | commit | scope | instructions | functions | untranslated | `NYI` | hard failures | Rocq probe stops at | note |
|---|---|---|---|---|---|---|---|---|---|
| 2026-11-13 | `685398ab` | whole model | 118 / 147 | 1135 / 1950 | 943 | 113 | 5 | `Access_kind` | inventory; only with a splice for the Sail crash and the 5 hard failures ignored |
| 2026-11-14 | `f9171dfb` | whole model | 118 / 147 | 1130 / 1950 | 953 | 103 | 0 | `Access_kind` | #65: no workarounds needed; the 10 vector helpers that the splice stubbed are untranslated |
| 2026-11-14 | `b8d43e8f` | core | 48 / 61 | 549 / 643 | 150 | 30 | 0 | `Access_kind` | first run restricted to the core |
| 2026-11-14 | `c71962a4`+ | core | 48 / 61, 1 clean | 554 / 647 | 149 | 30 | 0 | `Access_kind` | step 5: clean instructions, first baseline; operators are now named correctly in the call graph, which adds 4 functions to the scope |
| 2026-11-14 | `53075be3`+ | core | 48 / 61, 1 clean | 554 / 647 | 113 | 30 | 0 | `Capability` (39 of 506) | configuration clean-up: overloads and pragmas (P12), concurrency-interface and `regfp` types, `CapAddrInt`/`CapLen` ignored; `bv` widths parenthesised |
| 2026-11-14 | `1643ae27`+ | core | 48 / 61, 1 clean | 554 / 647 | 113 | 30 | 0 | `AccessType` (67 of 506) | Rocq record projections prefixed with the record name, so structs may share field names; all records compile |
| 2026-11-14 | `f3e6cd72`+ | core | 48 / 61, 1 clean | 554 / 647 | 94 | 30 | 0 | `AccessType` (67 of 525) | P08: top-level lets with computed values are evaluated by Sail's interpreter at translation time and become Rocq definitions; all 19 in scope translate |
| 2026-11-14 | `13f3d183`+ | core | 50 / 61, 1 clean | 569 / 647 | 79 | 34 | 0 | `AccessType` (67 of 525) | P04: vector literals of `bits(1)` elements become `bop.bvapp` chains; `FENCE`, `FENCE_TSO` and 13 other definitions translate (some reveal P01 holes and new extern calls) |
| 2026-11-14 | `2a85f662`+ | core | 52 / 61, 1 clean | 571 / 647 | 77 | 34 | 0 | `AccessType` (69 of 529) | P11: integer types with an `if` erased to `int` (`CGetLen`, `CGetTop`); `range` is `Z` in Rocq, so the `CapAddrInt`/`CapLen` stopgap is gone |
| 2026-11-14 | `f70c64eb`+ | core | 52 / 61, 1 clean | 571 / 647 | 77 | 30 | 0 | `AccessType` (69 of 529) | P01: `truncate(X @ zeros(E), w)` with run-time `E` rewritten to a shift; run-time shift amounts; native `truncate`; `getCapBoundsBits` has no holes |
| 2026-11-14 | `b2604dd0`+ | core | 53 / 61, 1 clean | 570 / 647 | 94 | 23 | 0 | `ext_control_addr_error` (222 of 553) | P02: a monomorphic copy of each union with type parameters per closed type argument (15 copies); `StoreCapImm` generated; 7 size-polymorphic memory reads (P13) now untranslated instead of ill-typed |
| 2026-11-14 | `045edc96`+ | core | 53 / 61, 1 clean | 570 / 647 | 94 | 23 | 0 | `TLB_Entry` (258 of 553) | tuple abbreviations are `(A * B)%type`; a value that shares its name with a type is `<name>_value` in Rocq (10 CHERIoT constants) |
| 2026-11-14 | `de4e6e60`+ | core | 53 / 61, 1 clean | 570 / 647 | 94 | 23 | 0 | `TLB_Entry` (258 of 1109) | linear `EqDec` instances for enums, unions and registers instead of `Derive EqDec`/`NoConfusion` (`coqc base.v` 591 s -> 13 s once Rocq gets past `TLB_Entry`) |
| 2026-11-14 | `15c31cfc`+ | core | 53 / 61, 1 clean | 565 / 647 | 105 | 23 | 0 | `machine.v:54` `Fun` (all of `base.v`) | Sv32 `TLB_Entry`/`TLB32_Entry` ignored (5 TLB functions untranslated); plain stops at the PMP registers' `NYI` in `Reg`; the loop's Rocq stage takes 9 s uncached (1:36 in all), 0.1 s cached |

Rows with a different scope are not comparable. Function totals before `b8d43e8f` come from a call graph that missed 9 functions, and before step 5 from one that merged the 5 operators into one.
A commit with `+` means: measured on that commit plus the changes of the commit that follows it.
