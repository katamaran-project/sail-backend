# CHERIoT translation progress

How much of the CHERIoT core translates to µSail and compiles in Rocq.
The core is the scope in [`tests/cheriot/scope.toml`](../../tests/cheriot/scope.toml): the base integer, M, Zicsr and CHERIoT capability instructions (61), and everything they reach.
The numbers come from the loop ([`docs/cheriot-loop.md`](../../docs/cheriot-loop.md)):

```sh
gmake cheriot CHERIOT_SAIL_DIR=<cheriot-sail> CHERIOT_CASE_STUDY=<katamaran>/case_study/Cheriot
```

**Updating this file.** When a commit changes the loop's numbers, update *Now* and *Blockers*, and add a row to *History*, in the same commit
(usually the commit that runs `gmake cheriot CHERIOT_ARGS=--update-baseline`).
The values are in the run's report, `metrics.json` and `progress.json`; `tests/cheriot/history.csv` has a line per run.
An instruction is *clean* when everything it reaches translates without holes and calls no missing function: it is ready for Rocq.

## Now

14 Nov 2026, measured on `c71962a4` plus the step 5 changes; cheriot-sail `518813e`, Katamaran `1688c950`.

| | core |
|---|---|
| instructions clean | **1 of 61** (`FENCEI`) |
| instructions generated | 48 of 61 |
| functions generated | 554 of 647 |
| untranslated definitions | 149 |
| `NYI` holes | 30, in 12 definitions |
| hard failures | 0 |
| calls to missing functions | 1381: extern 1141, untranslated 138, outside the scope 86 (from `execute`), skipped by configuration 14, unknown 2 |
| Rocq, plain | `base.v:593`, the first `NYI`, in `Access_kind` |
| Rocq, probe (`NYI` as an axiom) | `base.v:594`, in `Access_kind`: a type with a type parameter; 24 of 540 definitions before it |

The baseline (`tests/cheriot/baseline.json`) holds these numbers and the status of all 694 definitions in scope.

## Blockers

**What keeps instructions from being clean.** Each of these blocks 58 of the 61 instructions, because the code they share reaches it:
externs of the tracing and printing code (`print_reg`, `bits_str`, `concat_str`; the policy exclusion of `docs/cheriot-loop.md` §4.2 would remove them),
arithmetic externs (`emod_int`, `get_slice_int`, `shiftr`; P16, externs as foreign functions),
the computed top-level value `cap_len_width` (P08), and `getCapBoundsBits`, which has holes (P01).
The run's report ranks all blockers; `progress.json` lists them per instruction.

**Instructions not generated (13)**, by pile ([inventory](../../cheriot-inventory/20261113-1408/INVENTORY.md)):

| pile | instructions | cause |
|---|---|---|
| P02 | `BTYPE`, `RISCV_JAL`, `RISCV_JALR`, `LOAD`, `STORE`, `StoreCapImm` | matching on a union with type parameters (`Ext_ControlAddr_Error`, `MemValue`, `TR_Failure`) |
| P06 | `CSpecialRW`, `LoadCapImm` | assignment to a mutable local variable (`var`) |
| P04 | `FENCE`, `FENCE_TSO` | vector literal with `bits(1)` elements |
| P11 | `CGetLen`, `CGetTop` | `if` inside a numeric type |
| P07 | `CJALR` | register reference |

**Rocq:** `Access_kind`, a concurrency-interface type whose parameter is a type, stops `base.v` before any instruction. Next planned: ignore it by configuration, with P12.

**All piles in scope**, by definitions affected: P13 size-polymorphic functions 54, P12 overloads and pragmas 37 (mostly noise), P02 31, P08 computed top-level `let`s 19, P15 casts 18, P04 17, P09 mappings 16, P07 9, P01 6, P11 6, P14 6, P05 5, P06 4, P10 4, P03 3.

## History

| date | commit | scope | instructions | functions | untranslated | `NYI` | hard failures | Rocq probe stops at | note |
|---|---|---|---|---|---|---|---|---|---|
| 2026-11-13 | `685398ab` | whole model | 118 / 147 | 1135 / 1950 | 943 | 113 | 5 | `Access_kind` | inventory; only with a splice for the Sail crash and the 5 hard failures ignored |
| 2026-11-14 | `f9171dfb` | whole model | 118 / 147 | 1130 / 1950 | 953 | 103 | 0 | `Access_kind` | #65: no workarounds needed; the 10 vector helpers that the splice stubbed are untranslated |
| 2026-11-14 | `b8d43e8f` | core | 48 / 61 | 549 / 643 | 150 | 30 | 0 | `Access_kind` | first run restricted to the core |
| 2026-11-14 | `c71962a4`+ | core | 48 / 61, 1 clean | 554 / 647 | 149 | 30 | 0 | `Access_kind` | step 5: clean instructions, first baseline; operators are now named correctly in the call graph, which adds 4 functions to the scope |

Rows with a different scope are not comparable. Function totals before `b8d43e8f` come from a call graph that missed 9 functions, and before step 5 from one that merged the 5 operators into one.
A commit with `+` means: measured on that commit plus the changes of the commit that follows it.
