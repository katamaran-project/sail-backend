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

14 Nov 2026, measured on `f70c64eb` plus truncation of zero padding of run-time length (P01, `getCapBoundsBits`); cheriot-sail `518813e`, Katamaran `1688c950`.

| | core |
|---|---|
| instructions clean | **1 of 61** (`FENCEI`) |
| instructions generated | 52 of 61 |
| functions generated | 571 of 647 |
| untranslated definitions | 77 |
| `NYI` holes | 30, in 12 definitions |
| hard failures | 0 |
| calls to missing functions | 1492: extern 1283, untranslated 107, outside the scope 86 (from `execute`), skipped by configuration 14, unknown 2 |
| Rocq, plain | `base.v:744`, in `Inductive AccessType`: `The reference NYI was not found` (its type parameter is translated as a hole) |
| Rocq, probe (`NYI` as an axiom) | `base.v:745`, the same definition; 69 of 529 definitions before it |

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
What is left of P11 is the Sv32 TLB code, which is out of scope (virtual memory; see `cheriot-inventory/PROJECT-STATUS.md` §1a) (`make_TLB_Entry`, `match_TLB_Entry`, `flush_TLB_Entry`, `translate32`): the struct `TLB_Entry` has integer parameters, and these functions are polymorphic in them.
The run's report ranks all blockers; `progress.json` lists them per instruction.

**Instructions not generated (9)**, by pile ([inventory](../../cheriot-inventory/20261113-1408/INVENTORY.md)):

| pile | instructions | cause |
|---|---|---|
| P02 | `BTYPE`, `RISCV_JAL`, `RISCV_JALR`, `LOAD`, `STORE`, `StoreCapImm` | matching on a union with type parameters (`Ext_ControlAddr_Error`, `MemValue`, `TR_Failure`) |
| P06 | `CSpecialRW`, `LoadCapImm` | assignment to a mutable local variable (`var`) |
| P07 | `CJALR` | register reference |

**Rocq:** `base.v` stops at `Inductive AccessType`, the first of the unions with type parameters that in-scope code uses (`AccessType`, `MemoryOpResult`, `Ext_*_Check`, `PTW_Result`, `TR_Result`; P02).
Their type parameters are translated as `NYI` holes; this needs a design decision (how Katamaran's closed type universe represents a type-parametric union).
All records now compile: Rocq record projections are now named `<record>_<field>` (for example `Capability_address`), because Rocq projections are global and Sail structs may share field names.
The concurrency-interface types (`Access_kind`, `Mem_*_request`, ...) and the unused `regfp.sail` types are ignored by configuration; see `configuration.lisp`.
The range abbreviations `CapAddrInt` and `CapLen` are no longer ignored: `range` types are now `Z` in Rocq (P11).

**All piles in scope**, by definitions affected: P13 size-polymorphic functions 55, P02 31, P15 casts 24, P09 mappings 16, P07 9, P01 6, P11 4, P14 6, P05 5, P06 5, P10 4, P03 3; and 1 not in a pile (`match` on `bool`).

**Known problem, not yet visible in Rocq:** CHERIoT defines constants twice, as a type and as a value (`type cap_addr_width : Int = xlen` and `let cap_addr_width = sizeof(cap_addr_width)`); both become a Rocq `Definition` with the same name in `base.v` (10 names, 9 of them before P08). Rocq will reject the second one once it gets past the P02 types.

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

Rows with a different scope are not comparable. Function totals before `b8d43e8f` come from a call graph that missed 9 functions, and before step 5 from one that merged the 5 operators into one.
A commit with `+` means: measured on that commit plus the changes of the commit that follows it.
