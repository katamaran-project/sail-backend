# CHERIoT translation: progress

How much of the CHERIoT core translates to µSail and compiles in Rocq.

- The core: the scope in [`tests/cheriot/scope.toml`](../../../tests/cheriot/scope.toml) (61 instructions and everything they reach; [design](design.md#scope)).
- Numbers: from `gmake cheriot CHERIOT_SAIL_DIR=<cheriot-sail>` ([loop](loop.md)): the report, `metrics.json`, `progress.json`; [`history.csv`](history.csv) has a line per run.
- *Clean* instruction: everything it reaches translates without holes and calls no missing function; ready for Rocq.
- A commit that changes the numbers updates *Now* and adds a row to *History* (usually the commit that runs `CHERIOT_ARGS=--update-baseline`).

## Now

Measured on `e53999e9` plus matches at type abbreviations; cheriot-sail `518813e`, Katamaran `1688c950`.

| | core |
|---|---|
| instructions clean | **57 of 61**: `ADDIW`, `AUICGP`, `AUIPCC`, `BTYPE`, `CAndPerm`, `CClearTag`, `CGetAddr`, `CGetBase`, `CGetHigh`, `CGetLen`, `CGetPerm`, `CGetTag`, `CGetTop`, `CGetType`, `CIncAddr`, `CIncAddrImmediate`, `CJAL`, `CJALR`, `CMove`, `CRAM`, `CRRL`, `CSEQX`, `CSR`, `CSeal`, `CSetAddr`, `CSetBounds`, `CSetBoundsExact`, `CSetBoundsImmediate`, `CSetBoundsRoundDown`, `CSetHigh`, `CSpecialRW`, `CSub`, `CTestSubset`, `CUnseal`, `DIV`, `DIVW`, `EBREAK`, `ECALL`, `FENCE`, `FENCEI`, `FENCE_TSO`, `ITYPE`, `MRET`, `MUL`, `MULW`, `REM`, `REMW`, `RISCV_JAL`, `RISCV_JALR`, `RTYPE`, `RTYPEW`, `SFENCE_VMA`, `SHIFTIOP`, `SHIFTIWOP`, `SRET`, `UTYPE`, `WFI` |
| instructions generated | 61 of 61 |
| functions generated | 478 of 500 |
| untranslated definitions | 38 |
| `NYI` holes | 4 (`align_down` 3, `read_ram` 1; 3 make previously ill-typed widths explicit: a width that depends on a value, divides a size variable, or is existential) |
| hard failures | 0 |
| calls to missing functions | 100: outside the scope 86 (from `execute`), untranslated 14 |
| foreign functions | 38 externs in `FunX` (312 `stm_foreign` calls): 13 with a semantics (`emod_int`, `pow2`, `count_leading_zeros`, ...), 25 unconstrained (platform settings, memory, tags, ...) |
| policy exclusions | 39, removed with their uses (plus the pattern `undefined_*`, which matches nothing in scope yet): 10 no-op, 7 failure, 18 unconstrained, 1 value, 3 registers (`tests/cheriot/scope.toml`, [design](design.md#policy-exclusions)) |
| Rocq, plain | all of `base.v` (1091 definitions); `machine.v:2343`: 29 of its 505 definitions, then the first `NYI` hole (`align_down`) |
| Rocq, probe (`NYI` as an axiom) | all of `base.v` compiles; in `machine.v`, `Fun`, `FunX`, the `ForeignKit` and 369 of its 505 definitions; it stops at `machine.v:15075`, `htif_store`, on the memory path (a slice of `data : bits('p8_times_n)` needs `16 <= p8_times_n`, and Sail's flow typing makes `data` a `bits(32)` after `width == 4`; both need Sail's constraints). The probe replaces the body of each function with a hole or a call to a missing function by the axiom ([loop](loop.md#rocq)) |

The baseline (`tests/cheriot/baseline.json`) holds these numbers and the status of all definitions in scope.

## Known limitations

- Memory loads and stores (`LOAD`, `STORE`, `LoadCapImm`, `StoreCapImm`) are not clean: the size-polymorphic memory path needs Sail's constraints and width-indexed unions.
- Option patterns with a nested pattern (`Some(RV32)`, `Some(intr, priv)`) are untranslated: `translationMode`, `step`, `dispatchInterrupt`.
- The main decoder `encdec_backwards` is untranslated (a `match` on a `bool`).
- Plain Rocq stops at the first `NYI` hole in `machine.v`, which also still calls the 86 out-of-scope `execute_<C>`.
- PMP is absent (`sys_pmp_count = 0`) and virtual memory is excluded; see [design](design.md#scope).
- Details and what each blocks: [roadmap](roadmap.md).

## History

| date | commit | scope | instructions | functions | untranslated | `NYI` | hard failures | Rocq probe stops at | note |
|---|---|---|---|---|---|---|---|---|---|
| 2026-11-13 | `685398ab` | whole model | 118 / 147 | 1135 / 1950 | 943 | 113 | 5 | `Access_kind` | inventory; only with a splice for the Sail crash and the 5 hard failures ignored |
| 2026-11-14 | `f9171dfb` | whole model | 118 / 147 | 1130 / 1950 | 953 | 103 | 0 | `Access_kind` | no workarounds needed; the 10 vector helpers that the splice stubbed are untranslated |
| 2026-11-14 | `c6e58096` | core | 48 / 61 | 549 / 643 | 150 | 30 | 0 | `Access_kind` | first run restricted to the core |
| 2026-11-14 | `c6e58096`+ | core | 48 / 61, 1 clean | 554 / 647 | 149 | 30 | 0 | `Access_kind` | clean instructions, first baseline; operators are now named correctly in the call graph, which adds 4 functions to the scope |
| 2026-11-14 | `4e969f03`+ | core | 48 / 61, 1 clean | 554 / 647 | 113 | 30 | 0 | `Capability` (39 of 506) | configuration clean-up: overloads and pragmas, concurrency-interface and `regfp` types, `CapAddrInt`/`CapLen` ignored; `bv` widths parenthesised |
| 2026-11-14 | `4e969f03`+ | core | 48 / 61, 1 clean | 554 / 647 | 113 | 30 | 0 | `AccessType` (67 of 506) | Rocq record projections prefixed with the record name, so structs may share field names; all records compile |
| 2026-11-14 | `55b9b7b3`+ | core | 48 / 61, 1 clean | 554 / 647 | 94 | 30 | 0 | `AccessType` (67 of 525) | top-level lets with computed values are evaluated by Sail's interpreter at translation time and become Rocq definitions; all 19 in scope translate |
| 2026-11-14 | `ba47e37d`+ | core | 50 / 61, 1 clean | 569 / 647 | 79 | 34 | 0 | `AccessType` (67 of 525) | vector literals of `bits(1)` elements become `bop.bvapp` chains; `FENCE`, `FENCE_TSO` and 13 other definitions translate (some reveal run-time-width holes and new extern calls) |
| 2026-11-14 | `488b4409`+ | core | 52 / 61, 1 clean | 571 / 647 | 77 | 34 | 0 | `AccessType` (69 of 529) | integer types with an `if` erased to `int` (`CGetLen`, `CGetTop`); `range` is `Z` in Rocq, so the `CapAddrInt`/`CapLen` stopgap is gone |
| 2026-11-14 | `079ed8b4`+ | core | 52 / 61, 1 clean | 571 / 647 | 77 | 30 | 0 | `AccessType` (69 of 529) | `truncate(X @ zeros(E), w)` with run-time `E` rewritten to a shift; run-time shift amounts; native `truncate`; `getCapBoundsBits` has no holes |
| 2026-11-14 | `115d7189`+ | core | 53 / 61, 1 clean | 570 / 647 | 94 | 23 | 0 | `ext_control_addr_error` (222 of 553) | a monomorphic copy of each union with type parameters per closed type argument (15 copies); `StoreCapImm` generated; 7 size-polymorphic memory reads now untranslated instead of ill-typed |
| 2026-11-14 | `01eabff0`+ | core | 53 / 61, 1 clean | 570 / 647 | 94 | 23 | 0 | `TLB_Entry` (258 of 553) | tuple abbreviations are `(A * B)%type`; a value that shares its name with a type is `<name>_value` in Rocq (10 CHERIoT constants) |
| 2026-11-14 | `13f995df`+ | core | 53 / 61, 1 clean | 570 / 647 | 94 | 23 | 0 | `TLB_Entry` (258 of 1109) | linear `EqDec` instances for enums, unions and registers instead of `Derive EqDec`/`NoConfusion` (`coqc base.v` 591 s -> 13 s once Rocq gets past `TLB_Entry`) |
| 2026-11-14 | `73d51ac7`+ | core | 53 / 61, 1 clean | 565 / 647 | 105 | 23 | 0 | `machine.v:54` `Fun` (all of `base.v`) | Sv32 `TLB_Entry`/`TLB32_Entry` ignored (5 TLB functions untranslated); plain stops at the PMP registers' `NYI` in `Reg`; the loop's Rocq stage takes 9 s uncached (1:36 in all), 0.1 s cached |
| 2026-11-14 | `25dfadb9`+ | core | 53 / 61, 1 clean | 480 / 534 | 71 | 23 | 0 | `machine.v:49` `Fun` (all of `base.v`) | policy exclusions (definitions removed with their uses): printing, tracing and Sv32 (30 definitions); missing calls 1494 -> 373; 113 definitions leave the scope; translation 65 s -> 23 s |
| 2026-11-14 | `57d1c72b`+ | core | 53 / 61, 3 clean | 480 / 534 | 71 | 21 | 0 | `machine.v:49` `Fun` (all of `base.v`) | externs become µSail operations or foreign functions (`FunX`, `stm_foreign`, `ForeignCall`); missing calls 373 -> 143 (externs 240 -> 10); `FENCE`, `FENCE_TSO` clean |
| 2026-11-14 | `8e591b21`+ | core | 53 / 61, 20 clean | 483 / 534 | 71 | 26 | 0 | `machine.v:14299` `htif_store` (384 of 510) | widths and types as implicit arguments of `Fun`; `size_itself_int`; `zeros_implicit`/`ones` translated; `bit` is `ty.bvec 1`; widths that need Sail's constraints are holes (+5); the probe axiomatises bodies with holes or missing calls |
| 2026-11-14 | `9b45dd1c`+ | core | 58 / 61, 20 clean | 490 / 534 | 64 | 26 | 0 | `machine.v:14402` `htif_store` (386 of 517) | `C(e)` binds `e` to the tuple of the fields of a constructor with several fields; `BTYPE`, `RISCV_JAL`, `RISCV_JALR`, `LOAD`, `STORE` generated (not clean: memory path) |
| 2026-11-14 | `f40fe15e`+ | core | 59 / 61, 20 clean | 492 / 534 | 62 | 28 | 0 | `machine.v:14643` `htif_store` (387 of 519) | assignments to `var` locals are `stm_assign`; `CSpecialRW` generated; `setCapBounds` translates with 2 holes (existential widths); `LoadCapImm` now stops at a nested pattern |
| 2026-11-14 | `0328aa69`+ | core | 60 / 61, 20 clean | 494 / 534 | 60 | 29 | 0 | `machine.v:14824` `htif_store` (388 of 521) | nested patterns: inside the fields of a constructor (`TR_Failure(E_Extension(_), _)`); `LoadCapImm` generated; `ext_data_get_addr` translates with 1 hole |
| 2026-11-14 | `b2b44476`+ | core | 61 / 61, 20 clean | 497 / 534 | 46 | 29 | 0 | `machine.v:15041` `htif_store` (389 of 524) | calls to bitfield setters with a register reference inlined (read, `_update_*`, write); setters and `__deref` skipped; `CJALR`, `ext_init`, `ext_init_regs` generated |
| 2026-11-14 | `da9963ca`+ | core | 61 / 61, 20 clean | 497 / 529 | 46 | 29 | 0 | `machine.v:15041` `htif_store` (389 of 524) | loop: a setter call with a register reference counts as a call to `_update_*` in the call graph, as the translator inlines it; 5 setters leave the scope |
| 2026-11-14 | `da9963ca`+ | core | 61 / 61, 35 clean | 499 / 529 | 46 | 29 | 0 | `machine.v:15088` `htif_store` (391 of 526) | `num_of_ExceptionType`, `num_of_ext_exc_type` translated (the configuration skipped all `num_of_*`); the clean check skips calls into policy-excluded definitions, as the scope does: 15 more instructions clean |
| 2026-11-14 | `da9963ca`+ | core | 61 / 61, 50 clean | 502 / 529 | 43 | 29 | 0 | `machine.v:15942` `htif_store` (393 of 529) | Sail's `early_return` rewrite enabled; a `return` it cannot remove is refused; `capToEncCap`, `encCapabilityToCapability`, `mem_read_cap_revoked` generated: 15 more instructions clean |
| 2026-11-14 | `50a76d89`+ | core | 61 / 61, 55 clean | 502 / 529 | 43 | 25 | 0 | `machine.v:15933` `htif_store` (393 of 529) | `zero_extend(ones(n))` with run-time `n` rewritten to a shifted mask; `setCapBounds` without holes |
| 2026-11-14 | `28524b93`+ | core | 61 / 61, 55 clean | 477 / 500 | 39 | 12 | 0 | `machine.v:15061` `htif_store` (368 of 504) | PMP absent (policy exclusions); discarded calls keep their type; plain Rocq up to the untranslated-definitions marker in `base.v` |
| 2026-11-15 | `c90a4863`+ | core | 61 / 61, 55 clean | 477 / 500 | 39 | 12 | 0 | `machine.v:15061` `htif_store` (368 of 504) | listing of untranslated definitions escaped, marked by a comment: plain Rocq compiles all of `base.v`, stops at the first hole in `machine.v` |
| 2026-11-15 | `137304dd`+ | core | 61 / 61, 56 clean | 477 / 500 | 39 | 7 | 0 | `machine.v:15059` `htif_store` (368 of 504) | widths of `@` behind type abbreviations (`tagaddrbits`) resolved: `tag_addr_to_addr`, `ext_data_get_addr`, `execute_CSetHigh` without holes, `writeCSR` 5 -> 3; `CSetHigh` clean |
| 2026-11-15 | `26e0b943`+ | core | 61 / 61, 56 clean | 477 / 500 | 39 | 7 | 0 | `machine.v:15059` `htif_store` (368 of 504) | `undefined_*` policy-excluded as unconstrained by a pattern in `scope.toml`; no in-scope code reaches one yet, so the numbers are unchanged |
| 2026-11-15 | `b372ec63`+ | core | 61 / 61, 57 clean | 477 / 500 | 39 | 4 | 0 | `machine.v:15059` `htif_store` (368 of 504) | `zero_extend(m, v[hi .. 0])` with run-time `hi` rewritten to a mask: `writeCSR` (`vstart`) without holes; `CSR` clean; with the 13 functions over `bits('p8_times_n)` axiomatised, all of `machine.v` compiles |
| 2026-11-15 | `e53999e9`+ | core | 61 / 61, 57 clean | 478 / 500 | 38 | 4 | 0 | `machine.v:15075` `htif_store` (369 of 505) | a match or tuple pattern at a type abbreviation matches the abbreviated type: `ext_handle_data_check_error` translates |

Rows with a different scope are not comparable. Function totals before `c6e58096` come from a call graph that missed 9 functions, and before `c6e58096` from one that merged the 5 operators into one.
A commit with `+` means: measured on that commit plus the changes of the commit that follows it.
