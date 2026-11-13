# sail-backend: project status (November 2026)

*Written 13 November 2026, and updated the same day with the decisions in section 1a. For readers without a background in Sail or in translating it. The detailed technical inventory behind this summary is in `20261113-1408/INVENTORY.md`.*

## 1. Summary

- **What the project is.** sail-backend is a translator. It reads a processor specification written in the language **Sail** and writes the same specification in **µSail** (micro-Sail), a small language embedded in the proof assistant **Rocq** (formerly Coq). There, the **Katamaran** verification tool can prove properties about it.
- **The current goal.** Translate the specification of the **CHERIoT** processor, a security-focused RISC-V microcontroller design, so that its security guarantees can be verified with Katamaran.
- **Where it stands.**
  - The translator handles small and medium examples well: 303 of its 334 end-to-end tests pass.
  - On the full CHERIoT specification, it now runs to the end and produces output. That takes two temporary workarounds.
  - About 58% of CHERIoT's functions are translated (1135 of 1963), and 118 of the 147 instruction implementations.
  - The output is **not yet accepted by Rocq**: the first unfinished piece stops it.
- **What is missing.** About a dozen language features that CHERIoT uses and the translator does not yet support, plus a lot of harmless noise.
  One of them is a design question: code that works for several data sizes, such as memory reads of 1, 2 or 4 bytes. Katamaran's own hand-written examples show a clean way to handle it, so the question has a clear recommended answer.
- **Effort.** Most of the remaining gaps are small (days each). A few are medium (one to two weeks). The size-generic memory code is the one large item (several weeks).
- **Decisions (13 November 2026).** The first target is the CHERIoT core only: base integer instructions, multiplication, control registers and the capability instructions. Everything else is excluded by configuration rather than translated. The next piece of work is a working end-to-end setup with a fast, test-driven loop. See section 1a.

## 1a. Decisions taken (13 November 2026)

| question | decision |
|---|---|
| **Scope** of the first verified model | The **CHERIoT core**: base integer instructions (RV32I/E), multiplication (M), control and status registers (Zicsr), and the CHERIoT capability instructions. **Deferred:** bit manipulation (Zb\*), the vector extension, virtual memory (Sv32, TLB) and memory protection (PMP). |
| **Size-generic code** (section 6) | **Deferred.** Expected to be manageable in Katamaran, along the lines of Option B; to be revisited once the code base is better understood. |
| **Undefined values** | Always translated as **universally quantified**: the proof must hold for every possible value. In µSail terms, an `undefined_…` call becomes a fresh, unconstrained value (for example a foreign function whose contract says nothing about its result), never a fixed default. |
| **Exceptions** (`throw`/`try`) | Not a concern for now. |
| **Printing, tracing and other dead code** | **Excluded**, ideally purely **by configuration**. This covers assembly printing, trace output, string handling, and anything else that CHERIoT's core does not need. |
| **Build tool** | Always use **GNU make (`gmake`)**; the BSD/old make that ships with macOS is not supported. |

These decisions shrink the work considerably:
- loops (only used by bit manipulation and PMP), register arrays (PMP) and parameterised structs (TLB) move out of the critical path;
- the large size-generic item is postponed;
- exceptions need no design work yet.

The exclusions need a configuration mechanism that removes **both a definition and its uses**. The existing "ignore this function" option only drops the definition, so its callers end up calling a function that no longer exists.

## 2. Background

### What is being translated, and into what

A **processor specification** describes precisely what each machine instruction does: which registers it reads, how it computes a result, when it raises an exception, how memory is accessed.
**Sail** is a programming language designed for writing such specifications. Official specifications of RISC-V, Arm and CHERI processors are written in it. A Sail specification can be run as a simulator, and it is meant to be the authoritative description of the processor.

**Katamaran** is a tool, built inside the Rocq proof assistant, for *proving* properties of such specifications. For example: "no instruction sequence can ever read memory outside the bounds of a capability". It does not read Sail directly. It reads **µSail**, a deliberately small, simplified version of Sail written as Rocq definitions. µSail is small so that the proofs stay manageable. The cost is that many Sail conveniences are missing from it, such as loops, mutable variables, and functions that work for any data size.

So far, people have written µSail models **by hand**, by reading the Sail specification and re-expressing it. That is slow and error-prone. An example is Katamaran's RISC-V case study "RiscvPmp".

### What sail-backend does

sail-backend automates this step. It runs as a plug-in to the Sail compiler (`sail --katamaran`), in three stages:

1. **Sail's own front end** reads and type-checks the specification. It then simplifies it with a series of standard rewrites ("rewrite passes"), for example turning some convenient constructs into simpler ones.
2. **Sail → "nanosail".** The translator converts each definition (function, type, register, constant) into its own intermediate form. If it meets a feature it does not support, it gives up on *that definition only*, records it as "untranslated", and continues with the next one.
3. **nanosail → µSail.** It prints the result as Rocq files (`base.v` for types, `machine.v` for functions). If it meets something unsupported at this stage, it leaves a placeholder called `NYI` ("not yet implemented") in the output and carries on. A few situations stop this stage completely ("hard failures").

The output is then checked by Rocq. Only when Rocq accepts both files can verification work start.

## 3. The goal: CHERIoT

**CHERIoT** is a processor design from Microsoft and partners for small embedded devices. It extends RISC-V with **capabilities**: pointers that carry their own bounds and permissions, which the hardware checks on every memory access. This prevents whole classes of memory-safety bugs.
Its specification is written in Sail: 67 source files, of which 23 are CHERIoT-specific and 44 are reused from the standard RISC-V Sail model.

Verifying CHERIoT's security properties with Katamaran requires a µSail version of this specification. Producing that automatically is the current target of sail-backend.

## 4. Where things stand

### The translator in general

The project has 334 small end-to-end test programs. Each one is translated and then checked with Rocq.
- **303 pass.** The 31 failures are known gaps, such as tuples of three or more elements, some pattern matches, and some size-generic code.
- Six of the failures are not translator bugs: those tests were committed without some of their input files.
- The 2444 unit tests all pass.

### The CHERIoT specification

**1. The small library file (`prelude.sail`) translates.** Rocq rejects the result at the first `NYI` placeholder.

**2. The full specification** used to crash inside the Sail compiler before translation even started. The cause is now known:
- The translator asks Sail to run a rewrite pass (`remove_blocks`), and then to re-check the result.
- That rewrite discards information that the re-check needs. The information comes from an `assert` statement, which tells Sail that a value is within a certain range.
- The crash is triggered by the translator's choice of passes, not by a bug in the CHERIoT specification. Sail itself accepts the specification without complaint.

For this survey, the crash was avoided by substituting a few functions from an unused part of the specification (the RISC-V vector extension). Five further functions that stopped output generation completely were temporarily excluded.

**3. With those two workarounds, the translator runs to completion on the full specification.** The numbers:

| | |
|---|---|
| Functions translated | 1135 of 1963 (58%); 373 are deliberately excluded by configuration, 455 fail |
| Instruction implementations translated | 118 of 147 |
| Unfinished placeholders (`NYI`) in the output | 113, in 33 definitions |
| Calls to functions that do not exist in the output | 1955 calls to 152 functions |
| Accepted by Rocq | no: `base.v` stops at its first placeholder; `machine.v` has not been checked yet |

Most of the failures do not matter for verification. Of 768 failing definitions, only **176** can actually be reached when the processor runs (fetch, decode, execute an instruction). The rest are things like:
- the code that prints instructions as assembly text;
- generated helper functions that nothing calls;
- bookkeeping entries that the translator reports as "untranslated" but that have no meaning after Sail's type checker has run.

## 5. What does not work yet

Grouped by the kind of problem. Sizes: **S** = a few days, **M** = one to two weeks, **L** = several weeks and a design decision.

### Problems that block everything

- **The Sail crash** (S). The translator's list of rewrite passes needs adjusting, probably by dropping one pass or the re-check after it. This must be fixed first, so that the full specification can be translated without workarounds.

### Features the processor's core needs

- **Code that works for several data sizes** (L; **deferred**, see section 1a). Memory reads and writes are written once in Sail for 1, 2, 4 or 8 bytes. µSail requires every function to have one fixed type, so 54 functions currently come out with an undefined size in their type. This covers every load, store and instruction fetch. See section 6.
- **Data types with a type parameter** (M). Sail lets you define a type such as "result of a memory operation, holding either a value of *some type* or an exception". µSail does not directly support types with a parameter.
  - Most of these types are used with only one concrete type in CHERIoT. Those can simply be specialised, with no extra proof cost.
  - The memory-result type depends on the access size, so it goes together with the previous item.
- **Pattern matches that look inside a value inside an option** (M). For example, "if the result is Some pair (a, b), then …". These are not supported yet. Among others, this affects the top-level `step` function that runs one instruction.
- **Built-in and external functions** (M). Sail specifications call functions provided by the simulator, not defined in Sail: string operations, printing, platform settings (memory size, which extensions are enabled), raw memory access, and "undefined" values.
  The translator emits calls to them without declaring them, so they become 1955 calls to non-existent functions. µSail has a mechanism for this ("foreign functions" with hand-written specifications), but the translator does not use it yet.
  The open decisions are now settled (section 1a):
  - printing and string handling (1399 of the 1697 calls to built-ins) are dropped by configuration;
  - "undefined" values become universally quantified (unconstrained) values.

  What remains is the arithmetic built-ins and the platform and memory functions.
- **Lists of single bits written element by element** (S). For example `[bit_a, bit_b, bit_c]`. A recent Sail version changed the type of a single bit, and the translator was not updated. This one small issue blocks **both instruction decoders** and the capability-permission functions. It is the best value for effort on the list.
- **Integer arguments known only at run time** (M). Some µSail operations need a number to be fixed when the code is written, while Sail computes it at run time. Examples: adding an integer to a bit vector, a bit width taken from a named constant, an error message built from strings. This affects the core CHERI bounds-decoding function and caused four of the five hard failures.
- **Constants defined by an expression** (S–M). Top-level definitions such as "the null capability is this record" or "the maximum address is 2³² − 1" are not translated unless they are plain literals. 22 constants are affected, including the null capability and the root capabilities that the processor starts with.
- **Mutable local variables** (M). Four functions build a value step by step in a local variable, all in the CHERI core: bounds setting, capability encoding, and two instructions. Most such code is already simplified away by Sail's rewrites, but these four remain.
- **Loops, exceptions and early returns** (M). Loops (`foreach`) appear in 16 bit-manipulation instructions and in the memory-protection (PMP) code. Exception handlers (`try`) appear only in the top-level driver. Early `return` appears in one function, the capability revocation check, which is in scope. Loops with fixed bounds can be unrolled.
  **With the new scope, loops and exceptions are deferred** (bit manipulation and PMP are out of scope); only the early `return` remains.
- **Smaller items** (S each):
  - writes to individual fields of a register via a reference (3 functions, including the `CJALR` capability jump);
  - register arrays such as the 64 PMP configuration registers (**deferred** with PMP);
  - a few unusual numeric types (the virtual-memory part is **deferred**);
  - type conversions that the translator currently ignores, which should be checked.

### Noise, not real gaps

- 714 "untranslated" entries are overload declarations and compiler directives. They carry no meaning at this stage and should simply be skipped.
- 140 untranslated functions only convert instructions to and from assembly text. They are not needed for verification and can be excluded by configuration.
- 209 generated "set this register field" helpers are untranslated, but only three functions call them.

## 6. The key design decision: code that works for several sizes or types

Sail lets one function work for several sizes. For example, "read *n* bytes from memory and return an *8n*-bit value", for *n* = 1, 2, 4 or 8. µSail requires each function to have a single fixed type. There are two broad ways to bridge this.

**Option A: make one copy per size** ("monomorphisation"). Produce `mem_read_1`, `mem_read_2`, `mem_read_4`, `mem_read_8`, and so on for about 25 functions.
This is simple for the translator. But every copy then needs its own specification ("contract") and its own proof in Katamaran, which multiplies the verification work. The existing support for this in the translator also regressed with the latest Sail version.

**Option B: one definition with the size as a parameter** (recommended). Rocq, unlike µSail itself, can define a whole family of functions indexed by a number. Katamaran's own hand-written RISC-V model already does exactly this:
- its memory read is declared as `mem_read (bytes : nat)`;
- its memory-result type is a family indexed by `bytes`;
- the contract for the memory read is written once, and its correctness is proven **once, for all sizes at the same time**.

So this approach is known to work with Katamaran, and it avoids duplicated proofs. The translator would need to:
- emit size-indexed functions and types;
- turn Sail's size constraints (such as "*n* is between 1 and 8") into conditions on that parameter;
- fill in the concrete size at each call.

This is the largest single piece of work remaining. It is also the one that unblocks the entire memory path.

**Status: deferred** (section 1a). The expectation is that Katamaran can handle size-generic types reasonably well, along the lines of Option B. The approach will be chosen once the code base is better understood. In the meantime, memory access will have to be stubbed or excluded so that the rest of the core can be checked by Rocq.

For types that are generic in a *type* rather than a size, CHERIoT mostly uses them with one fixed type, so they can simply be specialised.

## 7. Suggested next steps

Updated for the decisions in section 1a. The first phase builds the infrastructure; the second fixes the gaps one by one inside it.

**Phase 1: a working end-to-end setup with a tight, test-driven loop.**

1. **A reproducible CHERIoT run without workarounds.**
   - Fix the Sail crash in the translator's rewrite list (S).
   - Make the case-study build (`gmake`) the single entry point for translating the core and running Rocq on the result.
2. **Configuration-driven scoping.** Exclude everything outside the core by configuration: printing and tracing, assembly syntax, strings, the deferred extensions, unused generated helpers.
   This probably needs one new configuration capability: removing a definition *and* replacing or dropping its calls. Also stop reporting overloads and directives as failures.
   The aim is that what is left to translate is exactly the CHERIoT core.
3. **A progress measure.** Turn the inventory scripts into one command that reports, after each change:
   - which in-scope definitions translate;
   - how many `NYI` placeholders remain;
   - how far Rocq gets.

   Keep the results over time, like the existing test history.
4. **A test for every gap.** For each remaining issue, a small end-to-end test (a few lines of Sail) that fails today and passes once the issue is fixed.
   Most gaps have no such test yet. These tests are fast, so they are the inner loop; the CHERIoT run is the outer loop.
5. **Get Rocq to check `machine.v` at all.** Until then, the only signal is the first error in `base.v`. This needs `NYI` placeholders to be tolerated or stubbed in a checking mode, and built-in functions to be declared (next phase).

**Phase 2: close the gaps on the critical path**, each driven by its test from step 4.

1. Quick wins (S each): lists of bits (unblocks the decoders), constants defined by expressions (null and root capabilities), small numeric-type issues, the simpler run-time-integer cases, early `return`.
2. Built-in and external functions as µSail foreign functions, with undefined values as unconstrained results (M).
3. Nested patterns, and specialising generic types that are used with a single type (M).
4. Mutable local variables (M), register field writes (S).
5. Later, after the code base is better understood: size-generic memory code (L).

Deferred with the out-of-scope extensions: loops, register arrays, parameterised structs, exceptions.

## 8. Open questions and other findings

The questions about scope, size-generic code, undefined values, exceptions and printing have been answered; see section 1a.

Still open:
- **Size-generic code.** Which design to use, once the code base is better understood. A Katamaran maintainer should confirm that its proof automation copes with translator-generated code as well as with hand-written code.
- **Memory access in the meantime.** Load and store instructions are in scope, but they depend on the deferred size-generic code. They will have to be stubbed or excluded for now.
- **Rewrite pass.** Can `remove_blocks` (the cause of the Sail crash) be dropped from the translator's list, or does something depend on it? A full test run will tell.

Smaller issues found along the way, none fixed yet:
- The case-study build script needs GNU make 4.x. **Decision: always use `gmake`**; the old BSD make that ships with macOS is not supported.
- Six recent tests were committed without their input files.
- The numbers of the `NYI` placeholders in the output do not match the numbers of their explanations.
- Katamaran's latest version fails to build a few experimental files. This does not affect what the translator needs.

**Where to find the details.**
- `cheriot-inventory/20261113-1408/INVENTORY.md`: the full technical inventory, with code examples, counts, options and source locations for every item.
- The same directory holds the scripts and data that regenerate it (`extract.py`, `piles.py` and the CSV files).
- `docs/handover.md` describes the state of the project at the previous handover.

## 9. Glossary

| term | meaning |
|---|---|
| **Sail** | A programming language for writing precise, executable specifications of processors. |
| **µSail (micro-Sail)** | A small subset of Sail's ideas, written as definitions inside Rocq, that Katamaran can reason about. |
| **Rocq** (formerly Coq) | A proof assistant: a tool in which mathematical definitions and proofs are written and machine-checked. |
| **Katamaran** | A verification tool built in Rocq that proves properties of µSail specifications, using separation logic. |
| **sail-backend** | This project: the translator from Sail to µSail, run as a Sail plug-in. |
| **nanosail** | The translator's internal intermediate representation, between Sail and µSail. |
| **CHERI / capability** | A hardware security approach in which pointers carry bounds and permissions that the processor checks on every use. |
| **CHERIoT** | A CHERI-based RISC-V design for small embedded devices. |
| **RISC-V** | An open processor instruction set; CHERIoT extends it. |
| **rewrite pass** | A step in the Sail compiler that transforms the specification into a simpler equivalent form. |
| **untranslated definition** | A Sail definition the translator skipped because it uses an unsupported feature. |
| **NYI placeholder** | A marker ("not yet implemented") the translator leaves in its output where it could not produce code. |
| **hard failure** | A problem that stops the translator from producing any output at all. |
| **contract** | A formal specification of a function (what it requires and what it guarantees), which Katamaran proves the function satisfies. |
| **monomorphisation** | Turning code that works for many sizes or types into separate copies, one per size or type. |
| **foreign function** | A function that µSail declares but does not define; its behaviour is given only by a hand-written contract. |
| **PMP** | Physical Memory Protection, a RISC-V feature that restricts which memory regions software may access. |
