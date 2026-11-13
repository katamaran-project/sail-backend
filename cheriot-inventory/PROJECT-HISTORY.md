# sail-backend: documentation and project history

*Compiled 13 November 2026 from every documentation file in the repository and its git history (7083 commits, 23 June 2023 – 6 November 2026).*

Dates and authors come from git:
- the **creation** and **last-change** dates of each file come from `git log --follow`;
- the date and author of **each section** of a document come from `git blame -w -M -C` on that section's lines (the author of most lines, and the date range of those lines).

They show when the text was written, not when the described code was. The scripts are in the session scratchpad (`blame_sections.py`); the commands are listed at the end.

---

## 1. At a glance

### Who built it, and when

| era | dates | people | commits | what happened |
|---|---|---|---:|---|
| **1. Prototype** | Jun – Aug 2023 | Marius (Goyet) | 30 | First proof of concept: a Sail plugin that prints a tiny subset of Sail as µSail |
| *(gap)* | Sep – Oct 2023 | — | 0 | |
| **2. Main development** | Nov 2023 – Mar 2025 | **Frederic Vogels** | **6985** | Essentially the whole current code base: nanosail, the two translation stages, Slang configuration, templates, the e2e test suite, and the documentation in `docs/` |
| **3. Maintenance** | Oct 2024 – Jul 2025 (+ Dec 2025, Apr 2026) | Sander Huyghebaert, Steven Keuchel, Dominique Devriese | 25 | Small features and fixes, Nix packaging, CI, Sail pin |
| **4. CHERIoT push** | Dec 2025 – Nov 2026 | Tex Schönlank | 43 | Debugging and test infrastructure, Sail 0.20.3 and Rocq 9.2 upgrades, the CHERIoT case study, `CLAUDE.md`/`HACKING.md`/handover |
| **5. Current** (uncommitted) | Nov 2026 | Denis Carnier (with Claude) | — | handoff, failure inventory, status documents (`cheriot-inventory/`) |

Commits per month: Frederic peaked in October 2024 (770) and February 2025 (808); 16 of his 17 active months have more than 100 commits each (the exception is April 2024, with 48). His last commit is 29 March 2025.

### The documentation files

| file | created | last change | written by | status |
|---|---|---|---|---|
| `README.md` | 2023-06-23 Marius | 2026-11-04 Tex | Marius (2023 versions), Frederic (current index), Tex | current; only an index of `docs/` |
| `docker/readme.md` | 2024-11-12 Frederic | 2026-11-13 removed | Frederic | **removed** with `docker/` in the cleanup (Docker setup pinned Sail 0.18 / Coq 8.18) |
| `tests/msp430-tests/tests-overview.txt`, `tests-history.txt` | 2024-11-25 Frederic | 2026-11-13 removed | Frederic | **removed** in the cleanup (generated files, last updated Nov 2024) |
| `rewrites/readme.md` → `archived_experiments/rewrites/readme.md` | 2025-03-28 Frederic | 2026-11-13 moved | Frederic | archived; path to `Rewrites.ml` corrected |
| `case-studies/README.md`, `archived_experiments/README.md` | 2026-11-13 | same | Denis / Claude | new, written in the cleanup |
| `docs/html-generation.md` | 2024-10-07 Frederic | 2025-03-15 | Frederic | current |
| `docs/installation.md` | 2023-06-23 Marius | 2026-11-06 Tex | Marius → Frederic → Tex | current (Tex rewrote it for Sail 0.20.3 / Rocq 9.2) |
| `docs/tests.md` | 2025-03-10 Frederic | 2026-10-24 Tex | Frederic, then mostly Tex | current |
| `docs/usage.md` | 2025-03-11 Frederic | 2026-11-04 Tex | Frederic (+ Tex's `translate-union-as-sum`) | current |
| `docs/slang.md` | 2025-03-12 Frederic | 2025-03-15 | Frederic | current, brief |
| `docs/monomorphization.md` | 2025-03-13 Frederic | 2026-11-13 | Frederic | current (line numbers and test names fixed 2026-11-13); `monomorphize-*` tests now fail |
| `tests/coq-tests/no-confusion-hom/readme.md` | 2025-03-13 Frederic | 2026-11-13 | Frederic | historical (Steven later removed the derivation it explains; a note now says so) |
| `docs/sail-notes.md` | 2025-03-14 Frederic | 2026-11-13 | Frederic | current knowledge; "Rewrites" section updated in the cleanup |
| `docs/code-overview.md` (+ 5 `.tex`/`.png` diagrams) | 2025-03-10 Frederic | 2026-11-13 | Frederic | current (typo "`NanosailToSail` module" fixed 2026-11-13) |
| `tests/coq-tests/passfails_sorted_by_size.txt` | 2026-05-21 Tex | 2026-11-13 removed | Tex | **removed** in the cleanup (stale data snapshot; its generating script stays) |
| `CLAUDE.md` | 2026-10-21 Tex | 2026-11-06 | Tex | current |
| `HACKING.md` | 2026-11-04 Tex | same | Tex | current (uses `make`; see note in §3) |
| `docs/handover.md` | 2026-11-04 Tex | 2026-11-05 | Tex | current as of handover |
| *deleted:* `notes.org` | 2024-06-12 Frederic | deleted 2025-03-27 | Frederic | TODO list; recovered from history, see §2.2 |
| *deleted:* `supported.txt` (feature list) | 2023-07-18 Marius | deleted 2024-11-19 | Marius | see §2.1 |
| *untracked:* `CHERIOT-INVENTORY-HANDOFF.md`, `cheriot-inventory/*.md` | 2026-11-13 | — | Denis / Claude | this survey |

### End-to-end test results over time (`tests/coq-tests/test-output/test-history.csv`, last run of each month)

| date | pass / fail | | date | pass / fail |
|---|---|---|---|---|
| 2024-09-18 (first record) | 43 / 25 | | 2025-07-21 | 277 / 27 |
| 2024-09-30 | 93 / 24 | | 2026-01-08 | 276 / 27 |
| 2024-10-22 | 107 / 28 | | 2026-06-17 | 276 / 28 |
| 2024-11-28 | 217 / 10 | | 2026-08-30 (test revamp, new tests) | 283 / 43 |
| 2025-01-28 | 242 / 24 | | 2026-09-01 (compile-time integers) | 305 / 22 |
| 2025-03-31 | 275 / 27 | | 2026-10-16 (Sail 0.20.3) | 303 / 25 |
| | | | 2026-11-13 (this survey) | 303 / 31 |

The suite grew from 68 to 334 tests; almost all of that growth was Frederic's (to about 300 by March 2025).

---

## 2. Chronological story

### 2.1 Era 1: Marius's prototype (23 Jun – 27 Aug 2023, 30 commits)

**What was built.**
- A first plugin translating a small Sail subset, written over four weeks: "minimal prettyprint functional" (26 Jun), "call, let and binOp" (30 Jun), "plugin" (3 Jul), "sail to nanosail" (5 Jul), "first prototype" (6 Jul), then tuples, product patterns and a test organisation.
- A commit named "presentation" is from 7 Jul; "final commit ?" is from 18 Jul, followed by README touch-ups and a `-w` width option in August.
- The name *nanosail* for the intermediate language dates from this era (5 Jul 2023).

**Documentation.**
- **`README.md`, 2023 version** (2023-07-06 → 2023-08-27, Marius). One long README:
  - install with opam on OCaml **5.0.0**, using dune, sail and pprint;
  - load the plugin with `sail -plugin …`;
  - the options `-katamaran`, `-list_notations` (Katamaran's list notations), `-ok`, `-w` (page width);
  - the "nanosail" unit tests;
  - "full backend" examples with make targets (`katamaran`, `katamaran-rewrites` to dump each rewrite step, `compare` against expected µSail);
  - "feature tests" comparing generated `.v` files to `expected/` files.
- **`supported.txt`** (2023-07-18 → 2023-08-22, Marius; deleted 2024-11-19 by Frederic as "remaining traces of feature tests"). The supported subset:
  - literals unit, bool, int, string;
  - types bool, int, unit, string, list, and tuples as nested pairs;
  - `if`, `let`, calls, recursive functions, simple list and product patterns.
  "Not well supported": multi-clause functions, guarded patterns, negation, type variables.

### 2.2 Era 2: Frederic Vogels builds the system (17 Nov 2023 – 29 Mar 2025, 6985 commits)

The milestones below come from the first appearance of directories and from commit messages. Frederic's commits are many and small, often one test or one refactoring each.

**Nov 2023 – Jan 2024: port and restructure.**
- 17–20 Nov 2023: "updated codebase for sail 0.17"; a working-example model with type definitions.
- 4 Dec 2023: the code is split into the **`nanosail`** library and the **`sail_plugin`** project. 6 Dec: "replaced NOT_YET_IMPLEMENTED by exceptions" (the start of today's NYI mechanism), top-level type constraints, numeric constraints.
- 4 Jan 2024: the **`monads`** library is split out; 5 Jan: unit-test suites `nanosail-tests` and `monad-tests`; switch to `[%here]` locations.
- 17 Jan 2024: the two stages get their present names, **`SailToNanosail`** and **`NanosailToMicrosail`** ("Gen -> NanosailToMicrosail").

**Feb – May 2024: configuration and templates.**
- Feb 2024: ConfigLib (the configuration library) and the Lisp ("callable evaluates its arguments"); removal of the mapping and impl translators.
- 5 Mar 2024: "**first steps towards templates**"; 8 Mar: template tests; 13 Mar: the `untranslated-definitions` template function.
- 8 Apr 2024: **`tests/coq-tests`** starts (`coq-tests/001`): the e2e suite that compiles output with coqc. Fixes follow for single-register models, EqDec and NoConfusion of register names, enum order.
- May 2024: type applications, unique identifiers.

**Jun – Sep 2024: the nanosail AST and the generation monad.**
- 7 Jun 2024: the **`Ast`** module directory (the IR in its current form); extended types; precedence formatter; many `atoms-*` and `operation-*` tests.
- 12 Jun 2024: **`notes.org`** created (see below).
- Jul 2024: generated EqDec instances for records, variants and enums reworked; "monadic" generators.
- 5–6 Aug 2024: the **`GenerationContext`** monad is introduced; Templates gets its own directory.
- Sep 2024: generation blocks around function definitions (the comment and annotation blocks of today's output); 18 Sep: **first recorded e2e run, 43 pass / 25 fail**.

**Oct – Dec 2024: case studies, CI, configuration language.**
- Oct 2024: records and unions tests, about 770 commits, the busiest month.
  - 18 Oct: tag `sail0.17`; Sander adds ounit2 as a dependency.
  - 29 Oct: **GitHub CI workflow** and Docker setup.
- Nov 2024: register initial values, `Equal`/`NotEqual`; the old feature tests removed; e2e at **217 / 10** (28 Nov).
  - 25 Nov: **`tests/msp430-tests`**, translating parts of the MSP430 Sail model (prelude, exceptions, types, register file, word/byte primitives; 7 of 8 passing on 26 Nov).
- 2 Dec: monomorphisation tests; 5 Dec: a **`rewrites/`** experiment framework, used to see what each Sail rewrite does; 12 Dec: Slang and ConfigLib moved into `nanosail`.

**Jan – Mar 2025: pattern matching, pretty printer, documentation.**
- Jan 2025: tag `culprit` (17 Jan); many `match-*` tests.
- Feb 2025: matching rewritten around **pattern trees** for unit, enum and variant matches (today's 3000-line `Match.ml`); 19 Feb: the **`Document`/PP** refactoring (the source-location-tagged pretty printer); 808 commits, the record month.
- 10–29 Mar 2025: the **documentation burst**. In under three weeks Frederic wrote nearly all of `docs/`: installation, tests, usage, slang, monomorphization, sail-notes, html-generation, code-overview, and the module diagrams (25–29 Mar).
  - 27 Mar: `notes.org` deleted ("removed notes").
  - Last commit 29 Mar 2025; e2e at **275 / 27** (31 Mar).

**`notes.org` — Frederic's TODO file** (2024-06-12 → deleted 2025-03-27, Frederic; 1648 lines, recovered from history).
- **Open TODOs at deletion:**
  - `stm_call`;
  - **a "Foreign Function Kit"**, and **"have regular functions be translated to foreign functions"**. This is exactly the gap that today's inventory calls P16: externs become calls to undefined functions, and `stm_foreign` is never generated;
  - check that ignored definitions are really ignored;
  - variants with a `unit` field should have 0 fields;
  - bitvectors ("Bit type in MuSail?");
  - `record_field_type`, `typedefkit`, `enum_denote`;
  - matching statements.
- **Ideas:**
  - open a GitHub issue about the pattern tree "blow up" (encode/decode functions for mappings), with case trees as in Agda (Ulf Norell's PhD) as a possible remedy;
  - have Sail inline functions;
  - check Sail's monomorphisation abilities;
  - triples in records.
- **Advice recorded from Steven:** derive `EqDec` manually as in RISC-V PMP and skip `NoConfusionHom` (with a link to the MinimalCaps commit).
- **Done:** base module (EqDec, record folds and unfolds, canonicals), use of monads in the base module, `match |>` notation, `stm_match_union_alt`.
- **Reference material:** Katamaran's `Val` denotation, the `bv` record, and the bitvector operations.

### 2.3 Era 3: maintenance by the Katamaran team (Oct 2024 – Jul 2025, plus Dec 2025 and Apr 2026; 25 commits)

- **Sander Huyghebaert** (Oct 2024 – Mar 2025, 10 commits):
  - ounit2 dependency, path typos, "OCaml 5.1.0 is required";
  - `not_vec` → `uop.bvnot` with a test (27–28 Feb 2025);
  - `stm_fail` with type and message (4 Mar);
  - support for the `update_vector_subrange` operator (6 Mar).
- **Steven Keuchel** (Mar 2025 – Dec 2025, 8 commits):
  - **Nix packaging** (`flake.nix`, 7 Mar 2025);
  - removal of the `NoConfusionHom`/`EqDec` derivation for registers (12 May; this makes the `no-confusion-hom` readme historical);
  - **bitvector concatenation order**, least significant first (1 Jun);
  - CI on all branches;
  - Nix and msp430 updates (Dec 2025).
- **Dominique Devriese** (Jun 2025 – Apr 2026, 7 commits):
  - better error message for unknown function clauses;
  - a bitvector test, and "**align order of rewrites a bit more closely with Coq plugin**" (13 Jun 2025, relevant to today's `remove_blocks` crash);
  - support for Sail operators via simple name mangling (21 Jul 2025);
  - **pin Sail to a known-working version** (8 Apr 2026).

e2e: 276–277 / 27 throughout.

### 2.4 Era 4: Tex Schönlank and the CHERIoT case study (17 Dec 2025 – 6 Nov 2026, 43 commits)

**Code and infrastructure.** Commit hashes in brackets; `docs/handover.md` groups them the same way.

- **Katamaran compatibility.** A call graph in the generated `ProgramModule` [`fa343ea1`, Dec 2025]; installation docs aligned with CI [`3b64fcb6`, Jan 2026].
- **Test history.** History kept as CSV with the commit of each run [`7cd28db6`, Mar 2026]; tests sorted by model size [`30a38cbb`, May 2026].
- **Logging and debugging** [Mar–Aug 2026]:
  - clearer stage names in the logs;
  - the Sail location attached to generation failures;
  - the `dump-nanosail-ast` option;
  - logging tests and `indent.sh`;
  - `reindent-ast.py`.
- **Test infrastructure.** `let-shadow-*` and `bitvectors-ones-*` tests [Aug 2026]; "claude attempt to revamp test infrastructure" [`a86c6ca4`, 30 Aug], which produced today's `run-tests.py` with sandboxes and rewrite diffs.
- **Translation.**
  - Compile-time-known integers through a stack of scopes [`e5dfd102`, 1 Sep] took the e2e results from 283/43 to 305/22.
  - Kind-identifier names `kid__…` [`57574d79`, Oct]; natural numbers; parameterised Rocq notations [`88d597f6`].
  - **Upgrade to Sail 0.20.3** [`d405a19a`, 16 Oct].
  - `translate-union-as-sum` [`998b14e5`, 3 Nov].
  - **Rocq 9.2 / OCaml 5.2.1** [`e4434f39`, 6 Nov].
- **Working with Claude and parallel work** [21 Oct – 4 Nov]: `CLAUDE.md`, `bin/sail-local` (test the local plugin instead of the installed one), `bin/new-feature` (one worktree per repository per feature); `.opam` files no longer tracked.
- **CHERIoT.** Work on katamaran's `tex-cheriot` branch (3 commits, 6 Nov): modular `make` calls, more ignored functions, `option`/`result` as `ty.sum`.

**Documentation written in this era:**
- `CLAUDE.md` (21 Oct – 6 Nov)
- `HACKING.md` (4 Nov)
- `docs/handover.md` (4–5 Nov)
- updates to `docs/tests.md` (21–24 Oct), `docs/installation.md` (4–6 Nov), `docs/usage.md` (3–4 Nov), `docs/monomorphization.md` (4 Nov)
- `tests/coq-tests/passfails_sorted_by_size.txt` (21 May)

### 2.5 Era 5: now (13 Nov 2026, uncommitted)

- `CHERIOT-INVENTORY-HANDOFF.md`: the brief for the CHERIoT failure survey.
- `cheriot-inventory/20261113-1408/INVENTORY.md`: the survey itself.
- `cheriot-inventory/PROJECT-STATUS.md`: a plain-English status, with your scope decisions in its §1a.
- This file.
- **Cleanup** (branch `cleanup`):
  - stale and unused files removed: `docker/`, accidentally committed test logs and result snapshots, unused scripts, the unused module `LexingPositionUtil`, the Makefile's `example` target;
  - case studies moved to `case-studies/` (`msp430`, `minimal-caps`);
  - old experiments moved to `archived_experiments/` (`rewrites`, `monomorphization-tests`);
  - docs updated to match.

---

## 3. Document by document

Ordered by when each document was first written. Each section is labelled **[date range, author]**, taken from `git blame` of that section.

### `README.md` (current version)
- **Index** [2024-11-13 … 2026-11-04, Frederic Vogels (7 lines), Tex (1)]: the project title, the Katamaran logo, and links to installation, usage (with HTML generation and monomorphisation), tests and code overview. All substance lives in `docs/`.
  The 2023 README by Marius is summarised in §2.1.

### `docs/html-generation.md`
Created 2024-10-07; the current text is from **[2025-03-11 … 2025-03-15, Frederic]**.
- Why it exists: each generated token can carry the OCaml locations that produced it, a kind of stack trace.
- How to use it: add a `*.template.html` file with a hover-tooltip stylesheet and `(generate (base-html-translation))`, and register it with `(template …)`.
- Caveat: generated HTML files once grew past 500 MB, too big for Chrome. Annotations were reduced, and perhaps should be reduced further.

### `docker/readme.md` [2024-11-12, Frederic] (removed 2026-11-13)
- `./build.sh`, `./run.sh`; inside the container, `source .venv/bin/activate; make`.
- The Docker files have not changed since November 2024. Probably stale: they predate Sail 0.20.3 and Rocq 9.2.

### `tests/msp430-tests/tests-overview.txt`, `tests-history.txt` [2024-11-25 … 11-26, Frederic] (removed 2026-11-13)
A snapshot of the MSP430 case-study tests: `001-prelude`, `002-exceptions` and `003-types` pass, `004-regfile` fails, plus three smaller tests. The history shows the suite growing from 1/1 to 7/1 over two days. Not updated since.

### `docs/installation.md`
- **Intro** [2025-03-10 … 2026-11-06, Frederic → Tex]: the steps mirror the GitHub workflow; the targets are OCaml 5.2.1 and Sail 0.20.3.
- **Setup commands** [2025-03-10, Frederic; updated 2026-11-04, Tex]:
  - system packages (`build-essential libgmp-dev z3 opam pkg-config zlib1g-dev`);
  - optional switch creation;
  - `opam pin add sail 0.20.3` [Tex, 2026-11-04];
  - generating the `.opam` files with `dune build *.opam`, because they are no longer tracked [Tex];
  - `dune build`, `dune test` [Tex], `dune install`.
- **Installing the plugin vs. loading it per call** [2025-03-10, Frederic; `bin/sail-local` added by Tex 2026-11-04]: `dune install` overwrites the plugin in the switch; `sail -plugin _build/…/sail_plugin_katamaran.cmxs` avoids that.
- **Checking the installation** [2025-03-10 … 11, Frederic]: `sail --katamaran` prints "No configuration file specified…".
- **End-to-end tests** [2026-11-04 … 06, Tex]:
  - the e2e tests also need Rocq and Katamaran in the same switch;
  - Katamaran main needs Rocq 9.2, rocq-iris 4.5, rocq-stdpp 1.13 and rocq-equations 1.3.2+9.2, via `opam repo add rocq-released`;
  - the generated code imports `From Stdlib`, so Coq 8.x no longer works;
  - Katamaran has no `.opam` file: build and install it **with its Makefile, not dune**, because dune also builds files that `_CoqProject` leaves out. Today's install hit exactly this.
- **Uninstalling** [2025-03-28, Frederic]: `dune uninstall`.

### `docs/code-overview.md` (+ diagrams `projects`, `overview`, `ast-module`, `sail-to-nanosail-module`, `nanosail-to-microsail-module` `.tex`/`.png`)
- **Project structure** [2025-03-10 … 29, Frederic]:
  - three projects: `monads`, `nanosail` (all logic) and `sail_plugin` (registration);
  - nanosail's dependencies must be linked statically into the plugin (`embed_in_plugin_libraries`).
  Diagram from 2025-03-25 … 29.
- **Translation overview** [2025-03-17 … 19, Frederic]: two phases, Sail → nanosail → µSail; templates decide where output goes.
- **`Ast` module** [2025-03-15 … 29, Frederic]:
  - nanosail follows µSail where they differ, separating expressions from statements;
  - it keeps numeric expressions and constraints for monomorphisation;
  - a program is a list of definitions (`Ast.Definition`).
- **Sail to nanosail** [2025-03-19 … 29, Frederic]: the `TranslationContext` monad (State + Result):
  - fresh identifiers;
  - storing and looking up definitions (`Ast.Definition.Select`);
  - recording polymorphic calls;
  - `NotYetImplemented` (skip the definition) vs. `Failure` ("halts the translation, use sparingly").
- **Nanosail to muSail** [2025-03-15 … 29, Frederic]: the `GenerationContext` monad holds all definitions and supports annotated output. The text says "`NanosailToSail` module", a typo for `NanosailToMicrosail`.
- **Blocks** [2025-03-24, Frederic]:
  - `GenerationContext.block` with `add_comment` and numbered `add_annotation`s, which nest;
  - this is how NYI holes are produced (`not_yet_implemented [%here]`).
- **Logging** [2025-03-11 … 24]: `VERBOSE=0…4`.
- **F-Expressions** [2025-03-11 … 24]: structured `to_fexpr` debug values, written `Head[pos…, key=val]`, and `pp_diff`.
- **`PP`** [2025-03-24]: replaced PPrint to allow per-fragment provenance annotations and decorations.
- **`StringOf`** [2025-03-24]: string conversions for Sail types, partly copied from Libsail.
- **Coding style** [2025-03-11 … 15, Frederic]:
  - *no wildcards*, so the compiler finds every match to update (with a note about OCaml builds hanging until type errors are fixed);
  - *module per type*;
  - *explicit `equal`*, accepting the boilerplate;
  - *explicit typing*, with functions deliberately receiving "too many" parameters to support incremental development.

### `docs/tests.md`
- **Unit tests** [2025-03-10, Frederic]: `tests/monad-tests` and `tests/nanosail-tests` via `make test`; logging is quiet unless `VERBOSE` is set.
- **End-to-end tests** [2025-03-10, Frederic; rewritten 2026-10-21 … 24, Tex]:
  - one directory per test (`model.sail` + `configuration.lisp` including `../shared-configuration.lisp`);
  - the plugin translates, then coqc compiles `base.v` and `machine.v`;
  - the tests use the plugin built in this checkout, not the installed one [Tex].
- **Running and selecting** [Frederic 2025-03-10; selection flags by Tex 2026-10-21]: `make e2e`, 20 parallel jobs by default, `--test`, `--include`, `--include-file`, `--exclude`, `--list`, `--list-nontests`.
- **Options** [mostly Tex, 2026-10-21 … 24]: `--no-coq`/`NO_COQ`, `--no-dump`, `--monomorphise`/`MONOMO`, `--keep-vo`, `--output-dir`, `--sail`, `--coqc`, `--plugin`, `--installed-plugin`.
- **Test output** [2026-10-21, Tex]: sandboxes `test-output/<timestamp>-<commit>[-dirty]/`, the `latest` link, rewrite dumps and diffs, `summary.txt`, `failures.txt`, and the CSV history.

### `docs/usage.md`
- **Usage** [2025-03-11 … 12, Frederic]: inputs are Sail files, `configuration.lisp` and templates; run `sail --katamaran --katamaran-config …`.
- **Configuration file** [2025-03-12 … 13, Frederic]: written in Slang; see `Configuration.ml` for all options. Documented options:
  - `template` (one or two arguments; `x.template.v` → `x.v`) [03-12 … 13];
  - `base-name` (default `UntitledBase`) and `program-name` (default `ModelProgram`) [03-12];
  - `include-original-code` [03-13]: on by default, and puts the original Sail code above each generated definition. The text says "*turning it off* causes the generated code to be annotated", which is inverted;
  - `annotate-functions-with-ast` [03-13]: prints the nanosail AST and "extended type" above each function;
  - `inline-definitions-in-notations` [03-13]: on by default, needed to derive `NoConfusionHom` for `Reg`;
  - `monomorphize` [03-13 … 26]: request named instances such as `foo_8_8` for `Int`-kinded variables;
  - **`translate-union-as-sum`** [**2026-11-03 … 04, Tex**]: two-constructor unions become `ty.sum`, polymorphically. Mapping: `option(T)` → `ty.option T`, `Some` → `exp_inl`, `match` → `stm_match_sum`. Limitations: no nested patterns inside constructors, and type-polymorphic functions such as `is_some` are unsupported;
  - `$include` [03-13]: textual include.
- **Template files** [2025-03-11 … 26, Frederic]:
  - lines between `(*<` and `>*)` are Slang code whose output replaces the block;
  - useful functions: `generate`, `untranslated-definitions?`, `untranslated-definitions`, `base-translation`, `program-translation`;
  - only two coarse chunks can be generated, and finer control would need more functions in `Templates.Prelude`.
- **Verbosity** [2025-03-11 … 12]: `VERBOSE` 0–4, default 2.

### `docs/slang.md` [2025-03-12 … 15, Frederic]
- **Values** [03-15]: a Scheme/Racket-like Lisp-1 with cons cells, nil, ints, booleans `#t`/`#f`, symbols (keywords `:x` evaluate to themselves), strings, callables and references.
- **Defining functions** [03-12]: `(define (f a b) body)`.
- **Heap** [03-12]: `(ref 0)`, `(@ r)`, `(@= r …)`.
- **Library** [03-13]: everything is in `Slang.Prelude`.

### `docs/monomorphization.md` [2025-03-13 … 15, Frederic; 2 lines by Tex 2026-11-04]
- **Working example and logging output** [03-13]: calls to a polymorphic `foo : forall 'n 'm. (bits('n), bits('m)) -> bits(4)` produce "call to polymorphic function" warnings. The text quotes `Function.ml:776`; it is now line 869.
- **Summary generation** [03-13 … 14]: a `polymorphic.template.txt` with `(generate (argument-types-of-polymorphic-function-calls))` lists the argument types per call.
- **Monomorphisation** [03-13 … 14]:
  - `(monomorphize "foo" "foo_8_8" '(("'n" 8) ("'m" 8)))` generates one `fun_foo_8_8` per request;
  - without any requests, a polymorphic function is translated as-is, which gives invalid Coq;
  - with requests, the original is skipped.
- **Missing features** [03-14 … 15; 2 lines by Tex]:
  - Sail's `toplevel_nexps` rewrite introduces extra variables plus constraints (`'n_plus_m == 'n + 'm`) that monomorphisation does not handle;
  - vectors exist in the IR but give NYI in µSail;
  - automatic monomorphisation is an idea only.
  "See the `tests/coq-tests/monomorphization` tests": the directories are actually called `monomorphize-*`, and all four fail today.

### `tests/coq-tests/no-confusion-hom/readme.md` [2025-03-13, Frederic]
Why `inline-definitions-in-notations` exists: `Definition wordBits := bv len_word` has to become `bv 16`, so that Rocq can `Derive … NoConfusionHom EqDec for Reg`. Historical since Steven removed that derivation (May 2025).

### `docs/sail-notes.md` [2025-03-14 … 28, Frederic]
- **Singleton types** [03-20]: `int('i)` (internally `itself('i)`) carries a type-level integer at run time; nanosail represents it as `Ast.Type.Int`. Includes an example AST.
- **Atoms** [03-20 … 24]: `atom` is a refinement type over `int`.
- **`exit()` return type** [03-14 … 28]:
  - Sail's `guarded_pats` rewrite adds `assert(false); exit(())` for missing cases;
  - `exit` returns `unit` but may be used at any type;
  - translated to `stm_fail`, which needs the type expected by the context. That is why `statement_of_aexp` carries an "expected type".
- **`Ast.Type.Implicit`** [03-14 … 26]: `implicit('n)` arguments are kept in the IR "out of laziness", a candidate for removal.
- **Rewrites** [03-14 … 26]: dump them with `--dump-rewrite-ast`; diff them with `process-rewrites.rb` (now superseded by `run-tests.py`'s `delta-*.diff`); change the active ones in `Rewrites.ml`.
- **Expressions vs statements** [03-14 … 20]: µSail separates them, while Sail doesn't. Register reads and calls are statements, so everything is let-bound (worked example `foo(r1) + foo(r2)`).
- **Type of register write** [03-27]: Sail's `r = x` has type unit, so it is translated as `stm_write_register …; tt`.
- **Functions and bindings** [03-27]: Sail's namespaces behave neither like a Lisp-1 nor a Lisp-2 (two examples).
- **Registers / shadowing registers** [03-27]: registers need `stm_read_register`/`stm_write_register`; Sail forbids variables that shadow registers, which makes them easy to recognise.
- Stubs with no content: "Generated Ast Module", "Full Type of Functions", "`sizeof`".

### `rewrites/readme.md` [2025-03-28, Frederic] (now `archived_experiments/rewrites/readme.md`)
The `rewrites/` directory holds small experiments showing what individual Sail rewrites do. Many rewrites had no visible effect on the models considered. The list of available and active rewrites is in `sail_plugin/rewrites.ml` (actually `Rewrites.ml`).

### `tests/coq-tests/passfails_sorted_by_size.txt` [2026-05-21, Tex] (removed 2026-11-13)
The pass/fail status of each e2e test, ordered by `model.sail` size, made with `sort_passfails_by_size.py`. A May 2026 snapshot.

### `CLAUDE.md` [2026-10-21 … 11-06, Tex]
- **Intro** [10-21 … 11-04]: the plugin and its targets; longer docs are in `docs/`.
- **Environment and build** [10-21 … 11-06]:
  - the Nix flake, or opam;
  - target versions (OCaml 5.2.1, Sail 0.20.3, Rocq 9.2, Katamaran main);
  - **do not `make install`** in shared switches; use `bin/sail-local`;
  - the Cheriot Makefile variables (`SAIL`, `CHERIOT_SAIL_DIR`, absolute `OUT_DIR`);
  - plugin libraries embedded statically, in a significant order.
- **Worktrees and feature workspaces** [10-27]: create worktrees only with `bin/new-feature`, and use the sibling worktrees inside a workspace.
- **Tests** [10-21]: unit tests (register suites in `NanosailTests.ml`), e2e flags and sandboxes, the CSV history, template tests.
- **Architecture** [10-21 … 11-04]: the three projects, the pipeline, `TranslationContext` (NYI vs `Failure`), `GenerationContext` (blocks, NYI holes), Slang templates, `PP`, `FExpr`, logging, `ExtBase`.
- **Coding conventions** [10-21]: no wildcards, module per type, `equal`, explicit types, `[%here]`, `make ws`.

### `HACKING.md` [2026-11-04, Tex]
- **Feature worktrees**: configure `newFeature.root` and `newFeature.repo` (Tex used `~/worktrees` and the katamaran and cheriot-sail checkouts); `bin/new-feature <branch>` creates `<root>/<branch>/<repo>` linked worktrees. Warning: submodules get full clones in each worktree.
- **e2e tests**: `dune build; tests/coq-tests/run-tests.py` (flags `--test`, `--no-coq`, `-j`); don't `make install`; use `bin/sail-local`.
- **CHERIoT case study**: `make -C <katamaran>/case_study/Cheriot test|sail SAIL=… CHERIOT_SAIL_DIR=… OUT_DIR=…`. `test` translates only `prelude.sail`; `sail` translates the full model.
  Note: this has to be **`gmake`** on macOS (see `PROJECT-STATUS.md` §1a).

### `docs/handover.md` [2026-11-04 … 05, Tex]
- **Where everything is**: sail-backend on GitHub; the CHERIoT case study on katamaran's `tex-cheriot` branch, on top of `29e8e065`, touching only `case_study/Cheriot/`.
- **What I did**, with hashes:
  - Katamaran compatibility (call graph);
  - debugging and logging (stage names, Sail locations in failures, `dump-nanosail-ast`, `reindent-ast.py`);
  - test infrastructure (CSV history, the `run-tests.py` rewrite with sandboxes, local plugin by default);
  - compile-time-known integers for `sail_ones`/`sail_zeros` (scope stack, `extract_compile_time_integer`, 18 `let-shadow-*` tests);
  - type variables → `kid__` names, naturals, parameterised notations;
  - the Sail 0.20.3 upgrade;
  - tooling for parallel work with Claude.
- **State at handover**: the last full e2e run "on the master branch" gave 309 pass / 25 fail, while the CSV history's last run says 303/31 (reproduced today). Known failures: (monomorphisation, nested unions, `bitvectors-variables` "Cannot determine the level", tuples ≥ 3); CHERIoT `make test` stops at an NYI (`base.v:277`) and `make sail` crashes in Sail (`type_error.ml:577`); unexamined warnings (`Match.ml` assertions, bit-vector element types, polymorphic functions, ignored casts).
- **Planned work**:
  1. get the full CHERIoT model through Sail (suspects: `recheck_defs`, `let VLEN`);
  2. decide sum vs. monomorphisation for polymorphic unions (the `translate-union-as-sum` rationale and limits);
  3. fix regressions (`monomorphize-single-int`, `-two-ints`, `bitvectors-variables`, tuples);
  4. housekeeping (check that the Nix flake's Sail is 0.20.3).

---

## 4. Cross-references and observations

- **Most of `docs/` is a March 2025 snapshot.** It was written in the last three weeks of Frederic's 17 months of work. Tex updated installation, tests and usage in 2026; `code-overview.md`, `sail-notes.md`, `slang.md` and `html-generation.md` have not changed since. They are still accurate in outline, as checked against the code during the inventory.
- **Several of today's gaps were already known.**
  - `notes.org` (2024–25) planned a Foreign Function Kit and translating functions to foreign functions: inventory pile P16.
  - `monomorphization.md` (Mar 2025) lists numeric constraints from `toplevel_nexps`, vectors, and automatic monomorphisation as missing: piles P10 and P13.
  - `sail-notes.md` explains `exit`/`stm_fail` and the expected-type mechanism behind some of today's type-mismatch warnings.
- **The rewrite pipeline has a history.** Frederic picked the rewrites partly from the Coq backend, and partly by experiment (`rewrites/`). Dominique realigned their order with the Coq plugin (Jun 2025). Today's CHERIoT crash comes from one of the Coq-backend rewrites (`remove_blocks`), and is now covered by the e2e test `assert-flow-typing`.
- **Two ways of installing Katamaran exist in the docs.** `installation.md` says to use Katamaran's Makefile. Today's install used dune minus the files `_CoqProject` excludes, which gives the same set of files.
- **Stale or incorrect details worth fixing when convenient:**
  - ~~`code-overview.md`: "`NanosailToSail` module" should be `NanosailToMicrosail`~~ (fixed);
  - ~~`usage.md`: the `include-original-code` description is inverted~~ (fixed; the option is actually *off* by default);
  - ~~`monomorphization.md`: the log line number and the test directory name are out of date~~ (fixed);
  - ~~`rewrites/readme.md`: the file is `Rewrites.ml`, not `rewrites.ml`~~ (fixed in the cleanup);
  - ~~`sail-notes.md`: `process-rewrites.rb` has been superseded by `run-tests.py`'s diffs~~ (fixed in the cleanup);
  - ~~`docker/readme.md` and the msp430 status files date from November 2024~~ (removed in the cleanup);
  - ~~`HACKING.md` uses `make`, which needs to be `gmake` on macOS~~ (fixed, also in `CLAUDE.md`);
  - ~~the `no-confusion-hom` readme describes a derivation that has since been removed~~ (historical note added).

## How this was compiled

```sh
git ls-files | grep -iE '\.(md|txt|org|tex)$|README|HACKING'        # the documentation files
python3 blame_sections.py <files…>                                  # per-section git blame (session scratchpad)
git log --diff-filter=DR --name-status -- '*.md' '*.org' '*.tex'    # deleted and renamed docs
git show fc3a5656^:notes.org; git show d1e991b5:README.md           # recovered old versions
git log --format='%ad|%an' --date=format:%Y-%m | sort | uniq -c     # activity per month
git log --reverse --format='%ad %h %an | %s' -- <dir> | head -1     # when each part of the code appeared
```
