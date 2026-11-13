![](katamaran_logo.svg)

# Handover: Tex Schönlank's work on sail-backend

This document is for whoever continues the sail-backend work. It covers:

1. Where the work is: repositories, branches, worktrees, and what is pushed.
2. What I did, grouped by topic, with commit hashes,
3. The state at handover,
4. What was planned next, roughly by priority.

How to build, test and run things, I explain in [`HACKING.md`](../HACKING.md), and further details are in [`CLAUDE.md`](../CLAUDE.md) and [`tests.md`](tests.md).

The overall goal of this work was to translate the **CHERIoT Sail model** (`cheriot-sail`) to Microsail.
We chose to focus on the Katamaran case study in `katamaran/case_study/Cheriot`.

## Where everything is

### sail-backend

`sail-backend`'s remote `origin` is `github.com:katamaran-project/sail-backend`.

### katamaran

I looked at sail-backend's performance on cheriot-sail as a case study.
The case study was set up in katamaran's commit `29e8e065` ("Commit non-working Cheriot case study as test for Tex").
My work is on its branch `tex-cheriot`, on top of `29e8e065`.
It only touches `case_study/Cheriot/`.

## What I did

### Katamaran compatibility

- `fa343ea1`: add a call graph to the generated `ProgramModule`.
  Newer Katamaran requires it.
- `3b64fcb6`: make `docs/installation.md` match the GitHub workflow.

### Debugging and logging infrastructure

These commits make it easier to find out *where* a translation fails:

- `df75c7e9`: the logs say clearly which stage is running (Sail→nanoSail, nanoSail→Microsail).
- `05dad547`, `4d66400f`: `FunDefKit` wraps the generation of each function definition in `augment_failure_with_sail_location`, so that a failure reports the Sail location.
- `ee8e5e6e`: the `GenerationContext` tracks the current Sail location. Errors now look like
  `... at vector.sail:326 in argument 1 in call to sail_zeros in body of function sail_ones`.
- `c9b1cd02`: configuration flag `(dump-nanosail-ast)`, which prints the nanoSail AST of each definition.
- `1fb309ca`: `LexingPositionUtil.lexing_position_to_string`.
  (Removed on 13 Nov 2026: no other module used it.)
- `796abee5`, `c107f1a2`: `tests/nanosail-logging-tests/`, small models for looking at the logs.
- `6d673d8f`: [`tests/reindent-ast.py`](../tests/reindent-ast.py), which pretty-prints dumped ASTs so that they can be read and diffed.

### Test infrastructure

- `7cd28db6`, `9f70c6cd`, `a56798ae`, `30a38cbb`: the test history is a CSV that records the commit of each run.
  Clearer output.
  `sort_passfails_by_size.py` sorts the tests by model size.
- `2bb8e226`, `6b675f43`, `7aa34312`: move test output out of the source tree, into `tests/coq-tests/test-output/`.
- `a86c6ca4`: **rewrite of [`tests/coq-tests/run-tests.py`](../tests/coq-tests/run-tests.py)**, with Claude.
  It replaces `test.sh`, `test-list.sh` and the Ruby helper scripts.
  Each run gets a sandbox `test-output/<timestamp>-<commit>[-dirty]/`, which contains the generated Rocq, the dumps of each rewrite pass, the diffs between passes, `summary.txt` and `failures.txt`.
  Tests run in parallel.
- `b5dc14cc`, `7e341252`: `run-tests.py` uses the plugin from this checkout's `dune build` by default (`--installed-plugin` for the old behaviour).
  The Makefile targets do the same.
- `6c32b185`: the msp430 and template tests take `SAIL` from the environment, so `bin/sail-local` can be passed to them.
- `dad154cd`: update [`docs/tests.md`](tests.md).

### Compile-time-known integer arguments: `sail_ones`/`sail_zeros`

**Problem.** Microsail needs the size argument of `sail_ones(n)`/`sail_zeros(n)` to be a literal.
The backend only recognized literals and top-level constants, so `let n = 3 in sail_ones(n)` failed with `sail_ones expects compile time known integer argument`.
CHERIoT has such calls.

**Solution** (main line, `e5dfd102`, in [`nanosail/NanosailToMicrosail/`](../nanosail/NanosailToMicrosail/)):
- `GenerationContext.t` has a *stack of scopes* `compile_time_integers : Z.t option Ast.Identifier.Map.t list`.
  `GC.with_compile_time_integers bindings computation` pushes a frame, runs the computation and pops the frame.
- `Statements.pp_let` pushes a frame for each `let`.
  `extract_compile_time_integer_from_statement` goes through `Located`, `Cast`, `Expression` and calls to `make_the_value`.
- `FunctionCalls.extract_compile_time_integer` evaluates `+`, `-` and `*` recursively.
  It looks variables up in the scope stack first and in the top-level definitions second.
- A stack (not a flat map) is needed for shadowing: in `let x = 1 in let x = x in sail_ones(x)`, the inner `x` must resolve to the outer one.

**Tests:** `let-shadow-*` (`59915126`, 18 tests for int/ones/zeros × basic, sandwich, alias, dependent, use-after, self-alias, plus `let-shadow-bools`), `let-shadow-ones-match` (`e5dfd102`), and `bitvectors-ones-with-{arithmetic,subtraction}-size*` (`087024ca`).
This fixed `bitvectors-128-ones` and the four `bitvectors-ones-with-*` tests.

On the CHERIoT side, the *definitions* of `sail_ones`, `sail_zeros`, `slice_mask` and `ones` in the Sail library must be ignored (katamaran `f41e3a93`), because their bodies call each other with non-literal sizes.
The calls themselves are translated natively.

### Type variables, naturals, notations

- `57574d79`: Sail kind identifiers (`'n`) are turned into names that are valid in Microsail/Rocq (`kid__n`).
  New test `bitvectors-variables` (type variables in type synonyms and records).
  **It still fails** in Rocq: `base.v: Error: Cannot determine the level.`
- `fa1e6f2d`: Microsail representation of natural numbers.
- `88d597f6`: Rocq `Notation`s generated by `BaseModule` can take parameters.

### Upgrade to Sail 0.20.3 (`d405a19a`)

From Sail 0.18 to 0.20.3.
The changes are in `Sail.ml`, `Literal.ml`, `Function.ml`, `ValueDefinition.ml`, `Rewrites.ml` and `StringOf/SailToString.ml`, among others.
The test configurations gained some options as well.
Thanks to this upgrade, the CHERIoT case study compiled further than before.
**Regressions:** `monomorphize-single-int` and `monomorphize-two-ints` now fail.

### Tooling for parallel work with Claude

- `b413e3d7`, `7e341252`, `a083d925`: [`CLAUDE.md`](../CLAUDE.md).
  It describes the build, the tests, the architecture and the coding conventions.
  It also tells agents not to run `make install`, because that overwrites the plugin in the shared opam switch.
- `12353ec5`: [`bin/sail-local`](../bin/sail-local), a drop-in `sail` that loads this checkout's built plugin.
- `57c19252`: [`bin/new-feature`](../bin/new-feature), see [Feature worktrees](#feature-worktrees).
- katamaran `2da6a025`, `81269b71`, `81b24b84` and cheriot-sail `7e01c42`: `SAIL`, `OUT_DIR` and `CHERIOT_SAIL_DIR` can be set from outside.
- `5e42d0ff`: the `.opam` files are no longer tracked because dune generates them from `dune-project`.

## State at handover

### End-to-end tests

The last full run on the master branch, gave **309 pass, 25 fail**.
The 25 failures are the same as on the main line.
Some of these tests are expected to fail because the feature is not implemented.
Grouped by cause, as far as I know:

Some tests fail because of the _long tuples problem_:
the *type* of a 3-tuple becomes a single `ty.tuple [ty.int; ty.int; ty.int]`, but the *value* becomes nested binary pairs
(`exp_binop bop.pair (exp_binop bop.pair 1 2) 3`), and these do not match.

Some other tests fail on monomorphization: `monomorphize-constraint` and `monomorphize-type`, and since the version bump to Sail 0.20.3 also `monomorphize-single-int` and `monomorphize-two-ints`.

Some tests fail due to matching on nested unions: `match-unions-nested`, `match-unions-nested-2`.

### CHERIoT case study

Suppose we run `make test` in katamaran's CHERIoT case study (i.e. compile the `prelude.sail` into Microsail and run `coqc` on its outputs).
With sail-backend's `master` branch and katamaran's `tex-cheriot` branch checked out, it currently fails on a _not yet implemented_ / NYI at line 277 in `base.v`.

Running `make sail` causes an internal error in Sail in both branches:
`internal error: unreachable code`, `src/lib/typ_error.ml` at line 577.

Errors and warnings in the prelude translation that I had not yet looked into:
- `Match.ml` assertion errors: `expected pattern AP_app (Some, AP_wild _, _) while matching type option(kid__a)`.
  These come from type-polymorphic library functions (`is_none`, `is_some`, `is_ok`, `is_err`, `ok_option`, `err_option`, `unwrap_or`).
- `Function.ml`: `Elements of bit vector should all be bits; they were Type:Bitvector[1]`, in `function bool_to_bits x = [bool_to_bit(x)]`.
- `Encountered polymorphic function ... No monomorphizations were requested`, for `neq_int` and others.
  `internal_pick` is the first top-level type constraint with this warning.
- `Statements.ml`: `Ignored cast` warnings.
  I don't know where these casts come from.

## Planned and open work

Roughly in order of decreasing priority.

### Get the full CHERIoT model through Sail

- Find out why `make sail` hits Sail's `unreachable code` in `typ_error.ml:577`, and whether this is related to `recheck_defs` and `let VLEN`.
- Then fix the NYI holes in `base.v` one by one, and get `coqc base.v` and `machine.v` to compile.

### Polymorphic unions: decide sum vs. monomorphization

Sail allows for polymorphic unions and CHERIoT makes use of this.
(They define an `option` like Haskell's `Maybe`, and a `result` like Haskell's `Either`.)
We had to choose whether to monomorphize all Sail code using such polymorphic unions, or to treat it differently.

In commit `998b14e5 2026-11-03 Tex Schönlank Add translate-union-as-sum: translate two-constructor unions to ty.sum` I chose to translate unions such as `option` and `result` into Microsail's `ty.sum`, which is binary and supports polymorphism.
I implement this as a configuration setting:

```lisp
(translate-union-as-sum "option" "Some" "None")
(translate-union-as-sum "result" "Ok" "Err")
```

The commit adds
- `Definition.Type.Sum`, `SumSide`, `Expression.Injection` and `Statement.MatchSum` to Nanosail
- a `Sum` node in the pattern tree in `Match.ml`
- generation of `ty.sum`/`exp_inl`/`exp_inr`/`stm_match_sum`.
- 
It is documented in `docs/usage.md`, and tested by six `sum-unions-*` tests, which pass.
Limitations: no nested patterns inside the constructors (`Some(Foo(x))`).
Functions that are themselves type-polymorphic (`is_some`) must still be ignored.

On the CHERIoT example, this got `coqc base.v` of the prelude from an NYI on line 33 to an NYI on line 277.

Another possibility is to monomorphize `option`, `MemoryOpResult` and the rest.
This means that every function that uses them must be monomorphized as well.
There are not that many polymorphic unions that actually end up being used with more than a few type instantiations.

Perhaps check other Sail models before deciding how general an approach to take.

### Fix the regressions and failing tests

- `monomorphize-single-int` and `monomorphize-two-ints` (since Sail 0.20.3).
- `bitvectors-variables`: "Cannot determine the level".
  Ask a Rocq expert.
- Tuples of length ≥ 3: choose either real n-ary tuples or nested pairs, consistently.
  My preference went to nested pairs because I am more certain that this has support in Microsail.
  Such tuples are not used much in practice, so this is not urgent.
- Low priority: check that no user identifier starts with the reserved `kid__` prefix, or generate fresh names in a monad.

### Housekeeping and pushing

- Nix: `flake.nix` takes Sail from nixpkgs.
  Check that it provides Sail 0.20.3.

