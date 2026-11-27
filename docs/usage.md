# Usage

To translate Sail to muSail, you need the following files:

* One or more Sail files that contain the code to be translated.
* A configuration file, `configuration.sexp` by convention.
* One or more template files.
  Each template file will lead to the generation of an output file.

Next, run the following command to perform the translation.

```bash
$ sail --katamaran --katamaran-config configuration.sexp SAILFILE...
```

## Worked example: CHERIoT from scratch

From nothing to Rocq files for the [CHERIoT Sail model](https://github.com/CHERIoT-Platform/cheriot-sail).
Needs opam, git, Python 3.11 or later, make, GNU make 4 or later for cheriot-sail's Makefile (macOS: install it as `gmake`), and Sail's system dependencies (GMP, Z3; see [installation](installation.md)).

### 1. Create an opam switch with the dependencies

```bash
opam switch create sail-backend --packages=ocaml.5.5.1,sail.0.20.3,dune,base,stdio,parsexp,pprint,ppx_here,ocaml_intrinsics_kernel,ounit2
eval $(opam env --switch=sail-backend)
```

### 2. Build the plugin

```bash
git clone https://github.com/katamaran-project/sail-backend.git
cd sail-backend/
dune build
bin/sail-local --katamaran
# No configuration file specified; use --katamaran-config to specify one
cd ..
```

- The plugin: `_build/default/sail_plugin/sail_plugin_katamaran.cmxs`.
- `bin/sail-local` is `sail` with this plugin loaded.
  Optional: `dune install` installs the plugin in the switch, so that plain `sail --katamaran` works too.

### 3. Get the CHERIoT model

```bash
git clone --recurse-submodules https://github.com/CHERIoT-Platform/cheriot-sail.git
git -C cheriot-sail checkout 518813e        # the commit the case study is tested against
git -C cheriot-sail submodule update        # sail-riscv at the matching commit
```

### 4. Translate

```bash
cd sail-backend/
make cheriot-translate CHERIOT_SAIL_DIR=$PWD/../cheriot-sail
```

- About a minute. Output in `case-studies/cheriot/output/`: `base.v` (types, registers) and `machine.v` (functions).
- Translates the core: 147 instructions and what they reach ([scope](../case-studies/cheriot/docs/design.md#scope)).
  `CHERIOT_ARGS=--no-scope` translates the whole model, which has holes (`NYI`) outside the core.
- The output directory also holds the inputs: the configuration and templates from [`case-studies/cheriot`](../case-studies/cheriot/README.md), `sources.txt` (the model's Sail files, in order), and `scope-ignores.sexp` (what is outside the core).
- `log.txt` is large (about 80 MB): the configuration's `(dump-nanosail-ast)` prints every definition's AST.

### 5. Compile with Rocq

Needs Rocq and Katamaran in the switch ([installation](installation.md#end-to-end-tests)).

```bash
cd case-studies/cheriot/output/
coqc base.v
coqc machine.v
```

- Both compile: `base.v` in about 7 seconds, `machine.v` in about a minute.
  Current state: [progress](../case-studies/cheriot/docs/progress.md).
- `make cheriot CHERIOT_SAIL_DIR=...` does steps 4 and 5 and measures the result: [the loop](../case-studies/cheriot/docs/loop.md).

## Configuration File

The configuration is a file of S-expressions, read as data (as dune reads its files): one form per option.

```lisp
;; A comment
(include ../shared-configuration.sexp)
(templates base.template.v machine.template.v)
(base-name MySailModelBase)
(dump-nanosail-ast)
(ignore (functions "undefined_*" log2) (types regstate))
```

- **Values.** A name is an atom (`log2`, `base.template.v`) or a string (`"undefined_*"`); strings are needed for text with spaces, parentheses, quotes or `;`, and are written as in OCaml (`"a \"quoted\" word"`).
  Patterns are usually quoted for readability, but need not be.
- **Flags.** `(dump-nanosail-ast)` turns a flag on; `(dump-nanosail-ast true)` and `(dump-nanosail-ast false)` set it.
- **Repetition.** Repeating an option adds to it: lists are combined, and for a single value (`base-name`) the last one wins.
- **Includes.** `(include <path>)` reads another configuration file at that point, as if its forms were written there.
  A relative path is relative to the directory of the file that includes it. A file that includes itself, directly or not, is an error.
- **Errors.** An unknown option, an unknown part of an option and a malformed value stop the translation with the file and line, such as `configuration.sexp:3: unknown option no-such-option`.

The options are read by the `Configuration` module.

### `templates`

```lisp
(templates base.template.v machine.template.v)
```

The translator processes each template and writes the result to the file named after it without `.template`: `machine.template.v` produces `machine.v`.
A template's name must contain `.template` exactly once. See [template files](#template-files).

### `base-name`

```lisp
(base-name MySailModelBase)
```

The name of the base module; the default is `UntitledBase`.

### `program-name`

```lisp
(program-name MySailModelProgram)
```

The name of the program module; the default is `ModelProgram`.

### `preserve-file-structure`

```lisp
(preserve-file-structure)
```

Gives every Sail file with functions its own Rocq file with the bodies of those functions, instead of putting all bodies in the output of `program-translation`.
Off by default.

Below, `base.v` stands for the output of the template with `base-translation` and `machine.v` for that of the template with `program-translation`;
their names come from `templates`. The translator writes:

| file | contents |
|---|---|
| `base.v` | unchanged: all types, constants and registers |
| `machine_decl.v` | named after `machine.v` (`model.v` gives `model_decl.v`); the module `ModelProgramDecl` (the program name followed by `Decl`): the declarations of the functions (`Fun`, `FunX`) and `Stm` |
| `Machine/dir/foo.v` | for each Sail file `dir/foo.sail` with functions: the bodies of its functions (`Definition fun_f`) |
| `machine.v` | the bodies of the functions of Sail's library, and the program module, which includes `ModelProgramDecl` and whose `FunDef` refers to the bodies, as in `Machine.dir.foo.fun_f` |

The declarations need a file of their own because of how Katamaran is built: every body needs `Fun`, which lists all functions, and `FunDef`, a match over `Fun`, needs all bodies.
The bodies only refer to each other through `Fun`, so the files in `Machine/` do not depend on each other.

`Machine/` mirrors the directories of the Sail files, relative to a root: by default the longest common directory of the model's files.
For CHERIoT, whose files are in `src/` and `sail-riscv/model/` of the cheriot-sail checkout, `src/cheri_insts.sail` becomes `Machine/src/cheri_insts.v`
and `sail-riscv/model/riscv_insts_base.sail` becomes `Machine/sail_riscv/model/riscv_insts_base.v`.
`preserve-file-structure-root` sets the root instead, relative to the directory in which Sail runs:

```lisp
(preserve-file-structure-root "../cheriot-sail")
```

`preserve-file-structure-directory` names the directory instead of `Machine` (default), which also changes the libraries' names, e.g. `Model.src.cheri_insts`:

```lisp
(preserve-file-structure-directory "Model")
```

It must be a name that Rocq allows: a letter followed by letters, digits, `_` and `'`; another name is an error.

The names of directories and files are changed where Rocq does not allow them: other characters than letters, digits, `_` and `'` become `_`
(`sail-riscv` becomes `sail_riscv`), and a name that does not start with a letter gets the prefix `sail_`.

* A function goes to the file of its clauses.
  The functions that Sail's `split` rewrite makes from the clauses of a scattered function (`execute_ADD`) go to the files of those clauses.
  A function whose clauses come from several files (the merged `execute`, the functions of a scattered mapping) goes to the file where Sail puts it, which is the file that declares it (`scattered function execute`).
* The functions that Sail generates for a bitfield `B` (`Mk_B`, `_get_B_F`, `_update_B_F`, ...) go to the file of `B`.
* The functions of Sail's library (the files included with `$include <...>`), and functions without a source location, go to `machine.v`.
* A Sail file without functions gets no file in `Machine/`.
  `.v` files in `Machine/` and its subdirectories that are no longer generated (for example, after renaming a Sail file) are removed; directories are not.
* `base.v` and `machine.v` must be in the same directory.
  A Sail file outside the root, two Sail files that become the same Rocq file (such as `foo-bar.sail` and `foo_bar.sail`),
  and a Sail file with the name of one of the generated files (`base`, `machine`, `machine_decl`) are errors:
  with `-R` (see below), `Require base` would then match both `base.v` and `Machine/base.v`.

Compile in dependency order: `base.v`, `machine_decl.v`, the files in `Machine/` (in any order, or in parallel), and `machine.v`.
`machine.v` Requires the files in `Machine/` as `Machine.dir.foo`, so the output directory must be on the load path, e.g.:

```bash
coqc -Q . "" base.v
coqc -Q . "" machine_decl.v
find Machine -name '*.v' | while read -r file; do coqc -Q . "" "$file"; done
coqc -Q . "" machine.v
```

The generated files `Require` each other by their names relative to the output directory (`base`, `machine_decl`, `Machine.dir.foo`).
So map the output directory to the empty prefix (`-Q . ""`, as above), or use `-R` with any prefix, which also finds libraries by these partial names;
`-Q` with a prefix does not work, since it only finds libraries by their full names.

### `include-original-code`

```lisp
(include-original-code)          ; on, same as (include-original-code true)
(include-original-code false)    ; off
```

This option is off by default.
Turning it on causes the generated code to be annotated
with the original Sail code.

For example,

```coq
(*
  Original Sail code
           
    val double : int -> int
    $[complete]
    function double x = $[overloaded { "name" = "*", "is_infix" = true }] mult_atom(2, x)
*)
Definition fun_double : Stm [
                              "x"  ∷  ty.int
                            ]
                            (ty.int) :=
  stm_exp (((exp_int 2%Z))*((exp_var "x"))).
```

### `pretty-print-function-calls`

```lisp
(pretty-print-function-calls)       ; on
(pretty-print-function-calls true)  ; on
(pretty-print-function-calls false) ; off
```

This option is off by default.
It determines how calls are printed: calls to Sail functions (`stm_call`) and calls to externs, which become foreign calls (`stm_foreign`).
On, they use Katamaran's `call` and `foreign` notations, which take the arguments one after another:

```coq
(call foo (exp_int 1%Z) (exp_false) (exp_val (ty.bvec 4) ([bv 1])))%exp
(foreign combine (exp_var "a") (exp_var "b"))%exp
(call baz)%exp                                      (* no arguments *)
```

Off, the arguments form an environment, a chain of `env.snoc`:

```coq
stm_call foo (env.snoc (env.snoc (env.snoc (env.nil)
                                           (_::_)
                                           ((exp_int 1%Z))%exp)
                                 (_::_)
                                 ((exp_false))%exp)
                       (_::_)
                       ((exp_val (ty.bvec 4) ([bv 1])))%exp)
stm_foreign combine (env.snoc (env.snoc (env.nil)
                                        (_::_)
                                        ((exp_var "a"))%exp)
                              (_::_)
                              ((exp_var "b"))%exp)
```

Both denote the same term: the notations stand for `stm_call` and `stm_foreign` with such an environment.

The notations only accept a name as the function, so two kinds of calls use `stm_call` and `stm_foreign` even when the option is on:

* calls to a function with a size variable that Rocq cannot infer and that is passed by name, such as `stm_call (store_bytes (kid__n := 4)) (env.snoc ...)`;
* the foreign calls that produce an unconstrained value, `stm_foreign (unconstrained (ty.int)) env.nil`.

### `annotate-functions-with-ast`

```lisp
(annotate-functions-with-ast)
```

Annotates functions with the nanosail AST.

```coq
    (*
      AST
        Def:Function[name=Identifier["foo"],
                     type=Def:FunctionType[parameters=[
                                                        Parameter[Identifier["x"],
                                                                  Type:Int[NumExpr:Var[Identifier["'n"]]]]
                                                      ],
                                           return_type=Type:Int[NumExpr:BinOp["Add",
                                                                              NumExpr:Var[Identifier["'n"]],
                                                                              NumExpr:Var[Identifier["'n"]]]]],
                     extended_type="TODO",
                     body=Stm:Expression[BinaryOp[Plus,
                                                  Var[Identifier["x"],
                                                      Type:Int[NumExpr:Var[Identifier["'n"]]]],
                                                  Var[Identifier["x"],
                                                      Type:Int[NumExpr:Var[Identifier["'n"]]]]]],
                     polymorphic=True,
                     monomorphs=[]]
      
      Extended type
        parameter x
          int($0)
        return value
          $0 + $0
    *)
    Definition fun_foo : Stm [
                               "x"  ∷  ty.int
                             ]
                             (ty.int) :=
      stm_exp (((exp_var "x"))+((exp_var "x"))).
```

### Other flags

- `(dump-nanosail-ast)`: prints each definition's nanosail AST to the log as soon as it is translated (large: about 80 MB for the CHERIoT core).
- `(pretty-print-lists)`, `(pretty-print-let)`, `(pretty-print-match-enum)`, `(pretty-print-binary-operators)`, `(pretty-print-function-calls)`: Rocq notations instead of plain constructors in the output; see the `Configuration` module for examples.
- `(literal-zeros-and-ones)`: `bv 0` instead of `Bitvector.bv.zero`, and likewise for ones.
- `(inline-definitions-in-notations false)`: on by default. Inlines definitions; added because Coq could not derive `NoConfusionHom` for `Reg` automatically (see `tests/coq-tests/no-confusion-hom`).
- `(pattern-tree-max-permutations 10000)`: the number of orderings tried when translating a `match`; see `SailToNanosail.Translate.Match`.
- `(template-block-delimiters "(*<" ">*)")`: the lines that start and end a template block.

### `monomorphize`

Consider the following Sail function:

```sail
val foo : forall 'n 'm. (bitvector('n), bitvector('m)) -> bitvector(4)
```

This function is polymorphic and should probably be monomorphized.
This can be achieved using

```lisp
(monomorphize
 (foo (name foo_8_8) (values ("'n" 8) ("'m" 8)))
 (foo (name foo_4_1) (values ("'n" 4) ("'m" 1)))
 (foo (name foo_1_2) (values ("'n" 1) ("'m" 2))))
```

This causes `foo` to be monomorphized three times into `foo_8_8`, `foo_4_1` and `foo_1_2`.
This feature is optional and has no tests at the moment; see [monomorphization](monomorphization.md).

For now, only type-level variables of kind `Int` can be specified.

### `union-as-sum`

Translates a union with two constructors to Microsail's built-in `ty.sum`, instead of to a `ty.union` of its own.
Because `ty.sum` is polymorphic, this also works for unions that are polymorphic in types,
which Microsail's `ty.union` cannot express.
All instances share the same representation: there is no need to monomorphize `option(int)` and `option(bits(3))` separately.

```lisp
(union-as-sum
 (option Some None)
 (result Ok Err))
```

Each entry names the union, the constructor that becomes `inl` and the constructor that becomes `inr`.
Because the constructors are named explicitly, unions whose names or constructor order
differ from Sail's `option` and `result` can be translated as well.

With the configuration above, Sail's

```sail
union option('a : Type) = { Some : 'a, None : unit }
union result('a : Type, 'b : Type) = { Ok : 'a, Err : 'b }
```

are translated as follows:

| Sail               | µSail                                                         |
|--------------------|---------------------------------------------------------------|
| `option(T)`        | `ty.sum T ty.unit`, i.e., `ty.option T`                       |
| `result(A, B)`     | `ty.sum A B`                                                  |
| `Some(x)`, `Ok(x)` | `exp_inl x`, printed as `exp_unop (uop.inl (σ2 := ...)) x`    |
| `None()`, `Err(x)` | `exp_inr` of `tt` or `x`, printed like `exp_inl`              |
| `match`            | `stm_match_sum`                                               |

Nested patterns inside the constructors (e.g., `Some(Foo(x))`) are not supported yet.
Functions that are polymorphic in types, such as `is_some : forall ('a : Type). option('a) -> bool`, are not supported by Microsail.
See the `tests/coq-tests/sum-unions-*` tests.

### Ignoring definitions

Ignored definitions are not translated; the log reports each one as skipped.
The option `ignore` selects them by name, in parts by kind of definition:

```lisp
(ignore
 (functions "undefined_*" "*_of_num" log2)   ; function definitions and their top-level type constraints
 (values initial_regstate)                    ; top-level let definitions
 (types regstate)                             ; type definitions
 (pragmas file_start file_end)                ; pragmas ($include_start, ...)
 (overloads))                                 ; all overloads: Sail resolves them
```

- **Patterns.** A name matches exactly, except that each `*` matches any text: `"undefined_*"` selects every name that starts with `undefined_`, `"*"` selects everything.
- **Exceptions.** `(except <names> ...)` among the names of a part exempts names from that part's patterns:

  ```lisp
  (ignore (functions "num_of_*" (except num_of_ExceptionType num_of_ext_exc_type)))
  ```

  An exception applies only to the patterns of the same part, in the same form: names that another form lists are still ignored.
- **Functions.** `functions` ignores both a function's definition and its top-level type constraint (its `val`).
  `function-definitions` and `type-constraints` ignore only one of the two.
- `(default-order false)` keeps Sail's `default Order` line, which is ignored by default.

Each form adds to the earlier ones.
The CHERIoT loop (`tests/cheriot/run.py`) uses `(ignore (functions ...) (values ...))` to ignore everything outside its scope.
See `tests/coq-tests/ignore-functions-by-name`.

### Renaming identifiers

A Sail definition named like a Katamaran definition clashes in Rocq (`write_register` is already declared by `DefaultRegStoreKit`).
There is no automatic renaming scheme: the clash fails in Rocq until the configuration renames the identifier.

```lisp
(rename
 (read_register read_register_model)
 (write_register write_register_model))
```

* Every occurrence of the identifier in the output is renamed: its definition and all its uses.
* The other options (`ignore`, `exclude`, ...) still take Sail's names.
* Renaming one identifier to two different names is an error; each form adds to the earlier ones.

See `tests/coq-tests/model-008`.

### Excluding definitions together with their uses

Ignoring a function only drops its definition: its callers still call it, so they refer to a function that `machine.v` does not define.
*Excluding* a definition drops it and also rewrites every use of it. The configuration says what a use becomes:

```lisp
(exclude
 (as-no-op print_reg trace_write)      ; a call becomes the unit value
 (as-failure translate32)              ; a call becomes stm_fail
 (as-unconstrained pmp_allows)         ; a call becomes an unconstrained value
 (as-value (pmp_check "None()"))       ; a call becomes the given Sail expression
 (registers pmpaddr0 pmpcfg_n))        ; left out of Reg
```

- **No-op**: each call becomes the unit value `stm_exp (exp_val ty.unit tt)`. Only for functions that return `unit`, such as printing and tracing.
  A call to such a function that has another type stops the translation with an error.
- **Failure**: each call becomes `stm_fail τ "f is excluded by the configuration"`, for code that must be unreachable.
  Whether a failure is harmless depends on Katamaran's `fail_rule_pre`: when it is false, a contract must show that the failure cannot happen.
- **Unconstrained**: each call becomes an unconstrained value of the call's type τ, `stm_foreign (unconstrained τ) env.nil`.
  The generated program declares one foreign function family, `unconstrained (σ : Ty) : FunX ctx.nil σ`, whose `ForeignCall` semantics allows every value of σ, never fails, and leaves registers and memory unchanged.
  A proof about a caller must therefore hold for every value the call could return: the value is universally quantified, as undefined values are.
  The result does not depend on the arguments, and two calls can return different values. The arguments are still computed.
  (`unconstrained` has an implicit argument, a value of σ, which Rocq finds by type class search. It only shows that σ has a value, which `ForeignProgress` needs; it plays no part in the semantics.)
- **Value**: each call becomes the given Sail expression, parsed, then type-checked at the type of the call in the call's environment, and translated as if it were written there.
  Arguments with an effect are still computed (`let excluded_call_arguments = <arguments> in <expression>`); the others are dropped.
  For a function whose result is known or chosen, such as a platform setting (`sys_pmp_count` is `0`) or a check that is assumed to pass (`pmpCheck` is `None()`).
  An expression that does not parse or type-check stops the translation with an error.
- **Registers**: an excluded register is left out of `Reg`. Each read of it becomes an unconstrained value of its type, as above, and each write a no-op (the written value is still computed).

An excluded function's definition and its `val` specification are not translated, as with `(ignore (functions ...))`.
The names are Sail's names. Each form adds to the earlier ones; excluding the same function in two different ways is an error.
The CHERIoT loop generates these forms from the `[policy-excluded]` table of `tests/cheriot/scope.toml` ([loop](../case-studies/cheriot/docs/loop.md#policy-exclusions)).
See the `tests/coq-tests/exclude-*` tests. `as-value` takes pairs of the function's name and the expression.

### Externs: muSail operations and foreign functions

A Sail *extern* is a `val` specification with an extern binding and no Sail body, such as
`val emod_int = pure {ocaml: "modulus", …, _: "emod_int"} : (int, int) -> int`.
A call to an extern becomes one of two things:

- **A muSail operation**, when the extern corresponds to one and the call's arguments allow it.
  The translator recognises an extern by its *binding key*: its binding for all targets (`_`), or else its binding for C (`c`), or else its Sail name.
  So sail-riscv's `val shiftl = pure {_: "shiftl"}` is recognised as Sail's `sail_shiftleft`.

  | binding key | becomes | condition |
  |---|---|---|
  | `shiftl`, `shiftr` | `bop.shiftl`, `bop.shiftr` (the amount converted as for `sail_shiftleft`) | amount or width known at compile time |
  | `shift_bits_left`, `shift_bits_right` | `bop.shiftl`, `bop.shiftr` | |
  | `sub_vec` | `bop.bvsub` | |
  | `sub_vec_int` | `bop.bvsub` (the integer converted as for `add_bits_int`) | |
  | `eq_anything` | `bop.relop bop.eq` | |
  | `vector_access` on bits | `uop.vector_subrange i 1` | index and width known at compile time |
  | `get_slice_int` | `uop.get_slice_int (n := w)` | width known, start 0 |
  | `sail_truncateLSB` | `uop.vector_subrange (n - m) m` | both widths known |

  Sail-named builtins that were already translated as operations (`sail_zero_extend`, `truncate`, `sail_shiftleft`, …) become foreign calls
  where they used to stay a call to an undefined function or become a hole (for example a width that is not known at compile time).
- **A foreign function** otherwise: a constructor of `FunX` with the extern's signature, and each call a `stm_foreign`:

  ```coq
  | popcount {kid__n : nat} : FunX [ "arg1" ∷ ty.bvec (kid__n) ] (ty.int)
  ...
  stm_foreign popcount (env.snoc env.nil (_::_) (exp_var "w"))
  ```

  The parameters are named `arg1`, `arg2`, …; a `unit` parameter stays, as for functions.
  The type-level integers of bitvector widths (and type variables) become implicit arguments, which Rocq infers at the call.
  Only the externs that some call is translated into a foreign call to get a constructor.
  An extern whose signature muSail cannot express (vectors, type applications, a result of a type variable's type) keeps the old translation, a `stm_call` to a function that `machine.v` does not define.

The semantics of a foreign function (`ForeignCall`, in `machine.v`) never fails and never changes the registers.
A `pure` extern leaves the memory unchanged; a `monadic` one may change it in any way.
Its result is either given by a Rocq term, or else *unconstrained*: any value of the result type, so that a proof about a caller must hold for every value the call could return (the value is universally quantified, as undefined values are).
Two calls to an unconstrained foreign function can return different values, even without arguments: a platform setting such as `plat_ram_base` is not a fixed constant unless the configuration gives it a term.

```lisp
(foreign-functions
 (semantics (gcd_int "Z.gcd arg1 arg2")
            (plat_ram_base "bv.of_Z 0x80000000"))
 (unconstrained pow2))
```

- `semantics` gives the result as a Rocq term, the body of
  `Definition semantics_<name> {kid__n : nat} (arg1 : Val σ1) … : Val τ := <term>.`
  The term refers to the arguments as `arg1`, `arg2`, …, to the type-level integers as `kid__n` (for `'n`), and can use `bv_width x`, the width of the bitvector `x`.
- `unconstrained` makes the result unconstrained, also when the translator has a default term.

Externs of Sail's library with a well-known meaning have a default term (by binding key; see `default_semantics` in `nanosail/NanosailToMicrosail/ForeignFunctions.ml`):
`emod_int`, `ediv_int` (Euclidean), `tdiv_int`, `tmod_int` (rounding towards zero), `pow2`, `min_int`, `max_int`, `abs_int`, `length` (the width), `count_leading_zeros`, `count_trailing_zeros`,
and, for the calls that do not become operations, `zeros` (`sail_zeros`), `shiftl`, `shiftr`, `get_slice_int`, `sail_truncate`, `sail_truncateLSB`, `zero_extend`, `sign_extend` and `vector_access`,
and the slices and updates whose indices are known only at run time (`v[i + 7 .. i]`, `[v with i + 7 .. i = x]`, `[v with i = b]` in a loop): `vector_subrange`, `vector_update_subrange`, `vector_update`.
All other externs (platform settings, memory and tag accesses, `cancel_reservation`, …) are unconstrained.
No foreign function reads the memory, so its contents cannot be observed; a term that reads it would need the writes to have a semantics too.
The names are Sail's names; giving a function two different semantics is an error.
See the tests `tests/coq-tests/externs-foreign-functions` and `externs-operations`.

### Functions polymorphic in a width or a type

A Sail function that is polymorphic in the width of a bitvector, or in a type, becomes one `Fun` constructor with the widths and types as implicit arguments,
in order of appearance in its signature, and one definition for all of them:

```coq
| extend_to {kid__n : nat} {kid__m : nat} : Fun [ "m" ∷ ty.int; "v" ∷ ty.bvec (kid__n) ] (ty.bvec (kid__m))
Definition fun_extend_to {kid__n : nat} {kid__m : nat} : Stm [ ... ] (ty.bvec (kid__m)) := ...
```

This is what Katamaran's RiscvPmp case study does by hand (`read_ram (bytes : nat)`); a contract can be proved once for all widths.
Rocq infers the arguments at each call. A width that only appears in an integer type (`int('n)`) needs none: `ty.int` has no index.

Sail's constraints on the widths become one implicit hypothesis, a boolean over `Z` under Katamaran's `IsTrue`:

```coq
| store_bytes {kid__p8_times_n : nat} {kid__n : nat}
    {H : IsTrue ((2 <=? Z.of_nat kid__n) && (Z.of_nat kid__n <=? 8) && (Z.of_nat kid__p8_times_n =? 8 * Z.of_nat kid__n))%Z}
    : Fun [ "width" ∷ ty.int; "data" ∷ ty.bvec (kid__p8_times_n) ] (ty.unit)
```

- Only the conjuncts that mention a width; their other size variables (`kid__n`, only in `int('n)`) become parameters too.
- Rocq proves the hypothesis at each call by type class search: Katamaran's hint computes it at concrete widths; the generated hint `sail_constraint` proves it with `lia` from the hypotheses in scope. It proves side conditions in the body the same way (a slice `data[15 .. 0]`).
- A size variable that Rocq cannot infer is passed by name, from the argument of type `int('n)`: `stm_call (store_bytes (kid__n := 4)) ...`.
- In a branch where Sail's flow typing fixes a size variable (`if width == 4`), the branch is translated at that value:
  a `match` on `PeanoNat.Nat.eq_dec kid__n 4` gives the equality, each variable whose type mentions it is rebound with `sail_cast`, and the other arm (unreachable for Sail) is `stm_fail`.
  A branch fact that fixes no value (`if width <= 8`) is decided with `sail_decide`, so that `lia` can use it.
- A let-bound width that Sail's solver proves equal to a size variable (`8 * 'n`, `8 * ('p / 8)` with `'p == 8 * 'n`) is that variable.

Still, inside such a function:

- an extension or truncation of a bitvector whose width is a size variable is a foreign call to the extern (`sail_zero_extend`, …);
- a width that depends on the *value* of a parameter (`val f : forall 'n. (itself('n), …)` with `bits('n)` in its body) or that divides a size variable (`bits(8 * ('n / 8))`) is a hole (`NYI`), with a message that says so;
- a call to a function whose result type alone mentions a type variable (`internal_error : … -> 'a`), whose value is discarded, gets the type `unit`.

A union with a type parameter used at a bitvector of open width (`MemoryOpResult(bits('n))`) is one family indexed by the width,
`Inductive MemoryOpResult_bits (kid__n : nat)` with the tag `Umemoryopresult_bits (kid__n : nat)`; its uses at closed widths are the family at that width.
A function polymorphic in a type that such a union takes (`forall ('t : Type). MemoryOpResult('t) -> ...`) is copied per type it is called at when there are one or two (a single copy keeps its name), and inlined at each call otherwise.
See the test `tests/coq-tests/unions-width-family`.

A struct with one numeric parameter is a record family indexed by it, as a union width family:

```sail
struct S('m) = { value : bits(8 * 'm), tag : bool }
```
```coq
Record S (kid__m : nat) : Set := MkS { S_value : bv (8 * kid__m); S_tag : bool }.
| Rs (kid__m : nat)                                (* in Records; EqDec via N * nat *)
ty.record (Rs 4)                                   (* S(4) *)
exp_record (Rs kid__n) [...]                       (* construction gives the index *)
stm_match_record (Rs _) ...                        (* field access: Rocq infers it *)
```

- Constructor and projections take the index implicitly (`MkS v t`, `S_value r`).
- A struct with several parameters, or a type parameter, is not a family.
- An alias with parameters is expanded: `mybits(8 * 'm)` is `bits(8 * 'm)`.
- Not yet: a field whose width Sail renamed in a signature (`bits(8 * 'n)` becomes `bits('p8_times_n)` by Sail's `toplevel_nexps`); Rocq sees `bv p` against `bv (8 * n)` and would need a cast.

See the tests `tests/coq-tests/records-width-family` and `tests/coq-tests/bitvectors-variables`.

Explicit monomorphisation (`monomorphize`) is unaffected.
See the tests `tests/coq-tests/size-parameters-implicit` and `tests/coq-tests/size-constraints-hypotheses`.

### Exceptions

- `throw e` is `stm_fail τ "failure"`: µSail has no exceptions.
- `try { e } catch { ... }` is the translation of `e`; the handlers are dropped (one warning per `try`).
  A `throw` inside `e` fails instead of being caught.

```sail
function foo(x) = try check(x) catch { Error_bad() => 0 }
```
```coq
Definition fun_foo : Stm [ "x" ∷ ty.int ] (ty.int) :=
  stm_call check (env.snoc (env.nil) (_::_) ((exp_var "x"))%exp).
```

See the test `tests/coq-tests/try-catch-dropped`.

### Loops

A loop (`foreach`, `while`, `repeat`) becomes a call to a *helper function* that runs it by recursion; the loop is not unrolled.

```sail
function popcount(v : bits(8)) -> int = {
  count : int = 0;
  foreach (i from 0 to 7)
    if v[i] == bitone then count = count + 1;
  count
}
```
```coq
| popcount_loop : Fun [ "i" ∷ ty.int; "жbound0" ∷ ty.int; "v" ∷ ty.bvec 8; "count" ∷ ty.int ] (ty.int)

Definition fun_popcount_loop : Stm [...] (ty.int) :=
  stm_if (exp_var "i" <= exp_var "жbound0")
    (stm_seq (* the body: if v[i] == bitone then count = count + 1 *)
             (stm_call popcount_loop [exp_var "i" + exp_int 1; exp_var "жbound0"; exp_var "v"; exp_var "count"]))
    (stm_exp (exp_var "count")).

(* in fun_popcount *)
stm_let "жstart4" ty.int (stm_exp (exp_int 0))
  (stm_let "жbound0" ty.int (stm_exp (exp_int 7))
     (stm_assign "count" (stm_call popcount_loop [exp_var "жstart4"; exp_var "жbound0"; exp_var "v"; exp_var "count"]))) ;;
stm_exp (exp_var "count")
```

- The helper is named after its function: `f_loop`, or `f_loop1`, `f_loop2`, … if `f` has several loops; it precedes `f`.
- Its parameters are the local variables the loop uses without binding them: first those it only reads, then those it assigns.
  A `foreach` helper takes the loop variable, the bound and the step (unless it is a literal) first.
- Its result is the assigned variables: `unit`, the value, or a tuple (`ty.prod` for two); the call assigns them back.
  The body assigns the helper's own parameters.
- `foreach`: counts up while `i <= bound`, or down (`downto`) while `i >= bound`, by the step; the bounds and the step are evaluated once, before the loop, as in Sail.
- `while c do body`: `if c then (body; recurse) else result`. `repeat body until c`: `body; if c then result else recurse`.
- A helper takes the widths of its parameters' types implicitly, and the constraints of its function that are over them.
- Why a function and not unrolling: a proof gives the helper one contract, the loop invariant, and symbolic execution uses it at the call
  instead of executing every iteration (an unrolled `foreach` over 32 bits whose body branches has up to 2^32 paths).

See the tests `tests/coq-tests/loops-*`.

## Template Files

The option [`templates`](#templates) names the template files; each one produces an output file.

A possible template file would be

```text
O tempora

(*<
  "some generated text"
>*)

O mores
```

After processing, the corresponding output file will contain

```text
O tempora

some generated text

O mores
```

Template files are processed line by line.
Lines outside a block are copied to the output file; each block is replaced by what it generates, followed by a newline.
A block starts with a line `(*<` and ends with a line `>*)`, which are Rocq comments, so that a template is a valid Rocq file
(the option `template-block-delimiters` changes them).

A block contains S-expressions, as in the configuration file, and generates their texts one after the other:

* an output name, replaced by that output:
  * `base-translation`, `program-translation`: the translation of the base module and of the program module;
  * `base-html-translation`, `program-html-translation`: the same, as HTML ([HTML generation](./html-generation.md));
  * `untranslated-definitions`: the untranslated definitions (their Sail source and locations), escaped so that they are valid inside a Rocq comment;
  * `ignored-definitions`: the ignored definitions;
  * `argument-types-of-polymorphic-function-calls`: see [monomorphization](./monomorphization.md);
* a string, copied as it is;
* `(when untranslated-definitions <contents> ...)`: the contents if there are untranslated definitions, else nothing.

The templates of the tests and case studies, such as `case-studies/cheriot/base.template.v`, are

```coq
(*<
  base-translation
>*)

(*<
  (when untranslated-definitions "(* UNTRANSLATED_DEFINITIONS: the Sail definitions below have no translation *)")
>*)
```

The outputs and conditions are defined in `Templates.Outputs`; a finer-grained output needs a new name there.

## Verbosity Level

The verbosity level can be set using the environment variable `VERBOSE`:

```bash
$ VERBOSE=0 sail ...
```

* `VERBOSE=0`: quiet mode. Suppresses all logging.
* `VERBOSE=1`: only errors are shown.
* `VERBOSE=2`: only errors and warnings are shown. This is the default.
* `VERBOSE=3`
* `VERBOSE=4`: maximum level.

See `LogLib.VerbosityLevel` for more detailed information.

## HTML Generation

See [dedicated page](./html-generation.md).
