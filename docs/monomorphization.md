# Monomorphization

- Optional feature: one copy of a polymorphic function per requested width.
- The CHERIoT case study does not use it: size-polymorphic functions are translated once, with implicit `nat` widths ([usage](usage.md#functions-polymorphic-in-a-width-or-a-type)).

## Working Example

Consider the following polymorphic Sail function:

```sail
val foo : forall 'n 'm. (bitvector('n), bitvector('m)) -> bitvector(4)
function foo(xs, ys) = {
  0x0
}
```

Say it is called by multiple other functions:

```sail
val bar : unit -> bitvector(4)
function bar() = foo(0b0, 0b00)


val baz : unit -> bitvector(4)
function baz(x) = foo(0b0000, 0b0)


val qux : unit -> bitvector(4)
function qux(x) = foo(0x12, 0x23)
```

## Logging Output

When translated as-is, the backend will emit warnings about calls to a polymorphic function:

```text
[WARNING] nanosail/SailToNanosail/Translate/Function.ml:869
          Call to polymorphic function detected
          Function name
            foo
          Location
            GeneratedLocation(model.sail:11)
          Type quantifier
            TypeQuantifier[[
                             Identifier["'n"],
                             Kind:Int
                           ],
                           [
                             Identifier["'m"],
                             Kind:Int
                           ]]
          Parameter types
            Type:Function[parameter_types=[
                                            Type:Bitvector[NumExpr:Var[Identifier["'n"]]],
                                            Type:Bitvector[NumExpr:Var[Identifier["'m"]]]
                                          ],
                          result_type=Type:Bitvector[4]]
          Argument types
              1: Type:Bitvector[1]
              2: Type:Bitvector[2]
[WARNING] nanosail/SailToNanosail/Translate/Function.ml:869
          Call to polymorphic function detected
          Function name
            foo
          Location
            GeneratedLocation(model.sail:15)
          Type quantifier
            TypeQuantifier[[
                             Identifier["'n"],
                             Kind:Int
                           ],
                           [
                             Identifier["'m"],
                             Kind:Int
                           ]]
          Parameter types
            Type:Function[parameter_types=[
                                            Type:Bitvector[NumExpr:Var[Identifier["'n"]]],
                                            Type:Bitvector[NumExpr:Var[Identifier["'m"]]]
                                          ],
                          result_type=Type:Bitvector[4]]
          Argument types
              1: Type:Bitvector[4]
              2: Type:Bitvector[1]
[WARNING] nanosail/SailToNanosail/Translate/Function.ml:869
          Call to polymorphic function detected
          Function name
            foo
          Location
            GeneratedLocation(model.sail:19)
          Type quantifier
            TypeQuantifier[[
                             Identifier["'n"],
                             Kind:Int
                           ],
                           [
                             Identifier["'m"],
                             Kind:Int
                           ]]
          Parameter types
            Type:Function[parameter_types=[
                                            Type:Bitvector[NumExpr:Var[Identifier["'n"]]],
                                            Type:Bitvector[NumExpr:Var[Identifier["'m"]]]
                                          ],
                          result_type=Type:Bitvector[4]]
          Argument types
              1: Type:Bitvector[8]
              2: Type:Bitvector[8]

```

The warnings give an indication of which monomorphizations are necessary, namely (using C++ notation)

* `foo<1, 2>`
* `foo<4, 1>`
* `foo<8, 8>`

## Summary Generation

The output from the logs, while complete, can be overwhelming, especially in cases where there are many calls to polymorphic functions.
An easier way to get an overview of which polymorphic calls have been made is the template output `argument-types-of-polymorphic-function-calls` ([template files](usage.md#template-files)).

Create a file `polymorphic.template.txt` with as content

```text
(*<
  argument-types-of-polymorphic-function-calls
>*)
```

Add it to the templates in the configuration file:

```lisp
(templates polymorphic.template.txt)
```

Then run the translator, which will then generate a file `polymorphic.txt` with the following contents:

```text
foo
  1: Argument types
       arg #1: ty.bvec (8)
       arg #2: ty.bvec (8)
  2: Argument types
       arg #1: ty.bvec (4)
       arg #2: ty.bvec (1)
  3: Argument types
       arg #1: ty.bvec (1)
       arg #2: ty.bvec (2)
```

## Monomorphization

To have monomorphizations generated, add the following to the configuration file:

```lisp
(monomorphize
 (foo (name foo_8_8) (values ("'n" 8) ("'m" 8)))
 (foo (name foo_4_1) (values ("'n" 4) ("'m" 1)))
 (foo (name foo_1_2) (values ("'n" 1) ("'m" 2))))
```

This causes the following Coq code to be generated:

```coq
Definition fun_foo_1_2 : Stm [
                               "xs"  ∷  ty.bvec (1);
                               "ys"  ∷  ty.bvec (2)
                             ]
                             (ty.bvec (4)) :=
  stm_exp (exp_val (ty.bvec 4) ([bv 0])).


Definition fun_foo_4_1 : Stm [
                               "xs"  ∷  ty.bvec (4);
                               "ys"  ∷  ty.bvec (1)
                             ]
                             (ty.bvec (4)) :=
  stm_exp (exp_val (ty.bvec 4) ([bv 0])).


Definition fun_foo_8_8 : Stm [
                               "xs"  ∷  ty.bvec (8);
                               "ys"  ∷  ty.bvec (8)
                             ]
                             (ty.bvec (4)) :=
  stm_exp (exp_val (ty.bvec 4) ([bv 0])).
```

**Note**
If no monomorphizations are requested, a polymorphic function will be translated
like any other function, but this leads to invalid Coq code.
If at least one monomorphization is requested, the polymorphic function
itself will be skipped and only the "monomorphs" will be translated.

## Examples

There are no tests for this feature any more.
The `tests/coq-tests/monomorphize-*` tests failed since the move to Sail 0.20.3 (the requests name size variables `'n`, while the translator now names them `kid__n`) and were removed; they are in the git history.

## Missing Features

### Dealing with Numeric Constraints

Consider the following Sail code:

```sail
val foo : forall 'n 'm, 'n > 0 & 'm > 0. (bitvector('n), bitvector('m), int('n + 'm)) -> bitvector('n + 'm)
```

It gets rewritten by Sail into

```sail
val foo : forall 'n 'm 'n_plus_m, ('n > 0 & 'm > 0 & 'n_plus_m == 'n + 'm). (bitvector('n), bitvector('m), itself('n_plus_m)) -> bitvector('n_plus_m)
```

In other words, an extra type level variable `'n_plus_m` was introduced and a new constraint relates it to `'n` and `'m`.

The transformation from 2 to 3+constraint takes place in the `toplevel_nexps` Sail rewrite.
(To see the effects of each rewrite, run an e2e test with such a function through `tests/coq-tests/run-tests.py --test <name>`
and look at `rewrite-NN-toplevel_nexps.sail` in `tests/coq-tests/test-output/latest/<name>/`.)
A simple possible solution would be to disable this rewrite, but it could also complicate other things.
See the [Sail note on rewrites](./sail-notes.md) for information about disabling rewrites.

### `vector`

There is limited support for Sail's `vector`s:

* Vectors exist in nanosail's type system (`Ast.Type.Vector`).
* Sail to nanosail translation recognizes them.
* The nanosail to muSail translation emits a NYI when it encounters vectors.
* The monomorphization code does only deal with numeric expressions, not types.

### Automatic Monomorphizations

Instead of having to explicitly request monomorphizations in the configuration file,
we could automatically monomorphize polymorphic functions whenever a call to such a function is encountered.
