# Monomorphization

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
[WARNING] nanosail/SailToNanosail/Translate/Function.ml:776
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
[WARNING] nanosail/SailToNanosail/Translate/Function.ml:776
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
[WARNING] nanosail/SailToNanosail/Translate/Function.ml:776
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
An easier way to get an overview of which polymorphic calls have been made is to rely on the `argument-types-of-polymorphic-function-calls` function which is usable in templates.

Create a file `polymorphic.template.txt` with as content

```text
(*<
  (generate (argument-types-of-polymorphic-function-calls))
>*)
```

Register it as a template in the `configuration.lisp` file:

```lisp
(template "polymorphic.template.txt")
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

To have monomorphizations generated, add the following code to `configuration.lisp`:

```lisp
(monomorphize "foo" "foo_8_8" '(
                                ("'n" 8)
                                ("'m" 8)
                                ))

(monomorphize "foo" "foo_4_1" '(
                                ("'n" 4)
                                ("'m" 1)
                                ))

(monomorphize "foo" "foo_1_2" '(
                                ("'n" 1)
                                ("'m" 2)
                                ))
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

When no monomorphizations have been requested for a polymorphic function `f`,
`f` will be translated into Coq as-is (albeit not correctly).
When N monomorphizations have been requested (with `N > 0`), no translation for `f` itself
will be generated; instead, you'll get `N` monomorphic functions.


TODO: complication with c=a+b
