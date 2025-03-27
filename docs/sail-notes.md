# Notes on Sail

Below are a few notes regarding Sail and its inner working and its differences with muSail.

## Singleton types

```sail
val index : forall 'n 'i, 0 <= 'i & 'i < 'n. (bits('n), int('i)) -> bit
function index(bv, idx) = bv[idx]
```
`'i` is a type-level variable with kind `Int`.
Its value is required at runtime, so the function `index` has a parameter `idx`.
To force with parameter to receive the same value as `'i`,
`idx` is given the type `int('i)`, which is a singleton type:
its only inhabitant is the value `'i` itself.

Internally, Sail uses the name `itself('i)` instead of `int('i)`.
In nanosail, we represent these types using `Ast.Type.Int`.

For example, the code above gets translated into the nanosail ASTs shown below:

```text
Def:TopLevelTypeConstraint[Identifier["index"],
                           type_quantifier=TypeQuantifier[[
                                                            Identifier["'n"],
                                                            Kind:Int
                                                          ],
                                                          [
                                                            Identifier["'i"],
                                                            Kind:Int
                                                          ]],
                           type=Type:Function[parameter_types=[
                                                                Type:Bitvector[NumExpr:Var[Identifier["'n"]]],
                                                                Type:Int[NumExpr:Var[Identifier["'i"]]]
                                                              ],
                                              result_type=Type:Bit],
                           polymorphic=True,
                           monomorphs=[]]

Def:Function[name=Identifier["index"],
             type=Def:FunctionType[parameters=[
                                                Parameter[Identifier["bv"],
                                                          Type:Bitvector[NumExpr:Var[Identifier["'n"]]]],
                                                Parameter[Identifier["idx"],
                                                          Type:Int[NumExpr:Var[Identifier["'i"]]]]
                                              ],
                                   return_type=Type:Bit],
             extended_type="TODO",
             body=Stm:Call[function_id=Identifier["bitvector_access"],
                           arguments=[
                                       Var[Identifier["bv"],
                                           Type:Bitvector[NumExpr:Var[Identifier["'n"]]]],
                                       Var[Identifier["idx"],
                                           Type:Int[NumExpr:Var[Identifier["'i"]]]]
                                     ]],
             polymorphic=True,
             monomorphs=[]]
```

## Atoms

Sail supports refinement types.
When an `int` needs to be accompanied by a predicate constraining the values it can take on,
the type `atom` is used, which expects a type argument (see `Libsail.Ast.typ_arg` and `Ast.TypeArgument.t`)
that represents the constraint.

## `exit()` Return Type

Typically, all branches of execution are expected to yield a value of the same type.
However, there seems to be an exception to this rule in Sail:
the `exit` function returns `unit`, and is allowed to be used in a context where another type is expected.
This is of course sound since `exit` never returns, but it is a corner case that requires specialized handling.

## `Ast.Type.Implicit`

```sail
val bvlen : forall 'n, 'n >= 0. (implicit('n), bits('n)) -> int('n)
function bvlen(r, _) = r
```

`implicit` allows arguments to be omitted when calling a function.
The function shown above uses `implicit` to determine the size of a bitvector.

`implicit` is not important for the translation to muSail, but nanosail nonetheless includes it in its AST.
The reason its inclusion is laziness: adding it required the fewest changes to existing code.
A possible improvement would be to remove it and adapt the functions related to it.

## Rewrites

Sail performs a number of rewrites on the program's AST before handing it over to the muSail backend.
It is possible to have these rewrites written to disk using Sail's `--dump-rewrite-ast` flag.
We also have a Ruby script `process-rewrites.rb` that generates diffs of consecutive rewrites,
which can be helpful when trying to find out what each rewrite does.

Changing which rewrites are active is done by editing `Rewrite.ml` in the `sail_plugin` project.

## Expressions vs Statements

Sail does not make a distinction between expressions and statements, whereas muSail does:

* Reading a register in Sail is no different from reading a variable, but is its own statement in muSail.
* Function calls are statements in muSail.
* `let x = s1 in s2` is a statement, with `s1` and `s2` also statements.
* ...

For example,

```sail
register r1 : int
register r2 : int

val bar : unit -> int
function bar () =
  foo(r1) + foo(r2)
```

needs to become

```ocaml
# Pseudocode
function bar () =
  let gen1 = 
    let gen2 = read_register r1
    in
    foo(gen2)
  in
  let gen3 =
    let gen4 = read_register r2
    in
    foo(gen4)
  in
  gen1 + gen3
```

or, in actual muSail,

```coq
Definition fun_bar : Stm [
                           "ж_0"  ∷  ty.unit
                         ]
                         (ty.int) :=
  let: "ga#0" :: ty.int := let: "жreg_r1_1" :: ty.int := stm_read_register r1
                           in
                             (call foo (exp_var "жreg_r1_1"))%exp
  in
    let: "ga#1" :: ty.int := let: "жreg_r2_2" :: ty.int := stm_read_register r2
                             in
                               (call foo (exp_var "жreg_r2_2"))%exp
    in
      stm_exp (((exp_var "ga#0"))+((exp_var "ga#1"))).
```

During the translation of Sail to muSail, we heavily rely
on `let` constructs as well as muSail's `stm_exp`,
which lifts expression up into the statement world.

The translation of expressions to statements 

## Type of Register Write

```sail
register r1 : int = 0

val foo : int -> unit
function foo(x) = {
  r1 = x
}
```

In Sail, the assignment `r1 = x` has type `unit`, whereas
in muSail, its equivalent would be typed `int`.
For this reason, we translate the above code to

```pseudo
r1 = x; ()
```

or

```coq
stm_seq (stm_write_register r1 (exp_var "x"))
        (stm_exp (exp_val (ty.unit) (tt))).
```

## Generated Ast Module

## Full Type of Functions

To be found in top level type constraints, but apparently not in functions themselves

## Functions and Bindings

```sail
val foo : int -> int
function foo(x) = x


val bar : int -> int
function bar(x) = {
  let f = foo             // <-- error
  in
  f(x)
}
```

The code above gets rejected: Sail complains `foo` is unbound.
This seems to imply that, like in a Lisp-2, there are two separate namespaces at play.
However, this is contradicted by

```sail
val foo : int -> int
function foo(x) = x


val bar : int -> int
function bar(x) = {
  let foo = 5            // <-- error
  in
  foo(foo)
}
```

Here, Sail complains that `foo` is already bound.

## Registers

In Sail, registers are used the same way as regular variables:

```sail
register r1 : int

val foo : int -> int
function foo(x) = r1 + x
```

In muSail, registers are special objects that need to be interacted
with using specialized commands `stm_read_register` and `stm_write_register`.
During translation it is therefore important to check
whether an identifier is bound to a register or not.

## Shadowing Registers

``` sail
register r1 : int

val foo : int -> int
function foo(x) = {
  let r1 = x            // <-- error
  in
  r1
}
```

```sail
register r1 : int

val foo : int -> int
function foo(r1) = {     // <-- error
  r1
}
```

Sail does not allow to introduce variable names that clash with registers.
This restriction makes it easier to distinguish regular variables from registers.
