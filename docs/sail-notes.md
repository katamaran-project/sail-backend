# Notes on Sail

Below are a few notes regarding Sail and its inner working.

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

## `Ast.Type.Implicit`

## Rewrites

TODO DUMP
TODO where to enable/disable

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

## Type of Assignment

## Generated Ast Module

## Full Type of Functions

To be found in top level type constraints, but apparently not in functions themselves

## Implicit
