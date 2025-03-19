# Notes on Sail

Below are a few notes regarding Sail and its inner working.

## Atoms, `itself` and `Int`



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

For example,

```sail
register r1 : int
register r2 : int

val bar : unit -> int
function bar () =
  foo(r1) + foo(r2)
```

becomes

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

During

## Type of Assignment

## Generated Ast Module
