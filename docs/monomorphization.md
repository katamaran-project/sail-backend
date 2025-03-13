# Monomorphization

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
              
[analogously for baz and qux]
```

TODO: complication with c=a+b
