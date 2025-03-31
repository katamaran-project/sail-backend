default Order dec

$include <prelude.sail>


enum MyEnum = { XXX, YYY }


val foo : (MyEnum, int) -> int
scattered function foo

function clause foo(XXX, x) = x
function clause foo(YYY, x) = x + 1


val bar : unit -> int
function bar() = foo(XXX, 1)