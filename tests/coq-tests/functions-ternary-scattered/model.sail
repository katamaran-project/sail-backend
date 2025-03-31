default Order dec

$include <prelude.sail>


enum MyEnum = { XXX, YYY }


val scatfoo : (MyEnum, int) -> int
scattered function scatfoo

function clause scatfoo(XXX, x) = x
function clause scatfoo(YYY, x) = x + 1



val mergedfoo : (MyEnum, int) -> int
function mergedfoo(x, y) = {
  match x {
    XXX => y,
    YYY => y + 1
  }
}


val bar1 : unit -> int
function bar1() = scatfoo(XXX, 1)

val bar2 : unit -> int
function bar2() = mergedfoo(XXX, 1)