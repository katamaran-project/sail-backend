val foo : (int, bool, bitvector(4)) -> int
function foo(x, y, z) = {
  x
}


val bar : unit -> int
function bar() = {
  foo(1, false, 0x1)
}