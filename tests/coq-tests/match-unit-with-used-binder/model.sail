val foo : unit -> int
function foo x = 0

val bar : unit -> int
function bar(x) = match x {
  y => foo(y)
}
