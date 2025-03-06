default Order dec

$include <prelude.sail>


val foo2 : unit -> list(int)
function foo2() = {
  [| 1, 2 |]
}

val foo3 : unit -> list(int)
function foo3() = {
  [| 1, 2, 3 |]
}

val foo4 : unit -> list(int)
function foo4() = {
  [| 1, 2, 3, 4 |]
}

val foo10 : unit -> list(int)
function foo10() = {
  [| 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 |]
}
