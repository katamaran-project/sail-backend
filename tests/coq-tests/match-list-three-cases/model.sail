default Order dec

$include <prelude.sail>


val f1 : list(int) -> int
function f1(l) = match l {
  [||]        => 0,
  [|x|]       => x,
  x :: y :: z => x+y,
}

val f2 : list(int) -> int
function f2(l) = match l {
  x :: y :: z => x+y,
  [|x|]       => x,
  [||]        => 0,
}

val f3 : list(int) -> int
function f3(l) = match l {
  x :: y :: z => x+y,
  [||]        => 0,
  [|x|]       => x,
}

val f4 : list(int) -> int
function f4(l) = match l {
  [|x|]       => x,
  [||]        => 0,
  x :: y :: z => x+y,
}

