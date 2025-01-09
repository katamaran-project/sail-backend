val f1 : list(int) -> int
function f1(l) = match l {
  [||]   => 0,
  x :: y => 1
}

val f2 : list(int) -> int
function f2(l) = match l {
  x :: y => 1,
  [||]   => 0,
}

val f3 : list(int) -> int
function f3(l) = match l {
  [||]   => 0,
  [|x|]  => 1,
  x :: y => 2,
}

val f4 : list(int) -> int
function f4(l) = match l {
  [|x|]  => 1,
  [||]   => 0,
  x :: y => 2,
}

val f5 : list(int) -> int
function f5(l) = match l {
  [||]        => 0,
  [|x|]       => 1,
  x :: y :: z => 2,
}

val f6 : list(int) -> int
function f6(l) = match l {
  x :: y :: z => 2,
  [|x|]       => 1,
  [||]        => 0,
}

val f7 : list(int) -> int
function f7(l) = match l {
  [||]   => 0,
  [| _ |] => 1,
  [| _, _ |] => 2,
  [| _, _, _ |] => 3,
  [| _, _, _, _ |] => 4,
  _ => 5,
}
