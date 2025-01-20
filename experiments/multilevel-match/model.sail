default Order dec

$include <prelude.sail>


union A = {
  A1 : (int, int),
  A2 : (int, int),
}

union B = {
  B1 : A,
  B2 : A,
  B3 : int,
}

union C = {
  C1 : B,
  C2 : A,
  C3 : A,
}

val foo : C -> string
function foo(x) = match x {
  C1(B1(A1(a, b))) => "a",
  C2(A2(a, b))     => "b",
  C1(B2(_))        => "c",
  C3(A1(a, b))     => "d",
  C3(A2(a, b))     => "e",
  _                => "z",
}


function main() -> unit = {
  let _ = foo(C1(B1(A2(2, 3))));
}
