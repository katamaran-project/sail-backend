default Order dec

$include <prelude.sail>



val foo : bool -> int
function foo(b) = match b {
  true => 1,
  false => 0,
}
