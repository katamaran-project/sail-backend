default Order dec

$include <prelude.sail>


val foo : bits(8) -> int
function foo(bv) = {
  match bv {
    0xFF => 0,
    0b0000_0001 => 2,
    _ => 3
  }
}
