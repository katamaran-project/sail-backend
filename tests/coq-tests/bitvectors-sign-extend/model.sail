default Order dec

$include <prelude.sail>


val foo : bits(12) -> bits(16)
function foo(bs) = {
  sail_sign_extend(bs, 16)
}
