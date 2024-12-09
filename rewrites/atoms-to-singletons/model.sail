default Order dec

$include <prelude.sail>


val foo : forall 'n. int('n) -> int('n)
function foo(x) = {
  x
}
