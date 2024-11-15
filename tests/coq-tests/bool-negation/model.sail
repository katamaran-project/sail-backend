default Order dec

$include <prelude.sail>


val foo : (bool) -> bool
function foo(x) = {
  not_bool(x)
}