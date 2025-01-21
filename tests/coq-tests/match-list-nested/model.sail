default Order dec

$include <prelude.sail>


enum Leone = {
  Good,
  Bad,
  Ugly
}

val foo : list(Leone) -> bool
function foo lst = {
  match lst {
    [| x, y |]    => true,
    [| x, y, z |] => true,
    _             => false,
  }
}
