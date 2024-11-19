default Order dec

$include <prelude.sail>


val two_min : list(int) -> bool
function two_min(l) = match l {
  [||]   => false,
  a :: t => match t {
    [||]   => false,
    b :: t' => true
  }
}
