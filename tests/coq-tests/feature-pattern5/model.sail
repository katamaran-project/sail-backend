default Order dec

$include <prelude.sail>


val two_min : list(int) -> bool
function two_min(l) = match l {
  [||]        => false,
  [|_|]       => false,
  a :: b :: t => true
}
