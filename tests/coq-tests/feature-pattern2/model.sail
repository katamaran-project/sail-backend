default Order dec

$include <prelude.sail>


val is_empty : list(int) -> bool
function is_empty(l) = match l {
  h :: t => false,
  [||]   => true
}
