default Order dec

$include <prelude.sail>


type number = int


val iszero : number -> bool
function iszero(x) = {
  match x {
    0 => true,
    _ => false
  }
}
