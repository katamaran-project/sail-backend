default Order dec

$include <prelude.sail>


val mynot : bool -> bool
function mynot(b) = match b {
  true => false,
  false => true
}
