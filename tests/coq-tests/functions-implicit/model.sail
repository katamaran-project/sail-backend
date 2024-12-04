default Order dec

$include <prelude.sail>


val bvlen : forall 'n, 'n >= 0. (implicit('n), bits('n)) -> int('n)
function bvlen(r, _) = {
  r
}
