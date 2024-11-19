default Order dec

$include <prelude.sail>


val flip : (int, bool) -> (bool, int)
function flip(p) = match p {
  (l, r) => (r, l)
}
