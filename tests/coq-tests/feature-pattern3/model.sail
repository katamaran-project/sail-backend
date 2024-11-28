default Order dec

$include <prelude.sail>


val flip : (int, bool) -> (bool, int)
function flip(n, b) = (b, n)
