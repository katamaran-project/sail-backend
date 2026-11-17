default Order dec
$include <prelude.sail>

// a loop in a size-polymorphic function: the helper takes the width implicitly
val count_ones : forall 'n, 'n > 0. (int('n), bits('n)) -> int
function count_ones(n, v) = {
  count : int = 0;
  foreach (i from 0 to (n - 1))
    if v[i] == bitone then count = count + 1;
  count
}

val count_ones_32 : bits(32) -> int
function count_ones_32(v) = count_ones(32, v)
