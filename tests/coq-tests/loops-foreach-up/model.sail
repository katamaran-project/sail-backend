default Order dec
$include <prelude.sail>

// counts the bits of v that are set
val popcount : bits(8) -> int
function popcount(v) = {
  count : int = 0;
  foreach (i from 0 to 7)
    if v[i] == bitone then count = count + 1;
  count
}
