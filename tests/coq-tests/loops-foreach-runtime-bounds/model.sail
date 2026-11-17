default Order dec
$include <prelude.sail>

// bounds and step known only at run time, three updated variables
val sums : (int, int, int) -> (int, int, int)
function sums(low, high, step) = {
  total : int = 0;
  count : int = 0;
  last : int = low;
  foreach (i from low to high by step) {
    total = total + i;
    count = count + 1;
    last = i
  };
  (total, count, last)
}
