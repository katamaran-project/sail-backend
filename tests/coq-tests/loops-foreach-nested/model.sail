default Order dec
$include <prelude.sail>

// a loop in a loop: the inner loop's helper reads the outer loop's variable
val triangle : int -> int
function triangle(n) = {
  total : int = 0;
  foreach (i from 1 to n)
    foreach (j from 1 to i)
      total = total + 1;
  total
}
