default Order dec
$include <prelude.sail>

// the number of halvings until x is 0: a while loop
val log2_floor : int -> int
function log2_floor(x) = {
  n : int = x;
  steps : int = 0;
  while n > 1 do {
    n = tdiv_int(n, 2);
    steps = steps + 1
  };
  steps
}

// a repeat loop: the body runs at least once
val countdown : int -> int
function countdown(x) = {
  n : int = x;
  repeat n = n - 1 until n <= 0;
  n
}
