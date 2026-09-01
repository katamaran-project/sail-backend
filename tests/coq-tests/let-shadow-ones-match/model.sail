default Order dec

$include <prelude.sail>


/*
   The tuple pattern rebinds x, shadowing the let-bound x = 10.  After the match,
   the let-bound x must be visible again, so sail_ones(x) there is 10 bits wide.

   Note that Sail alpha-renames the pattern binders before the backend sees them,
   so the shadowing itself never reaches nanosail; what this test pins down is
   that a match does not disturb the enclosing scope.
*/
val foo : (int, int) -> bits(10)
function foo(p, q) = {
  let x = 10 in
  {
    match (p, q) {
      (x, y) => ()
    };
    sail_ones(x)
  }
}
