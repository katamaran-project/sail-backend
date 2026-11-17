default Order dec
$include <prelude.sail>

register counter : int

// no local variable is updated: the loop only writes a register
val bump : int -> unit
function bump(n) = {
  foreach (i from 1 to n)
    counter = counter + i
}
