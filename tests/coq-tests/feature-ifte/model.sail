default Order dec

$include <prelude.sail>


val mynot : bool -> bool
function mynot(b) = if b then false else true


/* val mynot2 : bool -> bool */
/* function mynot2(b) = match b { */
/*   true => false, */
/*   false => true */
/* } */
