default Order dec

$include <prelude.sail>

/*
   An assert whose message is only known at run time.
   Messages have no meaning for verification, so the translation may replace it.
*/

val check : (bool, string) -> unit
function check(condition, message) = {
  assert(condition, message)
}
