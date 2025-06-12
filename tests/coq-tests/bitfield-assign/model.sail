
default Order dec

$include <vector.sail>

bitfield Mstatus : bits(1) = {
  VS   : 0
}

register mstatus : Mstatus

val foo : unit -> unit
function foo() = {
  mstatus[VS] = 0b1
}
