default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail>
$include "generated.sail"

scattered union exception
union clause exception = notImplmented : string
union clause exception = notAllowed : string
union clause exception = undefindedBehavior : string
union clause exception = undefindedInstruction : bits(16)
