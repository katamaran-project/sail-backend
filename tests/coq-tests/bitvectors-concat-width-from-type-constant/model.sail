default Order dec
$include <prelude.sail>

val zeros_implicit : forall 'n, 'n >= 0 . implicit('n) -> bits('n)
function zeros_implicit (n) = sail_zeros(n)
overload zeros = {zeros_implicit}

// CHERIoT's tag_addr_to_addr: widths from type-level constants behind a type abbreviation
type xlen : Int = 32
type log2_cap_size : Int = 3
let log2_cap_size = sizeof(log2_cap_size)
type tagaddrbits = bits(xlen - log2_cap_size)

val tag_addr_to_addr : tagaddrbits -> bits(xlen)
function tag_addr_to_addr(tag_addr) = tag_addr @ zeros(log2_cap_size)
