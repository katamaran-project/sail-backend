default Order dec

$include <prelude.sail>

/*
   A tag write declared pure, as in CHERI models: val MEMw_tag = "write_tag_bool".
   Its binding key write_tag_bool marks it as a memory write: its ForeignCall allows the memory
   to change, as for a monadic extern. A tag read declared pure leaves the memory unchanged.
*/

val MEMr_tag = "read_tag_bool"  : bits(64) -> bool
val MEMw_tag = "write_tag_bool" : (bits(64), bool) -> unit

val clear_tag : bits(64) -> unit
function clear_tag(addr) = MEMw_tag(addr, false)

val tag_of : bits(64) -> bool
function tag_of(addr) = MEMr_tag(addr)
