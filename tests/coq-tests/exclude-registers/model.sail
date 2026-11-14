default Order dec

$include <prelude.sail>

/*
   (exclude-registers '("pmpaddr0" "pmpcfg_n")) leaves pmpaddr0 and pmpcfg_n out
   of Reg. A read of an excluded register becomes an unconstrained value of the
   register's type, a write a no-op (the written value is still computed).
   pmpcfg_n is a vector of bitvectors, a register type that Reg cannot hold
   yet (as CHERIoT's PMP registers); excluding it is the only way to get Reg
   through Rocq.
*/

register pmpaddr0 : bits(32)
register pmpcfg_n : vector(4, bits(8))
register mstatus  : bits(32)

val get_addr : unit -> bits(32)
function get_addr() = pmpaddr0

val set_addr : bits(32) -> unit
function set_addr(v) = pmpaddr0 = v + mstatus

val add_addr : bits(32) -> bits(32)
function add_addr(v) = pmpaddr0 + v
