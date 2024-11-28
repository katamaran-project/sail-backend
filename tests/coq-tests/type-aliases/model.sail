default Order dec

$include <prelude.sail>


type regid   = bits(2)
type dst     = regid
type src     = regid
type imm     = bits(12)
type imm_ext = bits(20)
