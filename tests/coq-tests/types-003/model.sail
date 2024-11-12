default Order dec

$include <prelude.sail>


type regname = bits(2)
type dst     = regname
type src     = regname
type imm     = bits(12)
type imm_ext = bits(20)
