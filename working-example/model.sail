type xlen       : Int = 32
type xlen_bytes : Int = 4
type xlen_bits  : Int = xlen_bytes * 8
type foo        : Int = 3 * 8

register RAX : bitvector(32)
register RBX : bitvector(32)
register RCX : int
