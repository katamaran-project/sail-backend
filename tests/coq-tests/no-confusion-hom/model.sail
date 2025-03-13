default Order dec

$include <prelude.sail>


type len_word: Int = 16
type wordBits      = bits(len_word)

register verbosity  : bits(8) = 0b00000000
register old_PC_reg : wordBits = 0b0000000000000000
