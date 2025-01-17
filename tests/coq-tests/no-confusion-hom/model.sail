default Order dec

$include <prelude.sail>


type len_word: Int = 16
type wordBits      = bits(len_word)

register verbosity  : bits(64)
register old_PC_reg : wordBits
