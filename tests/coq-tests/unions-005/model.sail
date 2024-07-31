default Order dec

$include <prelude.sail>

type word = int

enum regid      = { regid1, regid2, regid3, regid4 }

scattered union ast
union clause ast = JALR_CAP       : (word, word)
union clause ast = CJALR          : (word, word, word)
union clause ast = RET            : unit


union IntInstruction = {
    Read1  : (int, int),
    Write1 : (int, int),
}

union RegInstruction = {
    Read2      : (regid, regid),
    Write2     : (regid, regid),
}
