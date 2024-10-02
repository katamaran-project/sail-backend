default Order dec

type word = int


scattered union ast
union clause ast = JALR_CAP       : (word, word)
union clause ast = CJALR          : (word, word, word)
union clause ast = CJAL	          : (word, word)
union clause ast = BNE            : (word, word, word)
union clause ast = LD             : (word, word, word)
union clause ast = SD             : (word, word, word)
union clause ast = ADDI           : (word, word, word)
union clause ast = ADD            : (word, word, word)
union clause ast = SUB            : (word, word, word)
union clause ast = SLT            : (word, word, word)
union clause ast = SLTI           : (word, word, word)
union clause ast = SLTIU          : (word, word, word)
union clause ast = SLTU           : (word, word, word)
union clause ast = CMOVE          : (word, word)
union clause ast = CINCOFFSET     : (word, word, word)
union clause ast = CANDPERM       : (word, word, word)
union clause ast = CSETBOUNDS     : (word, word, word)
union clause ast = CSETBOUNDSWORD : (word, word, word)
union clause ast = CGETTAG        : (word, word)
union clause ast = CGETPERM       : (word, word)
union clause ast = CGETBASE       : (word, word)
union clause ast = CGETLEN        : (word, word)
union clause ast = CGETADDR       : (word, word)
union clause ast = FAIL           : unit
union clause ast = RET            : unit
