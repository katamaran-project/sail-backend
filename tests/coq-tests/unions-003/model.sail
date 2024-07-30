default Order dec


scattered union ast
union clause ast = JALR_CAP      : (int, int)
union clause ast = CJALR         : (int, int, int)
union clause ast = CJAL	         : (int, int)
union clause ast = BNE           : (int, int, int)
union clause ast = LD            : (int, int, int)
union clause ast = SD            : (int, int, int)
union clause ast = ADDI          : (int, int, int)
union clause ast = ADD           : (int, int, int)
union clause ast = SUB           : (int, int, int)
union clause ast = SLT           : (int, int, int)
union clause ast = SLTI          : (int, int, int)
union clause ast = SLTIU         : (int, int, int)
union clause ast = SLTU          : (int, int, int)
union clause ast = CMOVE         : (int, int)
union clause ast = CINCOFFSET    : (int, int, int)
union clause ast = CANDPERM      : (int, int, int)
union clause ast = CSETBOUNDS    : (int, int, int)
union clause ast = CSETBOUNDSINT : (int, int, int)
union clause ast = CGETTAG       : (int, int)
union clause ast = CGETPERM      : (int, int)
union clause ast = CGETBASE      : (int, int)
union clause ast = CGETLEN       : (int, int)
union clause ast = CGETADDR      : (int, int)
union clause ast = FAIL          : unit
union clause ast = RET           : unit
