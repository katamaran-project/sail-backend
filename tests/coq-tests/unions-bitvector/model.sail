default Order dec

$include <prelude.sail>


type regid   = bits(2)
type dst     = regid
type src     = regid
type imm     = bits(12)
type imm_ext = bits(20)
let imm_size = 12
let imm_ext_size = 20


scattered union ast
union clause ast = JALR_CAP      : (dst, src)
union clause ast = CJALR         : (dst, src, imm)
union clause ast = CJAL	         : (dst, imm_ext)
union clause ast = BNE           : (src, src, imm)
union clause ast = LD            : (dst, src, imm)
union clause ast = SD            : (src, src, imm)
union clause ast = ADDI          : (dst, src, imm)
union clause ast = ADD           : (dst, src, src)
union clause ast = SUB           : (dst, src, src)
union clause ast = SLT           : (dst, src, src)
union clause ast = SLTI          : (dst, src, imm)
union clause ast = SLTIU         : (dst, src, imm)
union clause ast = SLTU          : (dst, src, src)
union clause ast = CMOVE         : (dst, src)
union clause ast = CINCOFFSET    : (dst, src, src)
union clause ast = CANDPERM      : (dst, src, src)
union clause ast = CSETBOUNDS    : (dst, src, src)
union clause ast = CSETBOUNDSIMM : (dst, src, imm)
union clause ast = CGETTAG       : (dst, src)
union clause ast = CGETPERM      : (dst, src)
union clause ast = CGETBASE      : (dst, src)
union clause ast = CGETLEN       : (dst, src)
union clause ast = CGETADDR      : (dst, src)
union clause ast = FAIL          : unit
union clause ast = RET           : unit
