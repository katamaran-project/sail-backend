#!/usr/bin/env python3
"""
Write a bare RV32 ELF for the CHERIoT C emulator, from a tiny assembly file.
No cross toolchain needed. See validation/docs/validate.md.

Usage:
    validation/scripts/mk-test-elf.py PROG.s OUT.elf [--base ADDR] [--tohost ADDR]

Prints one line per 32-bit word of the code: `<address>: <word>` (hex).

PROG.s: one instruction per line, `#` comments, `label:` before an instruction or alone,
registers as x0..x31, c0..c31 or ABI names (a0, ca0, ...).
    RV32I     add sub sll slt sltu xor srl sra or and   rd, rs1, rs2
              addi slti sltiu xori ori andi slli srli srai  rd, rs1, imm
              lui rd, imm20          lb lh lw lbu lhu rd, off(rs1)    sb sh sw rs2, off(rs1)
              beq bne blt bge bltu bgeu rs1, rs2, label|offset
              jal [rd,] label|offset    j label    jalr/cjalr cd, off(cs1) | cd, cs1[, imm]
              ecall ebreak mret nop wfi sret fence.i fence.tso
              fence [pred, succ] (iorw letters; none: iorw, iorw)    sfence.vma [rs1, rs2]
    M         mul mulh mulhsu mulhu div divu rem remu   rd, rs1, rs2
    Zicsr     csrrw csrrs csrrc rd, csr, rs1    csrrwi csrrsi csrrci rd, csr, uimm5
              (csr: a name in CSR below, or a number)
    CHERIoT   csetaddr cincaddr csetbounds csetboundsexact csetboundsrounddown candperm
              cseal cunseal csub ctestsubset cseqx csethigh   cd, cs1, rs2
              cincaddrimm csetboundsimm  cd, cs1, imm
              cgetperm cgettype cgetbase cgetlen cgettag cgetaddr cgethigh cgettop
              cmove ccleartag crrl cram   rd, cs1
              clc cd, off(rs1)     csc cs2, off(rs1)     auipcc auicgp cd, imm20
              cspecialr cd, SCR    cspecialw SCR, cs1    cspecialrw cd, SCR, cs1
              (SCR: mtcc, mtdc, mscratchc, mepcc)
    C         (16 bits; the next instruction follows at +2; rd'/rs' are x8-x15)
              c.nop [imm6]    c.addi c.li c.slli rd, imm    c.lui rd, imm6 (bits 17:12)
              c.srli c.srai c.andi rd', imm    c.sub c.xor c.or c.and rd', rs2'
              c.mv c.add rd, rs2    c.lw rd', off(rs1')    c.sw rs2', off(rs1')
              c.lwsp rd, off(sp)    c.swsp rs2, off(sp)    c.j c.jal label|offset
              c.beqz c.bnez rs1', label|offset    c.jr c.jalr rs1    c.ebreak
              c.addi16sp sp, imm    c.addi4spn rd', sp, imm
              (operands outside the instruction's constraints give its hint or
              reserved encoding, e.g. c.addi a0, 0 or c.lui a0, 0)
    CHERIoT C c.clc cd', off(cs1')    c.csc cs2', off(cs1')    c.clcsp cd, off(csp)
              c.cscsp cs2, off(csp)    c.cincaddr16csp csp, imm    c.cincaddr4cspn cd', csp, imm
              c.cjal label|offset    c.cjr c.cjalr cs1   (same encodings as c.addi16sp,
              c.addi4spn, c.jal, c.jr, c.jalr; c.clc, c.csc, c.clcsp, c.cscsp are
              c.fld, c.fsd, c.fldsp, c.fsdsp with funct3 bit 1 set)
    pseudo    li rd, imm32|label (always lui + addi)    mv rd, rs
              cla cd, label (auipcc cd, 0; cincaddrimm cd, cd, label - .)
    data      .word 0x...    .half 0x...
    .align N  pad with C.NOPs (executed) to a multiple of N bytes (N even)

Encodings: RV32I/M/Zicsr/C checked against llvm-mc (validation/scripts/check-encodings.sh);
CHERIoT ones from the encdec clauses in cheriot-sail src/cheri_insts.sail.

The ELF has one PT_LOAD segment (the code, at --base, default 0x80000000, also the entry)
and a symbol table with `tohost` (default 0x80001000), which the emulator requires.
"""

import argparse
import re
import struct
import sys

ABI = ["zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1",
       "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
       "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
       "t3", "t4", "t5", "t6"]

SCR = {"mtcc": 28, "mtdc": 29, "mscratchc": 30, "mepcc": 31}

CSR = {
    "mstatus": 0x300, "misa": 0x301, "mie": 0x304, "mtvec": 0x305, "mcounteren": 0x306,
    "mcountinhibit": 0x320, "mscratch": 0x340, "mepc": 0x341, "mcause": 0x342,
    "mtval": 0x343, "mip": 0x344, "mcycle": 0xB00, "minstret": 0xB02,
    "mcycleh": 0xB80, "minstreth": 0xB82, "mvendorid": 0xF11, "marchid": 0xF12,
    "mimpid": 0xF13, "mhartid": 0xF14, "mccsr": 0xBC0, "mshwm": 0xBC1, "mshwmb": 0xBC2,
    "cycle": 0xC00, "time": 0xC01, "instret": 0xC02,
}

CHERI_OP = 0x5B

# R-type: funct7, funct3, opcode
R_OPS = {
    "add": (0x00, 0, 0x33), "sub": (0x20, 0, 0x33), "sll": (0x00, 1, 0x33),
    "slt": (0x00, 2, 0x33), "sltu": (0x00, 3, 0x33), "xor": (0x00, 4, 0x33),
    "srl": (0x00, 5, 0x33), "sra": (0x20, 5, 0x33), "or": (0x00, 6, 0x33),
    "and": (0x00, 7, 0x33),
    "mul": (0x01, 0, 0x33), "mulh": (0x01, 1, 0x33), "mulhsu": (0x01, 2, 0x33),
    "mulhu": (0x01, 3, 0x33), "div": (0x01, 4, 0x33), "divu": (0x01, 5, 0x33),
    "rem": (0x01, 6, 0x33), "remu": (0x01, 7, 0x33),
    "csetbounds": (0x08, 0, CHERI_OP), "csetboundsexact": (0x09, 0, CHERI_OP),
    "csetboundsrounddown": (0x0A, 0, CHERI_OP),
    "cseal": (0x0B, 0, CHERI_OP), "cunseal": (0x0C, 0, CHERI_OP),
    "candperm": (0x0D, 0, CHERI_OP), "csetaddr": (0x10, 0, CHERI_OP),
    "cincaddr": (0x11, 0, CHERI_OP), "csub": (0x14, 0, CHERI_OP),
    "csethigh": (0x16, 0, CHERI_OP), "ctestsubset": (0x20, 0, CHERI_OP),
    "cseqx": (0x21, 0, CHERI_OP),
    # Zba, Zbb, Zbc, Zbs, Zbkb, Zbkx (RV32)
    "sh1add": (0x10, 2, 0x33), "sh2add": (0x10, 4, 0x33), "sh3add": (0x10, 6, 0x33),
    "andn": (0x20, 7, 0x33), "orn": (0x20, 6, 0x33), "xnor": (0x20, 4, 0x33),
    "min": (0x05, 4, 0x33), "minu": (0x05, 5, 0x33), "max": (0x05, 6, 0x33), "maxu": (0x05, 7, 0x33),
    "rol": (0x30, 1, 0x33), "ror": (0x30, 5, 0x33),
    "clmul": (0x05, 1, 0x33), "clmulr": (0x05, 2, 0x33), "clmulh": (0x05, 3, 0x33),
    "bclr": (0x24, 1, 0x33), "bext": (0x24, 5, 0x33), "binv": (0x34, 1, 0x33), "bset": (0x14, 1, 0x33),
    "pack": (0x04, 4, 0x33), "packh": (0x04, 7, 0x33),
    "xperm4": (0x14, 2, 0x33), "xperm8": (0x14, 4, 0x33),
}
# I-type: funct3, opcode
I_OPS = {
    "addi": (0, 0x13), "slti": (2, 0x13), "sltiu": (3, 0x13), "xori": (4, 0x13),
    "ori": (6, 0x13), "andi": (7, 0x13),
    "cincaddrimm": (1, CHERI_OP), "csetboundsimm": (2, CHERI_OP),
}
# Shifts by immediate: funct7, funct3
SHIFT_OPS = {"slli": (0x00, 1), "srli": (0x00, 5), "srai": (0x20, 5),
             "rori": (0x30, 5), "bclri": (0x24, 1), "bexti": (0x24, 5), "binvi": (0x34, 1), "bseti": (0x14, 1)}
# Zb* operations on one register (OP-IMM, the immediate fixed): imm12, funct3
UNARY_OPS = {"clz": (0x600, 1), "ctz": (0x601, 1), "cpop": (0x602, 1), "sext.b": (0x604, 1), "sext.h": (0x605, 1),
             "orc.b": (0x287, 5), "rev8": (0x698, 5), "brev8": (0x687, 5), "zip": (0x08F, 1), "unzip": (0x08F, 5)}
# Two-operand CHERI ops: 0x7F @ sub @ cs1 @ 000 @ rd @ 0x5B
CAP2_OPS = {
    "cgetperm": 0x00, "cgettype": 0x01, "cgetbase": 0x02, "cgetlen": 0x03, "cgettag": 0x04,
    "crrl": 0x08, "cram": 0x09, "cmove": 0x0A, "ccleartag": 0x0B, "cgetaddr": 0x0F,
    "cgethigh": 0x17, "cgettop": 0x18,
}
LOAD_OPS = {"lb": 0, "lh": 1, "lw": 2, "clc": 3, "lbu": 4, "lhu": 5}
STORE_OPS = {"sb": 0, "sh": 1, "sw": 2, "csc": 3}
BRANCH_OPS = {"beq": 0, "bne": 1, "blt": 4, "bge": 5, "bltu": 6, "bgeu": 7}
CSR_OPS = {"csrrw": 1, "csrrs": 2, "csrrc": 3, "csrrwi": 5, "csrrsi": 6, "csrrci": 7}
U_OPS = {"lui": 0x37, "auipcc": 0x17, "auicgp": 0x7B}
FIXED = {"ecall": 0x00000073, "ebreak": 0x00100073, "mret": 0x30200073, "nop": 0x00000013,
         "wfi": 0x10500073, "sret": 0x10200073, "fence.i": 0x0000100F, "fence.tso": 0x8330000F}
# Pseudo-instructions of two words.
TWO_WORDS = {"li", "cla"}
# 16-bit parcels.
HALF = {".half"}  # and every mnemonic starting with "c."


def size(mnem):
    """Bytes of one line's encoding."""
    return 8 if mnem in TWO_WORDS else 2 if is_half(mnem) else 4


def is_half(mnem):
    return mnem in HALF or mnem.startswith("c.")


def reg(name):
    name = name.strip().lower()
    if name in ABI:
        return ABI.index(name)
    if name == "fp":
        return 8
    m = re.fullmatch(r"[xc](\d+)", name)
    if m and int(m.group(1)) < 32:
        return int(m.group(1))
    if name == "cnull":
        return 0
    m = re.fullmatch(r"c(.+)", name)  # capability names: ca0, csp, ...
    if m and m.group(1) in ABI:
        return ABI.index(m.group(1))
    raise ValueError(f"bad register: {name}")


def check_range(v, bits, signed, text):
    lo = -(1 << (bits - 1)) if signed else 0
    hi = 1 << (bits - 1) if signed else 1 << bits
    if not lo <= v < hi:
        raise ValueError(f"immediate {text} out of range for {bits} bits")
    return v & ((1 << bits) - 1)


def imm(text, bits):
    """Signed or unsigned immediate of `bits` bits (both ranges accepted)."""
    v = int(text.strip(), 0)
    if not -(1 << (bits - 1)) <= v < (1 << bits):
        raise ValueError(f"immediate {text} out of range for {bits} bits")
    return v & ((1 << bits) - 1)


def r_type(f7, rs2, rs1, f3, rd, op):
    return (f7 << 25) | (rs2 << 20) | (rs1 << 15) | (f3 << 12) | (rd << 7) | op


def i_type(imm12, rs1, f3, rd, op):
    return (imm12 << 20) | (rs1 << 15) | (f3 << 12) | (rd << 7) | op


def s_type(imm12, rs2, rs1, f3, op):
    return ((imm12 >> 5) << 25) | (rs2 << 20) | (rs1 << 15) | (f3 << 12) | ((imm12 & 0x1F) << 7) | op


def b_type(off, rs2, rs1, f3):
    if off % 2:
        raise ValueError(f"branch offset {off} odd")
    o = check_range(off, 13, True, str(off))
    return (((o >> 12) & 1) << 31) | (((o >> 5) & 0x3F) << 25) | (rs2 << 20) | (rs1 << 15) \
        | (f3 << 12) | (((o >> 1) & 0xF) << 8) | (((o >> 11) & 1) << 7) | 0x63


def j_type(off, rd):
    if off % 2:
        raise ValueError(f"jump offset {off} odd")
    o = check_range(off, 21, True, str(off))
    return (((o >> 20) & 1) << 31) | (((o >> 1) & 0x3FF) << 21) | (((o >> 11) & 1) << 20) \
        | (((o >> 12) & 0xFF) << 12) | (rd << 7) | 0x6F


def mem_operand(text):
    m = re.fullmatch(r"\s*(-?\w+)?\s*\(\s*(\w+)\s*\)\s*", text)
    if not m:
        raise ValueError(f"bad memory operand: {text}")
    return imm(m.group(1) or "0", 12), reg(m.group(2))


def target(text, pc, labels):
    """Offset from pc to a label, or a numeric offset."""
    text = text.strip()
    if text in labels:
        return labels[text] - pc
    return int(text, 0)


def fence_set(text):
    """FENCE predecessor/successor set: letters of iorw (bits 3..0)."""
    text = text.strip().lower()
    if not text or any(c not in "iorw" for c in text):
        raise ValueError(f"bad fence set: {text}")
    return sum(1 << (3 - "iorw".index(c)) for c in set(text))


def csr_num(text):
    text = text.strip().lower()
    return CSR[text] if text in CSR else imm(text, 12)


def bits(v, hi, lo):
    """v[hi:lo]."""
    return (v >> lo) & ((1 << (hi - lo + 1)) - 1)


def place(v, *fields):
    """Scatter bits of v: fields (hi, lo, at) put v[hi:lo] at bit `at` up."""
    return sum(bits(v, hi, lo) << at for hi, lo, at in fields)


def creg(name):
    """3-bit register x8-x15."""
    r = reg(name)
    if not 8 <= r <= 15:
        raise ValueError(f"not a compressed register (x8-x15): {name}")
    return r - 8


def scaled(text, bits_, scale, signed):
    """An immediate that is a multiple of `scale`, in `bits_` bits (after scaling)."""
    v = int(text.strip(), 0)
    if v % scale:
        raise ValueError(f"immediate {text} not a multiple of {scale}")
    return check_range(v, bits_, signed, text)


def sp_operand(text):
    """off(sp)."""
    off, base = mem_operand_raw(text)
    if reg(base) != 2:
        raise ValueError(f"base must be sp: {text}")
    return off


def mem_operand_raw(text):
    m = re.fullmatch(r"\s*(-?\w+)?\s*\(\s*(\w+)\s*\)\s*", text)
    if not m:
        raise ValueError(f"bad memory operand: {text}")
    return m.group(1) or "0", m.group(2)


# C.SRLI, C.SRAI, C.ANDI: funct2 at 11:10. C.SUB ... C.AND: funct2 at 6:5.
C_SHIFT = {"c.srli": 0, "c.srai": 1, "c.andi": 2}
C_ARITH = {"c.sub": 0, "c.xor": 1, "c.or": 2, "c.and": 3}
# CJ format (C.JAL, C.J): imm[11|4|9:8|10|6|7|3:1|5].
CJ_FIELDS = ((11, 11, 12), (4, 4, 11), (9, 8, 9), (10, 10, 8), (6, 6, 7), (7, 7, 6), (3, 1, 3), (5, 5, 2))


def encode_c(mnem, args, pc, labels):
    """The 16-bit parcel of a C or CHERIoT C instruction."""
    a = args
    if mnem == "c.nop":
        i = check_range(int(a[0], 0), 6, True, a[0]) if a else 0
        return place(i, (5, 5, 12), (4, 0, 2)) | 0x1
    if mnem in ("c.addi", "c.li"):
        i = check_range(int(a[1], 0), 6, True, a[1])
        f3 = 0 if mnem == "c.addi" else 2
        return (f3 << 13) | place(i, (5, 5, 12), (4, 0, 2)) | (reg(a[0]) << 7) | 0x1
    if mnem == "c.lui":
        v = int(a[1], 0)
        v = v - (1 << 20) if v >= 0xFFFE0 else v      # llvm-mc writes -1 as 0xfffff
        i = check_range(v, 6, True, a[1])
        return (3 << 13) | place(i, (5, 5, 12), (4, 0, 2)) | (reg(a[0]) << 7) | 0x1
    if mnem in ("c.addi16sp", "c.cincaddr16csp"):
        i = scaled(a[-1], 10, 16, True)
        return (3 << 13) | (2 << 7) | place(i, (9, 9, 12), (4, 4, 6), (6, 6, 5), (8, 7, 3), (5, 5, 2)) | 0x1
    if mnem in ("c.addi4spn", "c.cincaddr4cspn"):
        if reg(a[1]) != 2:
            raise ValueError("source must be sp")
        i = scaled(a[2], 10, 4, False)
        return place(i, (5, 4, 11), (9, 6, 7), (2, 2, 6), (3, 3, 5)) | (creg(a[0]) << 2)
    if mnem in C_SHIFT:
        i = check_range(int(a[1], 0), 6, mnem == "c.andi", a[1])
        return (4 << 13) | (C_SHIFT[mnem] << 10) | place(i, (5, 5, 12), (4, 0, 2)) | (creg(a[0]) << 7) | 0x1
    if mnem in C_ARITH:
        return (4 << 13) | (3 << 10) | (creg(a[0]) << 7) | (C_ARITH[mnem] << 5) | (creg(a[1]) << 2) | 0x1
    if mnem in ("c.j", "c.jal", "c.cjal"):
        o = target(a[0], pc, labels)
        if o % 2:
            raise ValueError(f"jump offset {o} odd")
        f3 = 5 if mnem == "c.j" else 1
        return (f3 << 13) | place(check_range(o, 12, True, str(o)), *CJ_FIELDS) | 0x1
    if mnem in ("c.beqz", "c.bnez"):
        o = target(a[1], pc, labels)
        if o % 2:
            raise ValueError(f"branch offset {o} odd")
        o = check_range(o, 9, True, str(o))
        f3 = 6 if mnem == "c.beqz" else 7
        return (f3 << 13) | place(o, (8, 8, 12), (4, 3, 10), (7, 6, 5), (2, 1, 3), (5, 5, 2)) \
            | (creg(a[0]) << 7) | 0x1
    if mnem == "c.slli":
        i = check_range(int(a[1], 0), 6, False, a[1])
        return place(i, (5, 5, 12), (4, 0, 2)) | (reg(a[0]) << 7) | 0x2
    if mnem in ("c.lw", "c.sw", "c.clc", "c.csc"):
        off_text, base = mem_operand_raw(a[1])
        r = (creg(a[0]) << 2) | (creg(base) << 7)
        if mnem in ("c.lw", "c.sw"):
            o = scaled(off_text, 7, 4, False)
            return ((2 if mnem == "c.lw" else 6) << 13) | place(o, (5, 3, 10), (2, 2, 6), (6, 6, 5)) | r
        o = scaled(off_text, 8, 8, False)
        return ((3 if mnem == "c.clc" else 7) << 13) | place(o, (5, 3, 10), (7, 6, 5)) | r
    if mnem in ("c.lwsp", "c.clcsp"):
        r = reg(a[0]) << 7
        if mnem == "c.lwsp":
            o = scaled(sp_operand(a[1]), 8, 4, False)
            return (2 << 13) | place(o, (5, 5, 12), (4, 2, 4), (7, 6, 2)) | r | 0x2
        o = scaled(sp_operand(a[1]), 9, 8, False)
        return (3 << 13) | place(o, (5, 5, 12), (4, 3, 5), (8, 6, 2)) | r | 0x2
    if mnem in ("c.swsp", "c.cscsp"):
        r = reg(a[0]) << 2
        if mnem == "c.swsp":
            o = scaled(sp_operand(a[1]), 8, 4, False)
            return (6 << 13) | place(o, (5, 2, 9), (7, 6, 7)) | r | 0x2
        o = scaled(sp_operand(a[1]), 9, 8, False)
        return (7 << 13) | place(o, (5, 3, 10), (8, 6, 7)) | r | 0x2
    if mnem in ("c.jr", "c.cjr", "c.jalr", "c.cjalr"):
        link = mnem.endswith("jalr")
        return (4 << 13) | (link << 12) | (reg(a[0]) << 7) | 0x2
    if mnem in ("c.mv", "c.add"):
        return (4 << 13) | ((mnem == "c.add") << 12) | (reg(a[0]) << 7) | (reg(a[1]) << 2) | 0x2
    if mnem == "c.ebreak":
        return 0x9002
    raise ValueError(f"unknown mnemonic: {mnem}")


def encode(mnem, args, pc, labels):
    """The parcels of one instruction at pc: 32-bit words, or a 16-bit one (HALF)."""
    if mnem == ".word":
        return [int(args[0], 0) & 0xFFFFFFFF]
    if mnem == ".half":
        return [int(args[0], 0) & 0xFFFF]
    if mnem.startswith("c."):
        return [encode_c(mnem, args, pc, labels)]
    if mnem in FIXED:
        return [FIXED[mnem]]
    if mnem == "fence":
        pred, succ = (fence_set(args[0]), fence_set(args[1])) if args else (0xF, 0xF)
        return [(pred << 24) | (succ << 20) | 0x0F]
    if mnem == "sfence.vma":
        rs1, rs2 = (reg(args[0]), reg(args[1])) if args else (0, 0)
        return [r_type(0x09, rs2, rs1, 0, 0, 0x73)]
    if mnem in R_OPS:
        f7, f3, op = R_OPS[mnem]
        return [r_type(f7, reg(args[2]), reg(args[1]), f3, reg(args[0]), op)]
    if mnem in I_OPS:
        f3, op = I_OPS[mnem]
        return [i_type(imm(args[2], 12), reg(args[1]), f3, reg(args[0]), op)]
    if mnem in SHIFT_OPS:
        f7, f3 = SHIFT_OPS[mnem]
        return [r_type(f7, check_range(int(args[2], 0), 5, False, args[2]), reg(args[1]), f3, reg(args[0]), 0x13)]
    if mnem in UNARY_OPS:
        imm12, f3 = UNARY_OPS[mnem]
        return [i_type(imm12, reg(args[1]), f3, reg(args[0]), 0x13)]
    if mnem == "zext.h":                         # pack rd, rs, zero
        return [r_type(0x04, 0, reg(args[1]), 4, reg(args[0]), 0x33)]
    if mnem in CAP2_OPS:
        return [r_type(0x7F, CAP2_OPS[mnem], reg(args[1]), 0, reg(args[0]), CHERI_OP)]
    if mnem in LOAD_OPS:
        off, rs1 = mem_operand(args[1])
        return [i_type(off, rs1, LOAD_OPS[mnem], reg(args[0]), 0x03)]
    if mnem in STORE_OPS:
        off, rs1 = mem_operand(args[1])
        return [s_type(off, reg(args[0]), rs1, STORE_OPS[mnem], 0x23)]
    if mnem in U_OPS:
        return [(imm(args[1], 20) << 12) | (reg(args[0]) << 7) | U_OPS[mnem]]
    if mnem in BRANCH_OPS:
        return [b_type(target(args[2], pc, labels), reg(args[1]), reg(args[0]), BRANCH_OPS[mnem])]
    if mnem in ("jal", "cjal"):
        rd, t = (reg(args[0]), args[1]) if len(args) == 2 else (1, args[0])
        return [j_type(target(t, pc, labels), rd)]
    if mnem == "j":
        return [j_type(target(args[0], pc, labels), 0)]
    if mnem in ("jalr", "cjalr"):
        if len(args) == 1:                       # cjalr cs1: cd = cra
            rd, off, rs1 = 1, 0, reg(args[0])
        elif "(" in args[1]:                     # cjalr cd, off(cs1)
            rd = reg(args[0])
            off, rs1 = mem_operand(args[1])
        else:                                    # cjalr cd, cs1[, imm]
            rd, rs1 = reg(args[0]), reg(args[1])
            off = imm(args[2], 12) if len(args) > 2 else 0
        return [i_type(off, rs1, 0, rd, 0x67)]
    if mnem in CSR_OPS:
        f3 = CSR_OPS[mnem]
        src = check_range(int(args[2], 0), 5, False, args[2]) if f3 >= 5 else reg(args[2])
        return [i_type(csr_num(args[1]), src, f3, reg(args[0]), 0x73)]
    if mnem in ("cspecialr", "cspecialrw"):
        scr = SCR[args[1].lower()]
        cs1 = reg(args[2]) if len(args) > 2 else 0
        return [r_type(0x01, scr, cs1, 0, reg(args[0]), CHERI_OP)]
    if mnem == "cspecialw":
        return [r_type(0x01, SCR[args[0].lower()], reg(args[1]), 0, 0, CHERI_OP)]
    if mnem == "mv":
        return [i_type(0, reg(args[1]), 0, reg(args[0]), 0x13)]
    if mnem == "li":
        v = (labels[args[1]] if args[1] in labels else int(args[1], 0)) & 0xFFFFFFFF
        lo = ((v & 0xFFF) ^ 0x800) - 0x800        # sign-extended low 12 bits
        hi = ((v - lo) >> 12) & 0xFFFFF
        rd = reg(args[0])
        return [(hi << 12) | (rd << 7) | 0x37, i_type(lo & 0xFFF, rd, 0, rd, 0x13)]
    if mnem == "cla":
        cd = reg(args[0])
        off = target(args[1], pc, labels)
        return [(cd << 7) | 0x17, i_type(check_range(off, 12, True, str(off)), cd, 1, cd, CHERI_OP)]
    raise ValueError(f"unknown mnemonic: {mnem}")


def parse(path):
    """[(line number, label or None, mnemonic or None, args)] of PROG.s."""
    out = []
    with open(path) as f:
        for n, raw in enumerate(f, 1):
            line = raw.split("#", 1)[0].strip()
            label = None
            m = re.match(r"([A-Za-z_.][\w.]*):\s*(.*)", line)
            if m:
                label, line = m.group(1), m.group(2)
            mnem, _, rest = line.partition(" ")
            args = [a.strip() for a in rest.split(",")] if rest.strip() else []
            if label or mnem:
                out.append((n, label, mnem.lower() or None, args))
    return out


def align_pad(pc, args):
    """Bytes from pc to the next multiple of .align's N."""
    return -pc % int(args[0], 0)


def assemble(path, base):
    lines = parse(path)
    labels = {}
    pc = base
    for n, label, mnem, args in lines:              # pass 1: label addresses
        if label:
            if label in labels:
                sys.exit(f"{path}:{n}: label {label} defined twice")
            labels[label] = pc
        if mnem == ".align":
            pc += align_pad(pc, args)
        elif mnem:
            pc += size(mnem)
    code = b""
    for n, _, mnem, args in lines:                  # pass 2: encodings
        if not mnem:
            continue
        if mnem == ".align":
            code += struct.pack("<H", 0x0001) * (align_pad(base + len(code), args) // 2)
            continue
        try:
            fmt = "<H" if is_half(mnem) else "<I"
            code += b"".join(struct.pack(fmt, p) for p in encode(mnem, args, base + len(code), labels))
        except (ValueError, KeyError, IndexError) as e:
            sys.exit(f"{path}:{n}: {e}")
    code += bytes(-len(code) % 4)                   # a final 16-bit parcel: pad with 0 (c.illegal)
    return [w for (w,) in struct.iter_unpack("<I", code)]


def elf32(code, base, tohost):
    """ELF32 LE, EM_RISCV, ET_EXEC: header, one phdr, code, strtabs, symtab, section headers."""
    ehsize, phsize, shsize = 52, 32, 40
    code_off = ehsize + phsize
    shstr = b"\0.text\0.symtab\0.strtab\0.shstrtab\0"
    strtab = b"\0tohost\0_start\0"
    # Elf32_Sym: name, value, size, info, other, shndx (SHN_ABS = 0xfff1 for tohost)
    syms = struct.pack("<IIIBBH", 0, 0, 0, 0, 0, 0)
    syms += struct.pack("<IIIBBH", 1, tohost, 8, 0x11, 0, 0xFFF1)  # GLOBAL OBJECT
    syms += struct.pack("<IIIBBH", 8, base, 0, 0x10, 0, 1)         # GLOBAL NOTYPE
    shstr_off = code_off + len(code)
    strtab_off = shstr_off + len(shstr)
    symtab_off = (strtab_off + len(strtab) + 3) & ~3
    sh_off = symtab_off + len(syms)

    ehdr = b"\x7fELF" + bytes([1, 1, 1, 0]) + bytes(8)
    ehdr += struct.pack("<HHIIIIIHHHHHH", 2, 243, 1, base, ehsize, sh_off, 0,
                        ehsize, phsize, 1, shsize, 5, 4)
    phdr = struct.pack("<IIIIIIII", 1, code_off, base, base, len(code), len(code), 5, 4)

    def sh(name, typ, flags, addr, off, size, link=0, info=0, align=1, entsize=0):
        return struct.pack("<IIIIIIIIII", name, typ, flags, addr, off, size, link, info, align, entsize)

    shdrs = sh(0, 0, 0, 0, 0, 0)
    shdrs += sh(1, 1, 0x6, base, code_off, len(code), align=4)            # .text: PROGBITS, AX
    shdrs += sh(7, 2, 0, 0, symtab_off, len(syms), link=3, info=1, align=4, entsize=16)
    shdrs += sh(15, 3, 0, 0, strtab_off, len(strtab))                     # .strtab
    shdrs += sh(23, 3, 0, 0, shstr_off, len(shstr))                       # .shstrtab

    out = ehdr + phdr + code + shstr + strtab
    out += bytes(symtab_off - len(out)) + syms
    return out + shdrs


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("prog")
    p.add_argument("out")
    p.add_argument("--base", type=lambda s: int(s, 0), default=0x80000000)
    p.add_argument("--tohost", type=lambda s: int(s, 0), default=0x80001000)
    a = p.parse_args()
    words = assemble(a.prog, a.base)
    code = b"".join(struct.pack("<I", w) for w in words)
    with open(a.out, "wb") as f:
        f.write(elf32(code, a.base, a.tohost))
    for i, w in enumerate(words):
        print(f"{a.base + 4 * i:08x}: {w:08x}")


if __name__ == "__main__":
    main()
