#!/usr/bin/env python3
"""
Write a bare RV32 ELF for the CHERIoT C emulator, from a tiny assembly file.
No cross toolchain needed. See validation/docs/emulator.md.

Usage:
    validation/scripts/mk-test-elf.py PROG.s OUT.elf [--base ADDR] [--tohost ADDR]

PROG.s: one instruction per line, `#` comments, registers as x0..x31 or ABI names.
    addi rd, rs1, imm      add/sub/... rd, rs1, rs2      lui rd, imm20
    lw rd, off(rs1)        sw rs2, off(rs1)
    cspecialr cd, SCR      (SCR: mtcc, mtdc, mscratchc, mepcc)
    csetaddr cd, cs1, rs2  cincaddr cd, cs1, rs2         cincaddrimm cd, cs1, imm
    .word 0x...            any other encoding

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

# R-type: funct7, funct3, opcode
R_OPS = {
    "add": (0x00, 0, 0x33), "sub": (0x20, 0, 0x33), "sll": (0x00, 1, 0x33),
    "slt": (0x00, 2, 0x33), "sltu": (0x00, 3, 0x33), "xor": (0x00, 4, 0x33),
    "srl": (0x00, 5, 0x33), "sra": (0x20, 5, 0x33), "or": (0x00, 6, 0x33),
    "and": (0x00, 7, 0x33), "mul": (0x01, 0, 0x33),
    "csetaddr": (0x10, 0, 0x5B), "cincaddr": (0x11, 0, 0x5B),
}
# I-type: funct3, opcode
I_OPS = {
    "addi": (0, 0x13), "slti": (2, 0x13), "sltiu": (3, 0x13), "xori": (4, 0x13),
    "ori": (6, 0x13), "andi": (7, 0x13), "cincaddrimm": (1, 0x5B),
}
LOAD_OPS = {"lb": 0, "lh": 1, "lw": 2, "lbu": 4, "lhu": 5}
STORE_OPS = {"sb": 0, "sh": 1, "sw": 2}


def reg(name):
    name = name.strip().lower()
    if name in ABI:
        return ABI.index(name)
    if name == "fp":
        return 8
    m = re.fullmatch(r"[xc](\d+)", name)
    if m and int(m.group(1)) < 32:
        return int(m.group(1))
    m = re.fullmatch(r"c(.+)", name)  # capability names: ca0, csp, ...
    if m and m.group(1) in ABI:
        return ABI.index(m.group(1))
    raise ValueError(f"bad register: {name}")


def imm(text, bits):
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


def mem_operand(text):
    m = re.fullmatch(r"\s*(-?\w+)\s*\(\s*(\w+)\s*\)\s*", text)
    if not m:
        raise ValueError(f"bad memory operand: {text}")
    return imm(m.group(1), 12), reg(m.group(2))


def encode(line):
    mnem, _, rest = line.partition(" ")
    mnem = mnem.lower()
    args = [a.strip() for a in rest.split(",")] if rest.strip() else []
    if mnem == ".word":
        return int(args[0], 0) & 0xFFFFFFFF
    if mnem in R_OPS:
        f7, f3, op = R_OPS[mnem]
        return r_type(f7, reg(args[2]), reg(args[1]), f3, reg(args[0]), op)
    if mnem in I_OPS:
        f3, op = I_OPS[mnem]
        return i_type(imm(args[2], 12), reg(args[1]), f3, reg(args[0]), op)
    if mnem in LOAD_OPS:
        off, rs1 = mem_operand(args[1])
        return i_type(off, rs1, LOAD_OPS[mnem], reg(args[0]), 0x03)
    if mnem in STORE_OPS:
        off, rs1 = mem_operand(args[1])
        return s_type(off, reg(args[0]), rs1, STORE_OPS[mnem], 0x23)
    if mnem == "lui":
        return (imm(args[1], 20) << 12) | (reg(args[0]) << 7) | 0x37
    if mnem in ("cspecialr", "cspecialrw"):
        scr = SCR[args[1].lower()]
        cs1 = reg(args[2]) if len(args) > 2 else 0
        return r_type(0x01, scr, cs1, 0, reg(args[0]), 0x5B)
    raise ValueError(f"unknown mnemonic: {mnem}")


def assemble(path):
    words = []
    with open(path) as f:
        for n, raw in enumerate(f, 1):
            line = raw.split("#", 1)[0].strip()
            if not line:
                continue
            try:
                words.append(encode(line))
            except (ValueError, KeyError, IndexError) as e:
                sys.exit(f"{path}:{n}: {e}")
    return words


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
    words = assemble(a.prog)
    code = b"".join(struct.pack("<I", w) for w in words)
    with open(a.out, "wb") as f:
        f.write(elf32(code, a.base, a.tohost))
    for i, w in enumerate(words):
        print(f"{a.base + 4 * i:08x}: {w:08x}")


if __name__ == "__main__":
    main()
