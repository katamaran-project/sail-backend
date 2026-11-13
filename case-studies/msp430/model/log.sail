$include <string.sail>

register verbosity : bits(64)
register old_PC_reg  : wordBits /* program counter    */

val regName: (Register) -> string
function regName(reg) = {
    match reg {
        PC    =>    "PC_reg",
        SP    =>    "SP_reg",
        SRCG1 => "SRCG1_reg",
        CG2   =>   "CG2_reg",
        R4    =>    "R4_reg",
        R5    =>    "R5_reg",
        R6    =>    "R6_reg",
        R7    =>    "R7_reg",
        R8    =>    "R8_reg",
        R9    =>    "R9_reg",
        R10   =>   "R10_reg",
        R11   =>   "R11_reg",
        R12   =>   "R12_reg",
        R13   =>   "R13_reg",
        R14   =>   "R14_reg",
        R15   =>   "R15_reg",
    }
}

//TODO this can be done folowing the methids from the specification
//example: 0(R1)

val AMstring: (AM) -> string
function AMstring(am) = {
    match am {
        REGISTER_MODE  => "Register mode",
        INDEXED_MODE  => "Indexed mode",
        INDIRECT_REGISTER_MODE  => "Indirect register mode",
        INDIRECT_AUTOINCREMENT_MODE  => "Indirect Autoincrement Mode",
    }
}

val BWstring: (BW) -> string
function BWstring(bw) = {
    match bw {
        WORD_INSTRUCTION  => ".w",
        BYTE_INSTRUCTION  => ".b",
    }
}

val BWSizeString: (BW) -> string
function BWSizeString(bw) = {
    match bw {
        WORD_INSTRUCTION  => ".16",
        BYTE_INSTRUCTION  => ".8",
    }
}

val duopName: (doubleop) -> string
function duopName(op) = {
    match op {
        MOV  => "MOV",
        ADD  => "ADD",
        ADDC => "ADDC",
        SUB  => "SUB",
        SUBC => "SUBC",
        CMP  => "CMP",
        DADD => "DADD",
        BIT  => "BIT",
        BIC  => "BIC",
        BIS  => "BIS",
        XOR  => "XOR",
        AND  => "AND",
    }
}
val singleopName: (singleop) -> string
function singleopName(op) = {
    match op {
        RRC  => "RRC",
        RRA  => "RRA",
        PUSH => "PUSH",
        SWPB => "SWPB",
        CALL => "CALL",
        RETI => "RETI",
        SXT  => "SXT",
    }
}
val jumpName: (jump) -> string
function jumpName(op) = {
    match op {
        JEQ => "JEQ",
        JNE => "JNE",
        JC  => "JC",
        JNC => "JNC",
        JN  => "JN",
        JGE => "JGE",
        JL  => "JL",
        JMP => "JMP"
        }
}


/* [not used]
val log_bits : forall 'n. (string, bits('n)) -> unit
function log_bits(s, b) =
  if verbosity[0] == bitone then print_bits(s, b) */

val logWithVerbosity: forall 'n, 'n < 64 & 'n >= 0. (int('n),string) -> unit
function logWithVerbosity(n,s) = {
    if verbosity[n..n] == 0b1 then
    print_endline(s);
}
