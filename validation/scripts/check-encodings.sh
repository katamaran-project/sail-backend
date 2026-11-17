#!/usr/bin/env bash
# Standard RV32I/M/Zicsr/C encodings of mk-test-elf.py = llvm-mc's (Homebrew LLVM).
# CHERIoT encodings: only the compressed loads and stores (below); the rest is in validate.md.
#   validation/scripts/check-encodings.sh [LLVM_BIN]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
llvm=${1:-/opt/homebrew/opt/llvm/bin}
[[ -x $llvm/llvm-mc ]] || { echo "no llvm-mc in $llvm: skipped"; exit 0; }
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

cat > "$tmp/t.s" <<'EOF'
start:
add a0, a1, a2
sub t0, t1, t2
sll s0, s1, a0
slt a0, a1, a2
sltu a0, a1, a2
xor a0, a1, a2
srl a0, a1, a2
sra a0, a1, a2
or a0, a1, a2
and a0, a1, a2
mul a0, a1, a2
mulh a0, a1, a2
mulhsu a0, a1, a2
mulhu a0, a1, a2
div a0, a1, a2
divu a0, a1, a2
rem a0, a1, a2
remu a0, a1, a2
addi a0, a1, -1
addi a0, a1, 2047
addi a0, a1, -2048
slti a0, a1, 5
sltiu a0, a1, 5
xori a0, a1, -1
ori a0, a1, 0x7f
andi a0, a1, 0xff
slli a0, a1, 31
srli a0, a1, 1
srai a0, a1, 7
lui a0, 0x80001
lui a0, 0xfffff
lb a0, -4(a1)
lh a0, 2(a1)
lw a0, 0x100(a1)
lbu a0, 0(a1)
lhu a0, 2047(a1)
sb a0, -2048(a1)
sh a0, 2(a1)
sw a0, 0x100(a1)
fwd:
beq a0, a1, fwd
bne a0, a1, start
blt a0, a1, end
bge a0, a1, fwd
bltu a0, a1, start
bgeu a0, a1, end
jal ra, end
jal zero, start
jalr ra, 8(a0)
jalr zero, -4(t0)
csrrw a0, mscratch, a1
csrrs a0, mcause, zero
csrrc a0, mstatus, a1
csrrwi a0, mscratch, 5
csrrsi a0, mstatus, 8
csrrci a0, mstatus, 31
csrrw zero, mtval, t0
ecall
ebreak
mret
nop
wfi
sret
fence
fence rw, rw
fence r, w
fence iorw, o
fence.i
fence.tso
sfence.vma
sfence.vma a0, a1
csrrs a0, cycle, zero
csrrs a0, instret, zero
end:
add zero, zero, zero
EOF

# C: a file of its own, as llvm-mc with +c compresses the instructions above.
# An even number of 16-bit parcels, so that the words line up.
cat > "$tmp/c.s" <<'EOF'
cstart:
c.addi a0, 1
c.addi a1, -1
c.addi s0, 31
c.addi a5, -32
c.nop
c.addi t0, 7
c.li a0, 0
c.li t1, 31
c.li s1, -32
c.lui a0, 1
c.lui ra, 31
c.lui t2, 0xfffe0
c.lui a3, 0xfffff
c.addi16sp sp, 16
c.addi16sp sp, 496
c.addi16sp sp, -512
c.addi4spn a0, sp, 4
c.addi4spn s1, sp, 1020
c.srli a0, 1
c.srli s0, 31
c.srai a1, 7
c.andi a2, -1
c.andi a3, 0
c.andi s1, -32
c.sub a0, a1
c.xor s0, s1
c.or a2, a3
c.and a5, s0
c.mv a0, a1
c.mv t6, ra
c.add a0, a1
c.add s11, t0
c.lw a0, 0(a1)
c.lw s1, 124(a5)
c.sw a2, 4(s0)
c.sw a3, 64(s1)
c.lwsp a0, 0(sp)
c.lwsp t0, 252(sp)
c.swsp a1, 4(sp)
c.swsp t6, 252(sp)
c.slli a0, 1
c.slli t3, 31
c.jr a0
c.jalr t0
c.ebreak
c.j cstart
c.j cend
c.jal cstart
c.jal cend
c.beqz a0, cstart
c.bnez s1, cend
c.beqz a5, 254
c.bnez a0, -256
c.j 2046
c.j -2048
cend:
c.nop
EOF

# CHERIoT C: llvm-mc does not know them; c.clc, c.csc, c.clcsp, c.cscsp have the
# layout of c.fld, c.fsd, c.fldsp, c.fsdsp with funct3 bit 1 set (bit 14).
cat > "$tmp/cc.s" <<'EOF'
c.clc ca0, 0(cs0)
c.clc cs1, 248(ca5)
c.csc ca1, 8(ca2)
c.csc ca5, 248(cs1)
c.clcsp cra, 0(csp)
c.clcsp ct6, 504(csp)
c.cscsp ca0, 8(csp)
c.cscsp cs11, 504(csp)
EOF
cat > "$tmp/cc-llvm.s" <<'EOF'
c.fld fa0, 0(s0)
c.fld fs1, 248(a5)
c.fsd fa1, 8(a2)
c.fsd fa5, 248(s1)
c.fldsp ft1, 0(sp)
c.fldsp ft11, 504(sp)
c.fsdsp fa0, 8(sp)
c.fsdsp fs11, 504(sp)
EOF
# c.cincaddr16csp, c.cincaddr4cspn, c.cjal, c.cjr, c.cjalr: the encodings of
# c.addi16sp, c.addi4spn, c.jal, c.jr, c.jalr (mk-test-elf.py shares the code).

compare() {  # compare FILE MATTR [LLVM_FILE [XOR]]: mk-test-elf.py's 16-bit parcels = llvm-mc's
             # (each XOR'd with XOR); prints the number of parcels
  "$here/mk-test-elf.py" "$tmp/$1" "$tmp/ours.elf" | cut -d' ' -f2 \
    | sed -E 's/(....)(....)/\2\n\1/' > "$tmp/ours.txt"
  "$llvm/llvm-mc" --triple=riscv32 -mattr="$2" -filetype=obj "$tmp/${3:-$1}" -o "$tmp/llvm.o"
  "$llvm/llvm-objcopy" -O binary --only-section=.text "$tmp/llvm.o" "$tmp/llvm.bin"
  od -An -v -tx2 "$tmp/llvm.bin" | tr -s ' ' '\n' | sed '/^$/d' \
    | while read -r h; do printf '%04x\n' $((0x$h ^ ${4:-0})); done > "$tmp/llvm.txt"
  diff -u "$tmp/llvm.txt" "$tmp/ours.txt" >&2 || exit 1
  wc -l < "$tmp/ours.txt" | tr -d ' '
}
parcels=$(compare t.s +m,+zifencei,-relax)
cparcels=$(compare c.s +c,-relax)
ccparcels=$(compare cc.s +c,+d,-relax cc-llvm.s 0x4000)
echo "encodings equal to llvm-mc: $((parcels / 2)) words (RV32I/M/Zicsr)," \
  "$cparcels parcels (C), $ccparcels parcels (CHERIoT C)"
