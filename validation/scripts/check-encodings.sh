#!/usr/bin/env bash
# Standard RV32I/M/Zicsr encodings of mk-test-elf.py = llvm-mc's (Homebrew LLVM).
# CHERIoT encodings are not checked here (llvm-mc does not know them): see validate.md.
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
end:
add zero, zero, zero
EOF

"$here/mk-test-elf.py" "$tmp/t.s" "$tmp/ours.elf" | cut -d' ' -f2 > "$tmp/ours.txt"
"$llvm/llvm-mc" --triple=riscv32 -mattr=+m,-relax -filetype=obj "$tmp/t.s" -o "$tmp/llvm.o"
"$llvm/llvm-objcopy" -O binary --only-section=.text "$tmp/llvm.o" "$tmp/llvm.bin"
od -An -v -tx4 "$tmp/llvm.bin" | tr -s ' ' '\n' | sed '/^$/d' > "$tmp/llvm.txt"
diff -u "$tmp/llvm.txt" "$tmp/ours.txt"
echo "encodings: $(wc -l < "$tmp/ours.txt" | tr -d ' ') words equal to llvm-mc"
