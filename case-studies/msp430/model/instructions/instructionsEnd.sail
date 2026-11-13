//structure taken from https://github.com/riscv/sail-riscv/blob/master/model/riscv_insts_begin.sail


union clause ast = DOESNOTUNDERSTAND : (bits(16))
mapping clause encdec = DOESNOTUNDERSTAND(a)
  <-> a:bits(16)

function clause execute(DOESNOTUNDERSTAND(a)) = {
  throw(undefindedInstruction(a))
}

end ast
end encdec
end execute