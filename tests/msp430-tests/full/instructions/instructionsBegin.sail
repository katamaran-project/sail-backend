//structure taken from https://github.com/riscv/sail-riscv/blob/master/model/riscv_insts_begin.sail

scattered union ast

val execute : ast -> unit
scattered function execute

val encdec : ast <-> bits(16)
scattered mapping encdec
