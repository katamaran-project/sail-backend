default Order dec
$include <prelude.sail>

type xlen : Int = 32

enum word_width = { WORD, DOUBLE }

register r32 : bits(32)
register r64 : bits(64)

val store32 : bits(32) -> unit
function store32(v) = r32 = v

val store64 : bits(64) -> unit
function store64(v) = r64 = v

// sail-riscv's execute_STORE: Sail checks the guarded arm assuming xlen >= 64, so it passes a
// bits(xlen) where a bits(64) is expected; the guard becomes an if, false at compile time
val store : (word_width, bits(xlen)) -> unit
function store(width, v) = match width {
  WORD                          => store32(v),
  DOUBLE if sizeof(xlen) >= 64  => store64(v),
  _                             => ()
}
