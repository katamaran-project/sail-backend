default Order dec

$include <prelude.sail>

/*
   (exclude-functions-as-failure '("translate32")) removes translate32 and turns
   each call into a failure (stm_fail), for code that must be unreachable:
   CHERIoT has no virtual memory, so its Sv32 address translation is never called.
   translate32's body has a loop, which the translator does not support; with the
   exclusion, it is not translated at all.
*/

enum SATPMode = { Bare, Sv32 }

register satp_mode : SATPMode

val translate32 : bits(32) -> bits(34)
function translate32(va) = {
  var acc : bits(34) = sail_zero_extend(va, 34);
  foreach (i from 0 to 1) {
    acc = acc + (0b00 @ va)
  };
  acc
}

val translateAddr : bits(32) -> bits(34)
function translateAddr(va) =
  match satp_mode {
    Bare => sail_zero_extend(va, 34),
    Sv32 => translate32(va)
  }

val translate_only : bits(32) -> bits(34)
function translate_only(va) = translate32(va)
