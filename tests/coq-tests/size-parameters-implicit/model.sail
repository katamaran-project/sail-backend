default Order dec

$include <prelude.sail>

/*
   Functions polymorphic in the width of a bitvector (or in a type) are one Fun constructor
   each, with the width (or type) as an implicit argument, instead of one copy per width:

     | extend_to {kid__m : nat} {kid__n : nat} : Fun [...] (ty.bvec (kid__m))

   Rocq infers the arguments at each call. Inside such a function, operations whose side
   conditions mention the width (uop.zext needs n <= m) are foreign calls to the extern
   (sail_zero_extend), which needs no side condition.
   A function whose result type alone mentions a type variable (fail_with : ... -> 'a) is
   called for its effect with that type fixed to unit.
   A bit is a bitvector(1) (Sail's library: type bit = bitvector(1)).
*/

val extend_to : forall 'n 'm, 'm >= 'n. (int('m), bits('n)) -> bits('m)
function extend_to(m, v) = sail_zero_extend(v, m)

val extend_byte : bits(8) -> bits(32)
function extend_byte(b) = extend_to(32, b)

val extend_half : bits(16) -> bits(64)
function extend_half(h) = extend_to(64, h)

val fail_with : forall ('a : Type). string -> 'a
function fail_with(message) = exit()

val check : bool -> unit
function check(ok) = if not_bool(ok) then fail_with("not ok")

val stop_then_continue : unit -> int
function stop_then_continue() = {
  fail_with("stop");
  0
}

register flag : bit

val clear_flag : unit -> unit
function clear_flag() = flag = 0b0

val set_flag : unit -> unit
function set_flag() = flag = bitone

val shift_by_size : forall 'n, 'n > 0. (int('n), bits(32)) -> bits(32)
function shift_by_size(n, v) = sail_shiftleft(v, n)
