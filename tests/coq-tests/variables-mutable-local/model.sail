default Order dec

$include <prelude.sail>

/*
  Assignments to mutable local variables (var) become muSail's stm_assign.
*/

/* An integer updated in both branches of an if. */
val count : (bool, bool) -> int
function count(a, b) = {
  var n : int = 0;
  if a then n = n + 1;
  if b then n = n + 2 else n = n + 10;
  n
}

/* A copy taken before the assignment keeps the old value. */
val old_and_new : int -> (int, int)
function old_and_new(x) = {
  var v : int = x;
  let old = v;
  v = v + 1;
  (old, v)
}

/* A parameter copied into a variable: assigning the variable leaves the parameter alone. */
val copy_of_parameter : int -> int
function copy_of_parameter(x) = {
  var v : int = x;
  v = 5;
  x + v
}

/* A bitvector updated bit by bit and slice by slice, as CHERIoT's capToEncCap. */
val flags : (bool, bool) -> bits(8)
function flags(a, b) = {
  var r : bits(8) = 0x00;
  if a then r[5] = bitone;
  if b then r[3 .. 2] = 0b11;
  r
}

/* A struct whose field slice is updated, as CHERIoT's CSpecialRW. */
struct Cap = {
  address : bits(32),
  tag     : bool,
}

val align : Cap -> Cap
function align(c) = {
  var c2 = c;
  if c2.tag then c2.address[1 .. 0] = 0b00;
  c2
}
