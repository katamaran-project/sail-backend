default Order dec

$include <prelude.sail>

/*
  Early returns. Sail's early_return rewrite pulls a return out of the
  positions where the rest of the function does not run after it, so that
  the function body computes the returned value directly.
*/

/* A return at the end of a block, after assignments to a variable, as CHERIoT's capToEncCap. */
struct Enc = {
  perms : bits(4),
  addr  : bits(8),
}

val encode : (bool, bool, bits(8)) -> Enc
function encode(a, b, addr) = {
  var perms : bits(4) = 0x0;
  if a then perms[3] = bitone;
  if b then perms[1 .. 0] = 0b11 else perms[1 .. 0] = 0b01;
  return struct {
    perms = perms,
    addr = addr
  }
}

/* A return of a computed value at the end of the body. */
val double : int -> int
function double(x) = {
  let y = x + x;
  return y
}

/* Returns in both branches of an if. */
val max_int : (int, int) -> int
function max_int(x, y) = {
  if x > y then return x else return y
}

/* An early return in one branch: the rest of the block becomes the other branch. */
val clamp : int -> int
function clamp(x) = {
  if x < 0 then return 0;
  let y = x + 1;
  y
}

/* Returns in the arms of a match. */
enum Color = { Red, Green, Blue }

val code : Color -> bits(2)
function code(c) = {
  match c {
    Red   => return 0b01,
    Green => return 0b10,
    Blue  => return 0b11
  }
}

/* An early return in a branch, followed by a return at the end. */
val sign : int -> int
function sign(x) = {
  if x == 0 then return 0;
  if x > 0 then return 1;
  return -1
}

/* An early return after a register write: the writes after the if run only in the other branch. */
register R : int

val write_then_return : int -> int
function write_then_return(x) = {
  if x < 0 then { R = 1; return 0 } else { R = 2 };
  R = 3;
  x
}
