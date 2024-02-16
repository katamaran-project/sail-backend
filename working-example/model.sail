default Order dec

$include <prelude.sail>

enum Permission = O | R | RW | E

type address     = bits(64)

struct Capability = {
  cap_permission: Permission,
  cap_begin:      address,
  cap_end:        address,
  cap_cursor:     address,
}

register PC : Capability

val nextPC : unit -> Capability
function nextPC() = {
  let cursor = PC.cap_cursor in
  {PC with cap_cursor = cursor + 4}
}

/* val nextPC : unit -> address */
/* function nextPC() = { */
/*   let cursor = PC.cap_cursor in */
/*   cursor + 4 */
/* } */


/* enum foo = AAAA | BBBB | CCCC */

/* union ast = { */
/*   Add : (int, int), */
/*   Lit : int */
/* } */

/* register R1 : foo */
/* register R2 : foo */
/* register R3 : foo */
/* register R4 : foo */


/* val bar : int -> int */
/* function bar(x) = { */
/*   match R1 { */
/*     AAAA => 0, */
/*     BBBB => 1, */
/*     CCCC => 2 */
/*   } */
/* } */


/* val qux : list(int) -> int */
/* function qux(xs : list(int)) = { */
/*   match xs { */
/*     [||] => 1, */
/*     h::t => 2 */
/*   } */
/* } */

/* mapping double_map : int <-> foo = { */
/*   1 <-> AAAA, */
/*   2 <-> BBBB, */
/*   3 <-> CCCC */
/* } */

// mapping clause instruction_mapping = ADD(n) <-> n @ 0b001
