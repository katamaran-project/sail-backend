default Order dec

$include <prelude.sail>

/*
   (exclude-functions-as-unconstrained '("pmp_allows" "privilege_of")) removes
   pmp_allows and privilege_of, and turns each call into an unconstrained value
   of the call's type: a call to the foreign function `unconstrained σ`, whose
   semantics allows every value of σ. A proof about checked_read must then hold
   whatever pmp_allows returns, which is how undefined values are treated too.
   pmp_allows's body has a loop, which the translator does not support.
*/

enum Privilege = { User, Machine }

val privilege_of : bits(2) -> Privilege

val pmp_allows : (bits(32), bool) -> bool
function pmp_allows(addr, write) = {
  var ok : bool = false;
  foreach (i from 0 to 3) {
    ok = ok | addr == sail_zero_extend(0x0, 32)
  };
  ok
}

val checked_read : bits(32) -> bits(8)
function checked_read(addr) =
  if pmp_allows(addr, false) then 0x00 else 0xFF

val is_machine : bits(2) -> bool
function is_machine(p) =
  match privilege_of(p) {
    User    => false,
    Machine => true
  }
