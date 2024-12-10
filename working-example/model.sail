default Order dec

$include <prelude.sail>


val first : forall 'a 'k, 'k >= 1. vector('k, 'a) -> 'a
function first(n) = n[0]


val zeros : forall 'n, 'n > 0. implicit('n) -> bits('n)
function zeros(n) = sail_zero_extend(0b0, n)


register r0 : bits(5)
register r1 : bits(10)

register q0 : vector(5, int)
register q1 : vector(10, int)


val increment : unit -> unit
function increment() = {
  r0 = zeros();
  r1 = zeros()
}


val addition : unit -> int
function addition() = {
  first(q0) + first(q1)
}


function main() -> unit = {
  let _ = addition();
}
