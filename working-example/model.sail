default Order dec

$include <prelude.sail>


val zeros : forall 'n, 'n > 0. implicit('n) -> bits('n)
function zeros(n) = sail_zero_extend(0b0, n)


register r0 : bits(5)
register r1 : bits(10)


val increment : unit -> unit
function increment() = {
  r0 = zeros();
  r1 = zeros()
}

function main() -> unit = {
  increment()
}
