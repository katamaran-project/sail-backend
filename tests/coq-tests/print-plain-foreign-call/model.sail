default Order dec

$include <prelude.sail>

/*
   Calls to externs become foreign calls (stm_foreign). With pretty-print-function-calls,
   they use Katamaran's foreign notation,

     (foreign combine (exp_var "ga#0") (exp_var "ga#1"))%exp

   and without it (print-plain-foreign-call, same model), an environment:

     stm_foreign combine (env.snoc (env.snoc (env.nil) (_::_) ((exp_var "ga#0"))%exp) (_::_) ((exp_var "ga#1"))%exp)

   Covered: one and two arguments, a monadic extern, an extern that is polymorphic in a width
   (Rocq infers its implicit argument), and a call to a Sail function next to them.
*/

val read_device = monadic {_: "read_device"} : bits(32) -> bits(32)
val combine     = pure {_: "combine"} : (bits(32), bits(32)) -> bits(32)
val popcount    = pure {_: "my_popcount"} : forall 'n, 'n >= 1. bits('n) -> int

val read_two : (bits(32), bits(32)) -> bits(32)
function read_two(a, b) = combine(read_device(a), read_device(b))

val ones_in_byte : bits(8) -> int
function ones_in_byte(b) = popcount(b)

val ones_in_both : (bits(8), bits(8)) -> int
function ones_in_both(x, y) = ones_in_byte(x) + popcount(y)
