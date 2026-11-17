default Order dec
$include <prelude.sail>

// the number of leading zeros, as sail-riscv's CLZ: two updated variables, counting down
val clz : bits(32) -> nat
function clz(v) = {
  result : nat = 0;
  done : bool = false;
  foreach (i from 31 downto 0)
    if not_bool(done) then if v[i] == bitzero
                           then result = result + 1
                           else done = true;
  result
}
