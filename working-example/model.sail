default Order dec

$include <prelude.sail>


register reg : int = 0


val increment : unit -> unit
function increment() = {
  reg = reg + 1
}

function main() -> unit = {
  increment()
}
