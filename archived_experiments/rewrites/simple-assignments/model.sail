default Order dec

$include <prelude.sail>


struct MyStruct = {
  foo : int,
  bar : bool
}


register r0 : MyStruct


val qux : unit -> unit
function qux() = {
  r0.foo = 0
}
