default Order dec

$include <prelude.sail>


struct MyStruct = {
  foo : int,
  bar : bool
}


val baz : unit -> MyStruct
function baz() = {
  struct { foo=1, bar=false }
}
