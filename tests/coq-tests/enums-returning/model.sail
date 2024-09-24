default Order dec

$include <prelude.sail>


enum MyEnum = { Foo, Bar, Baz }



val foo : unit -> MyEnum
function foo () = {
  Foo
}
