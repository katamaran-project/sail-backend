default Order dec

$include <prelude.sail>


default Order dec

$include <prelude.sail>


union Foo = {
  FooX : int,
  FooY  : int
}

union Bar = {
  BarX : Foo,
  BarY : Foo
}
  
val foo : Bar -> int
function foo(x) = match x {
  BarX(FooX(n)) => n,
  BarX(FooY(n)) => n,
  BarY(FooX(n)) => n,
  BarY(FooY(n)) => n
}


function main() -> unit = {
  let _ = foo(BarX(FooY(2)));
}
