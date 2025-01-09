default Order dec

$include <prelude.sail>


enum MyEnum = {
  Foo,
  Bar
}


val my_function : (MyEnum, MyEnum) -> string
scattered function my_function

function clause my_function(Foo, Foo) = "A"
function clause my_function(Foo, x)   = "B"
function clause my_function(Bar, Foo) = "C"
function clause my_function(Bar, _)   = "D"
