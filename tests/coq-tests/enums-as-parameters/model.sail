default Order dec

$include <prelude.sail>


enum Foo = { Bar, Baz }


val func : Foo -> int
scattered function func


function clause func(Bar) = 1
function clause func(Baz) = 2
