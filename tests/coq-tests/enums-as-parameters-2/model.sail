default Order dec

$include <prelude.sail>


enum Foo = { Bar, Baz }


val func : (Foo, Foo) -> int
scattered function func


function clause func(Bar, Bar) = 1
function clause func(Baz, Bar) = 2
function clause func(Bar, Baz) = 3
function clause func(Baz, Baz) = 4
