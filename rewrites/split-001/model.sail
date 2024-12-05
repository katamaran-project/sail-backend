default Order dec

$include <prelude.sail>


union MyUnion = {
    Foo : unit,
    Bar : unit
}

val execute : MyUnion -> unit
scattered function execute

function clause execute(Foo()) = ()
function clause execute(Bar()) = ()
