default Order dec

$include <prelude.sail>


union MyUnion = {
    Foo : int,
    Bar : unit,
    Baz : (int, int)
}

type Alias = MyUnion

