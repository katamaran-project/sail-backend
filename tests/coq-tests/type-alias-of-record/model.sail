default Order dec

$include <prelude.sail>


struct MyRecord = {
    Foo : int,
    Bar : unit,
    Baz : int
}

type Alias = MyRecord
