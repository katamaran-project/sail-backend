default Order dec

$include <prelude.sail>


val my_enum_to_int : int <-> bool
mapping my_enum_to_int = {
  0 <-> false,
  1 <-> true
}