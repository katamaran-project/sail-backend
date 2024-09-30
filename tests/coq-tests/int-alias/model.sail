default Order dec

$include <prelude.sail>


type number = int


val add : (number, number) -> number
function add(x, y) = {
  x + y
}