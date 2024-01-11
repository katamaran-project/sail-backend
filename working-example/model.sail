default Order dec

$include <prelude.sail> 


enum foo = A | B


val bar : foo -> bool
function bar(x : foo) = {
  match x {
    A => true,
    B => false
  }
}