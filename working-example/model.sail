default Order dec

$include <prelude.sail> 


enum foo = A | B


val bar : foo -> bool
function bar(tralala : foo) = {
  match tralala {
    A => true,
    B => false
  }
}