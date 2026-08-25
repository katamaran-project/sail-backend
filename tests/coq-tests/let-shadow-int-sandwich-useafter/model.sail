default Order dec

$include <prelude.sail>


val foo : unit -> int
function foo() = {
  let x = 1 in
  {
    {
      let y = x + 1 in
      let x = 10 in
      ()
    };
    x
  }
}
