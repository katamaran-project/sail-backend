default Order dec

$include <prelude.sail>


val foo : unit -> bits(1)
function foo() = {
  let x = 1 in
  {
    {
      let y = x + 1 in
      let x = 10 in
      ()
    };
    sail_ones(x)
  }
}
