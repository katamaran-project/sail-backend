default Order dec

$include <prelude.sail>


enum Instruction = { Increment, Decrement }

register eax : int
register eip : int


val execute : Instruction -> unit
scattered function execute


function clause execute(Increment) = {
    eax = eax + 1;
    eip = eip + 1;
}

function clause execute(Decrement) = {
    eax = eax - 1;
    eip = eip + 1;
}