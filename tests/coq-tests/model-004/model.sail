default Order dec

$include <prelude.sail>


type address = int


enum Mode = { Running, Halted }

union Instruction = {
    Increment  : unit,
    Decrement  : unit,
    JumpIfZero : int,
    Halt       : unit
}

register eax : int
register eip : int



val execute : Instruction -> Mode
scattered function execute


function clause execute(Increment()) = {
    eax = eax + 1;
    eip = eip + 1;
    Running
}

function clause execute(Decrement()) = {
    eax = eax - 1;
    eip = eip + 1;
    Running
}

function clause execute(JumpIfZero(delta)) = {
    if eax == 0 then {
      eip = eip + delta;
    }
    else {
      eip = eip + 1;
    };
    Running
}

function clause execute(Halt()) = {
    Halted
}