default Order dec

$include <prelude.sail>


union Instruction = {
    Increment  : unit,
    Decrement  : unit,
    JumpIfZero : int
}

register eax : int = 0
register eip : int = 0


val execute : Instruction -> unit
scattered function execute


function clause execute(Increment()) = {
    eax = eax + 1;
    eip = eip + 1;
}

function clause execute(Decrement()) = {
    eax = eax - 1;
    eip = eip + 1;
}

function clause execute(JumpIfZero(delta)) = {
    if eax == 0 then {
      eip = eip + delta;
    }
    else {
      eip = eip + 1;
    }
}
