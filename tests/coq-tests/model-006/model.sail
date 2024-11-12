default Order dec

$include <prelude.sail>


type word    = int
type address = word

enum regid      = { regid1, regid2, regid3, regid4 }
enum Mode       = { Running, Halted }
enum Comparison = { Less, Equal, Greater }

register r1 : word = 0
register r2 : word = 0
register r3 : word = 0
register r4 : word = 0

register pc  : address    = 0
register cmp : Comparison = Equal


union Instruction = {
    Read       : (regid, regid),
    Write      : (regid, regid),
    Addition   : (regid, regid, regid),
    Compare    : (regid, regid),
    JumpIfLess : regid,
    Halt       : unit
}


val read_memory : address -> word
function read_memory ptr = {
  0
}

val write_memory : (address, word) -> unit
function write_memory (ptr, value) = {
  ()
}


val next_instruction : unit -> unit
function next_instruction() = {
    pc = pc + 4
}


val my_read_register : regid -> word
function my_read_register(r) = {
  match r {
    regid1 => r1,
    regid2 => r2,
    regid3 => r3,
    regid4 => r4
  }
}


val my_write_register : (regid, word) -> unit
function my_write_register(r, w) = {
  match r {
    regid1 => r1 = w,
    regid2 => r2 = w,
    regid3 => r3 = w,
    regid4 => r4 = w
  }
}


val execute : Instruction -> Mode
scattered function execute


function clause execute(Read(r_address, r_target)) = {
  let address = my_read_register(r_address);
  let word = read_memory(address);
  my_write_register(r_target, word);
  next_instruction();
  Running
}

function clause execute(Write(r_address, r_word)) = {
  let address = my_read_register(r_address);
  let word = my_read_register(r_word);
  write_memory(address, word);
  next_instruction();
  Running
}

function clause execute(Addition(source1, source2, target)) = {
  let value1 = my_read_register(source1);
  let value2 = my_read_register(source2);
  let sum = value1 + value2;
  my_write_register(target, sum);
  next_instruction();
  Running
}

function clause execute(Compare(x, y)) = {
  let v1 = my_read_register(x);
  let v2 = my_read_register(y);
  if v1 < v2 then cmp = Less
  else if v1 > v2 then cmp = Greater
  else cmp = Equal;
  next_instruction();
  Running
}

function clause execute(JumpIfLess(r_target)) = {
  let target = my_read_register(r_target);
  match cmp {
    Less => pc = target,
    _    => next_instruction ()
  };
  Running
}

function clause execute(Halt()) = {
  Halted
}

