default Order dec

$include <prelude.sail>


type word    = int
type address = word

enum regid      = { regid1, regid2, regid3, regid4 }
enum Mode       = { Running, Halted }
enum Comparison = { Less, Equal, Greater }

register r1 : word
register r2 : word
register r3 : word
register r4 : word

register pc : address
register cmp : Comparison


union Instruction = {
    Read       : (regid, regid),
    Write      : (regid, regid),
    Compare    : (regid, regid)
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


val read_register : regid -> word
function read_register(r) = {
  match r {
    regid1 => r1,
    regid2 => r2,
    regid3 => r3,
    regid4 => r4
  }
}


val write_register : (regid, word) -> unit
function write_register(r, w) = {
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
  let address = read_register(r_address);
  let word = read_memory(address);
  write_register(r_target, word);
  next_instruction();
  Running
}

function clause execute(Write(r_address, r_word)) = {
  let address = read_register(r_address);
  let word = read_register(r_word);
  write_memory(address, word);
  next_instruction();
  Running
}

function clause execute(Compare(x, y)) = {
  let v1 = read_register(x);
  let v2 = read_register(y);
  if v1 < v2 then cmp = Less
  else if v1 > v2 then cmp = Greater
  else cmp = Equal;
  next_instruction();
  Running
}