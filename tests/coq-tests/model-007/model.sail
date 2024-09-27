default Order dec

$include <prelude.sail>


type word    = int
type address = word

enum regid      = { regid1, regid2, regid3, regid4 }
enum Mode       = { Running, Halted }
enum Comparison = { Less, Equal, Greater }

struct Capability = {
  range_start    : address,
  range_end      : address,
  pointer        : address,
  read_access    : bool,
  write_access   : bool,
  execute_access : bool,
}

union RegisterContents = {
  CapVal    : Capability,
  SimpleVal : word
}

register r1 : RegisterContents
register r2 : RegisterContents
register r3 : RegisterContents
register r4 : RegisterContents

register pc  : Capability
register cmp : Comparison


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
    pc.pointer = pc.pointer + 1
}


val read_register : regid -> RegisterContents
function read_register(r) = {
  match r {
    regid1 => r1,
    regid2 => r2,
    regid3 => r3,
    regid4 => r4
  }
}


val write_register : (regid, RegisterContents) -> unit
function write_register(r, w) = {
  match r {
    regid1 => r1 = w,
    regid2 => r2 = w,
    regid3 => r3 = w,
    regid4 => r4 = w
  }
}


val is_in_range : Capability -> bool
function is_in_range(cap) = {
  (cap.range_start <= cap.pointer) & (cap.pointer <= cap.range_end)
}


val execute : Instruction -> Mode
scattered function execute


function clause execute(Read(r_address, r_target)) = {
  match read_register(r_address) {
    CapVal(capability) => {
      if is_in_range(capability) & capability.read_access then {
        let value = read_memory(capability.pointer);
        write_register(r_target, SimpleVal(value));
        next_instruction ();
        Running
      }
      else Halted
    },
    SimpleVal(_) => Halted
  }
}

function clause execute(Write(r_address, r_word)) = {
  match read_register(r_address) {
    SimpleVal(_) => Halted,
    CapVal(capability) => {
      if is_in_range(capability) & capability.write_access then {
        match read_register(r_word) {
          CapVal(_) => Halted,
          SimpleVal(v) => {
            write_memory(capability.pointer, v);
            next_instruction ();
            Running
          }
        }
      }
      else Halted
    }
  }
}

function clause execute(Addition(source1, source2, target)) = {
  match read_register(source1) {
    CapVal(_) => Halted,
    SimpleVal(v1) => {
      match read_register(source2) {
        CapVal(_) => Halted,
        SimpleVal(v2) => {
          let sum = v1 + v2;
          write_register(target, SimpleVal(sum));
          next_instruction ();
          Running
        }
      }
    }
  }
}

function clause execute(Compare(x, y)) = {
  match read_register(x) {
    CapVal(_) => Halted,
    SimpleVal(v1) => {
      match read_register(y) {
        CapVal(_) => Halted,
        SimpleVal(v2) => {
          if v1 < v2
          then cmp = Less
          else if v1 > v2
          then cmp = Greater
          else cmp = Equal;
          next_instruction ();
          Running
        }
      }
    }
  }
}

function clause execute(JumpIfLess(r_target)) = {
  match read_register(r_target) {
    SimpleVal(_) => Halted,
    CapVal(capability) => {
      match cmp {
        Less => pc = capability,
        _    => next_instruction ()
      };
      Running
    }
  }
}

function clause execute(Halt()) = {
  Halted
}

