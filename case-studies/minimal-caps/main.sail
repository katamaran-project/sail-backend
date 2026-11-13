default Order dec

$include <prelude.sail>
$include <string.sail>
$include "prelude_minimal_caps.sail"
$include "capabilities.sail"
$include "instructions.sail"

function wordToString(w) : word -> string = {
  match w {
    Cap(c) => concat_str("[Capability] ", capToString(c)),
    Num(n) => concat_str("[Integer]    ", bits_str(n))
  }
}

register instr_count : int

overload print = { print_endline }

function print_register_dump() : unit -> unit = {
  //print(concat_str("PC: ", wordToString(Cap(PC))));
  //foreach (i from 0 to 3)  // TODO: use length function (currently gives error in gcc compilation)
    //print(concat_str(concat_str(regname(i), ": "), wordToString(*(registers[i]))))
    ()
}

val cycle_limit_reached = pure { ocaml: "cycle_limit_reached", c: "cycle_limit_reached" } : unit -> bool
function cycle_limit_reached() = false

val elf_entry = pure {
  ocaml: "Elf_loader.elf_entry",
  lem: "elf_entry",
  c: "elf_entry"
} : unit -> int

val init_registers : address -> unit
function init_registers(initialPC) = {
  PC = default_capability;
  PC.cap_cursor = initialPC;
  R0 = Cap(default_capability);
}

function fetch_and_execute() : unit -> bool = {
  if readAllowed(PC.cap_permission) & withinBounds(PC) then {
    let instr = MEMri(PC.cap_cursor);
    instr_count = instr_count + 1;
    print_bits("pc: ", PC.cap_cursor);
    print_bits("instr: ", instr);
    let loop_again : bool = match decode(instr) {
      Some(ast) => execute(ast),
      None() => { print("invalid instruction"); false }
    };
    loop_again & not_bool (cycle_limit_reached())
  }
  else
    false
}

function main() : unit -> unit = {
  instr_count = 0;
  init_registers(get_slice_int(64, elf_entry(), 0));
  while (fetch_and_execute()) do
    ();
  print("Machine Output");
  print_register_dump();
  print(concat_str_dec("Instruction Count: ", instr_count))
}
