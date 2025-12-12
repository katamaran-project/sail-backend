// Simple top level fetch and execute loop.
val fetch_and_execute : unit -> unit

function fetch_and_execute () =
  while true do {
    try {
      logWithVerbosity(3,"pc: " + bits_str(PC_reg));
      old_PC_reg = PC_reg;
      let Word(instr) = fetch() in {
        logWithVerbosity(1,bits_str(old_PC_reg)+ "  " + bits_str(instr));
        let node:ast = encdec(instr) in {
          execute(node);
        }
      };
    } catch {
      _ => {
        let resTest :WordByte = readMem(WORD_INSTRUCTION,old_PC_reg);
        exit(());
    }
  }
}

val elf_entry = pure {
  ocaml: "Elf_loader.elf_entry",
  lem: "elf_entry",
  c: "elf_entry"
} : unit -> int

function main() : unit -> unit = {
  verbosity = __GetVerbosity();
  //print_bits("verbosity: =", verbosity);
  //print_endline("start");

  let b = get_slice_int(16, elf_entry(), 0);
  PC_reg = b;
  //print_bits("entry: ", b);
  fetch_and_execute ()
}


//TODO: figure out ram location -> 0x0200
//      figure out progra location -> 0x10000? -> 90% sure this is wrong
//      status register finsish at line 62
