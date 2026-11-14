"""
Analysis of one translation of the CHERIoT model (see docs/cheriot-loop.md).

The modules read the files of a run directory, in which `sail --katamaran`
translated the model: base.v, machine.v, the log, the Sail AST dumps and the
coqc logs.  They use only the standard library.

    items       the item record, and reading and writing items.csv
    extract     items from the generated .v files, the log and the coqc logs
    callgraph   the call graph of the rewritten Sail AST, and reachability
    dangling    calls in machine.v to functions that it does not define
    piles       sorting items into piles (root causes, as in the inventory)

The modules started as the scripts of the CHERIoT inventory of 13 Nov 2026
(cheriot-inventory/20261113-1408/INVENTORY.md).
"""
