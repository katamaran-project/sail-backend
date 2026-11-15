"""
Analysis of one translation of the CHERIoT model (see case-studies/cheriot/docs/loop.md).

The modules read the files of a run directory, in which `sail --katamaran`
translated the model: base.v, machine.v, the log, the Sail AST dumps and the
coqc logs.  They use only the standard library.

    items       the item record, and reading and writing items.csv
    extract     items from the generated .v files, the log and the coqc logs
    callgraph   the call graph of the rewritten Sail AST, and reachability
    dangling    calls in machine.v to functions that it does not define
    piles       sorting items into piles, by cause

The modules started as the scripts of a first inventory of the CHERIoT translation.
"""
