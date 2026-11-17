# State format

The text form of a CHERIoT processor state that both sides print, so that two states compare with `diff -u`.
Rocq: `validation/rocq/Dump.v`. Emulator: `validation/emulator/dump_state.c`. Files: `validation/compare/statefile.py`.

## Grammar

```
state     ::= reg-line* mem-line* tag-line*
reg-line  ::= key SP value LF
mem-line  ::= "mem" SP addr SP byte LF
tag-line  ::= "tag" SP addr SP bool LF
key       ::= seg ("." seg)*                 seg ::= [A-Za-z0-9_]+
addr      ::= "0x" hexdigit{8,}
byte      ::= "0x" hexdigit{2}
hexdigit  ::= [0-9a-f]
run       ::= (step SP line)*                step ::= "0" | [1-9][0-9]*
```

- Register lines sorted bytewise by key (`LC_ALL=C sort`), then `mem` and `tag` by address. Each key once.
- Run file: step 0 is the state after reset, step i after the i-th `step`.

## Values

| type | line(s) | example |
|---|---|---|
| `bits(n)` | `0x` and ceil(n/4) digits | `minstret 0x0000000000000001` |
| `bool` | `true`, `false` | `htif_done false` |
| `int` | decimal | |
| enum | constructor name | `cur_privilege Machine` |
| struct | one line per field, `key.field` | `x1.address 0x00000005` |
| union | `key Ctor`, payload under `key.Ctor` | |
| tuple | `key.0`, `key.1`, ... | |
| `unit` | `()` | |

- Capabilities: the 19 fields of the register form (`tag`, permissions, `reserved`, `E`, `B`, `T`, `otype`, `address`), not the memory encoding.
- Memory: one `mem` line per byte written since reset (its last value), one `tag` line per tag written (address: the granule's first byte). The program image is not listed.
- `step_result`: the result of the call that made the state: `()` after reset, `true`/`false` after a step (`false`: no instruction executed, e.g. a fetch fault), `"<message>"` after an `stm_fail`. The emulator has no failure value, so a failure line is a divergence by itself.

Delta of one `ADDI x1, x0, 5`:

```
PC 0x80000004
instbits 0x00500093
minstret 0x0000000000000001
nextPC 0x80000004
step_result true
x1.address 0x00000005
```

## Delta

- `statefile.py delta BEFORE AFTER` (`Dump.delta` in Rocq): the lines of `AFTER` that are new or changed.
- Registers never disappear and memory lines only accumulate, so the delta is everything that changed, except a key that is gone: a union's payload after its constructor changed (no register is a union yet).

## Delta run

What Rocq prints in `gmake validate` (`Dump.delta_run`): a run file with each state after the first as its changes.

```
delta-run ::= "0" LF ("0" SP line)* (step LF (step SP change)*)*     steps 1, 2, ... in order
change    ::= line | "-" key
```

- A line with the step alone opens that state; state 0 in full, every later state as its changes from the one before: a new or changed line, or `-key` for a key that is gone.
- `statefile.py undelta RUN` rebuilds the run file, every state in full; `validate.py` compares those, so every state is compared in full.
- `gmake -C validation/rocq dumps` checks the rebuilt `addi-add-sw` run against the run Rocq prints in full; `validate.py --full-states` does it for every case.

## Coverage

- Rocq lists every register of the model (`enum (sigT 𝑹𝑬𝑮)`), named by its constructor; a new register appears without a list to edit.
- The emulator's dump is generated from the Rocq reset state's register names; `gmake -C validation/emulator check-regs` checks both have the same keys.
- Left out: the vector registers `vr0`–`vr31` (vector extension out of scope, 65536 bits each).
- Some registers keep their start value through reset (e.g. `mvendorid`, `satp`, `sepc`); both sides start them at 0. `DumpADDI.v` pins this set: a translation change that alters it fails there.
