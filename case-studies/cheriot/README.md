# CHERIoT case study

Translates the core of the [CHERIoT Sail model](https://github.com/microsoft/cheriot-sail) to µSail and checks the result with Rocq.

## Files

- `configuration.lisp`: the translation's configuration (Slang); names the templates below.
- `base.template.v`, `machine.template.v`: templates for the generated `base.v` (types, registers) and `machine.v` (functions).
- `output/`: the output of `make cheriot-translate` (not tracked).
- `printvar.mak`: prints a variable of cheriot-sail's Makefile; the loop uses it to get the model's sources (`SAIL_SRCS`).
- `docs/`: design, roadmap, progress and the loop's manual (below).

## Running

From the repository root, with a cheriot-sail checkout (and its `sail-riscv` submodule):

```sh
make cheriot-translate CHERIOT_SAIL_DIR=/path/to/cheriot-sail    # translation only
make cheriot CHERIOT_SAIL_DIR=/path/to/cheriot-sail              # the loop
make cheriot CHERIOT_SAIL_DIR=... CHERIOT_ARGS=--no-coq          # the loop without Rocq
```

- `cheriot-translate`: writes `base.v` and `machine.v` to `output/`; about a minute. Walkthrough from scratch: [usage](../../docs/usage.md#worked-example-cheriot-from-scratch).
- `cheriot`: translates the core with this checkout's plugin, compiles with `coqc`, reports metrics; about 1.5 minutes.
- Fails if anything got worse than in `tests/cheriot/baseline.json`.
- Output: `tests/cheriot/output/latest/`.
- Details: [docs/loop.md](docs/loop.md).

## Documentation

- [design.md](docs/design.md): scope and translation choices, with examples.
- [roadmap.md](docs/roadmap.md): what is still missing for a full translation, and what each gap blocks.
- [progress.md](docs/progress.md): current numbers and their history; `docs/history.csv` has one line per loop run.
- [loop.md](docs/loop.md): the loop: running it, scope file, metrics, ratchet.
- [translator-bugs.md](docs/translator-bugs.md): translator bugs the loop found, fixed and open.
