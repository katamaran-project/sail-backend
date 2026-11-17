# Case studies

Whole Sail models translated with the plugin, as opposed to the small end-to-end tests in `tests/coq-tests`.

| directory | model | how to run | status |
|---|---|---|---|
| [`cheriot/`](cheriot/README.md) | the CHERIoT core ([cheriot-sail](https://github.com/microsoft/cheriot-sail)) | `gmake cheriot CHERIOT_SAIL_DIR=...` from the repository root | work in progress; see [progress](cheriot/docs/progress.md) |
| `msp430/` | the MSP430 Sail model | `gmake -C case-studies/msp430` (calls `sail` from `PATH`) | translates; `coqc base.v` stops at `UNTRANSLATED_DEFINITIONS` (some definitions are untranslated), `machine.v` has 2 `NYI`s |
| `minimal-caps/` | a small capability machine (MinimalCaps) | `make minimal-caps` (`make mc`) | not maintained: its Makefile passes the old option spelling `-katamaran_config` (now `--katamaran-config`), and its configuration uses removed options |

- `tests/msp430-tests` holds a separate, smaller set of MSP430 tests (`make msp430-tests`).

## Comparing a translation with its Sail

`validation/scripts/gen_comparison.py` turns an output directory of the plugin into one self-contained HTML page: each Rocq definition on the left, the Sail it was translated from on the right.

```sh
validation/scripts/gen_comparison.py case-studies/cheriot/output  # writes case-studies/cheriot/output/comparison.html
make cheriot-compare                                          # the same
```

- Input: the generated `.v` files (all `*.v` except `*.template.v`, or `--rocq FILE`), the Sail files (`sources.txt`, else the `sail` command recorded in `log.txt`) and `log.txt` itself.
- Matching: `fun_f` via the source locations in the nanosail AST dump in `log.txt`, so configure `(dump-nanosail-ast)`; without it, by name only.
  Types, registers and constants by name; bitfield accessors (`Mk_B`, `_get_B_F`, ...) to their `bitfield`; EqDec/Finite instances grouped under their type.
- Links per definition: VS Code (to the line), `file://`, and GitHub (from the file's git `origin` and `HEAD`, or `--github DIR=URL`).
  VS Code and `file://` links use absolute paths, so they only work on the machine that generated the page.
- Sources moved since the translation: `--path-map OLD=NEW`. Sail's library is found through `sail --dir` or the paths in the dump; else `--sail-lib DIR`.
- The page names the definitions it could not match (filter "Zonder Sail-tegenhanger"); the script prints their number.
