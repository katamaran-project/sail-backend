# Case studies

Whole Sail models translated with the plugin, as opposed to the small end-to-end tests in `tests/coq-tests`.

| directory | model | how to run | status |
|---|---|---|---|
| [`cheriot/`](cheriot/README.md) | the CHERIoT core ([cheriot-sail](https://github.com/microsoft/cheriot-sail)) | `gmake cheriot CHERIOT_SAIL_DIR=...` from the repository root | work in progress; see [progress](cheriot/docs/progress.md) |
| `msp430/` | the MSP430 Sail model | `gmake -C case-studies/msp430` (calls `sail` from `PATH`) | translates; `coqc base.v` stops at `UNTRANSLATED_DEFINITIONS` (some definitions are untranslated), `machine.v` has 2 `NYI`s |
| `minimal-caps/` | a small capability machine (MinimalCaps) | `make minimal-caps` (`make mc`) | not maintained: its Makefile passes the old option spelling `-katamaran_config` (now `--katamaran-config`), and its configuration uses removed options |

- `tests/msp430-tests` holds a separate, smaller set of MSP430 tests (`make msp430-tests`).
