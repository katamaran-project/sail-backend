# Case studies

Sail models that are translated with the plugin as a whole, as opposed to the small end-to-end tests in `tests/coq-tests`.
`cheriot/` holds the CHERIoT configuration and templates (moved from katamaran's `case_study/Cheriot` on 14 Nov 2026; see `cheriot/README.md`).
Its translation is measured by the loop in `tests/cheriot` (`docs/cheriot-loop.md`), and `cheriot/PROGRESS.md` tracks the results.

| directory | model | how to run | status (13 Nov 2026) |
|---|---|---|---|
| `msp430/` | the MSP430 Sail model | `gmake -C case-studies/msp430` (calls `sail` from `PATH`) | translates completely; `coqc base.v` then fails at `UNTRANSLATED_DEFINITIONS`, because some definitions are left untranslated, and `machine.v` has 2 `NYI`s |
| `minimal-caps/` | a small capability machine (MinimalCaps) | `make minimal-caps` (`make mc`) from the repository root | not maintained since June 2024: its Makefile passes `-katamaran_config`, an old spelling of this plugin's configuration option that the current plugin does not register (it is now `--katamaran-config`), and its configuration uses options that no longer exist |

`tests/msp430-tests` contains a separate, smaller set of MSP430 tests (`make msp430-tests`).
