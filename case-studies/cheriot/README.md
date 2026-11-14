# CHERIoT case study

The configuration and templates that translate the [CHERIoT Sail model](https://github.com/microsoft/cheriot-sail) to Microsail,
and [`PROGRESS.md`](PROGRESS.md), which tracks how much of it translates.

- `configuration.lisp`, `base.template.v`, `machine.template.v`: the translation's configuration and the templates it names.
- `printvar.mak`: prints a variable of cheriot-sail's Makefile; the loop uses it to get the model's Sail sources (`SAIL_SRCS`).

They moved here from katamaran's `case_study/Cheriot` (branch `tex-cheriot`, commit `28ab8ab9`) on 14 Nov 2026,
so that the configuration is versioned with the translator features it uses. That directory's Makefile, which runs the
translation by hand, stayed in katamaran.

The loop translates and measures the model with this configuration by default:
`gmake cheriot CHERIOT_SAIL_DIR=/path/to/cheriot-sail` from the repository root (`tests/cheriot/run.py`, see `docs/cheriot-loop.md`).
