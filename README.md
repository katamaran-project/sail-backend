<p align="center">
  <img src="docs/katamaran_logo.svg" alt="Katamaran" width="360">
</p>

# Sail Katamaran Backend

A [Sail](https://github.com/rems-project/sail) plugin (`sail --katamaran`) that translates Sail ISA models to µSail,
the Rocq embedding verified with [Katamaran](https://github.com/katamaran-project/katamaran).

- Status: translates small and medium models; whole ISA models are work in progress (see [case studies](case-studies/README.md)).
- Targets Sail 0.20.3, OCaml 5.5.1, Rocq 9.2 and Katamaran main.

## Quick start

```sh
nix develop                 # or an opam switch, see docs/installation.md
dune build
bin/sail-local --katamaran --katamaran-config configuration.sexp model.sail
```

- `bin/sail-local` is `sail` with the plugin built in this checkout; `dune install` installs it into the switch instead.
- The configuration (S-expressions, read as data) names the templates that receive the generated Rocq ([usage](docs/usage.md#configuration-file)).
- Small examples: each directory of `tests/coq-tests` holds a `model.sail` and its `configuration.sexp`.

## Documentation

- [Installation](docs/installation.md)
- [Usage](docs/usage.md): configuration, templates, verbosity
  - [HTML generation](docs/html-generation.md)
  - [Monomorphization](docs/monomorphization.md) (optional)
- [Tests](docs/tests.md)
- [Code overview](docs/code-overview.md)
- [Notes on Sail](docs/sail-notes.md)
- [Contributing](CONTRIBUTING.md): build, tests, worktrees, coding conventions
- [Case studies](case-studies/README.md), among them [CHERIoT](case-studies/cheriot/README.md)
- [Archived experiments](archived_experiments/README.md)
