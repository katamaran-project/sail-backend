# Usage

To translate Sail to muSail, you need the following files:

* One or more Sail files that contain the code to be translated.
* A `configuration.lisp` file.
* One or more template files.

```bash
$ sail --katamaran --katamaran-config configuration.lisp SAILFILE...
```

## Configuration file


See the `Configuration` module for up to date information.

## Template Files


## Verbosity Level

The verbosity level can be set using the environment variable `VERBOSE`:

```bash
$ VERBOSE=0 sail ...
```

* `VERBOSE=0`: quiet. Suppresses all logging.
* `VERBOSE=1`: only errors are shown.
* `VERBOSE=2`: only errors and warnings are shown. This is the default.
* `VERBOSE=3`
* `VERBOSE=4`: maximum level.

See `LogLib.VerbosityLevel` for up to date information.

## HTML Generation

See [dedicated page](./html-generation.md).
