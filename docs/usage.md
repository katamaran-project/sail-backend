# Usage

To translate Sail to muSail, you need the following files:

* One or more Sail files that contain the code to be translated.
* A `configuration.lisp` file.
* One or more template files.
  Each template file will lead to the generation of an output file.

Next, run the following command to perform the translation.

```bash
$ sail --katamaran --katamaran-config configuration.lisp SAILFILE...
```

## Configuration File

The configuration is specified using the [Slang language](./slang.md) in `configuration.lisp`.
See the `Configuration` module for more detailed information.

The following functions can be used:

### `template`

```lisp
; Example: machine.template.v serves as template for machine.v
(template "machine.template.v")

; Example: xxx.v serves as template for yyy.v
(template "xxx.v" "yyy.v")
```

The `template` function registers a template file and the corresponding output file name.
Multiple calls can be made to register multiple input/output pairs.
You probably want to register at least one template.

* When given two arguments, the first is taken as the input file

* When given a single argument, the file name must contain `".template"`.
  The output file name is then determined by removing this substring, e.g.,
  `(template "aaa.template.v")`


### `base-name`

```lisp
; Example
(base-name "MySailModelBase")
```

`base-name` is a function that takes a single string as argument,
which will be used as the name of the base module.
If no call to `base-name` is made, the default name (`"UntitledBase"`) will be used.

### `program-name`

```lisp
; Example
(program-name "MySailModelBase")
```

Sets the name of the program module.
If no call to `program-name` is made, the default name of `"ModelProgram"` will be used.



## Template Files


## Verbosity Level

The verbosity level can be set using the environment variable `VERBOSE`:

```bash
$ VERBOSE=0 sail ...
```

* `VERBOSE=0`: quiet mode. Suppresses all logging.
* `VERBOSE=1`: only errors are shown.
* `VERBOSE=2`: only errors and warnings are shown. This is the default.
* `VERBOSE=3`
* `VERBOSE=4`: maximum level.

See `LogLib.VerbosityLevel` for more detailed information.

## HTML Generation

See [dedicated page](./html-generation.md).
