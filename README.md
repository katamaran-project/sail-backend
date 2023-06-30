Setup:
```sh
opam install . --deps-only
dune build
```

Test:
```sh
dune exec test/main.exe <option> <input>
```
\<input\> can be any example name (*lists*, *long*, *prod* or *expr* for the moment). Without <input>, every example is load.
`--help` to see the list of options
