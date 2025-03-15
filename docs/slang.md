# Slang

Slang is a fairly standard Lisp-1, inspired by Scheme/Racket.

## Values

All supported values are listed in the `Slang.Value` module
(which is an alias for `Slang.Recursive.Value`).

* Cons cells
* Nil (`()`)
* Integers (represented by `int` in OCaml)
* Booleans (`#t` and `#f`)
* Symbols
* Strings
* Callables
* References

Symbols starting with `:` are also called keywords.
Unlike other symbols, they evaluate to themselves.

## Defining Functions

```lisp
(define (function-name arg-1 arg-2)
  body)
```

## Heap

```lisp
(define my-variable (ref 0))

; Reading
(@ my-variable)

; Updating
(@= my-variable (+ 1 my-variable))
```

## Library

All special forms and predefined functions are implemented in the `Slang.Prelude` module.
