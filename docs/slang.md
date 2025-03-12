# Slang

Slang is a fairly standard Lisp-1.

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
