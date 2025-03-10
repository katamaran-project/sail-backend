open Shared



(* Included for testing purposes *)
let is_prime =
  define {|
    (define (prime? n)
      (and (> n 1)
           (all? (lambda (k) (not (= 0 (% n k))))
                 (range 2 n))))
  |}


let constant_function =
  define {|
    (define (constant-function value)
      (lambda (x) value))
  |}


let initialize =
  let definitions = [
    is_prime;
    constant_function;
  ]
  in
  EC.ignore @@ EC.sequence definitions
