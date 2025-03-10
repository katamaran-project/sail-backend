open Shared



(* Included for testing purposes *)
let is_prime =
  define {|
    (define (prime? n)
      (and (> n 1)
           (all? (lambda (k) (not (= 0 (% n k))))
                 (range 2 n))))
  |}


let initialize =
  let definitions = [
    is_prime;
  ]
  in
  EC.ignore @@ EC.sequence definitions
