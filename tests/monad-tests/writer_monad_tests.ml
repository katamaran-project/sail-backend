open OUnit2

module IntMonoid = struct
  type t = int

  let empty = 0

  let ( <+> ) = (+)
end

module IntWriter = Monads.Writer.Make(IntMonoid)

open Monads.Notations.Star(IntWriter)
open Monads.Util.Make(IntWriter)


let test_repeat =
  "repeat" >:: fun _ ->
    let f = repeat ~f:(IntWriter.write 1) 5
    in
    let (_, r) = IntWriter.run f
    in
    assert_equal 5 r


let test_suite =
  "writer monad test suite" >::: [
    test_repeat
  ]
