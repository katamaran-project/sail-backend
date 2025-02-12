open OUnit2

module Bool    = Bool
module Enum    = Enum
module Variant = Variant


let test_suite = "adorn" >::: [
    Enum.test_suite;
    Variant.test_suite;
    Bool.test_suite;
  ]
