open OUnit2


let test_suite = "adorn" >::: [
    Enum.test_suite;
    Variant.test_suite;
    Bool.test_suite;
  ]
