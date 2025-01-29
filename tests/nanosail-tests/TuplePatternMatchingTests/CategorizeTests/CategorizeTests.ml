open OUnit2


let test_suite = "categorize" >::: [
    Enum.test_suite;
    Variant.test_suite;
  ]
