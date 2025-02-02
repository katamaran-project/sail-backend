open OUnit2


let test_suite = "build pattern tree" >::: [
    Enum.test_suite;
    Variant.test_suite;
  ]
