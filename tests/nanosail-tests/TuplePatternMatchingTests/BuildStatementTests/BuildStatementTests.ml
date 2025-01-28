open OUnit2


let test_suite = "build statement" >::: [
    Enum.test_suite;
    Int.test_suite;
    Mix.test_suite;
  ]
