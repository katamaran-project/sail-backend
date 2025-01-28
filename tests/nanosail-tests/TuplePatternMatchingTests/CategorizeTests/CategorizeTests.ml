open OUnit2

module Enum = Enum


let test_suite = "categorize" >::: [
    Enum.test_suite
  ]
