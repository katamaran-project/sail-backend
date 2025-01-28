open OUnit2

module Enum = Enum


let test_suite = "build chain" >::: [
    Enum.test_suite
  ]
