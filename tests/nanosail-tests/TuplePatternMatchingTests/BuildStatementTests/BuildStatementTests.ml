open OUnit2

module Enum = Enum


let test_suite = "build statement" >::: [
    Enum.test_suite
  ]
