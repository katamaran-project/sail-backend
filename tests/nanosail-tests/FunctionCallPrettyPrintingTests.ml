open Nanosail.ExtBase
open OUnit2


module Configuration = Nanosail.Configuration
module PP            = Nanosail.PP
module Statement     = Nanosail.NanosailToMicrosail.MuSail.Statement
module FunctionCalls = Nanosail.NanosailToMicrosail.FunctionCalls


(*
   Configuration settings are global: run f with pretty-print-function-calls set to the given value,
   and restore the earlier value afterwards, so that other tests are not affected.
*)
let with_pretty_print_function_calls (value : bool) (f : unit -> 'a) : 'a =
  let original = Configuration.(get pretty_print_function_calls)
  in
  Configuration.(set pretty_print_function_calls) value;
  Stdlib.Fun.protect f ~finally:(fun () -> Configuration.(set pretty_print_function_calls) original)


(* Arguments are parenthesized by the callers, so they are here too *)
let argument (name : string) : PP.t =
  PP.string @@ Printf.sprintf "(%s)" name


let lines (strings : string list) : string =
  String.concat ~sep:"\n" strings


let test_case
      ?(pretty   : bool option  = None)
       (name     : string            )
       (document : unit -> PP.t      )
       (expected : string            ) : test
  =
  name >:: fun _ -> begin
      let render () =
        PP.to_string @@ document ()
      in
      let actual =
        match pretty with
        | Some value -> with_pretty_print_function_calls value render
        | None       -> render ()
      in
      let msg =
        Printf.sprintf "expected\n%s\ngot\n%s" expected actual
      in
      assert_equal ~msg expected actual
    end


let test_default =
  "off by default" >:: fun _ -> begin
      (* The tests below restore the value they set, so this holds whatever the order *)
      assert_bool "pretty-print-function-calls is off by default" (not Configuration.(get pretty_print_function_calls))
    end


(* The printers of MuSail.Statement, which do not depend on the configuration *)
let test_printers =
  let test = test_case ~pretty:None
  and foo  = PP.string "foo"
  in
  "printers" >::: [
    test "call notation, no arguments"     (fun () -> Statement.pp_call_using_notation foo [])                                     "(call foo)%exp";
    test "call notation, one argument"     (fun () -> Statement.pp_call_using_notation foo [ argument "x" ])                       "(call foo (x))%exp";
    test "call notation, three arguments"  (fun () -> Statement.pp_call_using_notation foo [ argument "a"; argument "b"; argument "c" ]) "(call foo (a) (b) (c))%exp";

    test "foreign notation, no arguments"  (fun () -> Statement.pp_foreign_call_using_notation foo [])                             "(foreign foo)%exp";
    test "foreign notation, one argument"  (fun () -> Statement.pp_foreign_call_using_notation foo [ argument "x" ])               "(foreign foo (x))%exp";
    test "foreign notation, two arguments" (fun () -> Statement.pp_foreign_call_using_notation foo [ argument "a"; argument "b" ])  "(foreign foo (a) (b))%exp";

    test "stm_call, no arguments"          (fun () -> Statement.pp_call foo [])                                                    "stm_call foo (env.nil)";
    test "stm_call, one argument"          (fun () -> Statement.pp_call foo [ argument "x" ])
      (lines [
           "stm_call foo (env.snoc (env.nil)";
           "                       (_::_)";
           "                       ((x))%exp)";
         ]);
    test "stm_call, two arguments"         (fun () -> Statement.pp_call foo [ argument "x"; argument "y" ])
      (lines [
           "stm_call foo (env.snoc (env.snoc (env.nil)";
           "                                 (_::_)";
           "                                 ((x))%exp)";
           "                       (_::_)";
           "                       ((y))%exp)";
         ]);

    test "stm_foreign, no arguments"       (fun () -> Statement.pp_foreign_call foo [])                                            "stm_foreign foo (env.nil)";
    test "stm_foreign, one argument"       (fun () -> Statement.pp_foreign_call foo [ argument "x" ])
      (lines [
           "stm_foreign foo (env.snoc (env.nil)";
           "                          (_::_)";
           "                          ((x))%exp)";
         ]);
  ]


(* The choice between the printers, made by pretty-print-function-calls *)
let test_configured_syntax =
  let name                = FunctionCalls.Name (PP.string "foo")
  and with_named_argument = FunctionCalls.WithNamedArguments (PP.string "(foo (kid__n := 4))")
  in
  let call (callee : FunctionCalls.callee) (arguments : PP.t list) () : PP.t =
    FunctionCalls.pp_function_call_using_configured_syntax callee arguments
  and foreign (arguments : PP.t list) () : PP.t =
    FunctionCalls.pp_foreign_call_using_configured_syntax (PP.string "foo") arguments
  in
  let on  = test_case ~pretty:(Some true)
  and off = test_case ~pretty:(Some false)
  in
  "configured syntax" >::: [
    "on" >::: [
      on  "call"                         (call name [ argument "x" ])                  "(call foo (x))%exp";
      on  "call, no arguments"           (call name [])                                "(call foo)%exp";
      on  "foreign call"                 (foreign [ argument "x"; argument "y" ])      "(foreign foo (x) (y))%exp";
      on  "foreign call, no arguments"   (foreign [])                                  "(foreign foo)%exp";

      (* Katamaran's call notation only accepts a name: such a callee falls back on stm_call *)
      on  "call with named size argument" (call with_named_argument [ argument "x" ])
        (lines [
             "stm_call (foo (kid__n := 4)) (env.snoc (env.nil)";
             "                                       (_::_)";
             "                                       ((x))%exp)";
           ]);
    ];
    "off" >::: [
      off "call"                         (call name [])                                "stm_call foo (env.nil)";
      off "foreign call"                 (foreign [])                                  "stm_foreign foo (env.nil)";
      off "call with named size argument" (call with_named_argument [])                "stm_call (foo (kid__n := 4)) (env.nil)";
      off "foreign call, one argument"   (foreign [ argument "x" ])
        (lines [
             "stm_foreign foo (env.snoc (env.nil)";
             "                          (_::_)";
             "                          ((x))%exp)";
           ]);
    ];
  ]


let test_suite =
  "function call pretty printing" >::: [
    test_default;
    test_printers;
    test_configured_syntax;
  ]
