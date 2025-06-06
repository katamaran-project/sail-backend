open Base
open OUnit2
open Nanosail.Templates


let create_input (lines : string list) : (module Blocks.INPUT) =
  let lines = ref lines
  in
  let module Result = struct
    let next_line () =
      match !lines with
      | line::remaining -> begin
          lines := remaining;
          Some line
        end
      | _ -> None

    let is_block_entry = String.equal "<<"
    let is_block_exit  = String.equal ">>"
  end
  in
  (module Result)


module Output = struct
  type t =
    | OutOfBlockLine of string
    | Block          of string list

  let equal (x : t) (y : t) : bool =
    match x with
    | OutOfBlockLine s1 -> begin
        match y with
        | OutOfBlockLine s2 -> String.equal s1 s2
        | _                 -> false
      end

    | Block lines_1 -> begin
        match y with
        | Block lines_2 -> List.equal String.equal lines_1 lines_2
        | _             -> false
      end
end


let test_process_lines =
  let test
      (lines    : string list  )
      (expected : Output.t list)
    =
    String.concat ~sep:"\n" lines >:: fun _ -> begin
        let output : Output.t list ref = ref []
        in
        let process_out_of_block_line line =
          output := (Output.OutOfBlockLine line) :: !output
        and process_block lines =
          output := (Output.Block lines) :: !output
        and input = create_input lines
        in
        let () =
          Blocks.process_lines
            input
            ~process_out_of_block_line
            ~process_block
        in
        let actual =
          List.rev !output
        in
        assert_equal
          ~cmp:(List.equal Output.equal)
          expected
          actual
      end
  in
  "process lines" >::: [
    test [] [];

    test [
      "a";
    ] [
      OutOfBlockLine "a";
    ];

    test [
      "a";
      "b";
    ] [
      OutOfBlockLine "a";
      OutOfBlockLine "b";
    ];

    test [
      "<<";
      "a";
      "b";
      ">>";
    ] [
      Block ["a"; "b"];
    ];

    test [
      "<<";
      "a";
      "b";
      ">>";
      "<<";
      "c";
      "d";
      ">>";
    ] [
      Block ["a"; "b"];
      Block ["c"; "d"];
    ];

    test [
      "a";
      "<<";
      "b";
      "c";
      ">>";
      "d";
    ] [
      OutOfBlockLine "a";
      Block [ "b"; "c" ];
      OutOfBlockLine "d"
    ];
  ]

let test_suite =
  "block" >::: [
    test_process_lines;
  ]
