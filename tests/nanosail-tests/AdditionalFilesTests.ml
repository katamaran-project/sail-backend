open Base
open OUnit2
open Nanosail

module AdditionalFiles = Templates.AdditionalFiles


let read (path : string) : string =
  Stdio.In_channel.read_all path


(* The files of the Sail files are given by their paths below Machine/, e.g. "src/a" for Machine/src/a.v *)
let pending ?(sail_files_directory : string = "Machine") (directory : string) (sail_files : (string * string) list) : AdditionalFiles.pending =
  let sail_files = List.map sail_files ~f:(fun (module_path, contents) -> (String.split module_path ~on:'/', contents))
  in
  {
    directory;
    declarations_library = "machine_decl";
    declarations         = "(* declarations *)";
    sail_files_directory;
    sail_files;
  }


(* Runs f with a fresh directory, and with the output of base-translation in it, as base.v *)
let with_output_directory (f : string -> unit) : unit =
  let directory = Stdlib.Filename.temp_dir "additional-files" ""
  in
  AdditionalFiles.reset ();
  AdditionalFiles.current_output_file := Some (Stdlib.Filename.concat directory "base.v");
  AdditionalFiles.record_base_translation ();
  AdditionalFiles.current_output_file := None;
  Exn.protect ~f:(fun () -> f directory) ~finally:AdditionalFiles.reset


let test_library_name_of_output_file =
  "library_name_of_output_file" >:: fun _ -> begin
      assert_equal ~printer:Fn.id "machine" (AdditionalFiles.library_name_of_output_file "out/machine.v");
      assert_equal ~printer:Fn.id "base"    (AdditionalFiles.library_name_of_output_file "base.v");
    end


let test_writes_files =
  "writes the declarations and the files of the Sail files" >:: fun _ -> begin
      with_output_directory @@ fun directory -> begin
        AdditionalFiles.pending := [ pending directory [ ("a", "(* a *)"); ("src/sub/b", "(* b *)") ] ];
        AdditionalFiles.write_all ();
        let machine_directory = Stdlib.Filename.concat directory "Machine"
        in
        assert_equal ~printer:Fn.id
          "Require Export base.\n\n(* declarations *)\n"
          (read @@ Stdlib.Filename.concat directory "machine_decl.v");
        assert_equal ~printer:Fn.id
          "Require Import base machine_decl.\n\n(* a *)\n"
          (read @@ Stdlib.Filename.concat machine_directory "a.v");
        assert_equal ~printer:Fn.id
          "Require Import base machine_decl.\n\n(* b *)\n"
          (read @@ Stdlib.Filename.concat machine_directory "src/sub/b.v");
        assert_bool "write_all forgets what it wrote" (List.is_empty !AdditionalFiles.pending)
      end
    end


let test_removes_stale_files =
  "removes .v files in Machine and its subdirectories that are no longer generated, and nothing else" >:: fun _ -> begin
      with_output_directory @@ fun directory -> begin
        let in_machine (path : string) : string = Stdlib.Filename.concat (Stdlib.Filename.concat directory "Machine") path
        in
        AdditionalFiles.pending := [ pending directory [ ("a", "(* a *)"); ("old", "(* old *)"); ("src/b", "(* b *)"); ("src/old", "(* old *)") ] ];
        AdditionalFiles.write_all ();
        Stdio.Out_channel.write_all (in_machine "src/notes.txt") ~data:"kept";
        AdditionalFiles.pending := [ pending directory [ ("a", "(* a *)"); ("src/b", "(* b *)") ] ];
        AdditionalFiles.write_all ();
        assert_bool "a.v is still there"      (Stdlib.Sys.file_exists @@ in_machine "a.v");
        assert_bool "src/b.v is still there"  (Stdlib.Sys.file_exists @@ in_machine "src/b.v");
        assert_bool "old.v is removed"        (not @@ Stdlib.Sys.file_exists @@ in_machine "old.v");
        assert_bool "src/old.v is removed"    (not @@ Stdlib.Sys.file_exists @@ in_machine "src/old.v");
        assert_bool "src/notes.txt is kept"   (Stdlib.Sys.file_exists @@ in_machine "src/notes.txt");
      end
    end


let test_configured_directory =
  "writes the files of the Sail files into the configured directory" >:: fun _ -> begin
      with_output_directory @@ fun directory -> begin
        AdditionalFiles.pending := [ pending ~sail_files_directory:"Model" directory [ ("src/a", "(* a *)") ] ];
        AdditionalFiles.write_all ();
        assert_equal ~printer:Fn.id
          "Require Import base machine_decl.\n\n(* a *)\n"
          (read @@ Stdlib.Filename.concat directory "Model/src/a.v");
        assert_bool "nothing in Machine" (not @@ Stdlib.Sys.file_exists @@ Stdlib.Filename.concat directory "Machine")
      end
    end


let fails_with (description : string) (expected_fragment : string) (f : unit -> unit) : test =
  description >:: fun _ -> begin
      match f () with
      | exception Failure message ->
        assert_bool
          (Printf.sprintf "message %S does not mention %S" message expected_fragment)
          (String.is_substring message ~substring:expected_fragment)
      | () -> assert_failure "expected a failure"
    end


let test_failures =
  "failures" >::: [
    fails_with "no template generates base-translation" "base-translation" begin fun () ->
      let directory = Stdlib.Filename.temp_dir "additional-files" ""
      in
      AdditionalFiles.reset ();
      AdditionalFiles.pending := [ pending directory [ ("a", "(* a *)") ] ];
      Exn.protect ~f:AdditionalFiles.write_all ~finally:AdditionalFiles.reset
    end;
    fails_with "base.v in another directory" "same directory" begin fun () ->
      with_output_directory @@ fun directory -> begin
        AdditionalFiles.pending := [ pending (Stdlib.Filename.concat directory "elsewhere") [ ("a", "(* a *)") ] ];
        AdditionalFiles.write_all ()
      end
    end;
  ]


let test_html_base_translation_is_not_a_library =
  "base-translation into an HTML file does not count as the base library" >:: fun _ -> begin
      AdditionalFiles.reset ();
      AdditionalFiles.current_output_file := Some "out/base.v";
      AdditionalFiles.record_base_translation ();
      AdditionalFiles.current_output_file := Some "out/base.html";
      AdditionalFiles.record_base_translation ();
      assert_equal ~printer:(Option.value ~default:"<none>") (Some "out/base.v") !AdditionalFiles.base_output_file;
      AdditionalFiles.reset ()
    end


let test_suite =
  "additional files" >::: [
    test_library_name_of_output_file;
    test_writes_files;
    test_removes_stale_files;
    test_configured_directory;
    test_failures;
    test_html_base_translation_is_not_a_library;
  ]
