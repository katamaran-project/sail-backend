open Base
open OUnit2
open Nanosail

module FileStructure = NanosailToMicrosail.FileStructure


let test_module_name_of_path =
  let test (path : string) (expected : string) =
    path >:: fun _ -> begin
        assert_equal ~printer:Fn.id expected (FileStructure.module_name_of_path path)
      end
  in
  "module_name_of_path" >::: [
    test "riscv_insts_base.sail"              "riscv_insts_base";
    test "src/cheri_insts.sail"               "cheri_insts";
    test "/absolute/path/to/model.sail"       "model";
    test "riscv-insts.sail"                   "riscv_insts";
    test "a.b.sail"                           "a_b";
    test "prime'.sail"                        "prime'";
    test "1st.sail"                           "sail_1st";
    test "_hidden.sail"                       "sail__hidden";
    test "no_extension"                       "no_extension";
  ]


let test_components_of_path =
  let test (path : string) (expected : string list) =
    path >:: fun _ -> begin
        assert_equal
          ~printer:(String.concat ~sep:"/")
          ~cmp:(List.equal String.equal)
          expected
          (FileStructure.components_of_path ~current_directory:"/work/model" path)
      end
  in
  "components_of_path" >::: [
    test "foo.sail"                  [ "work"; "model"; "foo.sail" ];
    test "./foo.sail"                [ "work"; "model"; "foo.sail" ];
    test "src/./foo.sail"            [ "work"; "model"; "src"; "foo.sail" ];
    test "../src/foo.sail"           [ "work"; "src"; "foo.sail" ];
    test "/abs//dir/../foo.sail"     [ "abs"; "foo.sail" ];
    test "/../foo.sail"              [ "foo.sail" ];
  ]


let test_root_directory =
  let test (description : string) (root : string option) (files : string list) (expected : string list) =
    description >:: fun _ -> begin
        assert_equal
          ~printer:(String.concat ~sep:"/")
          ~cmp:(List.equal String.equal)
          expected
          (FileStructure.root_directory ~current_directory:"/work" ~root files)
      end
  in
  "root_directory" >::: [
    test "one file"                            None             [ "model.sail" ]                                            [ "work" ];
    test "files in subdirectories"             None             [ "src/a.sail"; "sail-riscv/model/b.sail"; "model.sail" ]   [ "work" ];
    test "files in one subdirectory"           None             [ "src/a.sail"; "src/b.sail" ]                              [ "work"; "src" ];
    test "files in sibling subdirectories"     None             [ "/x/y/a/f.sail"; "/x/y/b/g.sail" ]                        [ "x"; "y" ];
    test "no files"                            None             []                                                          [];
    test "given root, relative"                (Some "..")      [ "src/a.sail" ]                                            [];
    test "given root, absolute"                (Some "/x/y/")   [ "src/a.sail" ]                                            [ "x"; "y" ];
  ]


let test_destination_of_file =
  let library_directories = [ "/opt/sail/lib"; "/nix/store/xyz-sail/share/sail/lib/" ]
  in
  let destination (description : string) (root : string list) (file : string option) : FileStructure.Destination.t =
    ignore description;
    FileStructure.destination_of_file ~library_directories ~current_directory:"/work" ~root file
  in
  let test (description : string) ?(root : string list = [ "work" ]) (file : string option) (expected : FileStructure.Destination.t) =
    description >:: fun _ -> begin
        assert_equal
          ~printer:FileStructure.Destination.to_string
          ~cmp:FileStructure.Destination.equal
          expected
          (destination description root file)
      end
  in
  let sail_file (module_path : string list) (path : string) (relative_path : string) : FileStructure.Destination.t =
    SailFile { module_path; path; relative_path }
  in
  "destination_of_file" >::: [
    test "no location"                        None                                                  Assembly;
    test "library file"                       (Some "/opt/sail/lib/vector.sail")                    Assembly;
    test "library directory with slash"       (Some "/nix/store/xyz-sail/share/sail/lib/flow.sail") Assembly;
    test "file in a subdirectory of library"  (Some "/opt/sail/lib/arch/x.sail")                    Assembly;
    test "file in the root"                   (Some "./model.sail")                                 (sail_file [ "model" ] "./model.sail" "model.sail");
    test "file in a subdirectory"             (Some "src/cheri_insts.sail")                         (sail_file [ "src"; "cheri_insts" ] "src/cheri_insts.sail" "src/cheri_insts.sail");
    test "directories are sanitised"          (Some "sail-riscv/model/riscv-regs.sail")             (sail_file [ "sail_riscv"; "model"; "riscv_regs" ] "sail-riscv/model/riscv-regs.sail" "sail-riscv/model/riscv-regs.sail");
    test "absolute path"                      ~root:[ "x" ] (Some "/x/y/f.sail")                    (sail_file [ "y"; "f" ] "/x/y/f.sail" "y/f.sail");
    test "directory with a library's prefix"  ~root:[ "opt"; "sail" ] (Some "/opt/sail/library/x.sail") (sail_file [ "library"; "x" ] "/opt/sail/library/x.sail" "library/x.sail");
    "file outside the root" >:: begin fun _ ->
      match destination "outside" [ "work"; "src" ] (Some "other/f.sail") with
      | exception Failure message ->
        assert_bool message (String.is_substring message ~substring:"is not below the root directory /work/src")
      | result -> assert_failure @@ "expected a failure, got " ^ FileStructure.Destination.to_string result
    end;
  ]


let test_file_of_location =
  let position_in (handle : Libsail.Sail_file.handle) : Libsail.Sail_file.position =
    { pos_fname = handle; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let check (expected : string option) (location : Libsail.Parse_ast.l) : unit =
    assert_equal
      ~printer:(Option.value ~default:"<none>")
      ~cmp:(Option.equal String.equal)
      expected
      (FileStructure.file_of_location location)
  in
  "file_of_location" >::: [
    "unknown location" >:: (fun _ -> check None Libsail.Parse_ast.Unknown);
    "Sail's virtual file EMPTY" >:: begin fun _ ->
      let position = position_in Libsail.Sail_file.dummy
      in
      check None (Libsail.Parse_ast.Range (position, position))
    end;
    "a Sail file, also through Generated" >:: begin fun _ ->
      let path = Stdlib.Filename.temp_file "file-structure" ".sail"
      in
      Stdio.Out_channel.write_all path ~data:"default Order dec\n";
      let position = position_in @@ Libsail.Sail_file.open_file (Libsail.Sail_file.Path.actual path)
      in
      check (Some path) (Libsail.Parse_ast.Range (position, position));
      check (Some path) (Libsail.Parse_ast.Generated (Libsail.Parse_ast.Range (position, position)));
      Stdlib.Sys.remove path
    end;
  ]


let test_type_of_generated_function =
  let type_names = [ "Mstatus"; "Mstatush"; "Misa"; "pte" ]
  in
  let test (function_name : string) (expected : string option) =
    function_name >:: fun _ -> begin
        assert_equal
          ~printer:(Option.value ~default:"<none>")
          ~cmp:(Option.equal String.equal)
          expected
          (FileStructure.type_of_generated_function ~type_names function_name)
      end
  in
  "type_of_generated_function" >::: [
    test "Mk_Misa"              (Some "Misa");
    test "_get_Misa_MXL"        (Some "Misa");
    test "_set_Misa_C"          (Some "Misa");
    test "_update_Mstatus_MIE"  (Some "Mstatus");
    test "_get_Mstatush_MBE"    (Some "Mstatush");
    test "_get_Unknown_F"       None;
    test "get_Misa_MXL"         None;
    test "Misa_MXL"             None;
    test "_get_Misa"            (Some "Misa");
    test "_get_Misas_X"         None;
  ]


(* Groups strings "path:item" by path (relative to the root); "-:item" goes to the program's own file *)
let group ?(reserved_names : string list = []) (items : string list) : string FileStructure.grouping =
  let destination_of (item : string) : FileStructure.Destination.t =
    match String.lsplit2 item ~on:':' with
    | Some ("-", _)    -> Assembly
    | Some (path, _)   -> FileStructure.destination_of_file ~library_directories:[] ~current_directory:"/work" ~root:[ "work" ] (Some path)
    | None             -> Assembly
  in
  FileStructure.group ~directory:"Machine" ~destination_of ~reserved_names items


let show (grouping : string FileStructure.grouping) : string =
  let show_file (sail_file : string FileStructure.sail_file) =
    Printf.sprintf "%s(%s)=[%s]" (FileStructure.ModulePath.to_library ~directory:"Machine" sail_file.module_path) sail_file.relative_path (String.concat ~sep:"," sail_file.items)
  in
  Printf.sprintf "%s; assembly=[%s]"
    (String.concat ~sep:" " (List.map grouping.sail_files ~f:show_file))
    (String.concat ~sep:"," grouping.assembly)


let test_group =
  let test (description : string) (items : string list) (expected : string) =
    description >:: fun _ -> begin
        assert_equal ~printer:Fn.id expected (show (group items))
      end
  in
  "group" >::: [
    test "files in order of their first function, functions in order"
      [ "b.sail:f"; "a.sail:g"; "-:lib"; "b.sail:h"; "a.sail:i" ]
      "Machine.b(b.sail)=[b.sail:f,b.sail:h] Machine.a(a.sail)=[a.sail:g,a.sail:i]; assembly=[-:lib]";
    test "files with the same name in different directories"
      [ "x/foo.sail:f"; "y/foo.sail:g"; "foo.sail:h" ]
      "Machine.x.foo(x/foo.sail)=[x/foo.sail:f] Machine.y.foo(y/foo.sail)=[y/foo.sail:g] Machine.foo(foo.sail)=[foo.sail:h]; assembly=[]";
    test "one file through different paths"
      [ "x/foo.sail:f"; "./x/../x/foo.sail:g" ]
      "Machine.x.foo(x/foo.sail)=[x/foo.sail:f,./x/../x/foo.sail:g]; assembly=[]";
    test "nothing"
      []
      "; assembly=[]";
    test "only the program's own file"
      [ "-:f"; "-:g" ]
      "; assembly=[-:f,-:g]";
  ]


let test_group_failures =
  let fails (description : string) ?(reserved_names : string list = []) (items : string list) (expected_fragment : string) =
    description >:: fun _ -> begin
        match group ~reserved_names items with
        | exception Failure message ->
          assert_bool
            (Printf.sprintf "message %S does not mention %S" message expected_fragment)
            (String.is_substring message ~substring:expected_fragment)
        | grouping -> assert_failure @@ "expected a failure, got " ^ show grouping
      end
  in
  "group failures" >::: [
    fails "two Sail files that become the same file"
      [ "foo-bar.sail:f"; "foo_bar.sail:g" ]
      "foo-bar.sail and foo_bar.sail would both become Machine/foo_bar.v";
    fails "two directories that become the same directory"
      [ "a-b/f.sail:f"; "a_b/f.sail:g" ]
      "Machine/a_b/f.v";
    fails "a Sail file with the name of a generated file"
      ~reserved_names:[ "base"; "machine"; "machine_decl" ]
      [ "a.sail:f"; "model/machine.sail:g" ]
      "model/machine.sail";
  ]


let test_is_valid_name =
  let test (name : string) (expected : bool) =
    name >:: fun _ -> assert_equal ~printer:Bool.to_string expected (FileStructure.is_valid_name name)
  in
  "is_valid_name" >::: [
    test "Machine"   true;
    test "Model_2'"  true;
    test "my-model"  false;
    test "2nd"       false;
    test "_model"    false;
    test "a/b"       false;
    test ""          false;
  ]


let test_directory_setting =
  let with_directory (name : string) (f : unit -> unit) : unit =
    let original = Configuration.(get preserve_file_structure_directory)
    in
    Configuration.(set preserve_file_structure_directory) name;
    Exn.protect ~f ~finally:(fun () -> Configuration.(set preserve_file_structure_directory) original)
  in
  "preserve-file-structure-directory" >::: [
    "default" >:: begin fun _ ->
      assert_equal ~printer:Fn.id "Machine" (FileStructure.directory ())
    end;
    "configured" >:: begin fun _ ->
      with_directory "Model" @@ fun () -> begin
        assert_equal ~printer:Fn.id "Model" (FileStructure.directory ());
        assert_equal ~printer:Fn.id "Model/src/a.v"  (FileStructure.ModulePath.to_file    ~directory:(FileStructure.directory ()) [ "src"; "a" ]);
        assert_equal ~printer:Fn.id "Model.src.a"    (FileStructure.ModulePath.to_library ~directory:(FileStructure.directory ()) [ "src"; "a" ])
      end
    end;
    "not a name that Rocq allows" >:: begin fun _ ->
      with_directory "my-model" @@ fun () -> begin
        match FileStructure.directory () with
        | exception Failure message -> assert_bool message (String.is_substring message ~substring:"\"my-model\"")
        | directory                 -> assert_failure @@ "expected a failure, got " ^ directory
      end
    end;
  ]


let test_suite =
  "file structure" >::: [
    test_module_name_of_path;
    test_is_valid_name;
    test_directory_setting;
    test_components_of_path;
    test_root_directory;
    test_destination_of_file;
    test_file_of_location;
    test_type_of_generated_function;
    test_group;
    test_group_failures;
  ]
