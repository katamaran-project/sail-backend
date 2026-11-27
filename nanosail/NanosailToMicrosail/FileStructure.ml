(*

   Where the functions go when the configuration asks to preserve the file structure
   of the Sail model (preserve-file-structure, see docs/usage.md#preserve-file-structure):

   - the functions of a Sail file dir/foo.sail go to Machine/dir/foo.v (the name Machine is
     configurable: preserve-file-structure-directory), where dir is the
     directory of the Sail file relative to a root: by default the longest common directory
     of the model's files (see root_directory);
   - the functions of Sail's library, and functions without a source location,
     go to the file that puts the program together (machine.v).

*)
open ExtBase


(* Whether Rocq allows a name for a file or directory, and so as part of the name of a library *)
let is_valid_name (name : string) : bool =
  (not @@ String.is_empty name)
  && Char.is_alpha (String.get name 0)
  && String.for_all name ~f:(fun character -> Char.is_alphanum character || Char.equal character '_' || Char.equal character '\'')


(*
   The directory, relative to the output file of the program, that holds the files of the Sail files:
   the setting preserve-file-structure-directory (by default Machine).
   Fails if Rocq does not allow it as a name, since it is also the first part of the libraries' names.
*)
let directory () : string =
  let directory = Configuration.(get preserve_file_structure_directory)
  in
  if
    is_valid_name directory
  then
    directory
  else
    failwith @@ Printf.sprintf
      "preserve-file-structure-directory: %S is not a name that Rocq allows for a directory (a letter, followed by letters, digits, _ and ')"
      directory


(*
   The Rocq file of a Sail file, as the path below the directory of the Sail files (Machine/) without .v,
   e.g., [ "src"; "cheri_insts" ] for Machine/src/cheri_insts.v, whose library is Machine.src.cheri_insts.
*)
module ModulePath = struct
  type t = string list

  let equal (module_path_1 : t) (module_path_2 : t) : bool =
    List.equal String.equal module_path_1 module_path_2

  (* The file, relative to the directory of the program's own file, given the directory of the Sail files *)
  let to_file ~(directory : string) (module_path : t) : string =
    String.concat ~sep:"/" (directory :: module_path) ^ ".v"

  (* The Rocq library, given the directory of the Sail files *)
  let to_library ~(directory : string) (module_path : t) : string =
    String.concat ~sep:"." (directory :: module_path)

  (* The file, relative to the directory of the Sail files, for messages *)
  let to_string (module_path : t) : string =
    String.concat ~sep:"/" module_path ^ ".v"

  (* The name of the file, without directories *)
  let name (module_path : t) : string =
    Option.value ~default:"" @@ List.last module_path
end


(* Where a function goes *)
module Destination = struct
  type t =
    | SailFile of { module_path : ModulePath.t; path : string; relative_path : string }   (* path: the Sail file; relative_path: the same, relative to the root *)
    | Assembly

  let equal (destination_1 : t) (destination_2 : t) : bool =
    match destination_1, destination_2 with
    | SailFile { module_path = module_path_1; path = path_1; relative_path = relative_path_1 },
      SailFile { module_path = module_path_2; path = path_2; relative_path = relative_path_2 } -> begin
        ModulePath.equal module_path_1 module_path_2 && String.equal path_1 path_2 && String.equal relative_path_1 relative_path_2
      end
    | Assembly, Assembly                                     -> true
    | SailFile _, Assembly                                   -> false
    | Assembly, SailFile _                                   -> false

  let to_string (destination : t) : string =
    match destination with
    | SailFile { module_path; path; relative_path = _ } -> Printf.sprintf "%s (from %s)" (ModulePath.to_string module_path) path
    | Assembly                                          -> "the program's own file"
end


(*
   A file or directory name as Rocq allows it: the characters that Rocq does not allow
   replaced by underscores, and the prefix sail_ if it does not start with a letter.
*)
let sanitise_name (name : string) : string =
  let sanitised =
    String.map name ~f:(fun character ->
        if Char.is_alphanum character || Char.equal character '_' || Char.equal character '\''
        then character
        else '_'
      )
  in
  if
    (not @@ String.is_empty sanitised) && Char.is_alpha (String.get sanitised 0)
  then
    sanitised
  else
    "sail_" ^ sanitised


(* The name of the Rocq file (without .v) for a Sail file: its base name without .sail, sanitised *)
let module_name_of_path (path : string) : string =
  let base_name = Stdlib.Filename.basename path
  in
  sanitise_name @@ Option.value ~default:base_name @@ String.chop_suffix base_name ~suffix:".sail"


(*
   The components of a path, made absolute with current_directory if it is relative,
   without . and with .. resolved (without looking at the file system, so not through symbolic links).
*)
let components_of_path
    ~(current_directory : string)
    (path               : string) : string list
  =
  let absolute =
    if Stdlib.Filename.is_relative path then Stdlib.Filename.concat current_directory path else path
  in
  List.rev @@ List.fold (String.split absolute ~on:'/') ~init:[] ~f:(fun reversed component ->
      match component with
      | ""   -> reversed
      | "."  -> reversed
      | ".." -> Option.value ~default:[] @@ List.tl reversed
      | name -> name :: reversed
    )


let rec common_prefix (components_1 : string list) (components_2 : string list) : string list =
  match components_1, components_2 with
  | first_1 :: rest_1, first_2 :: rest_2 when String.equal first_1 first_2 -> first_1 :: common_prefix rest_1 rest_2
  | _ :: _, _ :: _ -> []
  | [], _          -> []
  | _ :: _, []     -> []


(* The longest common directory of files, given as components *)
let common_directory (files : string list list) : string list =
  let directory (file : string list) : string list =
    Option.value ~default:[] @@ List.drop_last file
  in
  match List.map files ~f:directory with
  | []                   -> []
  | first :: directories -> List.fold directories ~init:first ~f:common_prefix


(*
   The root that the directories of the Sail files are relative to: root if it is given
   (relative to the current directory), and otherwise the longest common directory of the files.
*)
let root_directory
    ~(current_directory : string       )
    ~(root              : string option)
    (files              : string list  ) : string list
  =
  match root with
  | Some root -> components_of_path ~current_directory root
  | None      -> common_directory @@ List.map files ~f:(components_of_path ~current_directory)


(*
   The directories that hold Sail's library (the files included with $include <...>).
   Sail looks for them in $SAIL_DIR/lib, or else in the share/sail/lib directory of its
   installation, which is next to the share/libsail directory that holds its plugins.
*)
let library_directories () : string list =
  let from_environment =
    match Stdlib.Sys.getenv_opt "SAIL_DIR" with
    | Some sail_directory -> [ Stdlib.Filename.concat sail_directory "lib" ]
    | None                -> []
  and from_installation =
    (* each plugin directory is <prefix>/share/libsail/plugins *)
    List.map Libsail.Libsail_sites.Sites.plugins ~f:(fun plugin_directory ->
        let share_directory = Stdlib.Filename.dirname @@ Stdlib.Filename.dirname plugin_directory
        in
        Stdlib.Filename.concat (Stdlib.Filename.concat share_directory "sail") "lib"
      )
  in
  List.append from_environment from_installation


let is_in_directory ~(directory : string) (path : string) : bool =
  let directory =
    if String.is_suffix directory ~suffix:"/" then directory else directory ^ "/"
  in
  String.is_prefix path ~prefix:directory


let is_library_file
    ~(library_directories : string list)
    (path                 : string     ) : bool
  =
  List.exists library_directories ~f:(fun directory -> is_in_directory ~directory path)


(*
   Where a function with the given source file goes, given the root of the model's files (see root_directory).
   Fails if the file is not below the root.
*)
let destination_of_file
    ~(library_directories : string list  )
    ~(current_directory   : string       )
    ~(root                : string list  )
    (file                 : string option) : Destination.t
  =
  match file with
  | None      -> Assembly
  | Some path -> begin
      if
        is_library_file ~library_directories path
      then
        Assembly
      else begin
        let components = components_of_path ~current_directory path
        in
        let relative =
          if List.is_prefix components ~prefix:root ~equal:String.equal
          then Some (List.drop components (List.length root))
          else None
        in
        match relative with
        | Some (_ :: _ as relative) -> begin
            let directories = List.drop_last_exn relative
            in
            SailFile {
              module_path   = List.append (List.map directories ~f:sanitise_name) [ module_name_of_path path ];
              path;
              relative_path = String.concat ~sep:"/" relative;
            }
          end
        | Some [] | None -> failwith @@ Printf.sprintf
            "preserve-file-structure: the Sail file %s is not below the root directory /%s (see preserve-file-structure-root)"
            path (String.concat ~sep:"/" root)
      end
    end


(*
   The Sail file of a location, if any. A location in one of Sail's virtual files
   (e.g. EMPTY, which Sail uses for some definitions it generates, such as Mk_B for a bitfield B)
   does not count.
*)
let file_of_location (location : Libsail.Parse_ast.l) : string option =
  match Libsail.Reporting.loc_file location with
  | Some handle -> begin
      let path = Libsail.Sail_file.to_path handle
      in
      if Libsail.Sail_file.Path.is_virtual path
      then None
      else Some (Libsail.Sail_file.Path.to_string path)
    end
  | None -> None


(*
   The locations of the arms of a match that is the whole body of a function: those of their
   patterns and bodies, since merging the clauses of a function gives the arms the location of its first clause
*)
let match_arm_locations (body : Sail.type_annotation Libsail.Ast.exp) : Libsail.Parse_ast.l list =
  let Libsail.Ast.E_aux (unwrapped_body, _) = body
  in
  let locations_of_arm (arm : Sail.type_annotation Libsail.Ast.pexp) : Libsail.Parse_ast.l list =
    let (Libsail.Ast.P_aux (_, (pattern_location, _)), _, Libsail.Ast.E_aux (_, (body_location, _)), _) = Libsail.Ast_util.destruct_pexp arm
    in
    [ pattern_location; body_location ]
  in
  match unwrapped_body with
  | E_match (_, arms)        -> List.concat_map arms ~f:locations_of_arm
  | E_lit _                  -> []
  | E_vector _               -> []
  | E_block _                -> []
  | E_id _                   -> []
  | E_typ (_, _)             -> []
  | E_app (_, _)             -> []
  | E_tuple _                -> []
  | E_if (_, _, _)           -> []
  | E_loop (_, _, _, _)      -> []
  | E_for (_, _, _, _, _, _) -> []
  | E_vector_append (_, _)   -> []
  | E_list _                 -> []
  | E_cons (_, _)            -> []
  | E_struct (_, _)          -> []
  | E_struct_update (_, _)   -> []
  | E_field (_, _)           -> []
  | E_let (_, _, _)          -> []
  | E_assign (_, _)          -> []
  | E_sizeof _               -> []
  | E_return _               -> []
  | E_exit _                 -> []
  | E_config _               -> []
  | E_ref _                  -> []
  | E_throw _                -> []
  | E_try (_, _)             -> []
  | E_assert (_, _)          -> []
  | E_var (_, _, _)          -> []
  | E_undef                  -> []
  | E_internal_plet (_, _, _)-> []
  | E_internal_return _      -> []
  | E_internal_value _       -> []
  | E_internal_assume (_, _) -> []
  | E_constraint _           -> []


(*
   The Sail file of a function.

   The location of the definition is not always the function's own: Sail puts the definitions
   that it makes from a scattered function at the scattered function (scattered function execute).
   This holds for the functions that the split rewrite makes from its clauses (execute_ADD),
   while the clauses themselves keep their locations.
   So the file is that of the function's clauses and of the arms of a match that is its whole body,
   if they are all in one file. If they are in several, the function is made of clauses from several
   files (the merged scattered function execute, or the functions of a scattered mapping), and its
   file is that of its definition: the one that declares the scattered function.
*)
let file_of_sail_definition (sail_definition : Sail.sail_definition) : string option =
  let Libsail.Ast.DEF_aux (unwrapped_definition, annotation) = sail_definition
  in
  let file_of_definition = file_of_location annotation.loc
  in
  let files_of_parts : string list =
    match unwrapped_definition with
    | DEF_fundef (FD_aux (FD_function (_, _, clauses), _)) -> begin
        let locations =
          List.concat_map clauses ~f:(fun (FCL_aux (FCL_funcl (_, pexp), (clause_annotation, _))) ->
              let (_, _, body, _) = Libsail.Ast_util.destruct_pexp pexp
              in
              clause_annotation.loc :: match_arm_locations body
            )
        in
        List.dedup_and_sort ~compare:String.compare @@ List.filter_map locations ~f:file_of_location
      end
    | DEF_type _                 -> []
    | DEF_mapdef _               -> []
    | DEF_impl _                 -> []
    | DEF_let (_, _)             -> []
    | DEF_val _                  -> []
    | DEF_outcome (_, _)         -> []
    | DEF_instantiation (_, _)   -> []
    | DEF_fixity (_, _, _)       -> []
    | DEF_overload (_, _)        -> []
    | DEF_default _              -> []
    | DEF_scattered _            -> []
    | DEF_measure (_, _, _)      -> []
    | DEF_loop_measures (_, _)   -> []
    | DEF_register _             -> []
    | DEF_internal_mutrec _      -> []
    | DEF_constraint _           -> []
    | DEF_pragma (_, _)          -> []
  in
  match files_of_parts with
  | [ file ]        -> Some file
  | []              -> file_of_definition
  | _ :: _ :: _     -> file_of_definition


(*
   The type that a function without a source location belongs to, if the function is
   one that Sail generates for a bitfield B (Mk_B, _get_B_F, _set_B_F, _update_B_F),
   given the names of the types. The longest type name wins, since B_F can also be
   B'_F' for another type B' that is a prefix of B (e.g., Mstatus and Mstatush).
*)
let type_of_generated_function
    ~(type_names    : string list)
    (function_name  : string     ) : string option
  =
  let prefixes = [ "Mk_"; "_get_"; "_set_"; "_update_" ]
  in
  let matches_type (rest : string) (type_name : string) : bool =
    String.equal rest type_name || String.is_prefix rest ~prefix:(type_name ^ "_")
  in
  List.find_map prefixes ~f:(fun prefix -> String.chop_prefix function_name ~prefix)
  |> Option.bind ~f:(fun rest ->
      List.filter type_names ~f:(matches_type rest)
      |> List.max_elt ~compare:(fun name_1 name_2 -> Int.compare (String.length name_1) (String.length name_2))
    )


(*
   The functions grouped by destination: the files of the Sail files, in the order in which
   their first function occurs, and the functions that go to the program's own file.
   Within each group, the functions keep their order.

   Fails if two different Sail files get the same Rocq file (e.g., foo-bar.sail and foo_bar.sail),
   or if a Sail file gets one of the names in reserved_names (the program's own files, which
   a Require could otherwise not tell apart from them).
*)
type 'a sail_file =
  {
    module_path   : ModulePath.t;
    path          : string;          (* the Sail file *)
    relative_path : string;          (* the Sail file, relative to the root *)
    items         : 'a list;
  }

type 'a grouping =
  {
    sail_files : 'a sail_file list;
    assembly   : 'a list;
  }


let group
    ~(directory      : string             )
    ~(destination_of : 'a -> Destination.t)
    ~(reserved_names : string list        )
    (items           : 'a list            ) : 'a grouping
  =
  let sail_files : 'a sail_file list ref = ref []   (* reversed, with reversed items *)
  and assembly   : 'a list ref           = ref []
  in
  List.iter items ~f:(fun item ->
      match destination_of item with
      | Assembly -> assembly := item :: !assembly
      | SailFile { module_path; path; relative_path } -> begin
          let name = ModulePath.name module_path
          in
          if List.mem reserved_names name ~equal:String.equal
          then failwith @@ Printf.sprintf
              "preserve-file-structure: the Sail file %s would become %s, whose name is also that of a generated file (%s.v); rename the Sail file"
              path (ModulePath.to_file ~directory module_path) name;
          match List.find !sail_files ~f:(fun (sail_file : 'a sail_file) -> ModulePath.equal sail_file.module_path module_path) with
          | Some other when not (String.equal other.relative_path relative_path) -> failwith @@ Printf.sprintf
              "preserve-file-structure: the Sail files %s and %s would both become %s; rename one of them"
              other.path path (ModulePath.to_file ~directory module_path)
          | Some _ ->
            sail_files := List.map !sail_files ~f:(fun (sail_file : 'a sail_file) ->
                if ModulePath.equal sail_file.module_path module_path then { sail_file with items = item :: sail_file.items } else sail_file
              )
          | None ->
            sail_files := { module_path; path; relative_path; items = [ item ] } :: !sail_files
        end
    );
  {
    sail_files = List.rev_map !sail_files ~f:(fun (sail_file : 'a sail_file) -> { sail_file with items = List.rev sail_file.items });
    assembly   = List.rev !assembly;
  }
