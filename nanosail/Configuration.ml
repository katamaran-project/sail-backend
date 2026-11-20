(*

   This module serves as globally accessible source of configuration settings.

   The settings are read from a configuration file of S-expressions (docs/usage.md#configuration-file),
   one form per option:

     (include ../shared-configuration.sexp)
     (templates base.template.v machine.template.v)
     (dump-nanosail-ast)
     (ignore (functions "undefined_*" log2) (types regfp))

   Repeating an option adds to it: lists are combined, later values of single settings win.

*)
open ExtBase


module Setting = struct
  type 'a t = 'a ref

  let mk (initial : 'a) : 'a t = ref initial
  let get (setting : 'a t) : 'a = !setting
  let set (setting : 'a t) (value : 'a) : unit = setting := value
  let update (setting : 'a t) ~(f : 'a -> 'a) : unit = setting := f !setting
end


type monomorphization_request =
  {
    monomorphization_identifier : Ast.Identifier.t;
    substitutions               : (Ast.Identifier.t * int) list;
  }

type template_translation =
  {
    template_filename : string;
    output_filename   : string;
  }

(*
   Requests that a union be translated to muSail's ty.sum.
   See the option union-as-sum below.
*)
type sum_union =
  {
    union_identifier  : Ast.Identifier.t;
    left_constructor  : Ast.Identifier.t;
    right_constructor : Ast.Identifier.t;
  }


(*
   What the uses of a function that the configuration excludes become.
   See the option exclude below.
*)
module FunctionExclusion = struct
  type t =
    | NoOp            (* a call becomes the unit value; only for functions returning unit *)
    | Failure         (* a call becomes stm_fail: the call must be unreachable *)
    | Unconstrained   (* a call becomes an unconstrained value of the call's type *)
    | Value of string (* a call becomes this Sail expression, type-checked at the call's type *)

  let equal (x : t) (y : t) : bool =
    match x, y with
    | NoOp         , NoOp          -> true
    | Failure      , Failure       -> true
    | Unconstrained, Unconstrained -> true
    | Value x      , Value y       -> String.equal x y
    | NoOp         , (Failure | Unconstrained | Value _)
    | Failure      , (NoOp | Unconstrained | Value _)
    | Unconstrained, (NoOp | Failure | Value _)
    | Value _      , (NoOp | Failure | Unconstrained) -> false

  (* The configuration entry that selects this exclusion, for messages *)
  let configuration_name (exclusion : t) : string =
    match exclusion with
    | NoOp          -> "(exclude (as-no-op ...))"
    | Failure       -> "(exclude (as-failure ...))"
    | Unconstrained -> "(exclude (as-unconstrained ...))"
    | Value _       -> "(exclude (as-value ...))"
end


(*
   The semantics of a foreign function, i.e., of a Sail extern that is not translated into a muSail operation.
   See the option foreign-functions below.
*)
module ForeignFunctionSemantics = struct
  type t =
    | Given of string (* a Rocq term computing the result from the arguments arg1, arg2, ... *)
    | Unconstrained   (* the result is any value of the result type *)

  let equal (x : t) (y : t) : bool =
    match x, y with
    | Given x      , Given y       -> String.equal x y
    | Unconstrained, Unconstrained -> true
    | Given _      , Unconstrained
    | Unconstrained, Given _       -> false
end


(*
   Name of the generated base module.

     (base-name MinimalCapsBase)
*)
let base_name = Setting.mk "UntitledBase"

(*
   Name of the generated program module.

     (program-name MyLittleProgram)
*)
let program_name = Setting.mk "ModelProgram"

(*
   Verbosity level.

   This can only be configured by setting an environment variable named "VERBOSE" to a certain number representing
   the desired verbosity level.
   To see which values are valid and which verbosity levels they represent, see LogLib.VerbosityLevel.of_int.

     $ VERBOSE=0 sail ...        # Suppress all logging
     $ VERBOSE=4 sail ...        # Enable all logging
*)
let verbosity_level_environment_variable = "VERBOSE"

let verbosity_level =
  let setting = Setting.mk LogLib.VerbosityLevel.default
  in
  begin
    match Sys.getenv_opt verbosity_level_environment_variable with
    | Some value -> Setting.set setting (LogLib.VerbosityLevel.of_int (Int.of_string value))
    | None       -> ()
  end;
  setting

(*
   The translation of match expressions can lead to an explosion in code generation.
   We therefore try out many different translations and look for the one that leads
   to the least amount of generated code.
   This is done in a brute force way: all permutations of a list are tried one after the other,
   i.e., there is a superexponential growth in the number of possibilities to consider.

   This setting imposes an upper limit on this number of permutations.
   For more detailed information, see the SailToNanosail.Translate.Match module.

     (pattern-tree-max-permutations 10000)
*)
let pattern_tree_maximum_permutations = Setting.mk 1000

(*
   Annotate muSail definitions with their corresponding Sail definition.

   The following Sail code

      val double : int -> int
      function double(x) = {
        2 * x
      }

   becomes

     (*
       Original Sail code

         val double : int -> int
         $[complete]
         function double x = $[overloaded { "name" = "*", "is_infix" = true }] mult_atom(2, x)
     *)
     Definition fun_double : Stm [
                                   "x"  ∷  ty.int
                                 ]
                                 (ty.int) :=
       stm_exp (((exp_int 2%Z))*((exp_var "x"))).

     (include-original-code)          ; same as (include-original-code true)
     (include-original-code false)
*)
let include_original_code = Setting.mk false

(*
   Ignore overload definitions: overloads are resolved by Sail, so there's no need to process them.

     (ignore (overloads))
*)
let ignore_overloads = Setting.mk false

(*
   Ignore the default Order line.

     (ignore (default-order false))
*)
let ignore_default_order = Setting.mk true

(*
   Plain printing produces

      (cons (exp_int 1%Z)
            (cons (exp_int 2%Z)
                  (cons (exp_int 3%Z)
                        nil)))

   Pretty printing produces

      [
        exp_int 1%Z;
        exp_int 2%Z;
        exp_int 3%Z
      ]
*)
let pretty_print_lists = Setting.mk false

(*
   Plain printing produces

    stm_let "x"
            (ty.int)
            (stm_exp (exp_int 5%Z))
            (stm_exp (exp_var "x"))

   Pretty printing produces

    let: "x" :: ty.int := stm_exp (exp_int 5%Z)
    in
      stm_exp (exp_var "x")
*)
let pretty_print_let = Setting.mk false

(*
   Plain printing produces

      stm_match_enum Emyenum
                     (stm_exp (exp_var "ж0"))
                     (fun K => match K with
                               | x => stm_exp (exp_int 1%Z)
                               | y => stm_exp (exp_int 2%Z)
                               | z => stm_exp (exp_int 3%Z)
                               end)

   Pretty printing produces

      match: (stm_exp (exp_var "ж0")) in Emyenum with
       | x  =>  stm_exp (exp_int 1%Z)
       | y  =>  stm_exp (exp_int 2%Z)
       | z  =>  stm_exp (exp_int 3%Z)
      end
*)
let pretty_print_match_enum = Setting.mk false

(*
   Plain printing produces

    stm_exp (exp_binop bop.bvadd (exp_var "x") (exp_var "y")).

   Pretty printing produces

     stm_exp (((exp_var "x") +ᵇ (exp_var "y"))).
*)
let pretty_print_binary_operators = Setting.mk false

(*
   Plain printing produces

     stm_call foo (env.snoc (env.snoc (env.snoc (env.nil) (_::_) ((exp_int 1%Z))%exp) (_::_) ((exp_false))%exp) (_::_) ((exp_val (ty.bvec 4) ([bv 1])))%exp).

   Pretty printing produces

     (call foo (exp_int 1%Z) (exp_false) (exp_val (ty.bvec 4) ([bv 1])))%exp.
*)
let pretty_print_function_calls = Setting.mk false

(*
   Outputs extra debug info.
   Superseded by HTML generation.
*)
let show_generation_blocks = Setting.mk false

(* Use bv 0 instead of Bitvector.bv.zero, analogously for ones *)
let bitvectors_zeros_ones_as_literal = Setting.mk false

(* Needed in certain circumstances to make Coq successfully derive NoConfusionHom for Reg. *)
let inline_definitions_in_notations = Setting.mk true

(* Annotates functions with the nanosail AST in F-Expression form. *)
let annotate_functions_with_ast = Setting.mk false

(* Dumps a sail definition's nanosail AST to stdout as soon as it is completed. *)
let dump_nanosail_ast = Setting.mk false

(*
   Definitions to ignore, by name; see the option ignore below.
*)
let ignored_pragmas              = Setting.mk NameFilter.empty
let ignored_types                = Setting.mk NameFilter.empty
let ignored_values               = Setting.mk NameFilter.empty
let ignored_functions            = Setting.mk NameFilter.empty   (* definition and top-level type constraint *)
let ignored_function_definitions = Setting.mk NameFilter.empty
let ignored_type_constraints     = Setting.mk NameFilter.empty

(*
   Definitions removed together with their uses; see the option exclude below.
*)
let excluded_functions = Setting.mk (Base.Map.empty (module Base.String) : FunctionExclusion.t Base.Map.M(Base.String).t)
let excluded_registers = Setting.mk (Base.Set.empty (module Base.String))

(*
   Semantics of foreign functions given by the configuration, by Sail name; see the option foreign-functions below.
*)
let foreign_function_semantics = Setting.mk (Base.Map.empty (module Base.String) : ForeignFunctionSemantics.t Base.Map.M(Base.String).t)

(*
   New names for Sail identifiers in the output, by Sail name; see the option rename below.
*)
let renamed_identifiers = Setting.mk (Base.Map.empty (module Base.String) : string Base.Map.M(Base.String).t)

let template_block_left_delimiter  = Setting.mk "(*<"
let template_block_right_delimiter = Setting.mk ">*)"

let template_translations = Setting.mk ([] : template_translation list)

let sum_unions = Setting.mk ([] : sum_union list)

let monomorphization_requests : monomorphization_request list Ast.Identifier.Map.t ref =
  ref Ast.Identifier.Map.empty


let get = Setting.get
let set = Setting.set


(*
   Reading the configuration
*)
module Reader = struct
  module Node = ConfigurationFile.Node

  let error = ConfigurationFile.error

  (* A name: an atom, quoted or not *)
  let name (node : Node.t) : string =
    match node with
    | Atom { text; _ }     -> text
    | List { location; _ } -> error location "expected a name, not a list"

  let names (nodes : Node.t list) : string list =
    List.map nodes ~f:name

  let single
      (option    : string    )
      (location  : ConfigurationFile.Location.t)
      (arguments : Node.t list) : Node.t
    =
    match arguments with
    | [ argument ] -> argument
    | _            -> error location @@ Printf.sprintf "%s takes one value" option

  (* (option) is true; (option true) and (option false) set it *)
  let flag
      (option    : string    )
      (location  : ConfigurationFile.Location.t)
      (arguments : Node.t list) : bool
    =
    match arguments with
    | []                                                    -> true
    | [ Atom { text = "true"; quoted = false; _ } ]         -> true
    | [ Atom { text = "false"; quoted = false; _ } ]        -> false
    | _                                                     -> error location @@ Printf.sprintf "%s takes no value, true or false" option

  let integer
      (option    : string    )
      (location  : ConfigurationFile.Location.t)
      (arguments : Node.t list) : int
    =
    let value = name (single option location arguments)
    in
    match Int.of_string_opt value with
    | Some n -> n
    | None   -> error location @@ Printf.sprintf "%s takes an integer, not %s" option value

  (* (a b) *)
  let pair (node : Node.t) : string * string =
    match node with
    | List { elements = [ first; second ]; _ } -> (name first, name second)
    | Atom { location; _ } | List { location; _ } -> error location @@ Printf.sprintf "expected a pair (<name> <value>), not %s" (Node.to_string node)

  (* (head arguments ...), the form of an option and of each of its parts *)
  let form (node : Node.t) : string * Node.t list * ConfigurationFile.Location.t =
    match node with
    | List { elements = Atom { text; quoted = false; _ } :: arguments; location } -> (text, arguments, location)
    | Atom { location; _ } | List { location; _ } -> error location @@ Printf.sprintf "expected (<option> ...), not %s" (Node.to_string node)

  (* The patterns of a name list, with the names of (except ...) entries as exceptions *)
  let name_filter (arguments : Node.t list) : NameFilter.t =
    let patterns, exceptions =
      List.partition_map arguments ~f:(fun (argument : Node.t) ->
          match argument with
          | Atom { text; _ }                                                         -> First text
          | List { elements = Atom { text = "except"; quoted = false; _ } :: excepted; _ } -> Second (names excepted)
          | List { location; _ }                                                     -> error location @@ Printf.sprintf "expected a name, a pattern or (except ...), not %s" (Node.to_string argument))
    in
    NameFilter.of_patterns ~patterns ~exceptions:(List.concat exceptions)

  let add_names (setting : NameFilter.t Setting.t) (arguments : Node.t list) : unit =
    Setting.update setting ~f:(fun filter -> NameFilter.union filter (name_filter arguments))

  let add_name_set (setting : Base.Set.M(Base.String).t Setting.t) (arguments : Node.t list) : unit =
    Setting.update setting ~f:(fun names' -> Base.Set.union names' (Base.Set.of_list (module Base.String) (names arguments)))

  let add_template (template_filename : string) (location : ConfigurationFile.Location.t) : unit =
    (* The output file is named after the template, without ".template" *)
    let pattern = String.Search_pattern.create ".template"
    in
    let output_filename =
      match String.Search_pattern.index_all pattern ~may_overlap:false ~in_:template_filename with
      | [_] -> String.Search_pattern.replace_first pattern ~in_:template_filename ~with_:""
      | _   -> error location @@ Printf.sprintf "the template %s must contain \".template\" exactly once" template_filename
    in
    Setting.update template_translations ~f:(fun templates -> { template_filename; output_filename } :: templates)

  let add_exclusions
      (location   : ConfigurationFile.Location.t)
      (exclusions : (string * FunctionExclusion.t) list) : unit
    =
    let add
        (excluded          : FunctionExclusion.t Base.Map.M(Base.String).t)
        ((name, exclusion) : string * FunctionExclusion.t                 ) : FunctionExclusion.t Base.Map.M(Base.String).t
      =
      match Base.Map.find excluded name with
      | None                                                              -> Base.Map.set excluded ~key:name ~data:exclusion
      | Some earlier when FunctionExclusion.equal earlier exclusion       -> excluded
      | Some earlier -> begin
          error location @@ Printf.sprintf "%s is excluded twice, by %s and by %s"
            name
            (FunctionExclusion.configuration_name earlier)
            (FunctionExclusion.configuration_name exclusion)
        end
    in
    Setting.update excluded_functions ~f:(fun excluded -> List.fold exclusions ~init:excluded ~f:add)

  let add_foreign_function_semantics
      (location : ConfigurationFile.Location.t)
      (pairs    : (string * ForeignFunctionSemantics.t) list) : unit
    =
    let add semantics (name, semantics') =
      match Base.Map.find semantics name with
      | None                                                                -> Base.Map.set semantics ~key:name ~data:semantics'
      | Some earlier when ForeignFunctionSemantics.equal earlier semantics' -> semantics
      | Some _                                                              -> error location @@ Printf.sprintf "foreign function %s is given two different semantics" name
    in
    Setting.update foreign_function_semantics ~f:(fun semantics -> List.fold pairs ~init:semantics ~f:add)

  let add_renaming
      (location : ConfigurationFile.Location.t)
      (pairs    : (string * string) list) : unit
    =
    let add renamed (name, new_name) =
      match Base.Map.find renamed name with
      | None                                            -> Base.Map.set renamed ~key:name ~data:new_name
      | Some earlier when String.equal earlier new_name -> renamed
      | Some earlier                                    -> error location @@ Printf.sprintf "%s is renamed to both %s and %s" name earlier new_name
    in
    Setting.update renamed_identifiers ~f:(fun renamed -> List.fold pairs ~init:renamed ~f:add)

  let add_monomorphization (node : Node.t) : unit =
    (* (f (name f_1_2) (values ('n 1) ('m 2))) *)
    let function_name, parts, location = form node
    in
    let part (wanted : string) : Node.t list =
      match List.filter_map parts ~f:(fun part -> let head, arguments, _ = form part in if String.equal head wanted then Some arguments else None) with
      | [ arguments ] -> arguments
      | _             -> error location @@ Printf.sprintf "the monomorphization of %s needs exactly one (%s ...)" function_name wanted
    in
    List.iter parts ~f:(fun part ->
        let head, _, part_location = form part in
        if not (List.mem [ "name"; "values" ] head ~equal:String.equal)
        then error part_location @@ Printf.sprintf "unknown part (%s ...) of a monomorphization; expected name or values" head);
    let monomorphization_identifier = Ast.Identifier.mk (name (single "name" location (part "name")))
    and substitutions =
      List.map (part "values") ~f:(fun value ->
          let type_variable, n = pair value in
          match Int.of_string_opt n with
          | Some n -> (Ast.Identifier.mk type_variable, n)
          | None   -> error (Node.location value) @@ Printf.sprintf "the value of %s must be an integer, not %s" type_variable n)
    in
    let request = { monomorphization_identifier; substitutions }
    in
    let update (requests : monomorphization_request list option) : monomorphization_request list =
      request :: Option.value ~default:[] requests
    in
    monomorphization_requests := Ast.Identifier.Map.update !monomorphization_requests (Ast.Identifier.mk function_name) ~f:update

  (* Applies each part (head ...) of an option such as (ignore (functions ...) (types ...)) *)
  let parts
      (option    : string)
      (arguments : Node.t list)
      ~(f        : string -> Node.t list -> ConfigurationFile.Location.t -> unit) : unit
    =
    ignore option;
    List.iter arguments ~f:(fun argument -> let head, part_arguments, location = form argument in f head part_arguments location)

  let unknown_part (option : string) (part : string) (location : ConfigurationFile.Location.t) (known : string list) : 'a =
    error location @@ Printf.sprintf "unknown part (%s ...) of %s; expected one of %s" part option (String.concat ~sep:", " known)

  let flags : (string * bool Setting.t) list =
    [
      ("include-original-code"           , include_original_code           );
      ("pretty-print-lists"              , pretty_print_lists              );
      ("pretty-print-let"                , pretty_print_let                );
      ("pretty-print-match-enum"         , pretty_print_match_enum         );
      ("pretty-print-binary-operators"   , pretty_print_binary_operators   );
      ("pretty-print-function-calls"     , pretty_print_function_calls     );
      ("show-generation-blocks"          , show_generation_blocks          );
      ("literal-zeros-and-ones"          , bitvectors_zeros_ones_as_literal);
      ("inline-definitions-in-notations" , inline_definitions_in_notations );
      ("annotate-functions-with-ast"     , annotate_functions_with_ast     );
      ("dump-nanosail-ast"               , dump_nanosail_ast               );
    ]

  let apply (node : Node.t) : unit =
    let option, arguments, location = form node
    in
    match List.Assoc.find flags option ~equal:String.equal with
    | Some setting -> Setting.set setting (flag option location arguments)
    | None -> begin
        match option with
        | "templates"                     -> List.iter arguments ~f:(fun argument -> add_template (name argument) (Node.location argument))
        | "base-name"                     -> Setting.set base_name (name (single option location arguments))
        | "program-name"                  -> Setting.set program_name (name (single option location arguments))
        | "pattern-tree-max-permutations" -> Setting.set pattern_tree_maximum_permutations (integer option location arguments)
        | "template-block-delimiters"     -> begin
            match arguments with
            | [ left; right ] -> Setting.set template_block_left_delimiter (name left); Setting.set template_block_right_delimiter (name right)
            | _               -> error location "template-block-delimiters takes two strings: the left and the right delimiter"
          end
        | "ignore" -> begin
            let known = [ "overloads"; "default-order"; "pragmas"; "types"; "values"; "functions"; "function-definitions"; "type-constraints" ]
            in
            parts option arguments ~f:(fun part part_arguments part_location ->
                match part with
                | "overloads"            -> Setting.set ignore_overloads (flag part part_location part_arguments)
                | "default-order"        -> Setting.set ignore_default_order (flag part part_location part_arguments)
                | "pragmas"              -> add_names ignored_pragmas part_arguments
                | "types"                -> add_names ignored_types part_arguments
                | "values"               -> add_names ignored_values part_arguments
                | "functions"            -> add_names ignored_functions part_arguments
                | "function-definitions" -> add_names ignored_function_definitions part_arguments
                | "type-constraints"     -> add_names ignored_type_constraints part_arguments
                | _                      -> unknown_part option part part_location known)
          end
        | "exclude" -> begin
            let known = [ "as-no-op"; "as-failure"; "as-unconstrained"; "as-value"; "registers" ]
            in
            parts option arguments ~f:(fun part part_arguments part_location ->
                let excluded_as (exclusion : FunctionExclusion.t) =
                  add_exclusions part_location @@ List.map (names part_arguments) ~f:(fun name -> (name, exclusion))
                in
                match part with
                | "as-no-op"         -> excluded_as NoOp
                | "as-failure"       -> excluded_as Failure
                | "as-unconstrained" -> excluded_as Unconstrained
                | "as-value"         -> add_exclusions part_location @@ List.map part_arguments ~f:(fun node -> let name, value = pair node in (name, FunctionExclusion.Value value))
                | "registers"        -> add_name_set excluded_registers part_arguments
                | _                  -> unknown_part option part part_location known)
          end
        | "foreign-functions" -> begin
            parts option arguments ~f:(fun part part_arguments part_location ->
                match part with
                | "semantics"     -> add_foreign_function_semantics part_location @@ List.map part_arguments ~f:(fun node -> let name, term = pair node in (name, ForeignFunctionSemantics.Given term))
                | "unconstrained" -> add_foreign_function_semantics part_location @@ List.map (names part_arguments) ~f:(fun name -> (name, ForeignFunctionSemantics.Unconstrained))
                | _               -> unknown_part option part part_location [ "semantics"; "unconstrained" ])
          end
        | "rename"        -> add_renaming location @@ List.map arguments ~f:pair
        | "union-as-sum"  -> begin
            List.iter arguments ~f:(fun (argument : Node.t) ->
                match argument with
                | List { elements = [ union; left; right ]; _ } -> begin
                    let sum_union =
                      {
                        union_identifier  = Ast.Identifier.mk (name union);
                        left_constructor  = Ast.Identifier.mk (name left);
                        right_constructor = Ast.Identifier.mk (name right);
                      }
                    in
                    Setting.update sum_unions ~f:(fun sum_unions -> sum_union :: sum_unions)
                  end
                | Atom { location; _ } | List { location; _ } -> error location @@ Printf.sprintf "expected (<union> <left constructor> <right constructor>), not %s" (Node.to_string argument))
          end
        | "monomorphize"  -> List.iter arguments ~f:add_monomorphization
        | _               -> error location @@ Printf.sprintf "unknown option %s" option
      end
end


let load_configuration (path : string) : unit =
  List.iter (ConfigurationFile.read path) ~f:Reader.apply


let requested_monomorphizations_for (function_identifier : Ast.Identifier.t) : monomorphization_request list option =
  Ast.Identifier.Map.find !monomorphization_requests function_identifier


(*
   Returns the union-as-sum request for the union with the given name, if any.
*)
let sum_union_for (union_identifier : Ast.Identifier.t) : sum_union option =
  List.find (get sum_unions) ~f:(fun (sum_union : sum_union) -> Ast.Identifier.equal sum_union.union_identifier union_identifier)


let should_ignore_pragma (identifier : string) : bool =
  NameFilter.matches (get ignored_pragmas) identifier

let should_ignore_type_definition (identifier : string) : bool =
  NameFilter.matches (get ignored_types) identifier

(*
   Whether to ignore the definition with the given name: (ignore (functions ...)) or
   (ignore (function-definitions ...)) selects it, or it is excluded.
*)
let should_ignore_function_definition (identifier : string) : bool =
  NameFilter.matches (get ignored_functions) identifier
  || NameFilter.matches (get ignored_function_definitions) identifier
  || Base.Map.mem (get excluded_functions) identifier

let should_ignore_top_level_type_constraint (identifier : string) : bool =
  NameFilter.matches (get ignored_functions) identifier
  || NameFilter.matches (get ignored_type_constraints) identifier
  || Base.Map.mem (get excluded_functions) identifier

let should_ignore_value_definition (identifier : string) : bool =
  NameFilter.matches (get ignored_values) identifier


(*
   How the uses of the function with the given (Sail) name are translated,
   if the configuration excludes it; see the option exclude.
*)
let function_exclusion (identifier : string) : FunctionExclusion.t option =
  Base.Map.find (get excluded_functions) identifier


(*
   Whether the configuration excludes the register with the given (Sail) name; see (exclude (registers ...)).
*)
let is_excluded_register (identifier : string) : bool =
  Base.Set.mem (get excluded_registers) identifier


let renamed (identifier : string) : string =
  Option.value (Base.Map.find (get renamed_identifiers) identifier) ~default:identifier


(*
   The semantics the configuration gives to the foreign function with the given (Sail) name, if any;
   see the option foreign-functions.
*)
let foreign_function_semantics_for (identifier : string) : ForeignFunctionSemantics.t option =
  Base.Map.find (get foreign_function_semantics) identifier
