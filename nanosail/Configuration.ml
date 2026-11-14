(*

   This module serves as globally accessible source of configuration settings.

*)
open ExtBase


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
   See the translate-union-as-sum Slang function below.
*)
type sum_union =
  {
    union_identifier  : Ast.Identifier.t;
    left_constructor  : Ast.Identifier.t;
    right_constructor : Ast.Identifier.t;
  }


(*
   What the uses of a function that the configuration excludes become.
   See the Slang functions exclude-functions-as-no-op, exclude-functions-as-failure
   and exclude-functions-as-unconstrained below.
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

  (* The name of the Slang function that selects this exclusion *)
  let slang_function_name (exclusion : t) : string =
    match exclusion with
    | NoOp          -> "exclude-functions-as-no-op"
    | Failure       -> "exclude-functions-as-failure"
    | Unconstrained -> "exclude-functions-as-unconstrained"
    | Value _       -> "exclude-functions-as-value"
end


module Implementation = struct
  include ConfigLib.BuildContext.Make(struct end)

  module EC = Slang.EvaluationContext

  open Slang.Prelude.Shared

  module Exported = struct
    (*

       Name of the generated base module.

       Example Usage
       -------------

         (base-name "MinimalCapsBase")

    *)
    let base_name = string "base-name" "UntitledBase"

    (*

       Name of the generated program module.

       Example Usage
       -------------

         (program-name "MyLittleProgram")

    *)
    let program_name = string "program-name" "ModelProgram"

    (*
       Verbosity level.

       This can only be configured by setting an environment variable named "VERBOSE" to a certain number representing
       the desired verbosity level.
       To see which values are valid and which verbosity levels they represent, see LogLib.VerbosityLevel.of_int.


       Example Usage
       -------------
       $ VERBOSE=0 sail ...        # Suppress all logging
       $ VERBOSE=4 sail ...        # Enable all logging
    *)
    let verbosity_level_environment_variable = "VERBOSE"
    let verbosity_level = environment_variable verbosity_level_environment_variable LogLib.VerbosityLevel.default (LogLib.VerbosityLevel.of_int <. Int.of_string)

    (*
       The translation of match expressions can lead to an explosion in code generation.
       We therefore try out many different translations and look for the one that leads
       to the least amount of generated code.
       This is done in a brute force way: all permutations of a list are tried one after the other,
       i.e., there is a superexponential growth in the number of possibilities to consider.

       This setting imposes an upper limit on this number of permutations.

       For more detailed information, see the SailToNanosail.Translate.Match module.


       Example Usage
       -------------

         (pattern-tree-max-permutations 10000)
    *)
    let pattern_tree_maximum_permutations = integer "pattern-tree-max-permutations" 1000

    (*
       Annotate muSail definitions with their corresponding Sail definition.

       Example
       -------
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


       Example Usage
       -------------

         (include-original-code #t)
         (include-original-code #f)
         (include-original-code)    ; Same as (include-original-code #t)

    *)
    let include_original_code = bool "include-original-code" false

    (*
       Ignore overload definitions
       Overloads should be resolved by Sail, so there's no need to process them.
    *)
    let ignore_overloads = bool "ignore-all-overloads" false

    (* Ignore the default Order line *)
    let ignore_default_order = bool "ignore-default-order" true

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
    let pretty_print_lists = bool "pretty-print-lists" false

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

       Example Usage
       -------------

         (pretty-print-let #t)
         (pretty-print-let #f)
         (pretty-print-let)        ; Same as (pretty-print-let #t)

    *)
    let pretty_print_let = bool "pretty-print-let" false

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
    let pretty_print_match_enum = bool"pretty-print-match-enum" false

    (*

       Plain printing produces

        stm_exp (exp_binop bop.bvadd (exp_var "x") (exp_var "y")).

       Pretty printing produces

         stm_exp (((exp_var "x") +ᵇ (exp_var "y"))).

    *)
    let pretty_print_binary_operators = bool "pretty-print-binary-operators" false

    (*

       Plain printing produces

         stm_call foo (env.snoc (env.snoc (env.snoc (env.nil) (_::_) ((exp_int 1%Z))%exp) (_::_) ((exp_false))%exp) (_::_) ((exp_val (ty.bvec 4) ([bv 1])))%exp).


       Pretty printing produces

         (call foo (exp_int 1%Z) (exp_false) (exp_val (ty.bvec 4) ([bv 1])))%exp.

    *)
    let pretty_print_function_calls = bool "pretty-print-function-calls" false

    (*
       Outputs extra debug info.
       Superseded by HTML generation.
    *)
    let show_generation_blocks = bool "show-generation-blocks" false

    (*
       Use bv 0 instead of Bitvector.bv.zero, analogously for ones
    *)
    let bitvectors_zeros_ones_as_literal = bool "literal-zeros-and-ones" false

    (*
       Needed in certain circumstances to make Coq successfully derive NoConfusionHom for Reg.
    *)
    let inline_definitions_in_notations = bool "inline-definitions-in-notations" true

    (*
       Annotates functions with the nanosail AST in F-Expression form.
    *)
    let annotate_functions_with_ast = bool "annotate-functions-with-ast" false
    
    (*
       Dumps a sail definition's nanosail AST to stdout as soon as it
       is completed.
    *)
    let dump_nanosail_ast = bool "dump-nanosail-ast" false

    let ignore_pragma_predicate                    = string_predicate "ignore-pragmas"                             (Fn.const false)
    let ignore_type_definition_predicate           = string_predicate "ignore-type-definition-predicate"           (Fn.const false)
    let ignore_value_definition_predicate          = string_predicate "ignore-value-definition-predicate"          (Fn.const false)
    let ignore_function_definition_predicate       = string_predicate "ignore-function-definition-predicate"       (Fn.const false)
    let ignore_top_level_type_constraint_predicate = string_predicate "ignore-top-level-type-constraint-predicate" (Fn.const false)

    (*
       Names of definitions to ignore in addition to those the predicates above select;
       see the Slang functions ignore-functions and ignore-values below.
    *)
    let ignored_function_names                     = ConfigLib.Setting.mk (Base.Set.empty (module Base.String))
    let ignored_value_names                        = ConfigLib.Setting.mk (Base.Set.empty (module Base.String))

    (*
       Definitions removed together with their uses;
       see the Slang functions exclude-functions-as-no-op, ..., and exclude-registers below.
    *)
    let excluded_functions                         = ConfigLib.Setting.mk (Base.Map.empty (module Base.String) : FunctionExclusion.t Base.Map.M(Base.String).t)
    let excluded_registers                         = ConfigLib.Setting.mk (Base.Set.empty (module Base.String))

    let template_block_left_delimiter              = string "template-block-left-delimiter"  "(*<"
    let template_block_right_delimiter             = string "template-block-right-delimiter" ">*)"

    let template_translations                      = ConfigLib.Setting.mk ([] : template_translation list)

    let sum_unions                                 = ConfigLib.Setting.mk ([] : sum_union list)
  end

  (*
     Defines Slang function named template.

       (template in-file)
       (template in-file out-file)
  *)
  let () =
    let exported_function_name = "template"
    and add_translation template_filename output_filename =
      ConfigLib.Setting.update Exported.template_translations ~f:(fun x -> { template_filename; output_filename } :: x)
    and derive_output_filename_from_template_filename (template_filename : string) : string =
      (*
         Given the filename of the template file, derives an output filename from it.
         This is achieved by looking for a ".template" substring in template_filename and remove it.
       *)
      let pattern = String.Search_pattern.create ".template"
      in
      match String.Search_pattern.index_all pattern ~may_overlap:false ~in_:template_filename with
      | [] -> begin
          (* No ".template" substring was found *)
          let error_message =
            Printf.sprintf
              "Error: when using (template %s), %s must contain the substring \".template\"."
              template_filename
              template_filename
          in
          failwith error_message
        end
      | [_] -> begin
          (* Replace ".template" by the empty string *)
          String.Search_pattern.replace_first pattern ~in_:template_filename ~with_:""
        end
      | _  -> begin
          (* Multiple occurrences of ".template" found *)
          let error_message =
            Printf.sprintf
              {|Error: when calling template with a single argument (in this case "%s"), it must contain ".template" at most once|}
              template_filename
          in
          failwith error_message
        end
    in
    let handler_function =
      (*
         Deals with

          (template input)
      *)
      let unary_version evaluated_arguments =
        match Slang.Converters.(map1 string) evaluated_arguments with
        | Some template_filename -> begin
            let output_filename =
              derive_output_filename_from_template_filename template_filename
            in
            add_translation template_filename output_filename;
            EC.return @@ Some Slang.Value.Nil
          end
        | None -> EC.return None
      (*
         Deals with

          (template input output)
      *)
      and binary_version evaluated_arguments =
        match Slang.Converters.(map2 string string) evaluated_arguments with
        | Some (template_filename, output_filename) -> begin
            add_translation template_filename output_filename;
            EC.return @@ Some Slang.Value.Nil
          end
        | None -> EC.return None
      in
      Slang.Functions.mk_multimethod [ unary_version; binary_version ]
    in
    export_callable exported_function_name handler_function

  (* helper function template-block-delimiters that allows setting both template block delimiters in one step *)
  let () = export_strict_function "template-block-delimiters" @@ fun evaluated_arguments -> begin
      let=! left, right = Slang.Converters.(map2 string string) evaluated_arguments
      in
      ConfigLib.Setting.set Exported.template_block_left_delimiter left;
      ConfigLib.Setting.set Exported.template_block_right_delimiter right
    end


  let () =
    let exported_function_name =
      "ignore-function-definition-and-top-level-type-constraints-predicate"
    and setter (predicate : string -> bool) : unit =
      (ConfigLib.Setting.set Exported.ignore_function_definition_predicate) predicate;
      (ConfigLib.Setting.set Exported.ignore_top_level_type_constraint_predicate) predicate
    in
    export_string_predicate_setter exported_function_name setter


  (*
     Slang functions that ignore definitions by name:

       (ignore-functions '("f" "g"))   ignores the function definitions f and g, and their top-level type constraints
       (ignore-values '("x"))          ignores the top-level let definition x

     The names are ignored in addition to those the ignore predicates select
     (ignore-function-definition-and-top-level-type-constraints-predicate and
     ignore-value-definition-predicate), whether the predicate is set before or after.
     Each call adds to the names of earlier calls.

     The CHERIoT loop (tests/cheriot/run.py) appends such calls to the case study's
     configuration, for the definitions outside the scope it translates.
  *)
  let () =
    let export_name_adder
        (export_as : string                        )
        (setting   : Base.Set.M(Base.String).t ConfigLib.Setting.t) : unit
      =
      export_strict_function export_as @@ fun evaluated_arguments -> begin
        let=! names = Slang.Converters.(map1 (list string)) evaluated_arguments
        in
        ConfigLib.Setting.update setting ~f:(fun ignored -> Base.Set.union ignored (Base.Set.of_list (module Base.String) names))
      end
    in
    export_name_adder "ignore-functions" Exported.ignored_function_names;
    export_name_adder "ignore-values"    Exported.ignored_value_names


  (*
     Slang functions that exclude definitions together with their uses.
     Ignoring a function (ignore-functions) only drops its definition, so that its callers
     call a function that no longer exists. Excluding it also rewrites every call to it:

       (exclude-functions-as-no-op '("f" "g"))
         Each call becomes the unit value. Only for functions that return unit,
         such as printing and tracing; a call to one that returns something else is an error.

       (exclude-functions-as-failure '("f"))
         Each call becomes a failure (muSail's stm_fail), for code that must be unreachable.

       (exclude-functions-as-unconstrained '("f"))
         Each call becomes an unconstrained value of the call's type: a call to the foreign
         function (unconstrained σ) of the generated program, whose semantics allows every
         value of σ, and leaves registers and memory unchanged. A proof about a caller must then
         hold for every value the call could return: the value is universally quantified.
         The arguments are still computed, but the result does not depend on them.

       (exclude-functions-as-value '(("f" "None()") ("g" "0")))
         Each call to f becomes the Sail expression None(), each call to g the expression 0.
         The expression is type-checked at the type of the call, in the environment of the call,
         and then translated as if it were written there. Arguments with an effect are still
         computed (let excluded_call_arguments = <arguments> in <expression>); the others are dropped.
         For a function whose result is known, such as a platform setting.

       (exclude-registers '("r"))
         Leaves the register r out of Reg. Each read of r becomes an unconstrained value of its
         type (as above), each write to r a no-op (the written value is still computed).

     An excluded function's definition and its val specification are left out, as with
     ignore-functions. A function excluded in two different ways is an error.
     Each call adds to the names of earlier calls.
     The names are Sail's names, as in ignore-functions.
  *)
  let () =
    let add_exclusions (exclusions : (string * FunctionExclusion.t) list) : unit =
      let add
          (excluded            : FunctionExclusion.t Base.Map.M(Base.String).t)
          ((name, exclusion)   : string * FunctionExclusion.t                 ) : FunctionExclusion.t Base.Map.M(Base.String).t
        =
        match Base.Map.find excluded name with
        | None                   -> Base.Map.set excluded ~key:name ~data:exclusion
        | Some earlier_exclusion -> begin
            if FunctionExclusion.equal earlier_exclusion exclusion
            then excluded
            else begin
              let describe (exclusion : FunctionExclusion.t) : string =
                match exclusion with
                | Value value                     -> Printf.sprintf "%s (with %s)" (FunctionExclusion.slang_function_name exclusion) value
                | NoOp | Failure | Unconstrained  -> FunctionExclusion.slang_function_name exclusion
              in
              let message =
                Printf.sprintf
                  "Error: %s is excluded twice, by %s and by %s"
                  name
                  (describe earlier_exclusion)
                  (describe exclusion)
              in
              failwith message
            end
          end
      in
      ConfigLib.Setting.update Exported.excluded_functions ~f:(fun excluded -> List.fold exclusions ~init:excluded ~f:add)
    in
    let export_function_excluder (exclusion : FunctionExclusion.t) : unit =
      export_strict_function (FunctionExclusion.slang_function_name exclusion) @@ fun evaluated_arguments -> begin
        let=! names = Slang.Converters.(map1 (list string)) evaluated_arguments
        in
        add_exclusions @@ List.map names ~f:(fun name -> (name, exclusion))
      end
    in
    export_function_excluder NoOp;
    export_function_excluder Failure;
    export_function_excluder Unconstrained;
    export_strict_function (FunctionExclusion.slang_function_name (Value "")) begin fun evaluated_arguments ->
      let=! pairs = Slang.Converters.(map1 (list (tuple2 string string))) evaluated_arguments
      in
      add_exclusions @@ List.map pairs ~f:(fun (name, value) -> (name, FunctionExclusion.Value value))
    end;
    export_strict_function "exclude-registers" @@ fun evaluated_arguments -> begin
      let=! names = Slang.Converters.(map1 (list string)) evaluated_arguments
      in
      ConfigLib.Setting.update Exported.excluded_registers ~f:(fun excluded -> Base.Set.union excluded (Base.Set.of_list (module Base.String) names))
    end


  (*
     Slang function to translate a union to muSail's ty.sum instead of to a ty.union of its own.
     Multiple calls are allowed, one per union.

       (translate-union-as-sum <union> <left constructor> <right constructor>)

     The union must have exactly the two given constructors.
     The left constructor becomes inl, the right one inr.
     muSail's ty.sum is polymorphic, so all instances of a polymorphic union share the same representation:
     option(int) and option(bits(3)) both become sums, instead of having to be monomorphized.

     For example, with Sail's standard library

       union option('a : Type) = {
         Some : 'a,
         None : unit
       }

       union result('a : Type, 'b : Type) = {
         Ok  : 'a,
         Err : 'b
       }

     the calls

       (translate-union-as-sum "option" "Some" "None")
       (translate-union-as-sum "result" "Ok" "Err")

     translate option(T) to ty.sum T ty.unit, which is muSail's ty.option T,
     and result(A, B) to ty.sum A B.
     Some(x) becomes exp_inl x, None() becomes exp_inr (exp_val ty.unit tt),
     and a match on either becomes stm_match_sum.
  *)
  let () =
    export_strict_function "translate-union-as-sum" @@ fun evaluated_arguments -> begin
      let=! union_identifier, left_constructor, right_constructor = Slang.Converters.(map3 string string string) evaluated_arguments
      in
      let sum_union =
        {
          union_identifier  = Ast.Identifier.mk union_identifier;
          left_constructor  = Ast.Identifier.mk left_constructor;
          right_constructor = Ast.Identifier.mk right_constructor;
        }
      in
      ConfigLib.Setting.update Exported.sum_unions ~f:(fun sum_unions -> sum_union :: sum_unions)
    end


  let monomorphization_requests : monomorphization_request list Ast.Identifier.Map.t ref =
    ref Ast.Identifier.Map.empty

  (*
     Adds an extra entry to monomorphization_requests.
  *)
  let register_monomorphization_request
      (polymorphic_identifier : Ast.Identifier.t        )
      (request                : monomorphization_request) : unit
    =
    let update (requests : monomorphization_request list option) : monomorphization_request list =
      let requests =
        Option.value ~default:[] requests
      in
      request :: requests
    in
    monomorphization_requests := Ast.Identifier.Map.update !monomorphization_requests polymorphic_identifier ~f:update


  (*
     Slang function to request monomorphizations for a specific function.
     Multiple calls are allowed.

     For example, take the following polymorphic function:

       val foo : forall 'n 'm. (bitvector('n), bitvector('m)) -> bitvector(4)

     It can be monomorphized for n=1 and m=2 with

       (monomorphize "foo"
                     "foo_1_2"
                     '(
                       ("'n" 1)
                       ("'m" 2)
                     ))
  *)
  let () =
    let exported_function_name =
      "monomorphize"
    and handler_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let open Monads.Notations.Star(Slang.EvaluationContext)
      in
      let* evaluated_arguments = EC.map ~f:Evaluation.evaluate arguments
      in
      let=! polymorphic_identifier, monomorphization_identifier, substitutions =
        Converters.(map3 string string (list (tuple2 string integer))) evaluated_arguments
      in
      let polymorphic_identifier      = Ast.Identifier.mk polymorphic_identifier
      and monomorphization_identifier = Ast.Identifier.mk monomorphization_identifier
      and substitutions               = List.map ~f:(fun (id, n) -> (Ast.Identifier.mk id, n)) substitutions
      in
      let monomorphization_request : monomorphization_request = {
        monomorphization_identifier;
        substitutions;
      }
      in
      register_monomorphization_request polymorphic_identifier monomorphization_request;
      EC.return @@ Value.Nil
    in
    export_callable exported_function_name handler_function
end

include Implementation.Exported


let requested_monomorphizations_for (function_identifier : Ast.Identifier.t) : monomorphization_request list option =
  Ast.Identifier.Map.find !Implementation.monomorphization_requests function_identifier


(*
   Returns the translate-union-as-sum request for the union with the given name, if any.
*)
let sum_union_for (union_identifier : Ast.Identifier.t) : sum_union option =
  List.find (ConfigLib.Setting.get sum_unions) ~f:(fun (sum_union : sum_union) -> Ast.Identifier.equal sum_union.union_identifier union_identifier)


let load_configuration = Implementation.load_configuration
let get                = ConfigLib.Setting.get
let set                = ConfigLib.Setting.set


(*
   Whether to ignore the definition with the given name: its ignore predicate
   selects it, or it is one of the names given to ignore-functions or ignore-values.
*)
let should_ignore_function_definition (identifier : string) : bool =
  get ignore_function_definition_predicate identifier
  || Base.Set.mem (get ignored_function_names) identifier
  || Base.Map.mem (get excluded_functions) identifier

let should_ignore_top_level_type_constraint (identifier : string) : bool =
  get ignore_top_level_type_constraint_predicate identifier
  || Base.Set.mem (get ignored_function_names) identifier
  || Base.Map.mem (get excluded_functions) identifier

let should_ignore_value_definition (identifier : string) : bool =
  get ignore_value_definition_predicate identifier || Base.Set.mem (get ignored_value_names) identifier


(*
   How the uses of the function with the given (Sail) name are translated,
   if the configuration excludes it; see exclude-functions-as-no-op and the like.
*)
let function_exclusion (identifier : string) : FunctionExclusion.t option =
  Base.Map.find (get excluded_functions) identifier


(*
   Whether the configuration excludes the register with the given (Sail) name; see exclude-registers.
*)
let is_excluded_register (identifier : string) : bool =
  Base.Set.mem (get excluded_registers) identifier
