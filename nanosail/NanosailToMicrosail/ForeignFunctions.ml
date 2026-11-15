(*
   Sail externs as muSail foreign functions.

   A Sail extern is a val specification with an extern binding and no Sail body, e.g.

     val emod_int = pure {ocaml: "modulus", ..., _: "emod_int"} : (int, int) -> int

   A call to an extern is translated in one of two ways (see FunctionCalls.translate_extern_call):

   * If the extern corresponds to a muSail operation (a shift, a slice, bitvector subtraction,
     equality, ...), the call becomes that operation.
   * Otherwise, the extern is a foreign function: a constructor of FunX with the extern's
     signature, and the call a stm_foreign. Only the externs that are called this way get a
     constructor (GenerationContext.use_foreign_function records them).

   Externs are recognised by their binding (see binding_key), not by their Sail name:
   models declare the same builtins under different names (sail-riscv's shiftl is Sail's
   sail_shiftleft, for instance).

   The semantics of a foreign function (ForeignCall, see pp_foreign_call) never fails and never
   changes the registers: Sail externs cannot write Sail registers. A pure extern leaves the
   memory unchanged; a monadic one may change it in any way. Its result is

   * given by a Rocq term (Semantics.Given): the configuration's (foreign-function-semantics),
     or else the default for the extern's binding (see default_semantics);
   * or else unconstrained: any value of the result type. A proof about a caller must then hold
     for every value the call may return, which is how undefined values are treated as well.

   No foreign function reads the memory, so leaving the memory unchanged for pure externs that
   in reality write it (such as tag writes declared pure) cannot be observed.
*)
open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end

module Semantics = Configuration.ForeignFunctionSemantics


type t =
  {
    identifier      : Ast.Identifier.t;      (* the extern's name, also its name in FunX *)
    sail_name       : string;                (* the extern's Sail name *)
    pure            : bool;                  (* pure (true) or monadic (false) *)
    bindings        : (string * string) list;(* the extern's bindings, e.g., ("c", "emod_int") *)
    parameter_types : Ast.Type.t list;
    result_type     : Ast.Type.t;
  }


(*
   The name by which the translator recognises the builtin an extern binds to:
   its binding for all targets (_), or else its binding for C, or else its Sail name.
*)
let binding_key (foreign_function : t) : string =
  let find (target : string) : string option =
    List.Assoc.find foreign_function.bindings ~equal:String.equal target
  in
  match find "_" with
  | Some key -> key
  | None     -> begin
      match find "c" with
      | Some key -> key
      | None     -> foreign_function.sail_name
    end


(* The names of the parameters, in the signature (FunX) and in the semantics *)
let parameter_name (index : int) : string =
  Printf.sprintf "arg%d" (index + 1)


(*
   The extern declared by the Sail definition, if it is the val specification of an extern:
   its Sail name and its extern binding.
*)
let extern_of_sail_definition (sail_definition : Sail.sail_definition) : (string * Libsail.Ast.extern) option =
  let Libsail.Ast.DEF_aux (unwrapped_definition, _annotation) = sail_definition
  in
  match unwrapped_definition with
  | DEF_val (VS_aux (VS_val_spec (_, identifier, Some extern), _)) -> Some (Libsail.Ast_util.string_of_id identifier, extern)
  | DEF_val (VS_aux (VS_val_spec (_, _, None), _))                 -> None
  | DEF_type _                 -> None
  | DEF_fundef _               -> None
  | DEF_mapdef _               -> None
  | DEF_impl _                 -> None
  | DEF_let (_, _)             -> None
  | DEF_outcome (_, _)         -> None
  | DEF_instantiation (_, _)   -> None
  | DEF_fixity (_, _, _)       -> None
  | DEF_overload (_, _)        -> None
  | DEF_default _              -> None
  | DEF_scattered _            -> None
  | DEF_measure (_, _, _)      -> None
  | DEF_loop_measures (_, _)   -> None
  | DEF_register _             -> None
  | DEF_internal_mutrec _      -> None
  | DEF_constraint _           -> None
  | DEF_pragma (_, _)          -> None


(* The Sail name of the function the Sail definition gives a body to, if any *)
let function_with_body (sail_definition : Sail.sail_definition) : string option =
  let Libsail.Ast.DEF_aux (unwrapped_definition, _annotation) = sail_definition
  in
  match unwrapped_definition with
  | DEF_fundef function_definition -> Some (Libsail.Ast_util.string_of_id @@ Libsail.Ast_util.id_of_fundef function_definition)
  | DEF_val _                  -> None
  | DEF_type _                 -> None
  | DEF_mapdef _               -> None
  | DEF_impl _                 -> None
  | DEF_let (_, _)             -> None
  | DEF_outcome (_, _)         -> None
  | DEF_instantiation (_, _)   -> None
  | DEF_fixity (_, _, _)       -> None
  | DEF_overload (_, _)        -> None
  | DEF_default _              -> None
  | DEF_scattered _            -> None
  | DEF_measure (_, _, _)      -> None
  | DEF_loop_measures (_, _)   -> None
  | DEF_register _             -> None
  | DEF_internal_mutrec _      -> None
  | DEF_constraint _           -> None
  | DEF_pragma (_, _)          -> None


(*
   Whether a type can appear in a FunX signature: muSail has a counterpart for it, and the
   type-level integers it mentions (bitvector widths) can be bound by implicit arguments.
*)
let rec is_supported_type (typ : Ast.Type.t) : bool =
  match typ with
  | Unit                       -> true
  | Bool                       -> true
  | Int _                      -> true
  | String                     -> true
  | Bit                        -> true
  | Nat                        -> true
  | Range (_, _)               -> true
  | Enum _                     -> true
  | Record _                   -> true
  | Variant _                  -> true
  | Alias (_, _)               -> true
  | Bitvector _                -> true
  | TypeVariable _             -> true
  | List element_type          -> is_supported_type element_type
  | Tuple element_types        -> List.for_all ~f:is_supported_type element_types
  | Sum (left, right)          -> is_supported_type left && is_supported_type right
  | Application (_, _)         -> false
  | Vector (_, _)              -> false
  | Implicit _                 -> false
  | Function _                 -> false


(* The implicit arguments of the foreign function's FunX constructor (see TypeLevelParameters) *)
let implicit_parameters (foreign_function : t) : (Ast.Identifier.t * Ast.Kind.t) list =
  TypeLevelParameters.of_types (List.append foreign_function.parameter_types [ foreign_function.result_type ])


(*
   The externs of the program that can become foreign functions: val specifications with
   an extern binding, without a Sail body, whose signature muSail can express.
*)
let all_externs (program : Ast.Program.t) : t list =
  let with_body =
    Base.Set.of_list (module Base.String) @@ List.filter_map ~f:(Fn.compose function_with_body fst) program.definitions
  in
  let extern_of ((sail_definition, definition) : Sail.sail_definition * Ast.Definition.t) : t option =
    match extern_of_sail_definition sail_definition, definition with
    | Some (sail_name, extern), TopLevelTypeConstraintDefinition { identifier; typ = Function { parameter_types; result_type }; _ } -> begin
        if
          Base.Set.mem with_body sail_name
          || not (List.for_all ~f:is_supported_type (result_type :: parameter_types))
          (* the result type needs a value that Rocq can construct (see pp_foreign_progress_case) *)
          || List.exists (TypeLevelParameters.of_type result_type) ~f:(fun (_, kind) -> match kind with Type -> true | Int | Bool -> false)
        then None
        else Some { identifier; sail_name; pure = extern.pure; bindings = extern.bindings; parameter_types; result_type }
      end
    | Some _, _ -> None
    | None  , _ -> None
  in
  List.filter_map ~f:extern_of program.definitions


(* The extern with the given name, if it can become a foreign function *)
let find (identifier : Ast.Identifier.t) : t option GC.t =
  let* program = GC.get_program
  in
  GC.return @@ List.find (all_externs program) ~f:(fun foreign_function -> Ast.Identifier.equal foreign_function.identifier identifier)


(*
   The default semantics of the externs of Sail's library with a well-known meaning, by binding key.
   The terms refer to the arguments as arg1, arg2, ...; see Configuration's foreign-function-semantics.
   Some of these externs become muSail operations when their arguments allow it
   (FunctionCalls.translate_extern_call); the semantics covers the other calls.
*)
let default_semantics (foreign_function : t) : Semantics.t =
  let given (term : string) : Semantics.t = Semantics.Given term
  in
  match binding_key foreign_function with
  (* Euclidean division and modulus: 0 <= emod_int a b < |b| *)
  | "emod_int"             -> given "Z.modulo arg1 (Z.abs arg2)"
  | "ediv_int"             -> given "(Z.sgn arg2 * Z.div arg1 (Z.abs arg2))%Z"
  (* division and remainder rounding towards zero *)
  | "tdiv_int"             -> given "Z.quot arg1 arg2"
  | "tmod_int"             -> given "Z.rem arg1 arg2"
  | "pow2"                 -> given "Z.pow 2 arg1"
  | "min_int"              -> given "Z.min arg1 arg2"
  | "max_int"              -> given "Z.max arg1 arg2"
  | "abs_int"              -> given "Z.abs arg1"
  | "count_leading_zeros"  -> given "(Z.of_nat (bv_width arg1) - Z.log2_up (bv.unsigned arg1 + 1))%Z"
  | "count_trailing_zeros" -> given "(if bv.unsigned arg1 =? 0 then Z.of_nat (bv_width arg1) else Z.log2 (Z.land (bv.unsigned arg1) (- bv.unsigned arg1)))%Z"
  (* shifts by an integer amount *)
  | "shiftl"               -> given "bv.of_Z (Z.shiftl (bv.unsigned arg1) arg2)"
  | "shiftr"               -> given "bv.of_Z (Z.shiftr (bv.unsigned arg1) arg2)"
  (* get_slice_int(len, n, start): bits start + len - 1 down to start of n *)
  | "get_slice_int"        -> given "bv.of_Z (Z.shiftr arg2 arg3)"
  (* truncate(v, n), truncateLSB(v, n): the n least, respectively most, significant bits of v *)
  | "zeros"                -> given "bv.zero"
  | "sail_truncate"        -> given "bv.of_Z (bv.unsigned arg1)"
  | "sail_truncateLSB"     -> given "bv.of_Z (Z.shiftr (bv.unsigned arg1) (Z.of_nat (bv_width arg1) - arg2))"
  | "zero_extend"          -> given "bv.of_Z (bv.unsigned arg1)"
  | "sign_extend"          -> given "bv.of_Z (bv.signed arg1)"
  | "vector_access"        -> begin
      (* the result is a bit, i.e., a bitvector(1) *)
      match foreign_function.parameter_types with
      | Bitvector _ :: _ -> given "bv.of_Z (Z.shiftr (bv.unsigned arg1) arg2)"
      | _                -> Semantics.Unconstrained
    end
  | _ -> Semantics.Unconstrained


(* The semantics of the foreign function: the configuration's, or else the default *)
let semantics (foreign_function : t) : Semantics.t =
  match Configuration.foreign_function_semantics_for foreign_function.sail_name with
  | Some semantics -> semantics
  | None           -> default_semantics foreign_function


(*
   The foreign functions used by the generated program (GenerationContext.used_foreign_functions),
   in the order of the Sail definitions.
*)
let used () : t list GC.t =
  let* program = GC.get_program
  and* used    = GC.used_foreign_functions
  in
  GC.return @@ List.filter (all_externs program) ~f:(fun foreign_function -> Base.Set.mem used foreign_function.identifier)


(* Name of the Rocq definition that computes the result of a foreign function with given semantics *)
let semantics_name (foreign_function : t) : PP.t =
  PP.string @@ "semantics_" ^ Ast.Identifier.to_string foreign_function.identifier


(*
   The FunX constructor of the foreign function, e.g.

     | popcount {kid__n : nat} : FunX [ "arg1" ∷ ty.bvec (kid__n) ] (ty.int)

   as the pair of its name and of the rest, for GenerationContext.pp_inductive_type.
*)
let pp_constructor (foreign_function : t) : (PP.t * PP.t * PP.t) GC.t =
  let* pp_parameter_types = GC.map ~f:Type.pp_type foreign_function.parameter_types
  and* pp_result_type     = Type.pp_type foreign_function.result_type
  in
  let pp_parameters =
    Coq.pp_list @@ List.mapi pp_parameter_types ~f:(fun index pp_type -> MuSail.pp_bind (PP.string @@ parameter_name index) pp_type)
  in
  let pp_implicit_parameters =
    TypeLevelParameters.pp_binders @@ implicit_parameters foreign_function
  in
  let pp_type =
    PP.separate_horizontally ~separator:PP.space [ PP.string "FunX"; pp_parameters; PP.(surround parens) pp_result_type ]
  in
  GC.return (Identifier.pp foreign_function.identifier, pp_implicit_parameters, pp_type)


(*
   For a foreign function with given semantics:

     Definition semantics_f {kid__n : nat} (arg1 : Val (ty.bvec kid__n)) (arg2 : Val ty.int) : Val ty.int :=
       <term>.
*)
let pp_semantics_definition (foreign_function : t) (term : string) : PP.t GC.t =
  let* pp_parameter_types = GC.map ~f:Type.pp_type foreign_function.parameter_types
  and* pp_result_type     = Type.pp_type foreign_function.result_type
  in
  let val_of (pp_type : PP.t) : PP.t =
    Coq.pp_application (PP.string "Val") [ PP.(surround parens) pp_type ]
  in
  let implicit_parameters =
    TypeLevelParameters.as_definition_parameters @@ implicit_parameters foreign_function
  and parameters =
    List.mapi pp_parameter_types ~f:(fun index pp_type -> (PP.string @@ parameter_name index, Some (val_of pp_type)))
  in
  GC.return begin
    Coq.pp_definition
      ~identifier:(semantics_name foreign_function)
      ~implicit_parameters
      ~parameters
      ~result_type:(Some (val_of pp_result_type))
      (PP.string term)
  end


(*
   The arguments of the foreign function, as values taken from the environment args:
   the last one is env.head args, the one before it env.head (env.tail args), and so on.
*)
let pp_arguments_from_environment (foreign_function : t) : PP.t list =
  let count = List.length foreign_function.parameter_types
  in
  List.init count ~f:(fun index ->
      let tails = count - 1 - index
      in
      let rec environment (tails : int) : string =
        if tails = 0 then "args" else Printf.sprintf "(env.tail %s)" (environment (tails - 1))
      in
      PP.string @@ Printf.sprintf "(env.head %s)" (environment tails)
    )


(*
   The case of ForeignCall for the foreign function, e.g.

     | @f _ => fun args res => γ' = γ /\ μ' = μ /\ res = inr (semantics_f (env.head args))
*)
let pp_foreign_call_case (foreign_function : t) : PP.t =
  let pattern =
    String.concat ~sep:" " @@ ("@" ^ Ast.Identifier.to_string foreign_function.identifier) :: List.map ~f:(Fn.const "_") (implicit_parameters foreign_function)
  in
  let memory =
    if foreign_function.pure then [ "μ' = μ" ] else []
  in
  let result =
    match semantics foreign_function with
    | Given _       -> begin
        let arguments = List.map ~f:PP.to_string @@ pp_arguments_from_environment foreign_function
        in
        Printf.sprintf "res = inr (%s)" (String.concat ~sep:" " (PP.to_string (semantics_name foreign_function) :: arguments))
      end
    | Unconstrained -> "exists v, res = inr v"
  in
  PP.string @@ Printf.sprintf "| %s => fun args res => %s" pattern (String.concat ~sep:" /\\ " (List.concat [ [ "γ' = γ" ]; memory; [ result ] ]))


(* The proof of ForeignProgress for the foreign function, after destruct f *)
let pp_foreign_progress_case (foreign_function : t) : PP.t =
  match semantics foreign_function with
  | Given _       -> PP.string "- exists γ, μ; eexists. cbn. repeat split; reflexivity."
  | Unconstrained -> PP.string "- exists γ, μ, (inr unconstrained_inhabitant). cbn. repeat split; eauto."
