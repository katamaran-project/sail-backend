(*
   Monad used during translation from Sail to nanosail.
   It is a combination of a state monad and a result monad.

   * It keeps track of all definition translations.
   * It can generate unique identifiers.
   * It stores information about calls to polymorphic functions.
*)

open! ExtBase


module Context = struct
  (*
     State implicity carried around during translation
  *)
  type t =
    {
      (*
         List of definitions.
         New definitions are added to this list as translation proceeds.
         Definitions are stored in reversed order, i.e., new definitions are added to the front of the list.
      *)
      definitions : Ast.Definition.t list;

      (* Counter used to generate unique identifiers *)
      next_id_index : int;

      (*
         Collects the types of the arguments used in calls to polymorphic functions.
         Can be used to produce a list of required monomorphizations.

         Example
         -------
         We have a function

           val foo : forall 'n 'm. (bitvector('n), bitvector('m)) -> bitvector(4)

         and calls

           foo(0b0, 0b0)
           foo(0b00, 0b00)
           foo(0b11, 0b01)
           foo(0b111, 0b0)

         then this map will contain

           foo -> [ [ bitvector(1); bitvector(1) ];
                    [ bitvector(2); bitvector(2) ];
                    [ bitvector(3); bitvector(1) ] ]

         The actual order of the lists of parameter types is left unspecified.
      *)
      polymorphic_argtypes : Ast.Type.t list list Ast.Identifier.Map.t;
    }

  let empty : t =
    {
      definitions          = [];
      next_id_index        = 0;
      polymorphic_argtypes = Ast.Identifier.Map.empty;
    }

  (*
     Accessors
  *)
  let definitions =
    let get (context : t) : Ast.Definition.t list =
      context.definitions
    and set
        (context     : t                    )
        (definitions : Ast.Definition.t list) : t
      =
      { context with definitions }
    in
    (get, set)

  let next_id_index =
    let get (context : t) : int =
      context.next_id_index
    and set
        (context       : t  )
        (next_id_index : int) : t
      =
      { context with next_id_index }
    in
    (get, set)

  let argument_types_of_polymorphic_function_calls =
    let get (context : t) : Ast.Type.t list list Ast.Identifier.Map.t =
      context.polymorphic_argtypes
    and set
        (context              : t                                        )
        (polymorphic_argtypes : Ast.Type.t list list Ast.Identifier.Map.t) : t
      =
      { context with polymorphic_argtypes }
    in
    (get, set)
end


module Error = struct
  type t =
    | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
    | Failure           of Lexing.position * string

  let to_string (error : t) =
    match error with
    | NotYetImplemented (ocaml_location, _sail_location, message) -> begin
        match message with
        | None     -> Printf.sprintf "not yet implemented (%s)" (StringOf.OCaml.position ocaml_location)
        | Some msg -> Printf.sprintf "not yet implemented (%s): %s" (StringOf.OCaml.position ocaml_location) msg
      end
    | Failure (ocaml_location, message) -> begin
        Printf.sprintf "failure (%s): %s" (StringOf.OCaml.position ocaml_location) message
      end
end


module Monad     = Monads.StateResult.Make(Context)(Error)
module MonadUtil = Monads.Util.Make(Monad)

include MonadUtil
open Monads.Notations.Star(Monad)


type 'a t      = 'a Monad.t
type 'a result = 'a Monad.result = Success of 'a      (* explicitly enumerating cases here prevents result from becoming abstract *)
                                 | Failure of Error.t

let return     = Monad.return
let act        = Monad.act
let error      = Monad.fail
let bind       = Monad.bind
let recover    = Monad.recover


let run (f : 'a t) : ('a * Ast.Type.t list list Ast.Identifier.Map.t) result =
  let wrapper =
    let* result = f
    and* polymorphic_argtypes = Monad.get Context.argument_types_of_polymorphic_function_calls
    in
    return (result, polymorphic_argtypes)
  in
  let result, _context = Monad.run wrapper Context.empty
  in
  result


let log
    (ocaml_position : Lexing.position                       )
    (logger         : Lexing.position -> PP.t lazy_t -> unit)
    (message        : PP.t lazy_t                           ) : unit t
  =
  act (fun () -> logger ocaml_position message)


(*
   For debugging purposes.

   Logs a debug "Entering <label>" message,
   performs the computation represented by result,
   then logs a "Exiting <label>" message.
*)
let translation_block
    (ocaml_position : Lexing.position)
    (label          : PP.t           )
    (result         : 'a t           ) : 'a t
  =
  let* () =
    let message =
      lazy begin
        PP.horizontal [
          PP.string "Entering ";
          label
        ]
      end
    in
    log ocaml_position Logging.debug message
  in
  let* result
  in
  let* () =
    let message =
      lazy begin
        PP.horizontal [
          PP.string "Exiting ";
          label
        ]
      end
    in
    log ocaml_position Logging.debug message
  in
  return result


let not_yet_implemented ?(message : string option) ocaml_position sail_position =
  let* () = log [%here] Logging.debug @@ lazy (PP.string "Not yet implemented")
  in
  Monad.fail @@ NotYetImplemented (ocaml_position, sail_position, message)


let check
    (ocaml_position : Lexing.position)
    (condition      : bool           )
    (message        : string lazy_t  ) : unit t
  =
  if not condition
  then Monad.fail @@ Failure (ocaml_position, Lazy.force message)
  else Monad.return ()


let fail ocaml_position message =
  Monad.fail @@ Failure (ocaml_position, message)


let definitions : Ast.Definition.t list t =
  Monad.get Context.definitions


(*
   Stores the given definition.
*)
let store_definition (definition : Ast.Definition.t) : unit t =
  let* old_definitions = definitions
  in
  let new_definitions = definition :: old_definitions
  in
  Monad.put Context.definitions new_definitions


(*
   Returns all definitions satisfying the selector
*)
let select_definitions (selector : (Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a list t =
  let* definitions
  in
  return @@ Ast.Definition.Select.(select selector definitions)


(*
   Returns definition satisfying selector. Causes failure if there are none or multiple.

   See Ast.Definition.Select.
*)
let lookup_definition (selector : (Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a t =
  let* definitions = select_definitions selector
  in
  match definitions with
  | [ definition ] -> return definition
  | []             -> fail [%here] @@ Printf.sprintf "no definition found matching the selector %s" selector#to_string
  | _              -> fail [%here] @@ Printf.sprintf "expected only one match for selector %s" selector#to_string


(*
   Returns definition satisfying selector. Causes failure if there are multiple matches.

   See Ast.Definition.Select.
*)
let lookup_definition_opt (selector : (Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a option t =
  let* definitions = select_definitions selector
  in
  match definitions with
  | [ definition ] -> return @@ Some definition
  | []             -> return @@ None
  | _              -> fail [%here] "expected only one match"


(*
   Looks up the type of a register with the given name.
*)
let lookup_register_type (identifier : Ast.Identifier.t) : Ast.Type.t option t =
  let* register_definition =
    lookup_definition_opt @@ Ast.Definition.Select.register_definition_named identifier
  in
  return begin
    Option.map
      register_definition
      ~f:(fun register_definition -> register_definition.typ)
  end


(*
   Checks if there exists a register with the given name.
*)
let is_register (identifier : Ast.Identifier.t) : bool t =
  MonadUtil.lift ~f:Option.is_some @@ lookup_register_type identifier


(*
  Looks up a variant that has a given constructor.
*)
let lookup_variant_by_constructor (constructor_identifier : Ast.Identifier.t) : Ast.Definition.Type.Variant.t option t =
  let has_constructor (variant_definition : Ast.Definition.Type.Variant.t) : bool =
    List.exists variant_definition.constructors ~f:(fun (id, _) -> Ast.Identifier.equal id constructor_identifier)
  in
  let* variant_definitions =
    select_definitions Ast.Definition.Select.(type_definition @@ of_variant)
  in
  return @@ List.find variant_definitions ~f:has_constructor


(*
   Generate a unique integer, i.e., an integer that was not previously
   generated by this function in the same translation context.
*)
let generate_unique_int : int t =
  let* index = Monad.get Context.next_id_index
  in
  let* () = Monad.put Context.next_id_index (index + 1)
  in
  return index


let generate_unique_identifier
    ?(prefix     : string = ""   )
    ?(suffix     : string = ""   )
    ?(underscore : bool   = false)
    (_           : unit          ) : Ast.Identifier.t t
  =
  let* index = generate_unique_int
  in
  let underscore : string =
    if underscore
    then "_"
    else ""
  in
  return @@ Ast.Identifier.mk_generated @@ Printf.sprintf "%s%s%d%s" underscore prefix index suffix


(*
   Generate a list of unique identifiers.
*)
let rec generate_unique_identifiers
    ?(prefix     : string = "" )
    ?(suffix     : string = "" )
    ?(underscore : bool = false)
    (count       : int         ) : Ast.Identifier.t list t
  =
  if
    Int.equal 0 count
  then
    return []
  else
    let* id  = generate_unique_identifier ~prefix ~suffix ~underscore ()
    and* ids = generate_unique_identifiers ~prefix ~suffix ~underscore (count - 1)
    in
    return @@ id :: ids


(*
   Should be called whenever a polymorphic function is called.
*)
let register_polymorphic_function_call_type_arguments
    (function_identifier : Ast.Identifier.t)
    (argument_types      : Ast.Type.t list ) : unit t
  =
  let* mapping = Monad.get Context.argument_types_of_polymorphic_function_calls
  in
  let update_list (argument_types_list : Ast.Type.t list list option) : Ast.Type.t list list =
    (* if this is our first encounter with the function, asssume it's been associated with [] *)
    let argument_types_list =
      Option.value argument_types_list ~default:[]
    in
    (* keep elements in the argument_types_list unique *)
    if
      List.mem
        argument_types_list
        argument_types
        ~equal:(List.equal Ast.Type.equal)
    then
      (* list already contains argument types; return it unchanged *)
      argument_types_list
    else
      (* add new argument types to list *)
      argument_types :: argument_types_list
  in
  let updated_mapping =
    Ast.Identifier.Map.update mapping function_identifier ~f:update_list
  in
  Monad.put Context.argument_types_of_polymorphic_function_calls updated_mapping
