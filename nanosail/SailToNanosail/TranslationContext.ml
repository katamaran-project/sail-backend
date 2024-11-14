open Base


module Context = struct
  type t =
    {
      definitions   : Ast.Definition.t list;  (* list of definitions                          *)
      next_id_index : int;                    (* counter used to generate unique identifiers  *)
    }

  let empty : t =
    {
      definitions = [];
      next_id_index = 0;
    }

  (*
     Accessors
  *)
  let definitions =
    let get (context : t) : Ast.Definition.t list = context.definitions
    and set (context : t) (definitions : Ast.Definition.t list) : t = { context with definitions }
    in
    (get, set)

  let next_id_index =
    let get (context : t) : int = context.next_id_index
    and set (context : t) (next_id_index : int) : t = { context with next_id_index }
    in
    (get, set)
end


module Error = struct
  type t =
    | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
    | AssertionFailure  of Lexing.position * string

  let to_string (error : t) =
    match error with
    | NotYetImplemented (ocaml_location, _sail_location, message) -> begin
        match message with
        | None     -> Printf.sprintf "not yet implemented (%s)" (StringOf.OCaml.position ocaml_location)
        | Some msg -> Printf.sprintf "not yet implemented (%s): %s" (StringOf.OCaml.position ocaml_location) msg
      end
    | AssertionFailure (ocaml_location, message) -> begin
        Printf.sprintf "assertion failure (%s): %s" (StringOf.OCaml.position ocaml_location) message
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
let run f      = Monad.run f Context.empty


let string_of_error (error : Error.t) : string =
  let string_of_ocaml_location (ocaml_location : Lexing.position) =
    Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
  in
  match error with
  | NotYetImplemented (ocaml_position, sail_location, message) -> begin
      let message =
        match message with
        | Some message -> message
        | None         -> "<no message>"
      in
      Printf.sprintf "NotYetImplemented(%s, ocaml:%s, sail:%s)" message (StringOf.OCaml.position ocaml_position) (StringOf.Sail.location sail_location)
    end
  | AssertionFailure (ocaml_location, message) -> Printf.sprintf "AssertionFailure(%s at %s)" message (string_of_ocaml_location ocaml_location)


(*
  If an error occurs while executing f, print it out and 'reraise' it.
  In other words, this function has no impact on the execution,
  except that it might print out an error.
*)
let debug_error f =
  let show e =
    Stdio.printf "%s\n" (string_of_error e);
    error e
  in
  recover f show


let not_yet_implemented ?(message = "") ocaml_position sail_position =
  let message =
    if String.is_empty message
    then None
    else Some message
  in
  Monad.fail @@ NotYetImplemented (ocaml_position, sail_position, message)


let check ocaml_position condition message =
  if not condition
  then Monad.fail @@ AssertionFailure (ocaml_position, Lazy.force message)
  else Monad.return ()


let fail ocaml_position message =
  Monad.fail @@ AssertionFailure (ocaml_position, message)


let definitions : Ast.Definition.t list t =
  Monad.get Context.definitions


let register (definition : Ast.Definition.t) =
  let* old_definitions = definitions
  in
  let new_definitions = definition :: old_definitions
  in
  Monad.put Context.definitions new_definitions


(*
   Looks up a type definition based on the name of the type.

   The extractor (see Ast.Extract) can be used to get a specific kind of type
*)
let lookup_type_definition_of_kind
      (extractor  : Ast.Definition.Type.t -> 'a option)
      (identifier : Ast.Identifier.t                  ) : 'a option t
  =
  let predicate (definition : Ast.Definition.t) : Ast.Definition.Type.t option =
    match definition with
    | TypeDefinition type_definition -> begin
        if
          Ast.Identifier.equal identifier (Ast.Definition.Type.identifier type_definition)
        then
          Some type_definition
        else
          None
      end
    | _ -> None
  in
  let* definitions
  in
  let type_definition = List.find_map definitions ~f:predicate
  in
  return @@ Option.bind type_definition ~f:extractor


let lookup_type_definition =
  lookup_type_definition_of_kind Ast.Definition.Select.of_anything


(* Looks up type of register with given name *)
let lookup_register_type (identifier : Ast.Identifier.t) : Ast.Type.t option t =
  let predicate (definition : Ast.Definition.t) : Ast.Definition.Register.t option =
    match definition with
    | RegisterDefinition register_definition ->
       begin
         if Ast.Identifier.equal register_definition.identifier identifier
         then Some register_definition
         else None
       end
    | _ -> None
  in
  let* definitions
  in
  return @@ Option.map (List.find_map definitions ~f:predicate) ~f:(fun r -> r.typ)


let is_register (identifier : Ast.Identifier.t) : bool t =
  MonadUtil.lift ~f:Option.is_some @@ lookup_register_type identifier


let lookup_definitions_of_kind (extractor : Ast.Definition.t -> 'a option) : 'a list t =
  let* definitions
  in
  return @@ List.filter_map ~f:extractor definitions


(*
  Looks up a variant that has a given constructor.
*)
let lookup_variant_by_constructor (constructor_identifier : Ast.Identifier.t) : Ast.Definition.Type.Variant.t option t =
  let has_constructor (variant_definition : Ast.Definition.Type.Variant.t) : bool =
    List.exists variant_definition.constructors ~f:(fun (id, _) -> Ast.Identifier.equal id constructor_identifier)
  in
  let* variant_definitions =
    lookup_definitions_of_kind Ast.Definition.Select.(type_definition of_variant)
  in
  return @@ List.find variant_definitions ~f:has_constructor


let generate_unique_int : int t =
  let* index = Monad.get Context.next_id_index
  in
  let* () = Monad.put Context.next_id_index (index + 1)
  in
  return index


let generate_unique_identifier ?(prefix = "") ?(underscore = false) () : Ast.Identifier.t t =
  let* index = generate_unique_int
  in
  let result = Configuration.tag_as_generated @@ Ast.Identifier.mk @@ Printf.sprintf "%s%d" prefix index
  in
  if underscore
  then return @@ Ast.Identifier.add_prefix "_" result
  else return result
