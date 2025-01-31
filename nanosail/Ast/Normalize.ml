open Base


module Context = struct
  type t = {
    substitutions : Identifier.t Identifier.Map.t
  }

  let empty : t =
    {
      substitutions = Identifier.Map.empty
    }

  let substitutions =
    let get (context : t) : Identifier.t Identifier.Map.t =
      context.substitutions
    and set
        (_context      : t                                    )
        (substitutions : Identifier.t Identifier.Map.t) : t
      =
      { substitutions }
    in
    (get, set)
end

module Monad = Monads.ComponentState.Make(Context)
open Monads.Notations.Star(Monad)


let requires_substitution (identifier : Identifier.t) : bool =
  Identifier.is_generated identifier


let substitute_identifier (identifier : Identifier.t) : Identifier.t Monad.t =
  if
    not @@ requires_substitution identifier
  then
    Monad.return identifier
  else begin
    let* substitutions = Monad.get Context.substitutions
    in
    match Identifier.Map.find substitutions identifier with
    | Some identifier' -> Monad.return identifier'
    | None -> begin
        let index =
          Identifier.Map.length substitutions
        in
        let identifier' =
          Identifier.mk_generated @@ Int.to_string index
        in
        let substitutions' =
          Identifier.Map.add_exn
            substitutions
            ~key:identifier
            ~data:identifier'
        in
        let* () = Monad.put Context.substitutions substitutions'
        in
        Monad.return identifier
      end

  end


(* let normalize_statement (statement : Statement.t) : Statement.t Monad.t = *)
  
