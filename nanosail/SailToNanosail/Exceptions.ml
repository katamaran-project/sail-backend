(*

   Exceptions as values.

   muSail has no exceptions. The translation of function bodies leaves Throw and Try nodes,
   which this pass replaces once the whole program is translated:

   * The caught constructors are those that the handlers of some try may catch (Try.caught).
   * A function raises if it is reachable from the body of a try (by calls) and
     a caught exception can leave it: a throw of a caught constructor, or a call to a raising function,
     outside the body of a try.
   * A raising function returns ty.sum τ E instead of τ (E the exception type):
     inl v for a normal result, inr e for a raised exception e.
     Each call to it in a raising function is followed by a match: on inr e, the caller returns inr e.

       let x = f(a) in body               stm_match_sum (call f a) "x" body "e" (inr e)

     A statement that cannot raise is unchanged, except that a result of the function becomes inl v.
   * try { body } catch { handler } matches the (lifted) body: inl v gives v, inr e runs the handler.
   * Everything else fails as before: a throw of a constructor that no try catches, a throw in a function that
     does not raise, and a raising call in a function that does not raise.

   A function that does not raise keeps its type and body, except for the raising calls it makes and its trys.
   With no try (or no try that catches anything), a throw is a failure and a try is its body.

*)
open ExtBase


module Statement  = Ast.Statement
module Expression = Ast.Expression
module Type       = Ast.Type
module Id         = Ast.Identifier


(* A construct this pass cannot handle; the function becomes an untranslated definition *)
exception Unsupported of string


type context =
  {
    (* the constructors of the exception type that some try may catch *)
    caught         : Id.Set.t;

    (* the raising functions, with their original return types *)
    raising        : Type.t Id.Map.t;

    (* the exception type and its constructors with their field types *)
    exception_type : Type.t;
    constructors   : (Id.t * Type.t list) list;
  }


(* The variables bound to a constructor application (let x = C(...)), so that throw x is known to throw C *)
type environment =
  {
    thrown_constructors : Id.t Id.Map.t;
    variable_types      : Type.t Id.Map.t;
  }


let failure_message : string = "failure"


let counter : int ref = ref 0

let fresh (prefix : string) : Id.t =
  Int.incr counter;
  Id.mk_generated @@ Printf.sprintf "exc_%s%d" prefix !counter


let rec strip (statement : Statement.t) : Statement.t =
  match statement with
  | Located (_, inner) -> strip inner
  | Cast (inner, _)    -> strip inner
  | _                  -> statement


let bind
    (environment : environment )
    (binder      : Id.t        )
    (typ         : Type.t option)
    (bound       : Statement.t option) : environment
  =
  let thrown_constructors =
    match Option.map ~f:strip bound with
    | Some (Expression (Variant { constructor_identifier; _ })) -> Id.Map.overwrite environment.thrown_constructors ~key:binder ~data:constructor_identifier
    | _                                                        -> Id.Map.remove environment.thrown_constructors binder
  and variable_types =
    match typ with
    | Some typ -> Id.Map.overwrite environment.variable_types ~key:binder ~data:typ
    | None     -> Id.Map.remove environment.variable_types binder
  in
  { thrown_constructors; variable_types }


let bind_all
    (environment : environment)
    (binders     : Id.t list  ) : environment
  =
  List.fold binders ~init:environment ~f:(fun environment binder -> bind environment binder None None)


type thrown =
  | Caught
  | Uncaught
  | Unknown


let classify_throw
    (context     : context     )
    (environment : environment )
    (value       : Expression.t) : thrown
  =
  let of_constructor (constructor : Id.t) : thrown =
    if Id.Set.mem context.caught constructor then Caught else Uncaught
  in
  let constructor : Id.t option =
    match value with
    | Variant { constructor_identifier; _ } -> Some constructor_identifier
    | Variable (identifier, _)              -> Id.Map.find environment.thrown_constructors identifier
    | _                                     -> None
  in
  match constructor with
  | Some constructor -> of_constructor constructor
  | None -> begin
      if Id.Set.is_empty context.caught
      then Uncaught
      else if List.for_all context.constructors ~f:(fun (constructor, _) -> Id.Set.mem context.caught constructor)
      then Caught
      else Unknown
    end


(*
   The binders of each case of a match, with the cases.
*)
let match_cases (pattern : Statement.match_pattern) : (Id.t list * Statement.t) list =
  match pattern with
  | MatchList { when_cons = (head, tail, when_cons); when_nil; _ } -> [ ([ head; tail ], when_cons); ([], when_nil) ]
  | MatchProduct { id_fst; id_snd; body; _ }                         -> [ ([ id_fst; id_snd ], body) ]
  | MatchTuple { binders; body; _ }                                  -> [ (List.map ~f:fst binders, body) ]
  | MatchBool { when_true; when_false; _ }                           -> [ ([], when_true); ([], when_false) ]
  | MatchEnum { cases; _ }                                           -> List.map ~f:(fun clause -> ([], clause)) @@ Id.Map.data cases
  | MatchVariant { cases; _ }                                        -> Id.Map.data cases
  | MatchSum { when_left = (left, when_left); when_right = (right, when_right); _ } -> [ ([ left ], when_left); ([ right ], when_right) ]


let map_match_cases
    (pattern : Statement.match_pattern                    )
    ~(f      : Id.t list -> Statement.t -> Statement.t    ) : Statement.match_pattern
  =
  match pattern with
  | MatchList data -> begin
      let head, tail, when_cons = data.when_cons
      in
      MatchList { data with when_cons = (head, tail, f [ head; tail ] when_cons); when_nil = f [] data.when_nil }
    end
  | MatchProduct data -> MatchProduct { data with body = f [ data.id_fst; data.id_snd ] data.body }
  | MatchTuple data   -> MatchTuple { data with body = f (List.map ~f:fst data.binders) data.body }
  | MatchBool data    -> MatchBool { data with when_true = f [] data.when_true; when_false = f [] data.when_false }
  | MatchEnum data    -> MatchEnum { data with cases = Id.Map.map_values ~f:(f []) data.cases }
  | MatchVariant data -> MatchVariant { data with cases = Id.Map.map_values data.cases ~f:(fun (binders, clause) -> (binders, f binders clause)) }
  | MatchSum data     -> begin
      let left, when_left   = data.when_left
      and right, when_right = data.when_right
      in
      MatchSum { data with when_left = (left, f [ left ] when_left); when_right = (right, f [ right ] when_right) }
    end


(*
   Whether a caught exception can leave the statement.
*)
let rec may_raise
    (context     : context    )
    (environment : environment)
    (statement   : Statement.t) : bool
  =
  let recurse = may_raise context environment
  in
  match statement with
  | Throw { value; _ }                -> begin
      match classify_throw context environment value with
      | Caught | Unknown -> true
      | Uncaught         -> false
    end
  | Call (identifier, _)              -> Id.Map.mem context.raising identifier
  | Try { body; exception_binder; handler; _ } -> recurse body && may_raise context (bind environment exception_binder None None) handler
  | Let { binder; binding_statement_type; binding_statement; body_statement } -> begin
      recurse binding_statement
      || may_raise context (bind environment binder (Some binding_statement_type) (Some binding_statement)) body_statement
    end
  | Match pattern                     -> List.exists (match_cases pattern) ~f:(fun (binders, clause) -> may_raise context (bind_all environment binders) clause)
  | DestructureRecord { binders; destructured_record; body; _ } -> recurse destructured_record || may_raise context (bind_all environment binders) body
  | Seq (left, right)                 -> recurse left || recurse right
  | AssignVariable { assigned_value; _ } -> recurse assigned_value
  | Cast (statement, _)               -> recurse statement
  | Specialize { body; _ }            -> recurse body
  | Located (_, statement)            -> recurse statement
  | Expression _                      -> false
  | ReadRegister _                    -> false
  | WriteRegister _                   -> false
  | Fail (_, _)                       -> false
  | Unconstrained _                   -> false


(* The functions that a statement calls *)
let rec called_functions (statement : Statement.t) : Id.Set.t =
  let recurse = called_functions
  in
  match statement with
  | Call (identifier, _)              -> Id.Set.singleton identifier
  | Try { body; handler; _ }          -> Id.Set.union (recurse body) (recurse handler)
  | Throw _                           -> Id.Set.empty
  | Let { binding_statement; body_statement; _ } -> Id.Set.union (recurse binding_statement) (recurse body_statement)
  | Match pattern                     -> Id.Set.unions @@ List.map ~f:(fun (_, clause) -> recurse clause) @@ match_cases pattern
  | DestructureRecord { destructured_record; body; _ } -> Id.Set.union (recurse destructured_record) (recurse body)
  | Seq (left, right)                 -> Id.Set.union (recurse left) (recurse right)
  | AssignVariable { assigned_value; _ } -> recurse assigned_value
  | Cast (statement, _)               -> recurse statement
  | Specialize { body; _ }            -> recurse body
  | Located (_, statement)            -> recurse statement
  | Expression _                      -> Id.Set.empty
  | ReadRegister _                    -> Id.Set.empty
  | WriteRegister _                   -> Id.Set.empty
  | Fail (_, _)                       -> Id.Set.empty
  | Unconstrained _                   -> Id.Set.empty


(* The functions called in the bodies of trys *)
let rec called_in_try_bodies (statement : Statement.t) : Id.Set.t =
  let recurse = called_in_try_bodies
  in
  match statement with
  | Try { body; handler; _ }          -> Id.Set.union (called_functions body) (recurse handler)
  | Call (_, _)                       -> Id.Set.empty
  | Throw _                           -> Id.Set.empty
  | Let { binding_statement; body_statement; _ } -> Id.Set.union (recurse binding_statement) (recurse body_statement)
  | Match pattern                     -> Id.Set.unions @@ List.map ~f:(fun (_, clause) -> recurse clause) @@ match_cases pattern
  | DestructureRecord { destructured_record; body; _ } -> Id.Set.union (recurse destructured_record) (recurse body)
  | Seq (left, right)                 -> Id.Set.union (recurse left) (recurse right)
  | AssignVariable { assigned_value; _ } -> recurse assigned_value
  | Cast (statement, _)               -> recurse statement
  | Specialize { body; _ }            -> recurse body
  | Located (_, statement)            -> recurse statement
  | Expression _                      -> Id.Set.empty
  | ReadRegister _                    -> Id.Set.empty
  | WriteRegister _                   -> Id.Set.empty
  | Fail (_, _)                       -> Id.Set.empty
  | Unconstrained _                   -> Id.Set.empty


(* The trys of a statement: their caught constructors and exception types *)
let rec trys (statement : Statement.t) : (Id.t list * Type.t) list =
  let recurse = trys
  in
  match statement with
  | Try { body; handler; caught; exception_type; _ } -> (caught, exception_type) :: List.append (recurse body) (recurse handler)
  | Call (_, _)                       -> []
  | Throw _                           -> []
  | Let { binding_statement; body_statement; _ } -> List.append (recurse binding_statement) (recurse body_statement)
  | Match pattern                     -> List.concat_map ~f:(fun (_, clause) -> recurse clause) @@ match_cases pattern
  | DestructureRecord { destructured_record; body; _ } -> List.append (recurse destructured_record) (recurse body)
  | Seq (left, right)                 -> List.append (recurse left) (recurse right)
  | AssignVariable { assigned_value; _ } -> recurse assigned_value
  | Cast (statement, _)               -> recurse statement
  | Specialize { body; _ }            -> recurse body
  | Located (_, statement)            -> recurse statement
  | Expression _                      -> []
  | ReadRegister _                    -> []
  | WriteRegister _                   -> []
  | Fail (_, _)                       -> []
  | Unconstrained _                   -> []


(*
   The result type of a lifted statement: ty.sum τ E, where τ is unit for a statement whose value is discarded.
*)
let lifted_type
    (context     : context      )
    (result_type : Type.t option) : Type.t
  =
  Sum (Option.value result_type ~default:Unit, context.exception_type)


let inject_left
    (context     : context      )
    (result_type : Type.t option)
    (value       : Expression.t ) : Statement.t
  =
  Expression (Injection { side = Left; left_type = Option.value result_type ~default:Unit; right_type = context.exception_type; value })


let raise_value
    (context     : context      )
    (result_type : Type.t option)
    (value       : Expression.t ) : Statement.t
  =
  Expression (Injection { side = Right; left_type = Option.value result_type ~default:Unit; right_type = context.exception_type; value })


(*
   A statement that cannot raise, as a lifted statement: its value v becomes inl v.
*)
let rec wrap
    (context     : context      )
    (result_type : Type.t option)
    (statement   : Statement.t  ) : Statement.t
  =
  match statement, result_type with
  | Located (location, inner), _       -> Located (location, wrap context result_type inner)
  | Fail (_, message), _               -> Fail (lifted_type context result_type, message)
  | Expression expression, Some _      -> inject_left context result_type expression
  | Expression (Value Unit), None      -> inject_left context None (Value Unit)
  | _, Some Unit                       -> Seq (statement, inject_left context result_type (Value Unit))
  | _, Some typ                        -> begin
      let binder = fresh "value"
      in
      Let {
        binder;
        binding_statement_type = typ;
        binding_statement      = statement;
        body_statement         = inject_left context result_type (Variable (binder, typ));
      }
    end
  | _, None                            -> Seq (statement, inject_left context None (Value Unit))


(*
   Runs a lifted statement of type ty.sum τ E, then continues with when_left on inl v (v bound to binder);
   on inr e, the result is when_right e.
   Without the match if it would return the scrutinee's value as it is (inl v => inl v, inr e => inr e).
*)
let match_lifted
    (context    : context                        )
    ~(scrutinee : Statement.t                    )
    ~(left_type : Type.t                         )
    ~(binder    : Id.t                           )
    ~(when_left : Statement.t                    )
    ~(when_right: Id.t * Statement.t             ) : Statement.t
  =
  let returns_left_unchanged =
    match strip when_left with
    | Expression (Injection { side = Left; left_type = injected_type; value; _ }) -> begin
        Type.equal injected_type left_type
        &&
        match value with
        | Variable (identifier, _) -> Id.equal identifier binder
        | Value Unit               -> Type.equal left_type Unit
        | _                        -> false
      end
    | _ -> false
  and returns_right_unchanged =
    let raised, clause = when_right
    in
    match strip clause with
    | Expression (Injection { side = Right; value = Variable (identifier, _); _ }) -> Id.equal identifier raised
    | _                                                                             -> false
  in
  if
    returns_left_unchanged && returns_right_unchanged
  then
    scrutinee
  else begin
    let matched = fresh "result"
    in
    Let {
      binder                 = matched;
      binding_statement_type = Sum (left_type, context.exception_type);
      binding_statement      = scrutinee;
      body_statement         = Match begin
          MatchSum {
            matched;
            left_type;
            right_type = context.exception_type;
            when_left  = (binder, when_left);
            when_right;
          }
        end;
    }
  end


(* on inr e, return inr e *)
let propagate
    (context     : context      )
    (result_type : Type.t option) : Id.t * Statement.t
  =
  let raised = fresh "raised"
  in
  (raised, raise_value context result_type (Variable (raised, context.exception_type)))


(*
   A statement in a function that does not raise: throws fail, raising calls fail on inr.
*)
let rec lower
    (context     : context    )
    (environment : environment)
    (statement   : Statement.t) : Statement.t
  =
  let recurse = lower context environment
  in
  match statement with
  | Throw { typ; _ } -> Fail (typ, failure_message)
  | Call (identifier, _) -> begin
      match Id.Map.find context.raising identifier with
      | Some return_type -> begin
          let value = fresh "value"
          and raised = fresh "raised"
          in
          match_lifted context
            ~scrutinee:statement
            ~left_type:return_type
            ~binder:value
            ~when_left:(Expression (Variable (value, return_type)))
            ~when_right:(raised, Fail (return_type, failure_message))
        end
      | None -> statement
    end
  | Try { body; result_type; exception_binder; handler; _ } -> begin
      if
        may_raise context environment body
      then begin
        let value = fresh "value"
        in
        match_lifted context
          ~scrutinee:(lift context environment (Some result_type) body)
          ~left_type:result_type
          ~binder:value
          ~when_left:(Expression (Variable (value, result_type)))
          ~when_right:(exception_binder, lower context (bind environment exception_binder None None) handler)
      end
      else recurse body
    end
  | Let { binder; binding_statement_type; binding_statement; body_statement } -> begin
      Let {
        binder;
        binding_statement_type;
        binding_statement = recurse binding_statement;
        body_statement    = lower context (bind environment binder (Some binding_statement_type) (Some binding_statement)) body_statement;
      }
    end
  | Match pattern -> Match (map_match_cases pattern ~f:(fun binders clause -> lower context (bind_all environment binders) clause))
  | DestructureRecord data -> DestructureRecord { data with destructured_record = recurse data.destructured_record; body = lower context (bind_all environment data.binders) data.body }
  | Seq (left, right) -> Seq (recurse left, recurse right)
  | AssignVariable data -> AssignVariable { data with assigned_value = recurse data.assigned_value }
  | Cast (inner, typ) -> Cast (recurse inner, typ)
  | Specialize data -> Specialize { data with body = recurse data.body }
  | Located (location, inner) -> Located (location, recurse inner)
  | Expression _ | ReadRegister _ | WriteRegister _ | Fail (_, _) | Unconstrained _ -> statement


(*
   A statement as a lifted statement: of type ty.sum τ E, with τ the given result type (unit if None:
   the value is discarded).
*)
and lift_or_wrap
    (context     : context      )
    (environment : environment  )
    (result_type : Type.t option)
    (statement   : Statement.t  ) : Statement.t
  =
  if may_raise context environment statement
  then lift context environment result_type statement
  else wrap context result_type (lower context environment statement)


and lift
    (context     : context      )
    (environment : environment  )
    (result_type : Type.t option)
    (statement   : Statement.t  ) : Statement.t
  =
  let continue_with = lift_or_wrap context environment result_type
  in
  match statement with
  | Located (location, inner) -> Located (location, lift context environment result_type inner)

  | Throw { value; _ } -> begin
      match classify_throw context environment value with
      | Caught   -> raise_value context result_type value
      | Uncaught -> Fail (lifted_type context result_type, failure_message)
      | Unknown  -> begin
          (* match on the constructor: a caught one is raised, any other fails *)
          let exception_type_identifier =
            match context.exception_type with
            | Variant identifier -> identifier
            | _                  -> raise (Unsupported "exception type is not a union")
          in
          let matched, bind_matched =
            match value with
            | Variable (identifier, _) -> (identifier, Fn.id)
            | _ -> begin
                let matched = fresh "thrown"
                in
                (matched, fun body -> Statement.Let { binder = matched; binding_statement_type = context.exception_type; binding_statement = Expression value; body_statement = body })
              end
          in
          let cases =
            Id.Map.of_alist_exn @@ List.map context.constructors ~f:(fun (constructor, field_types) ->
                (* a case binds one variable per field, and one for a constructor without fields *)
                let binders =
                  match field_types with
                  | [] -> [ fresh "field" ]
                  | _  -> List.map field_types ~f:(fun _ -> fresh "field")
                and clause =
                  if Id.Set.mem context.caught constructor
                  then raise_value context result_type (Variable (matched, context.exception_type))
                  else Fail (lifted_type context result_type, failure_message)
                in
                (constructor, (binders, clause)))
          in
          bind_matched @@ Match (MatchVariant { matched; matched_type = exception_type_identifier; cases })
        end
    end

  | Call (identifier, _) -> begin
      match Id.Map.find context.raising identifier, result_type with
      | None, _                                -> wrap context result_type statement
      | Some _, Some _                         -> statement
      | Some Unit, None                        -> statement
      | Some return_type, None                 -> begin
          match_lifted context
            ~scrutinee:statement
            ~left_type:return_type
            ~binder:(fresh "value")
            ~when_left:(inject_left context None (Value Unit))
            ~when_right:(propagate context None)
        end
    end

  | Try { body; result_type = try_result_type; exception_binder; handler; _ } -> begin
      if
        may_raise context environment body
      then begin
        let value = fresh "value"
        in
        match_lifted context
          ~scrutinee:(lift context environment (Some try_result_type) body)
          ~left_type:try_result_type
          ~binder:value
          ~when_left:(wrap context result_type (Expression (Variable (value, try_result_type))))
          ~when_right:(exception_binder, lift_or_wrap context (bind environment exception_binder None None) result_type handler)
      end
      else continue_with body
    end

  | Let { binder; binding_statement_type; binding_statement; body_statement } -> begin
      let body_environment = bind environment binder (Some binding_statement_type) (Some binding_statement)
      in
      if
        may_raise context environment binding_statement
      then
        match_lifted context
          ~scrutinee:(lift context environment (Some binding_statement_type) binding_statement)
          ~left_type:binding_statement_type
          ~binder
          ~when_left:(lift_or_wrap context body_environment result_type body_statement)
          ~when_right:(propagate context result_type)
      else
        Let {
          binder;
          binding_statement_type;
          binding_statement = lower context environment binding_statement;
          body_statement    = lift_or_wrap context body_environment result_type body_statement;
        }
    end

  | Seq (left, right) -> begin
      if
        may_raise context environment left
      then
        match_lifted context
          ~scrutinee:(lift context environment None left)
          ~left_type:Unit
          ~binder:(fresh "unit")
          ~when_left:(continue_with right)
          ~when_right:(propagate context result_type)
      else
        Seq (lower context environment left, continue_with right)
    end

  | Match pattern -> Match (map_match_cases pattern ~f:(fun binders clause -> lift_or_wrap context (bind_all environment binders) result_type clause))

  | DestructureRecord data -> begin
      let body_environment = bind_all environment data.binders
      in
      if
        may_raise context environment data.destructured_record
      then begin
        let record_type : Type.t = Record data.record_type_identifier
        and record = fresh "record"
        in
        match_lifted context
          ~scrutinee:(lift context environment (Some record_type) data.destructured_record)
          ~left_type:record_type
          ~binder:record
          ~when_left:begin
            DestructureRecord {
              data with
              destructured_record = Expression (Variable (record, record_type));
              body                = lift_or_wrap context body_environment result_type data.body;
            }
          end
          ~when_right:(propagate context result_type)
      end
      else
        DestructureRecord {
          data with
          destructured_record = lower context environment data.destructured_record;
          body                = lift_or_wrap context body_environment result_type data.body;
        }
    end

  | AssignVariable { variable_identifier; assigned_value } -> begin
      let variable_type =
        match Id.Map.find environment.variable_types variable_identifier with
        | Some typ -> typ
        | None     -> raise (Unsupported (Printf.sprintf "raising assignment to %s, whose type is unknown" (Id.to_string variable_identifier)))
      in
      let value = fresh "value"
      in
      match_lifted context
        ~scrutinee:(lift context environment (Some variable_type) assigned_value)
        ~left_type:variable_type
        ~binder:value
        ~when_left:(wrap context result_type (AssignVariable { variable_identifier; assigned_value = Expression (Variable (value, variable_type)) }))
        ~when_right:(propagate context result_type)
    end

  | Cast (inner, typ) -> Cast (lift context environment (Some typ) inner, Sum (typ, context.exception_type))

  | Specialize data -> begin
      Specialize {
        data with
        result_type = lifted_type context (Option.map result_type ~f:(fun _ -> data.result_type));
        body        = continue_with data.body;
      }
    end

  | Expression _ | ReadRegister _ | WriteRegister _ | Fail (_, _) | Unconstrained _ -> wrap context result_type statement


(*
   The functions of a list of definitions, with their monomorphs.
*)
let functions (definitions : (Sail.sail_definition * Ast.Definition.t) list) : Ast.Definition.Function.t list =
  List.concat_map definitions ~f:(fun (_, definition) ->
      match definition with
      | FunctionDefinition function_definition -> function_definition :: function_definition.monomorphs
      | TopLevelTypeConstraintDefinition _ | TypeDefinition _ | RegisterDefinition _
      | UntranslatedDefinition _ | ValueDefinition _ | IgnoredDefinition -> [])


let initial_environment (function_definition : Ast.Definition.Function.t) : environment =
  {
    thrown_constructors = Id.Map.empty;
    variable_types      = List.fold function_definition.function_type.parameters ~init:Id.Map.empty ~f:(fun map (key, data) -> Id.Map.overwrite map ~key ~data);
  }


(*
   The raising functions: reachable from a try's body, and a caught exception can leave them.
*)
let raising_functions
    (context   : context                         )
    (functions : Ast.Definition.Function.t list  ) : Type.t Id.Map.t
  =
  let by_name : Ast.Definition.Function.t Id.Map.t =
    List.fold functions ~init:Id.Map.empty ~f:(fun map f -> if Id.Map.mem map f.function_name then map else Id.Map.overwrite map ~key:f.function_name ~data:f)
  in
  let reachable : Id.Set.t =
    let rec close (reached : Id.Set.t) (frontier : Id.t list) : Id.Set.t =
      match frontier with
      | []                -> reached
      | name :: frontier  -> begin
          if Id.Set.mem reached name
          then close reached frontier
          else begin
            let callees =
              match Id.Map.find by_name name with
              | Some f -> Id.Set.to_list @@ called_functions f.function_body
              | None   -> []
            in
            close (Id.Set.add reached name) (List.append callees frontier)
          end
        end
    in
    close Id.Set.empty @@ Id.Set.to_list @@ Id.Set.unions @@ List.map functions ~f:(fun f -> called_in_try_bodies f.function_body)
  in
  let candidates =
    List.filter functions ~f:(fun f -> Id.Set.mem reachable f.function_name)
  in
  let rec fixed_point (raising : Type.t Id.Map.t) : Type.t Id.Map.t =
    let context = { context with raising }
    in
    let added =
      List.filter candidates ~f:(fun f ->
          not (Id.Map.mem raising f.function_name)
          && may_raise context (initial_environment f) f.function_body)
    in
    match added with
    | [] -> raising
    | _  -> fixed_point @@ List.fold added ~init:raising ~f:(fun raising f -> Id.Map.overwrite raising ~key:f.function_name ~data:f.function_type.return_type)
  in
  fixed_point Id.Map.empty


let transform_function
    (context             : context                   )
    (function_definition : Ast.Definition.Function.t ) : Ast.Definition.Function.t
  =
  let environment = initial_environment function_definition
  and return_type = function_definition.function_type.return_type
  in
  if
    Id.Map.mem context.raising function_definition.function_name
  then
    {
      function_definition with
      function_type = { function_definition.function_type with return_type = Sum (return_type, context.exception_type) };
      function_body = lift_or_wrap context environment (Some return_type) function_definition.function_body;
    }
  else
    {
      function_definition with
      function_body = lower context environment function_definition.function_body;
    }


let transform
    (definitions : (Sail.sail_definition * Ast.Definition.t) list) : (Sail.sail_definition * Ast.Definition.t) list
  =
  let functions = functions definitions
  in
  let all_trys = List.concat_map functions ~f:(fun f -> trys f.function_body)
  in
  let exception_type : Type.t =
    match all_trys with
    | (_, exception_type) :: _ -> exception_type
    | []                       -> Unit
  in
  let constructors : (Id.t * Type.t list) list =
    match exception_type with
    | Variant identifier -> begin
        let variants = Ast.Definition.Select.(select (type_definition @@ of_variant_named identifier) (List.map ~f:snd definitions))
        in
        match variants with
        | variant :: _ -> variant.constructors
        | []           -> []
      end
    | _ -> []
  in
  let context =
    {
      caught         = Id.Set.of_list @@ List.concat_map ~f:fst all_trys;
      raising        = Id.Map.empty;
      exception_type;
      constructors;
    }
  in
  let context = { context with raising = raising_functions context functions }
  in
  let transform_definition
      ((sail_definition, definition) : Sail.sail_definition * Ast.Definition.t) : Sail.sail_definition * Ast.Definition.t
    =
    match definition with
    | FunctionDefinition function_definition -> begin
        try
          let transformed =
            {
              (transform_function context function_definition) with
              monomorphs = List.map ~f:(transform_function context) function_definition.monomorphs
            }
          in
          (sail_definition, FunctionDefinition transformed)
        with Unsupported message -> begin
            let Libsail.Ast.DEF_aux (_, annotation) = sail_definition
            in
            let ocaml_location = [%here]
            in
            (sail_definition, UntranslatedDefinition { filename = ocaml_location.pos_fname; line_number = ocaml_location.pos_lnum; sail_location = annotation.loc; message = Some message })
          end
      end
    | TopLevelTypeConstraintDefinition _ | TypeDefinition _ | RegisterDefinition _
    | UntranslatedDefinition _ | ValueDefinition _ | IgnoredDefinition -> (sail_definition, definition)
  in
  List.map ~f:transform_definition definitions
