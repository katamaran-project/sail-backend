(*
   Specialisation of unions with type parameters.

   muSail has no unions with type parameters: Katamaran's Ty is built over the closed set of unions,
   so a union cannot be indexed by a Ty. A Sail union with a type parameter, such as

     union AccessType ('a : Type) = { Read : 'a, Write : 'a, ReadWrite : ('a, 'a), InstructionFetch : unit }

   is therefore translated to one monomorphic union for each closed type argument that the translated code uses,
   e.g., AccessType(ext_access_type).

   Before translation starts, a pass over the whole Sail AST (definitions ignored by the configuration excepted)
   collects these instances from all types in it: the types of all expressions and patterns,
   of top-level type constraints, registers, record fields, union constructors and type abbreviations.
   Type arguments that contain type variables (e.g., MemoryOpResult('t) inside a function that is polymorphic in 't)
   do not make an instance; types that mention them are not translated.

   Naming: a union that is used at one type only keeps Sail's names for itself and its constructors.
   A union used at several types gets a copy per type, whose name and constructor names have a suffix
   derived from the type argument (Res(bool) becomes Res_bool with constructors Val_bool, Exc_bool),
   because Rocq constructor names are global.

   Placement: a copy can only be defined after its type arguments are, which can be later than the union
   (CHERIoT defines AccessType in its prelude and ext_access_type later).
   Each copy is emitted after the last of the union's definition and the definitions of the types its
   type arguments mention.

   Functions are not copied: a function that is polymorphic in the union's type parameter stays untranslated.
*)
open ExtBase


module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_util
end


module Instance = struct
  type t =
    {
      union_identifier : Ast.Identifier.t                     ;  (* name of the union in Sail                     *)
      identifier       : Ast.Identifier.t                     ;  (* name of the monomorphic copy                   *)
      type_arguments   : S.typ_arg list                       ;  (* normalised, closed type arguments              *)
      constructors     : (S.type_union * S.typ * Ast.Identifier.t) list;  (* Sail constructor, its field type in the copy, its name in the copy *)
      position         : int                                  ;  (* index of the Sail definition after which the copy is emitted *)
      depth            : int                                  ;  (* number of specialised unions in the type arguments *)
    }

  (* Name in the copy of the constructor named constructor_identifier in Sail *)
  let constructor
      (instance               : t               )
      (constructor_identifier : Ast.Identifier.t) : Ast.Identifier.t option
    =
    let has_name (S.Tu_aux (Tu_ty_id (_, sail_identifier), _), _, _) =
      String.equal (S.string_of_id sail_identifier) (Ast.Identifier.to_string constructor_identifier)
    in
    Option.map (List.find instance.constructors ~f:has_name) ~f:(fun (_, _, identifier) -> identifier)

  let to_string (instance : t) : string =
    Printf.sprintf "%s(%s) as %s"
      (Ast.Identifier.to_string instance.union_identifier)
      (String.concat ~sep:", " @@ List.map ~f:S.string_of_typ_arg instance.type_arguments)
      (Ast.Identifier.to_string instance.identifier)
end


module Union = struct
  type t =
    {
      identifier   : S.id             ;
      kinded_ids   : S.kinded_id list ;
      constructors : S.type_union list;
      position     : int              ;
      definition   : Sail.sail_definition;
    }
end


type t =
  {
    environment : Libsail.Type_check.Env.t option;
    unions      : Union.t Ast.Identifier.Map.t     ;  (* the unions that are specialised, by name *)
    instances   : Instance.t list                  ;
  }


let empty : t =
  {
    environment = None;
    unions      = Ast.Identifier.Map.empty;
    instances   = [];
  }


let identifier_of_sail_id (identifier : S.id) : Ast.Identifier.t =
  Ast.Identifier.mk @@ S.string_of_id identifier


let rec simplify_type (typ : S.typ) : S.typ =
  let S.Typ_aux (unwrapped_type, location) = typ
  in
  let simplified : S.typ_aux =
    match unwrapped_type with
    | Typ_internal_unknown            -> unwrapped_type
    | Typ_id _                        -> unwrapped_type
    | Typ_var _                       -> unwrapped_type
    | Typ_fn (parameter_types, typ)   -> Typ_fn (List.map ~f:simplify_type parameter_types, simplify_type typ)
    | Typ_bidir (left, right)         -> Typ_bidir (simplify_type left, simplify_type right)
    | Typ_tuple types                 -> Typ_tuple (List.map ~f:simplify_type types)
    | Typ_app (identifier, arguments) -> Typ_app (identifier, List.map ~f:simplify_type_argument arguments)
    | Typ_exist (kinded_ids, nc, typ) -> Typ_exist (kinded_ids, nc, simplify_type typ)
  in
  S.Typ_aux (simplified, location)

and simplify_type_argument (type_argument : S.typ_arg) : S.typ_arg =
  let S.A_aux (unwrapped_argument, location) = type_argument
  in
  let simplified : S.typ_arg_aux =
    match unwrapped_argument with
    | A_typ typ  -> A_typ (simplify_type typ)
    | A_nexp n   -> A_nexp (S.nexp_simp n)
    | A_bool nc  -> A_bool nc
  in
  S.A_aux (simplified, location)


(* Expands type synonyms and simplifies numeric expressions, so that equal types have the same string representation *)
let normalise_type
    (environment : Libsail.Type_check.Env.t option)
    (typ         : S.typ                          ) : S.typ
  =
  let expanded =
    match environment with
    | Some environment -> (try Libsail.Type_check.Env.expand_synonyms environment typ with Libsail.Type_error.Type_error _ -> typ)
    | None             -> typ
  in
  simplify_type expanded


let normalise_type_argument
    (environment   : Libsail.Type_check.Env.t option)
    (type_argument : S.typ_arg                      ) : S.typ_arg
  =
  let S.A_aux (unwrapped_argument, location) = type_argument
  in
  match unwrapped_argument with
  | A_typ typ -> S.A_aux (A_typ (normalise_type environment typ), location)
  | A_nexp _  -> simplify_type_argument type_argument
  | A_bool _  -> type_argument


let key_of_type_arguments (type_arguments : S.typ_arg list) : string =
  String.concat ~sep:", " @@ List.map ~f:S.string_of_typ_arg type_arguments


let rec iter_type
    (f   : S.typ -> unit)
    (typ : S.typ        ) : unit
  =
  f typ;
  let S.Typ_aux (unwrapped_type, _location) = typ
  in
  match unwrapped_type with
  | Typ_internal_unknown            -> ()
  | Typ_id _                        -> ()
  | Typ_var _                       -> ()
  | Typ_fn (parameter_types, typ)   -> List.iter ~f:(iter_type f) parameter_types; iter_type f typ
  | Typ_bidir (left, right)         -> iter_type f left; iter_type f right
  | Typ_tuple types                 -> List.iter ~f:(iter_type f) types
  | Typ_app (_identifier, arguments) -> List.iter ~f:(iter_type_argument f) arguments
  | Typ_exist (_, _, typ)           -> iter_type f typ

and iter_type_argument
    (f             : S.typ -> unit)
    (type_argument : S.typ_arg    ) : unit
  =
  let S.A_aux (unwrapped_argument, _location) = type_argument
  in
  match unwrapped_argument with
  | A_typ typ -> iter_type f typ
  | A_nexp _  -> ()
  | A_bool _  -> ()


(* Identifiers of the types that a type argument mentions *)
let type_identifiers_in_type_argument (type_argument : S.typ_arg) : S.id list =
  let identifiers = ref []
  in
  let visit (typ : S.typ) : unit =
    let S.Typ_aux (unwrapped_type, _location) = typ
    in
    match unwrapped_type with
    | Typ_id identifier          -> identifiers := identifier :: !identifiers
    | Typ_app (identifier, _)    -> identifiers := identifier :: !identifiers
    | Typ_internal_unknown       -> ()
    | Typ_var _                  -> ()
    | Typ_fn (_, _)              -> ()
    | Typ_bidir (_, _)           -> ()
    | Typ_tuple _                -> ()
    | Typ_exist (_, _, _)        -> ()
  in
  iter_type_argument visit type_argument;
  !identifiers


(*
   Suffix for the names of a copy, derived from its type arguments, e.g., bool, bits_8, bits_32_ExceptionType.
*)
let suffix_of_type_arguments (type_arguments : S.typ_arg list) : string =
  let rec parts_of_type (typ : S.typ) : string list =
    let S.Typ_aux (unwrapped_type, _location) = typ
    in
    match unwrapped_type with
    | Typ_id identifier               -> [ S.string_of_id identifier ]
    | Typ_app (identifier, arguments) -> begin
        let name =
          match S.string_of_id identifier with
          | "bitvector" -> "bits"
          | name        -> name
        in
        name :: List.concat_map ~f:parts_of_type_argument arguments
      end
    | Typ_tuple types                 -> List.concat_map ~f:parts_of_type types
    | Typ_internal_unknown            -> [ S.string_of_typ typ ]
    | Typ_var _                       -> [ S.string_of_typ typ ]
    | Typ_fn (_, _)                   -> [ S.string_of_typ typ ]
    | Typ_bidir (_, _)                -> [ S.string_of_typ typ ]
    | Typ_exist (_, _, _)             -> [ S.string_of_typ typ ]

  and parts_of_type_argument (type_argument : S.typ_arg) : string list =
    let S.A_aux (unwrapped_argument, _location) = type_argument
    in
    match unwrapped_argument with
    | A_typ typ -> parts_of_type typ
    | A_nexp n  -> [ S.string_of_nexp n ]
    | A_bool nc -> [ S.string_of_n_constraint nc ]
  in
  let joined =
    String.concat ~sep:"_" @@ List.concat_map ~f:parts_of_type_argument type_arguments
  in
  let sanitised =
    String.map joined ~f:(fun c -> if Char.is_alphanum c || Char.equal c '_' then c else '_')
  in
  (* collapse runs of underscores and remove leading and trailing ones *)
  let parts =
    List.filter (String.split sanitised ~on:'_') ~f:(fun part -> not (String.is_empty part))
  in
  String.concat ~sep:"_" parts


let is_closed (type_arguments : S.typ_arg list) : bool =
  List.for_all type_arguments ~f:(fun type_argument -> Libsail.Ast_compare.KidSet.is_empty (S.tyvars_of_typ_arg type_argument))


(* The type definition in a definition, if it is one *)
let type_definition_of (definition : Sail.sail_definition) : Sail.type_annotation S.type_def option =
  let S.DEF_aux (unwrapped_definition, _annotation) = definition
  in
  match unwrapped_definition with
  | DEF_type type_definition  -> Some type_definition
  | DEF_fundef _              -> None
  | DEF_mapdef _              -> None
  | DEF_impl _                -> None
  | DEF_let (_, _)            -> None
  | DEF_val _                 -> None
  | DEF_outcome (_, _)        -> None
  | DEF_instantiation (_, _)  -> None
  | DEF_fixity (_, _, _)      -> None
  | DEF_overload (_, _)       -> None
  | DEF_default _             -> None
  | DEF_scattered _           -> None
  | DEF_measure (_, _, _)     -> None
  | DEF_loop_measures (_, _)  -> None
  | DEF_register _            -> None
  | DEF_internal_mutrec _     -> None
  | DEF_constraint _          -> None
  | DEF_pragma (_, _)         -> None


(*
   Collects the instances of the unions with type parameters in the given definitions.
   should_ignore tells which definitions the configuration ignores.
   is_sum tells which unions the configuration translates to sums (these are not specialised).
*)
let collect
    (definitions   : Sail.sail_definition list           )
    (environment   : Libsail.Type_check.env              )
    (should_ignore : Sail.sail_definition -> bool        )
    (is_sum        : Ast.Identifier.t -> bool            ) : t
  =
  let environment' = Some environment
  in
  let indexed_definitions =
    List.filter ~f:(fun (_, definition) -> not (should_ignore definition)) @@ List.mapi definitions ~f:(fun index definition -> (index, definition))
  in
  (* index of the definition of each type *)
  let type_positions : int Ast.Identifier.Map.t =
    let add_type_definition map (index, definition) =
      match type_definition_of definition with
      | Some type_definition -> Ast.Identifier.Map.overwrite map ~key:(Ast.Identifier.mk @@ Sail.identifier_of_type_definition type_definition) ~data:index
      | None                 -> map
    in
    List.fold_left indexed_definitions ~init:Ast.Identifier.Map.empty ~f:add_type_definition
  in
  (* the unions to specialise *)
  let unions : Union.t Ast.Identifier.Map.t =
    let add_union map (index, definition) =
      match type_definition_of definition with
      | Some (TD_aux (TD_variant (identifier, type_quantifier, constructors, _), _)) -> begin
          let kinded_ids = S.quant_kopts type_quantifier
          in
          let identifier' = identifier_of_sail_id identifier
          in
          if
            List.exists kinded_ids ~f:S.is_typ_kopt && not (is_sum identifier')
          then
            Ast.Identifier.Map.overwrite map ~key:identifier' ~data:({ identifier; kinded_ids; constructors; position = index; definition } : Union.t)
          else
            map
        end
      | Some (TD_aux (TD_abbrev (_, _, _), _))    -> map
      | Some (TD_aux (TD_record (_, _, _, _), _)) -> map
      | Some (TD_aux (TD_enum (_, _, _), _))      -> map
      | Some (TD_aux (TD_bitfield (_, _, _), _))  -> map
      | Some (TD_aux (TD_abstract (_, _, _), _))  -> map
      | None                                      -> map
    in
    List.fold_left indexed_definitions ~init:Ast.Identifier.Map.empty ~f:add_union
  in
  (* instances, by union and key *)
  let found : (Ast.Identifier.t * S.typ_arg list) list ref = ref []
  in
  let keys : (string, unit) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 16
  in
  let visit (typ : S.typ) : unit =
    let S.Typ_aux (unwrapped_type, _location) = typ
    in
    match unwrapped_type with
    | Typ_app (identifier, arguments) -> begin
        let identifier' = identifier_of_sail_id identifier
        in
        if
          Ast.Identifier.Map.mem unions identifier' && is_closed arguments
        then begin
          let key = Ast.Identifier.to_string identifier' ^ "(" ^ key_of_type_arguments arguments ^ ")"
          in
          if not (Stdlib.Hashtbl.mem keys key) then begin
            Stdlib.Hashtbl.add keys key ();
            found := (identifier', arguments) :: !found
          end
        end
      end
    | Typ_internal_unknown -> ()
    | Typ_id _             -> ()
    | Typ_var _            -> ()
    | Typ_fn (_, _)        -> ()
    | Typ_bidir (_, _)     -> ()
    | Typ_tuple _          -> ()
    | Typ_exist (_, _, _)  -> ()
  in
  let visit_type (typ : S.typ) : unit =
    iter_type visit (normalise_type environment' typ)
  in
  let visit_annotation ((location, tannot) : S.l * Libsail.Type_check.tannot) : S.l * Libsail.Type_check.tannot =
    begin
      match Libsail.Type_check.destruct_tannot tannot with
      | Some (_, typ) -> visit_type typ
      | None          -> ()
    end;
    (location, tannot)
  in
  let visit_definition (definition : Sail.sail_definition) : unit =
    let S.DEF_aux (unwrapped_definition, _annotation) = definition
    in
    begin
      match unwrapped_definition with
      | DEF_val (VS_aux (VS_val_spec (TypSchm_aux (TypSchm_ts (_, typ), _), _, _), _)) -> visit_type typ
      | DEF_register (DEC_aux (DEC_reg (typ, _, _), _))                               -> visit_type typ
      | DEF_type (TD_aux (type_definition, _)) -> begin
          match type_definition with
          | TD_record (_, _, fields, _)          -> List.iter fields ~f:(fun ((_, typ), _) -> visit_type typ)
          | TD_variant (_, _, constructors, _)   -> List.iter constructors ~f:(fun (S.Tu_aux (Tu_ty_id (typ, _), _)) -> visit_type typ)
          | TD_abbrev (_, _, type_argument)      -> iter_type_argument visit type_argument
          | TD_enum (_, _, _)                    -> ()
          | TD_bitfield (_, _, _)                -> ()
          | TD_abstract (_, _, _)                -> ()
        end
      | DEF_fundef _              -> ()
      | DEF_mapdef _              -> ()
      | DEF_impl _                -> ()
      | DEF_let (_, _)            -> ()
      | DEF_outcome (_, _)        -> ()
      | DEF_instantiation (_, _)  -> ()
      | DEF_fixity (_, _, _)      -> ()
      | DEF_overload (_, _)       -> ()
      | DEF_default _             -> ()
      | DEF_scattered _           -> ()
      | DEF_measure (_, _, _)     -> ()
      | DEF_loop_measures (_, _)  -> ()
      | DEF_internal_mutrec _     -> ()
      | DEF_constraint _          -> ()
      | DEF_pragma (_, _)         -> ()
    end;
    ignore (S.map_def_annot visit_annotation definition : Sail.sail_definition)
  in
  List.iter indexed_definitions ~f:(fun (_, definition) -> visit_definition definition);
  let found = List.rev !found
  in
  (* normalised type arguments of each found instance *)
  let found =
    List.map found ~f:(fun (union_identifier, arguments) -> (union_identifier, List.map ~f:(normalise_type_argument environment') arguments))
  in
  let instances_of (union_identifier : Ast.Identifier.t) =
    List.filter found ~f:(fun (identifier, _) -> Ast.Identifier.equal identifier union_identifier)
  in
  let make_instances (union : Union.t) : Instance.t list =
    let union_identifier = identifier_of_sail_id union.identifier
    in
    let union_instances = instances_of union_identifier
    in
    let single = List.length union_instances = 1
    in
    let used_suffixes : (string, unit) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 4
    in
    let make_instance (_, type_arguments) : Instance.t =
      let suffix =
        if single
        then ""
        else begin
          let base = suffix_of_type_arguments type_arguments
          in
          let rec unique (candidate : string) (counter : int) : string =
            if Stdlib.Hashtbl.mem used_suffixes candidate
            then unique (Printf.sprintf "%s_%d" base counter) (counter + 1)
            else candidate
          in
          let suffix = unique base 2
          in
          Stdlib.Hashtbl.add used_suffixes suffix ();
          "_" ^ suffix
        end
      in
      let substitute (typ : S.typ) : S.typ =
        match List.zip union.kinded_ids type_arguments with
        | Ok pairs -> List.fold_left pairs ~init:typ ~f:(fun typ (kinded_id, type_argument) -> S.typ_subst (S.kopt_kid kinded_id) type_argument typ)
        | Unequal_lengths -> typ
      in
      let constructors =
        let make_constructor (constructor : S.type_union) =
          let S.Tu_aux (Tu_ty_id (typ, identifier), _) = constructor
          in
          (constructor, substitute typ, Ast.Identifier.mk @@ S.string_of_id identifier ^ suffix)
        in
        List.map ~f:make_constructor union.constructors
      in
      let position =
        let positions_of_arguments =
          List.filter_map ~f:(fun identifier -> Ast.Identifier.Map.find type_positions (identifier_of_sail_id identifier)) @@
            List.concat_map ~f:type_identifiers_in_type_argument type_arguments
        in
        List.fold_left positions_of_arguments ~init:union.position ~f:Int.max
      in
      let depth =
        List.count ~f:(fun identifier -> Ast.Identifier.Map.mem unions (identifier_of_sail_id identifier)) @@
          List.concat_map ~f:type_identifiers_in_type_argument type_arguments
      in
      {
        union_identifier;
        identifier = Ast.Identifier.mk @@ S.string_of_id union.identifier ^ suffix;
        type_arguments;
        constructors;
        position;
        depth;
      }
    in
    List.map ~f:make_instance union_instances
  in
  {
    environment = environment';
    unions;
    instances   = List.concat_map ~f:make_instances (Ast.Identifier.Map.data unions);
  }


let is_specialised
    (union_instances  : t               )
    (union_identifier : Ast.Identifier.t) : bool
  =
  Ast.Identifier.Map.mem union_instances.unions union_identifier


(* The copies to emit after the definition with the given index, the copies whose arguments mention others last *)
let instances_after
    (union_instances : t  )
    (index           : int) : Instance.t list
  =
  let instances =
    List.filter union_instances.instances ~f:(fun (instance : Instance.t) -> instance.position = index)
  in
  List.stable_sort instances ~compare:(fun (x : Instance.t) (y : Instance.t) -> Int.compare x.depth y.depth)


module Lookup = struct
  type t =
    | NotSpecialised                   (* not a union that is specialised *)
    | Instance of Instance.t
    | NoInstance of string             (* a specialised union at type arguments that have no copy, e.g., because they contain type variables *)
end


let lookup
    (union_instances  : t             )
    (union_identifier : S.id          )
    (type_arguments   : S.typ_arg list) : Lookup.t
  =
  let union_identifier' = identifier_of_sail_id union_identifier
  in
  if
    not (is_specialised union_instances union_identifier')
  then
    Lookup.NotSpecialised
  else begin
    let type_arguments' = List.map ~f:(normalise_type_argument union_instances.environment) type_arguments
    in
    let key = key_of_type_arguments type_arguments'
    in
    let matches (instance : Instance.t) =
      Ast.Identifier.equal instance.union_identifier union_identifier' && String.equal (key_of_type_arguments instance.type_arguments) key
    in
    match List.find union_instances.instances ~f:matches with
    | Some instance -> Lookup.Instance instance
    | None          -> begin
        let reason =
          if is_closed type_arguments'
          then Printf.sprintf "no copy of union %s for type arguments %s" (Ast.Identifier.to_string union_identifier') key
          else Printf.sprintf "union %s with type parameter used at non-closed type arguments %s" (Ast.Identifier.to_string union_identifier') key
        in
        Lookup.NoInstance reason
      end
  end


(* Is the constructor (Sail name) one of a specialised union? *)
let is_constructor_of_specialised_union
    (union_instances        : t               )
    (constructor_identifier : Ast.Identifier.t) : bool
  =
  let has_constructor (union : Union.t) =
    List.exists union.constructors ~f:(fun (S.Tu_aux (Tu_ty_id (_, identifier), _)) -> String.equal (S.string_of_id identifier) (Ast.Identifier.to_string constructor_identifier))
  in
  List.exists (Ast.Identifier.Map.data union_instances.unions) ~f:has_constructor


(* The copy with the given name, if any *)
let find_instance
    (union_instances     : t               )
    (instance_identifier : Ast.Identifier.t) : Instance.t option
  =
  List.find union_instances.instances ~f:(fun (instance : Instance.t) -> Ast.Identifier.equal instance.identifier instance_identifier)


(*
   Name of a constructor (Sail name) in the type with the given name:
   the name in the copy if the type is a copy of a specialised union, the Sail name otherwise.
*)
let constructor_in
    (union_instances        : t               )
    (type_identifier        : Ast.Identifier.t)
    (constructor_identifier : Ast.Identifier.t) : Ast.Identifier.t
  =
  match find_instance union_instances type_identifier with
  | Some instance -> Option.value (Instance.constructor instance constructor_identifier) ~default:constructor_identifier
  | None          -> constructor_identifier


let instances (union_instances : t) : Instance.t list =
  union_instances.instances


(* The Sail definition of the union of which the instance is a copy *)
let union_definition
    (union_instances : t         )
    (instance        : Instance.t) : Sail.sail_definition option
  =
  Option.map (Ast.Identifier.Map.find union_instances.unions instance.union_identifier) ~f:(fun (union : Union.t) -> union.definition)
