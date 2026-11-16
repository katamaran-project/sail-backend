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

   Width families: a union with one type parameter that is used at a bitvector whose width is open
   (MemoryOpResult(bits('p8_times_n)) in a size-polymorphic function) gets one family indexed by the width
   instead, as Katamaran's RiscvPmp memory_op_result (bytes : nat):

     Inductive MemoryOpResult_bits (kid__n : nat) : Set := MemValue_bits : bv kid__n -> ... | ...

   Its uses at closed widths (MemoryOpResult(bits(32))) are the family at that width, so that a size-polymorphic
   function and its callers agree on the type. Its uses at other types are copies, as above.
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
      family           : bool                                 ;  (* a width family: type_arguments is [bitvector('n)] for its index 'n *)
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
    inlined     : (Sail.type_annotation S.pat * Sail.type_annotation S.exp) Ast.Identifier.Map.t;  (* see inlined_function *)
    copies      : (S.typ_arg list * S.id * Sail.sail_definition list) list Ast.Identifier.Map.t;     (* see function_copies *)
  }


let empty : t =
  {
    environment = None;
    unions      = Ast.Identifier.Map.empty;
    instances   = [];
    inlined     = Ast.Identifier.Map.empty;
    copies      = Ast.Identifier.Map.empty;
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
    (*
       bool('p) and int('n) are bool and int: the translation erases their index, so that
       MemoryOpResult(bool('p)), the type of a flow-typed boolean result, is MemoryOpResult(bool)
    *)
    | Typ_app (S.Id_aux (Id "atom_bool", identifier_location), _) -> Typ_id (S.Id_aux (Id "bool", identifier_location))
    | Typ_app (S.Id_aux (Id "atom", identifier_location), _)      -> Typ_id (S.Id_aux (Id "int", identifier_location))
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
let suffix_of_type_arguments ?(family_index : bool = false) (type_arguments : S.typ_arg list) : string =
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
    | A_nexp n  -> if family_index then [] else [ S.string_of_nexp n ]
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


(* The index of a width family *)
let family_index_kid : S.kid = S.mk_kid "n"


(*
   The shape of type arguments that mention exactly one bitvector, with its width replaced by the
   index of a family, and that width: [(bitvector(8 * 'n), bool)] gives [(bitvector('n), bool)] and 8 * 'n.
*)
let family_shape (type_arguments : S.typ_arg list) : (S.typ_arg list * S.nexp) option =
  let widths : S.nexp list ref = ref []
  in
  let rec shape_of_type (typ : S.typ) : S.typ =
    let S.Typ_aux (unwrapped_type, location) = typ
    in
    match unwrapped_type with
    | Typ_app (S.Id_aux (Id "bitvector", identifier_location), [ S.A_aux (A_nexp width, argument_location) ]) -> begin
        widths := width :: !widths;
        S.Typ_aux (Typ_app (S.Id_aux (Id "bitvector", identifier_location), [ S.A_aux (A_nexp (S.nvar family_index_kid), argument_location) ]), location)
      end
    | Typ_app (identifier, arguments) -> S.Typ_aux (Typ_app (identifier, List.map ~f:shape_of_argument arguments), location)
    | Typ_tuple types                 -> S.Typ_aux (Typ_tuple (List.map ~f:shape_of_type types), location)
    | Typ_internal_unknown | Typ_id _ | Typ_var _ | Typ_fn _ | Typ_bidir _ | Typ_exist _ -> typ
  and shape_of_argument (type_argument : S.typ_arg) : S.typ_arg =
    let S.A_aux (unwrapped_argument, location) = type_argument
    in
    match unwrapped_argument with
    | A_typ typ -> S.A_aux (A_typ (shape_of_type typ), location)
    | A_nexp _  -> type_argument
    | A_bool _  -> type_argument
  in
  let shape = List.map ~f:shape_of_argument type_arguments
  in
  match !widths with
  | [ width ] -> Some (shape, width)
  | _         -> None


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
  (* the unions used at a bitvector of open width: they get a width family *)
  let families : (string, S.typ_arg list) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 4      (* by union(shape), the shape *)
  in
  let family_key (union_identifier : Ast.Identifier.t) (shape : S.typ_arg list) : string =
    Ast.Identifier.to_string union_identifier ^ "(" ^ key_of_type_arguments shape ^ ")"
  in
  (* a shape is a family's if the width is its only size variable *)
  let open_family_shape (arguments : S.typ_arg list) : S.typ_arg list option =
    match family_shape arguments with
    | Some (shape, _) when not (Libsail.Ast_compare.KidSet.exists (fun kid -> Libsail.Ast_compare.Kid.compare kid family_index_kid <> 0)
                                  (List.fold shape ~init:Libsail.Ast_compare.KidSet.empty ~f:(fun kids argument -> Libsail.Ast_compare.KidSet.union kids (S.tyvars_of_typ_arg argument)))) -> Some shape
    | Some _ | None -> None
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
        let normalised = List.map ~f:(normalise_type_argument environment') arguments
        in
        if
          Ast.Identifier.Map.mem unions identifier'
          && not (is_closed normalised)
          && Option.is_some (open_family_shape normalised)
        then begin
          match open_family_shape normalised with
          | Some shape -> Stdlib.Hashtbl.replace families (family_key identifier' shape) shape
          | None       -> ()
        end
        else if
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
  let joins_family (union_identifier : Ast.Identifier.t) (arguments : S.typ_arg list) : bool =
    match family_shape arguments with
    | Some (shape, _) -> Stdlib.Hashtbl.mem families (family_key union_identifier shape)
    | None            -> false
  in
  let family_shapes (union_identifier : Ast.Identifier.t) : S.typ_arg list list =
    Stdlib.Hashtbl.fold (fun key shape shapes -> if String.equal key (family_key union_identifier shape) then shape :: shapes else shapes) families []
    |> List.sort ~compare:(fun x y -> String.compare (key_of_type_arguments x) (key_of_type_arguments y))
  in
  (* the instances of a union; a family stands for all its uses at bitvectors *)
  let instances_of (union_identifier : Ast.Identifier.t) : (Ast.Identifier.t * S.typ_arg list * bool) list =
    let copies =
      List.filter_map found ~f:(fun (identifier, arguments) ->
          if
            Ast.Identifier.equal identifier union_identifier
            && not (joins_family union_identifier arguments)
          then Some (identifier, arguments, false)
          else None
        )
    in
    List.map (family_shapes union_identifier) ~f:(fun shape -> (union_identifier, shape, true)) @ copies
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
    let make_instance (_, type_arguments, family) : Instance.t =
      let suffix =
        if single
        then ""
        else begin
          let base = if family then suffix_of_type_arguments ~family_index:true type_arguments else suffix_of_type_arguments type_arguments
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
        family;
      }
    in
    List.map ~f:make_instance union_instances
  in
  (*
     The functions polymorphic in a type that a specialised union takes as argument in their signature
     (MemoryOpResult_drop_meta : forall ('t : Type). MemoryOpResult(('t, bool)) -> MemoryOpResult('t)).
     Unions cannot be indexed by a Ty in Katamaran, so such a function cannot be one definition.
     - Used at one or two types (the shape of a width family counts as one type, see family_shape):
       one copy per type, type-checked again; with one type, the copy keeps the function's name.
     - Used at more types: its calls are inlined (see Function.inline_calls).
  *)
  let polymorphic_over_union : (S.id * S.kid * S.typquant * S.typ) list =
    List.filter_map indexed_definitions ~f:(fun (_, definition) ->
        let S.DEF_aux (unwrapped_definition, _annotation) = definition
        in
        match unwrapped_definition with
        | DEF_val (VS_aux (VS_val_spec (TypSchm_aux (TypSchm_ts (type_quantifier, typ), _), identifier, _), _)) -> begin
            let type_kids =
              List.filter_map (S.quant_kopts type_quantifier) ~f:(fun kinded_id -> if S.is_typ_kopt kinded_id then Some (S.kopt_kid kinded_id) else None)
            in
            let over_union = ref false
            in
            let visit (typ : S.typ) : unit =
              let S.Typ_aux (unwrapped_type, _location) = typ
              in
              match unwrapped_type with
              | Typ_app (union_identifier, arguments) when Ast.Identifier.Map.mem unions (identifier_of_sail_id union_identifier) -> begin
                  if List.exists arguments ~f:(fun argument ->
                      List.exists type_kids ~f:(fun kid -> Libsail.Ast_compare.KidSet.mem kid (S.tyvars_of_typ_arg argument)))
                  then over_union := true
                end
              | Typ_internal_unknown | Typ_id _ | Typ_var _ | Typ_fn _ | Typ_bidir _ | Typ_tuple _ | Typ_app _ | Typ_exist _ -> ()
            in
            iter_type visit typ;
            match type_kids with
            | [ type_kid ] when !over_union -> Some (identifier, type_kid, type_quantifier, typ)
            | _                             -> None
          end
        | DEF_type _ | DEF_fundef _ | DEF_mapdef _ | DEF_impl _ | DEF_let _ | DEF_outcome _ | DEF_instantiation _
        | DEF_fixity _ | DEF_overload _ | DEF_default _ | DEF_scattered _ | DEF_measure _ | DEF_loop_measures _ | DEF_register _
        | DEF_internal_mutrec _ | DEF_constraint _ | DEF_pragma _ -> None
      )
  in
  let is_polymorphic_over_union (identifier : S.id) =
    List.exists polymorphic_over_union ~f:(fun (identifier', _, _, _) -> String.equal (S.string_of_id identifier) (S.string_of_id identifier'))
  in
  (* the single clause of each such function *)
  let clauses : (string * (Sail.type_annotation S.pat * Sail.type_annotation S.exp)) list =
    List.filter_map indexed_definitions ~f:(fun (_, definition) ->
        let S.DEF_aux (unwrapped_definition, _annotation) = definition
        in
        match unwrapped_definition with
        | DEF_fundef (FD_aux (FD_function (_, _, [ FCL_aux (FCL_funcl (identifier, Pat_aux (Pat_exp (pattern, body), _)), _) ]), _))
          when is_polymorphic_over_union identifier -> Some (S.string_of_id identifier, (pattern, body))
        | DEF_type _ | DEF_fundef _ | DEF_mapdef _ | DEF_impl _ | DEF_let _ | DEF_val _ | DEF_outcome _ | DEF_instantiation _
        | DEF_fixity _ | DEF_overload _ | DEF_default _ | DEF_scattered _ | DEF_measure _ | DEF_loop_measures _ | DEF_register _
        | DEF_internal_mutrec _ | DEF_constraint _ | DEF_pragma _ -> None
      )
  in
  (* the types each such function is called at, as shapes (a width family's shape for a bitvector type) *)
  let call_types : (string, S.typ_arg list) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 4
  in
  let record_call (identifier : S.id) (expression : Sail.type_annotation S.exp) : unit =
    match List.find polymorphic_over_union ~f:(fun (identifier', _, _, _) -> String.equal (S.string_of_id identifier) (S.string_of_id identifier')) with
    | None                       -> ()
    | Some (_, type_kid, _, _) -> begin
        match Libsail.Ast_compare.KBindings.find_opt type_kid (try Libsail.Type_check.instantiation_of expression with _ -> Libsail.Ast_compare.KBindings.empty) with
        | None          -> ()
        | Some argument -> begin
            let argument = normalise_type_argument environment' argument
            in
            let shape =
              match family_shape [ argument ] with
              | Some (shape, _) -> shape
              | None            -> [ argument ]
            in
            let name = S.string_of_id identifier
            in
            let known = Stdlib.Hashtbl.find_all call_types name
            in
            if not (List.exists known ~f:(fun shape' -> String.equal (key_of_type_arguments shape') (key_of_type_arguments shape)))
            then Stdlib.Hashtbl.add call_types name shape
          end
      end
  in
  List.iter indexed_definitions ~f:(fun (_, definition) ->
      let S.DEF_aux (unwrapped_definition, _annotation) = definition
      in
      match unwrapped_definition with
      | DEF_fundef (FD_aux (FD_function (_, _, function_clauses), _)) -> begin
          List.iter function_clauses ~f:(fun (S.FCL_aux (FCL_funcl (_, S.Pat_aux (clause, _)), _)) ->
              let body =
                match clause with
                | Pat_exp (_, body)     -> body
                | Pat_when (_, _, body) -> body
              in
              let algebra =
                { Libsail.Rewriter.id_exp_alg with
                  e_aux = (fun (unwrapped_expression, annotation) ->
                      let expression = S.E_aux (unwrapped_expression, annotation)
                      in
                      begin
                        match unwrapped_expression with
                        | E_app (identifier, _) -> record_call identifier expression
                        | _                     -> ()
                      end;
                      expression) }
              in
              ignore (Libsail.Rewriter.fold_exp algebra body : Sail.type_annotation S.exp)
            )
        end
      | DEF_type _ | DEF_mapdef _ | DEF_impl _ | DEF_let _ | DEF_val _ | DEF_outcome _ | DEF_instantiation _
      | DEF_fixity _ | DEF_overload _ | DEF_default _ | DEF_scattered _ | DEF_measure _ | DEF_loop_measures _ | DEF_register _
      | DEF_internal_mutrec _ | DEF_constraint _ | DEF_pragma _ -> ()
    );
  (*
     The copies of a function at the shapes it is called at: a val spec and a function, with the type variable
     replaced by the shape (and the family's index, if any, quantified), type-checked in the global environment.
  *)
  let make_copies
      ((identifier, type_kid, type_quantifier, typ) : S.id * S.kid * S.typquant * S.typ)
      (shapes                                         : S.typ_arg list list            ) : (S.typ_arg list * S.id * Sail.sail_definition list) list option
    =
    match List.Assoc.find clauses ~equal:String.equal (S.string_of_id identifier), environment' with
    | None, _ | _, None                -> None
    | Some (pattern, body), Some environment -> begin
        let single = List.length shapes = 1
        in
        let copy (shape : S.typ_arg list) : (S.typ_arg list * S.id * Sail.sail_definition list) option =
          match shape with
          | [ argument ] -> begin
              let copy_identifier =
                if single
                then identifier
                else S.mk_id (S.string_of_id identifier ^ "_" ^ suffix_of_type_arguments ~family_index:true shape)
              in
              (* Sail refuses to declare a function twice: a copy that keeps the name is checked under another, then renamed *)
              let checked_identifier = S.mk_id (S.string_of_id copy_identifier ^ "__nanosail_copy")
              in
              let rename (definition : Sail.sail_definition) : Sail.sail_definition =
                let S.DEF_aux (unwrapped_definition, definition_annotation) = definition
                in
                let unwrapped_definition =
                  match unwrapped_definition with
                  | DEF_val (VS_aux (VS_val_spec (schema, _, extern), annotation)) -> S.DEF_val (VS_aux (VS_val_spec (schema, copy_identifier, extern), annotation))
                  | DEF_fundef (FD_aux (FD_function (recursive, annotation_option, function_clauses), annotation)) -> begin
                      let rename_clause (S.FCL_aux (FCL_funcl (_, clause), clause_annotation)) =
                        S.FCL_aux (FCL_funcl (copy_identifier, clause), clause_annotation)
                      in
                      S.DEF_fundef (FD_aux (FD_function (recursive, annotation_option, List.map ~f:rename_clause function_clauses), annotation))
                    end
                  | other -> other
                in
                S.DEF_aux (unwrapped_definition, definition_annotation)
              in
              let quantifier =
                let others =
                  List.filter type_quantifier ~f:(fun (S.QI_aux (item, _)) ->
                      match item with
                      | QI_id kinded_id    -> Libsail.Ast_compare.Kid.compare (S.kopt_kid kinded_id) type_kid <> 0
                      | QI_constraint _    -> true
                    )
                in
                if Libsail.Ast_compare.KidSet.mem family_index_kid (S.tyvars_of_typ_arg argument)
                then S.mk_qi_id K_int family_index_kid :: others
                else others
              in
              let typ' = S.typ_subst type_kid argument typ
              in
              let val_spec =
                S.mk_val_spec (VS_val_spec (S.mk_typschm quantifier typ', checked_identifier, None))
              and fundef =
                S.mk_fundef [ S.mk_funcl checked_identifier (Libsail.Type_check.strip_pat pattern) (Libsail.Type_check.strip_exp body) ]
              in
              match Libsail.Type_check.check_defs environment [ val_spec; fundef ] with
              | (definitions, _)  -> Some (shape, copy_identifier, List.map ~f:rename definitions)
              | exception _       -> None
            end
          | _ -> None
        in
        let copies = List.map ~f:copy shapes
        in
        if List.for_all copies ~f:Option.is_some then Some (List.filter_opt copies) else None
      end
  in
  let copies, inlined_names =
    List.fold polymorphic_over_union ~init:(Ast.Identifier.Map.empty, []) ~f:(fun (copies, inlined_names) ((identifier, _, _, _) as function_) ->
        let shapes = List.rev (Stdlib.Hashtbl.find_all call_types (S.string_of_id identifier))
        in
        let made =
          if List.length shapes >= 1 && List.length shapes <= 2 then make_copies function_ shapes else None
        in
        match made with
        | Some made -> (Ast.Identifier.Map.overwrite copies ~key:(identifier_of_sail_id identifier) ~data:made, inlined_names)
        | None      -> (copies, S.string_of_id identifier :: inlined_names)
      )
  in
  let inlined =
    List.fold clauses ~init:Ast.Identifier.Map.empty ~f:(fun map (name, clause) ->
        if List.mem inlined_names name ~equal:String.equal
        then Ast.Identifier.Map.overwrite map ~key:(Ast.Identifier.mk name) ~data:clause
        else map
      )
  in
  {
    environment = environment';
    unions;
    instances   = List.concat_map ~f:make_instances (Ast.Identifier.Map.data unions);
    inlined;
    copies;
  }


(* The parameter pattern and body of a function whose calls are inlined (see collect) *)
let inlined_function
    (union_instances : t     )
    (identifier      : S.id  ) : (Sail.type_annotation S.pat * Sail.type_annotation S.exp) option
  =
  Ast.Identifier.Map.find union_instances.inlined (identifier_of_sail_id identifier)


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
    | Family of Instance.t * S.nexp    (* a width family at the given width *)
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
    let family =
      match family_shape type_arguments' with
      | None                -> None
      | Some (shape, width) -> begin
          Option.map ~f:(fun instance -> (instance, width)) @@
            List.find union_instances.instances ~f:(fun (instance : Instance.t) ->
                instance.family
                && Ast.Identifier.equal instance.union_identifier union_identifier'
                && String.equal (key_of_type_arguments instance.type_arguments) (key_of_type_arguments shape))
        end
    in
    match family with
    | Some (instance, width) -> Lookup.Family (instance, width)
    | None -> begin
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


(* Whether the definition is the val spec or the function of a function whose calls are inlined *)
let is_inlined_definition
    (union_instances : t                   )
    (definition      : Sail.sail_definition) : bool
  =
  let S.DEF_aux (unwrapped_definition, _annotation) = definition
  in
  let name =
    match unwrapped_definition with
    | DEF_val value_specification   -> Some (Sail.identifier_of_top_level_type_constraint value_specification)
    | DEF_fundef function_definition -> Some (Sail.identifier_of_function_definition function_definition)
    | DEF_type _ | DEF_mapdef _ | DEF_impl _ | DEF_let _ | DEF_outcome _ | DEF_instantiation _
    | DEF_fixity _ | DEF_overload _ | DEF_default _ | DEF_scattered _ | DEF_measure _ | DEF_loop_measures _ | DEF_register _
    | DEF_internal_mutrec _ | DEF_constraint _ | DEF_pragma _ -> None
  in
  match name with
  | Some name -> Ast.Identifier.Map.mem union_instances.inlined (Ast.Identifier.mk name)
  | None      -> false


let definition_name (definition : Sail.sail_definition) : string option =
  let S.DEF_aux (unwrapped_definition, _annotation) = definition
  in
  match unwrapped_definition with
  | DEF_val value_specification    -> Some (Sail.identifier_of_top_level_type_constraint value_specification)
  | DEF_fundef function_definition -> Some (Sail.identifier_of_function_definition function_definition)
  | DEF_type _ | DEF_mapdef _ | DEF_impl _ | DEF_let _ | DEF_outcome _ | DEF_instantiation _
  | DEF_fixity _ | DEF_overload _ | DEF_default _ | DEF_scattered _ | DEF_measure _ | DEF_loop_measures _ | DEF_register _
  | DEF_internal_mutrec _ | DEF_constraint _ | DEF_pragma _ -> None


let is_val_spec (definition : Sail.sail_definition) : bool =
  let S.DEF_aux (unwrapped_definition, _annotation) = definition
  in
  match unwrapped_definition with
  | DEF_val _ -> true
  | DEF_fundef _ | DEF_type _ | DEF_mapdef _ | DEF_impl _ | DEF_let _ | DEF_outcome _ | DEF_instantiation _
  | DEF_fixity _ | DEF_overload _ | DEF_default _ | DEF_scattered _ | DEF_measure _ | DEF_loop_measures _ | DEF_register _
  | DEF_internal_mutrec _ | DEF_constraint _ | DEF_pragma _ -> false


(*
   For the val spec or function of a function that is copied per type (see collect), the copies'
   val specs or functions, which are translated in its place; None for any other definition.
*)
let copies_of_definition
    (union_instances : t                   )
    (definition      : Sail.sail_definition) : Sail.sail_definition list option
  =
  match definition_name definition with
  | None      -> None
  | Some name -> begin
      Option.map (Ast.Identifier.Map.find union_instances.copies (Ast.Identifier.mk name)) ~f:(fun copies ->
          List.concat_map copies ~f:(fun (_, _, definitions) ->
              List.filter definitions ~f:(fun definition' -> Bool.equal (is_val_spec definition') (is_val_spec definition))
            )
        )
    end


(*
   A call to a function with two copies is a call to the copy at the call's type: its name, and its type scheme
   (which the call's environment does not know yet). None if the function has one copy (it keeps its name) or none.
*)
let renamed_call
    (union_instances : t                         )
    (identifier      : S.id                      )
    (expression      : Sail.type_annotation S.exp) : (S.id * S.typquant * S.typ) option
  =
  match Ast.Identifier.Map.find union_instances.copies (identifier_of_sail_id identifier) with
  | None | Some [] | Some [ _ ] -> None
  | Some copies -> begin
      let instantiated =
        try Some (Libsail.Ast_compare.KBindings.bindings (Libsail.Type_check.instantiation_of expression)) with _ -> None
      in
      match instantiated with
      | Some [ (_, argument) ] -> begin
          let argument = normalise_type_argument union_instances.environment argument
          in
          let shape =
            match family_shape [ argument ] with
            | Some (shape, _) -> shape
            | None            -> [ argument ]
          in
          List.find_map copies ~f:(fun (shape', copy_identifier, definitions) ->
              if String.equal (key_of_type_arguments shape) (key_of_type_arguments shape')
              then
                List.find_map definitions ~f:(fun (S.DEF_aux (definition, _)) ->
                    match definition with
                    | DEF_val (VS_aux (VS_val_spec (TypSchm_aux (TypSchm_ts (type_quantifier, typ), _), _, _), _)) -> Some (copy_identifier, type_quantifier, typ)
                    | _ -> None
                  )
              else None
            )
        end
      | Some _ | None -> None
    end
