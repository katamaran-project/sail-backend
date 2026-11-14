(*

   EqDec instances whose size, and the time Rocq needs to check them, is linear in
   the number of constructors.

   Equations' Derive EqDec (and the Derive NoConfusion it relies on) is quadratic in
   the number of constructors: for CHERIoT's 147-constructor instruction union ast
   and its tag enum astConstructor, they took about 6 minutes together.

   - Enums (including the ...Constructor tag enums, Enums, Unions, Records and RegName)
     are numbered: to_N maps each value to its position, of_N maps a position back
     (List.nth in the list of values), and of_N (to_N x) = x holds by computation for
     each value. Two values are equal iff their numbers are, which N.eq_dec decides.

   - Unions are compared through their constructor tag (an enum, so decided as above)
     and the tuple of the constructor's fields (decided by the fields' own EqDec
     instances). tag, fields and build (the inverse of tag and fields) are defined by
     one match each, and build (tag x) (fields x) = x holds by computation.

   - Registers (sigT Reg) are numbered like enums.

   The generic parts (nanosail_eqdec_via_N, nanosail_eqdec_via_tag) are emitted once,
   by pp_generic_definitions. All proofs that compare equal values are transparent
   (Defined), so that eq_dec applied to two closed equal values reduces to left eq_refl.

*)
open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_generic_definitions () : PP.t =
  let lines = [
    "(* Generic parts of the EqDec instances below (see EqDec.ml in nanosail) *)";
    "Lemma nanosail_eqdec_via_N_injective {A : Type}";
    "  (to_N : A -> BinNums.N) (of_N : BinNums.N -> A) (H : forall x, of_N (to_N x) = x)";
    "  (x y : A) (e : to_N x = to_N y) : x = y.";
    "Proof. rewrite <- (H x), <- (H y), e. reflexivity. Defined.";
    "";
    "Definition nanosail_eqdec_via_N {A : Type}";
    "  (to_N : A -> BinNums.N) (of_N : BinNums.N -> A) (H : forall x, of_N (to_N x) = x) : EqDec A :=";
    "  fun x y =>";
    "    match BinNat.N.eq_dec (to_N x) (to_N y) with";
    "    | left e  => left (nanosail_eqdec_via_N_injective to_N of_N H x y e)";
    "    | right n => right (fun e => n (f_equal to_N e))";
    "    end.";
    "";
    "Lemma nanosail_eqdec_via_tag_equal {A K : Type} {F : K -> Type}";
    "  (tag : A -> K) (fields : forall x, F (tag x)) (build : forall k, F k -> A)";
    "  (H : forall x, build (tag x) (fields x) = x)";
    "  (x y : A) (e : tag x = tag y) (e' : eq_rect _ F (fields x) _ e = fields y) : x = y.";
    "Proof.";
    "  refine (eq_trans (eq_sym (H x)) (eq_trans _ (eq_trans (f_equal (build (tag y)) e') (H y)))).";
    "  clear e'. destruct e. reflexivity.";
    "Defined.";
    "";
    "Lemma nanosail_eqdec_via_tag_different {A K : Type} {F : K -> Type} {eqK : EqDec K}";
    "  (tag : A -> K) (fields : forall x, F (tag x))";
    "  (x y : A) (e : tag x = tag y) (n : eq_rect _ F (fields x) _ e <> fields y) : x <> y.";
    "Proof.";
    "  intros ->. apply n. rewrite (Eqdep_dec.UIP_dec eq_dec e eq_refl). reflexivity.";
    "Qed.";
    "";
    "Definition nanosail_eqdec_via_tag {A K : Type} {F : K -> Type} {eqK : EqDec K} (eqF : forall k, EqDec (F k))";
    "  (tag : A -> K) (fields : forall x, F (tag x)) (build : forall k, F k -> A)";
    "  (H : forall x, build (tag x) (fields x) = x) : EqDec A :=";
    "  fun x y =>";
    "    match eq_dec (tag x) (tag y) with";
    "    | left e =>";
    "        match eq_dec (eq_rect _ F (fields x) _ e) (fields y) with";
    "        | left e' => left (nanosail_eqdec_via_tag_equal tag fields build H x y e e')";
    "        | right n => right (nanosail_eqdec_via_tag_different tag fields x y e n)";
    "        end";
    "    | right n => right (fun e => n (f_equal tag e))";
    "    end.";
  ]
  in
  PP.annotate [%here] @@ PP.vertical @@ List.map ~f:PP.string lines


(* Rocq name made of a type's name and a suffix, e.g., ast_eqdec_tag *)
let derived_name (type_name : PP.t) (suffix : string) : PP.t =
  PP.horizontal [ type_name; PP.string suffix ]


(* x1 :: x2 :: ... :: nil, one item per line *)
let pp_cons_list (items : PP.t list) : PP.t =
  PP.vertical begin
    List.map items ~f:(fun item -> PP.horizontal [ item; PP.string " ::" ])
    @ [ PP.string "nil" ]
  end


(*

   The EqDec instance for an enum type_name with the given values (in order).

     Definition T_eqdec_to_N (x : T) : BinNums.N :=
       match x with
       | A => 0%N
       | B => 1%N
       end.

     Definition T_eqdec_of_N (n : BinNums.N) : T :=
       List.nth (BinNat.N.to_nat n) (A :: B :: nil)%list A.

     #[export] Instance T_EqDec : EqDec T :=
       nanosail_eqdec_via_N T_eqdec_to_N T_eqdec_of_N
         (fun x => match x return T_eqdec_of_N (T_eqdec_to_N x) = x with
                   | A => eq_refl
                   | B => eq_refl
                   end).

   For an enum without values: fun x => match x with end.

*)
let pp_enum_instance
    ~(type_name : PP.t     )
    ~(values    : PP.t list) : PP.t
  =
  let instance_line =
    PP.horizontal [ PP.string "#[export] Instance "; derived_name type_name "_EqDec"; PP.string " : EqDec "; type_name; PP.string " :=" ]
  in
  match values with
  | [] -> begin
      PP.annotate [%here] @@ PP.vertical [
        instance_line;
        PP.string "  fun x => match x with end.";
      ]
    end
  | first_value :: _ -> begin
      let to_N = derived_name type_name "_eqdec_to_N"
      and of_N = derived_name type_name "_eqdec_of_N"
      in
      let to_N_definition =
        let cases =
          List.mapi values ~f:(fun index value -> (value, PP.horizontal [ PP.integer index; PP.string "%N" ]))
        in
        PP.vertical [
          PP.horizontal [ PP.string "Definition "; to_N; PP.string " (x : "; type_name; PP.string ") : BinNums.N :=" ];
          PP.indent @@ PP.horizontal [ Coq.pp_match (PP.string "x") cases; Coq.eol ];
        ]
      and of_N_definition =
        PP.vertical [
          PP.horizontal [ PP.string "Definition "; of_N; PP.string " (n : BinNums.N) : "; type_name; PP.string " :=" ];
          PP.indent @@ PP.vertical [
            PP.string "List.nth (BinNat.N.to_nat n)";
            PP.indent @@ PP.horizontal [ PP.string "("; pp_cons_list values; PP.string ")%list " ];
            PP.indent @@ PP.horizontal [ first_value; Coq.eol ];
          ]
        ]
      and instance =
        let proof =
          let cases = List.map values ~f:(fun value -> (value, PP.string "eq_refl"))
          and matched =
            PP.horizontal [ PP.string "x return "; of_N; PP.string " ("; to_N; PP.string " x) = x" ]
          in
          PP.horizontal [ PP.string "(fun x => "; Coq.pp_match matched cases; PP.string ")" ]
        in
        PP.vertical [
          instance_line;
          PP.indent @@ PP.horizontal [ PP.string "nanosail_eqdec_via_N "; to_N; PP.string " "; of_N ];
          PP.indent @@ PP.indent @@ PP.horizontal [ proof; Coq.eol ];
        ]
      in
      PP.annotate [%here] @@ PP.paragraphs [ to_N_definition; of_N_definition; instance ]
    end


let pp_enum_definition_instance (enum_definition : Ast.Definition.Type.Enum.t) : PP.t =
  PP.annotate [%here] begin
    pp_enum_instance
      ~type_name:(Identifier.pp enum_definition.identifier)
      ~values:(List.map ~f:Identifier.pp enum_definition.cases)
  end


(*

   The EqDec instance for a union T (without type parameters) with constructor tag enum TConstructor.

     Definition T_eqdec_tag (x : T) : TConstructor :=
       match x with
       | C1 _ _ => KC1
       | C2     => KC2
       end.

     Definition T_eqdec_fields_type (k : TConstructor) : Set :=
       match k with
       | KC1 => (A * B)%type
       | KC2 => unit
       end.

     Definition T_eqdec_fields (x : T) : T_eqdec_fields_type (T_eqdec_tag x) :=
       match x with
       | C1 v0 v1 => (v0, v1)
       | C2       => tt
       end.

     Definition T_eqdec_build (k : TConstructor) : T_eqdec_fields_type k -> T :=
       match k return T_eqdec_fields_type k -> T with
       | KC1 => fun '(v0, v1) => C1 v0 v1
       | KC2 => fun _ => C2
       end.

     Definition T_eqdec_fields_EqDec (k : TConstructor) : EqDec (T_eqdec_fields_type k) :=
       match k return EqDec (T_eqdec_fields_type k) with
       | KC1 => eq_dec
       | KC2 => eq_dec
       end.

     #[export] Instance T_EqDec : EqDec T :=
       nanosail_eqdec_via_tag T_eqdec_fields_EqDec T_eqdec_tag T_eqdec_fields T_eqdec_build
         (fun x => match x return T_eqdec_build (T_eqdec_tag x) (T_eqdec_fields x) = x with
                   | C1 _ _ => eq_refl
                   | C2     => eq_refl
                   end).

   The EqDec instance of TConstructor must come before it.

*)
let pp_variant_instance (variant_definition : Ast.Definition.Type.Variant.t) : PP.t GC.t =
  let type_name      = Identifier.pp variant_definition.identifier
  and tag_type_name  = Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  in
  let tag         = derived_name type_name "_eqdec_tag"
  and fields_type = derived_name type_name "_eqdec_fields_type"
  and fields      = derived_name type_name "_eqdec_fields"
  and build       = derived_name type_name "_eqdec_build"
  and fields_eq   = derived_name type_name "_eqdec_fields_EqDec"
  in
  let constructors : (PP.t * PP.t * Ast.Type.t list) list =
    List.map variant_definition.constructors ~f:(fun (constructor, field_types) ->
        (
          Identifier.pp constructor,
          Identifier.pp @@ Identifier.reified_variant_constructor_name constructor,
          field_types
        )
      )
  in
  let variables (field_types : Ast.Type.t list) : PP.t list =
    List.mapi field_types ~f:(fun index _ -> PP.string @@ Printf.sprintf "v%d" index)
  in
  let pattern (constructor : PP.t) (arguments : PP.t list) : PP.t =
    PP.separate_horizontally ~separator:PP.space (constructor :: arguments)
  in
  let wildcard_pattern (constructor : PP.t) (field_types : Ast.Type.t list) : PP.t =
    pattern constructor @@ List.map field_types ~f:(fun _ -> PP.underscore)
  in
  (* the fields as one value: tt, v0, or (v0, v1, ...) *)
  let tuple (vs : PP.t list) : PP.t =
    match vs with
    | []  -> PP.string "tt"
    | [v] -> v
    | _   -> PP.(surround parens @@ separate_horizontally ~separator:(string ", ") vs)
  in
  let definition (header : PP.t list) (body : PP.t) : PP.t =
    PP.vertical [
      PP.horizontal header;
      PP.indent @@ PP.horizontal [ body; Coq.eol ];
    ]
  in
  let tag_definition =
    let cases = List.map constructors ~f:(fun (constructor, tag, field_types) -> (wildcard_pattern constructor field_types, tag))
    in
    definition
      [ PP.string "Definition "; tag; PP.string " (x : "; type_name; PP.string ") : "; tag_type_name; PP.string " :=" ]
      (Coq.pp_match (PP.string "x") cases)
  in
  let* fields_type_definition =
    let* cases =
      GC.map constructors ~f:(fun (_, tag, field_types) ->
          let* coq_types = GC.map ~f:Type.coq_type_of_nanotype field_types
          in
          let fields_type =
            match coq_types with
            | []  -> PP.string "unit"
            | [t] -> t
            | ts  -> Coq.pp_tuple_type ts
          in
          GC.return (tag, fields_type)
        )
    in
    GC.return begin
      definition
        [ PP.string "Definition "; fields_type; PP.string " (k : "; tag_type_name; PP.string ") : Set :=" ]
        (Coq.pp_match (PP.string "k") cases)
    end
  in
  let fields_definition =
    let cases =
      List.map constructors ~f:(fun (constructor, _, field_types) ->
          let vs = variables field_types
          in
          (pattern constructor vs, tuple vs)
        )
    in
    definition
      [ PP.string "Definition "; fields; PP.string " (x : "; type_name; PP.string ") : "; fields_type; PP.string " ("; tag; PP.string " x) :=" ]
      (Coq.pp_match (PP.string "x") cases)
  in
  let build_definition =
    let cases =
      List.map constructors ~f:(fun (constructor, tag, field_types) ->
          let vs = variables field_types
          in
          let lambda =
            match vs with
            | []  -> PP.horizontal [ PP.string "fun _ => "; constructor ]
            | [v] -> PP.horizontal [ PP.string "fun "; v; PP.string " => "; pattern constructor vs ]
            | _   -> PP.horizontal [ PP.string "fun '"; tuple vs; PP.string " => "; pattern constructor vs ]
          in
          (tag, lambda)
        )
    and matched =
      PP.horizontal [ PP.string "k return "; fields_type; PP.string " k -> "; type_name ]
    in
    definition
      [ PP.string "Definition "; build; PP.string " (k : "; tag_type_name; PP.string ") : "; fields_type; PP.string " k -> "; type_name; PP.string " :=" ]
      (Coq.pp_match matched cases)
  in
  let fields_eqdec_definition =
    let cases =
      List.map constructors ~f:(fun (_, tag, _) -> (tag, PP.string "eq_dec"))
    and matched =
      PP.horizontal [ PP.string "k return EqDec ("; fields_type; PP.string " k)" ]
    in
    definition
      [ PP.string "Definition "; fields_eq; PP.string " (k : "; tag_type_name; PP.string ") : EqDec ("; fields_type; PP.string " k) :=" ]
      (Coq.pp_match matched cases)
  in
  let instance =
    let proof =
      let cases =
        List.map constructors ~f:(fun (constructor, _, field_types) -> (wildcard_pattern constructor field_types, PP.string "eq_refl"))
      and matched =
        PP.horizontal [ PP.string "x return "; build; PP.string " ("; tag; PP.string " x) ("; fields; PP.string " x) = x" ]
      in
      PP.horizontal [ PP.string "(fun x => "; Coq.pp_match matched cases; PP.string ")" ]
    in
    PP.vertical [
      PP.horizontal [ PP.string "#[export] Instance "; derived_name type_name "_EqDec"; PP.string " : EqDec "; type_name; PP.string " :=" ];
      PP.indent @@ PP.separate_horizontally ~separator:PP.space [ PP.string "nanosail_eqdec_via_tag"; fields_eq; tag; fields; build ];
      PP.indent @@ PP.indent @@ PP.horizontal [ proof; Coq.eol ];
    ]
  in
  GC.return begin
    PP.annotate [%here] begin
      PP.paragraphs [
        tag_definition;
        fields_type_definition;
        fields_definition;
        build_definition;
        fields_eqdec_definition;
        instance;
      ]
    end
  end


(*

   The EqDec instance for sigT Reg, numbering the registers.

     Definition 𝑹𝑬𝑮_eqdec_to_N (x : sigT Reg) : BinNums.N :=
       match x with existT _ r => match r with
       | R1 => 0%N
       | R2 => 1%N
       end end.

     Definition 𝑹𝑬𝑮_eqdec_of_N (n : BinNums.N) : sigT Reg :=
       List.nth (BinNat.N.to_nat n) (existT _ R1 :: existT _ R2 :: nil)%list (existT _ R1).

     #[export] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=
       nanosail_eqdec_via_N 𝑹𝑬𝑮_eqdec_to_N 𝑹𝑬𝑮_eqdec_of_N
         (fun x => match x return 𝑹𝑬𝑮_eqdec_of_N (𝑹𝑬𝑮_eqdec_to_N x) = x with
                   | existT _ r => match r in Reg σ return 𝑹𝑬𝑮_eqdec_of_N (𝑹𝑬𝑮_eqdec_to_N (existT σ r)) = existT σ r with
                                   | R1 => eq_refl
                                   | R2 => eq_refl
                                   end
                   end).

   Without registers: fun x => match projT2 x with end.

*)
let pp_register_instance (register_names : PP.t list) : PP.t =
  let instance_line = PP.string "#[export] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :="
  in
  match register_names with
  | [] -> begin
      PP.annotate [%here] @@ PP.vertical [
        instance_line;
        PP.string "  fun x => match projT2 x with end.";
      ]
    end
  | first_register :: _ -> begin
      let to_N = PP.string "𝑹𝑬𝑮_eqdec_to_N"
      and of_N = PP.string "𝑹𝑬𝑮_eqdec_of_N"
      and existT (register_name : PP.t) : PP.t = PP.horizontal [ PP.string "existT _ "; register_name ]
      in
      let to_N_definition =
        let cases =
          List.mapi register_names ~f:(fun index register_name -> (register_name, PP.horizontal [ PP.integer index; PP.string "%N" ]))
        in
        PP.vertical [
          PP.horizontal [ PP.string "Definition "; to_N; PP.string " (x : sigT Reg) : BinNums.N :=" ];
          PP.indent @@ PP.vertical [
            PP.string "match x with";
            PP.string "| existT _ r =>";
            PP.indent @@ PP.indent @@ Coq.pp_match (PP.string "r") cases;
            PP.string "end.";
          ]
        ]
      and of_N_definition =
        PP.vertical [
          PP.horizontal [ PP.string "Definition "; of_N; PP.string " (n : BinNums.N) : sigT Reg :=" ];
          PP.indent @@ PP.vertical [
            PP.string "List.nth (BinNat.N.to_nat n)";
            PP.indent @@ PP.horizontal [ PP.string "("; pp_cons_list (List.map ~f:existT register_names); PP.string ")%list " ];
            PP.indent @@ PP.horizontal [ PP.string "("; existT first_register; PP.string ")"; Coq.eol ];
          ]
        ]
      and instance =
        let inner_match =
          let cases = List.map register_names ~f:(fun register_name -> (register_name, PP.string "eq_refl"))
          and matched =
            PP.horizontal [ PP.string "r in Reg σ return "; of_N; PP.string " ("; to_N; PP.string " (existT σ r)) = existT σ r" ]
          in
          Coq.pp_match matched cases
        in
        PP.vertical [
          instance_line;
          PP.indent @@ PP.horizontal [ PP.string "nanosail_eqdec_via_N "; to_N; PP.string " "; of_N ];
          PP.indent @@ PP.indent @@ PP.vertical [
            PP.horizontal [ PP.string "(fun x => match x return "; of_N; PP.string " ("; to_N; PP.string " x) = x with" ];
            PP.string "          | existT _ r =>";
            PP.indent @@ PP.indent @@ PP.indent @@ PP.indent @@ PP.indent @@ PP.indent @@ inner_match;
            PP.string "          end).";
          ]
        ]
      in
      PP.annotate [%here] @@ PP.paragraphs [ to_N_definition; of_N_definition; instance ]
    end


let variant_has_type_parameters (variant_definition : Ast.Definition.Type.Variant.t) : bool =
  let Ast.TypeQuantifier.TypeQuantifier items = variant_definition.type_quantifier
  in
  not @@ List.is_empty items


(*
   The EqDec instances for a union and its constructor tag enum, the latter first.
   A union with type parameters falls back on Derive EqDec (with Derive NoConfusion,
   see Katamaran.pp_no_confusion): its instance must be parameterized by EqDec instances
   for the parameters, which Derive does for us.
*)
let pp_variant_definition_instances (variant_definition : Ast.Definition.Type.Variant.t) : PP.t list GC.t =
  let tag_enum_instance =
    pp_enum_instance
      ~type_name:(Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier)
      ~values:(List.map variant_definition.constructors ~f:(fun (constructor, _) -> Identifier.pp @@ Identifier.reified_variant_constructor_name constructor))
  in
  if variant_has_type_parameters variant_definition
  then GC.return [ tag_enum_instance; Coq.pp_derive_eqdec_for (Identifier.pp variant_definition.identifier) ]
  else begin
    let* union_instance = pp_variant_instance variant_definition
    in
    GC.return [ tag_enum_instance; union_instance ]
  end
