(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:47 Prelude *)
  From Coq Require Import Classes.EquivDec
                          Strings.String.
  From stdpp Require finite.
  From Equations Require Import Equations.
  Require Import Katamaran.Base.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:47 Prelude *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:63 Register Definitions *)
  (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:78 Regname Inductive Type *)
    Inductive RegName : Set :=
      | RegName_r1
      | RegName_r2
      | RegName_r3
      | RegName_r4
      | RegName_pc
      | RegName_cmp
    .
  (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:78 Regname Inductive Type *)
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:63 Register Definitions *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:57 Translated Type Definitions *)
  (* type word = int *)
  Definition word := Z.

  (* type address = word *)
  Definition address := word.

  (* enum regid = {regid1, regid2, regid3, regid4} *)
  Inductive regid : Set :=
    | regid1
    | regid2
    | regid3
    | regid4
  .

  (* enum Mode = {Running, Halted} *)
  Inductive Mode : Set :=
    | Running
    | Halted
  .

  (* enum Comparison = {Less, Equal, Greater} *)
  Inductive Comparison : Set :=
    | Less
    | Equal
    | Greater
  .

  (*

    struct Capability = {
      range_start : address,
      range_end : address,
      pointer : address,
      read_access : bool,
      write_access : bool,
      execute_access : bool
    }

  *)
  Record Capability : Set :=
    MkCapability
      {
        range_start    : address;
        range_end      : address;
        pointer        : address;
        read_access    : Datatypes.bool;
        write_access   : Datatypes.bool;
        execute_access : Datatypes.bool;
      }.

  (* union RegisterContents = {CapVal : Capability, SimpleVal : word} *)
  (* <<<<< nanosail/NanosailToMicrosail/Types/Variants.ml:44 Union Inductive Type for RegisterContents *)
    Inductive RegisterContents : Set :=
      | CapVal    : Capability -> RegisterContents
      | SimpleVal : word -> RegisterContents
    .
  (* >>>>> nanosail/NanosailToMicrosail/Types/Variants.ml:44 Union Inductive Type for RegisterContents *)

  (* <<<<< nanosail/NanosailToMicrosail/Types/Variants.ml:64 Constructors Inductive Type forRegisterContents *)
    Inductive RegisterContentsConstructor : Set :=
      | Kcapval
      | Ksimpleval
    .
  (* >>>>> nanosail/NanosailToMicrosail/Types/Variants.ml:64 Constructors Inductive Type forRegisterContents *)

  (*

    union Instruction = {
      Read : (regid, regid),
      Write : (regid, regid),
      Addition : (regid, regid, regid),
      Compare : (regid, regid),
      JumpIfLess : regid,
      Halt : unit
    }

  *)
  (* <<<<< nanosail/NanosailToMicrosail/Types/Variants.ml:44 Union Inductive Type for Instruction *)
    Inductive Instruction : Set :=
      | Read       : regid -> regid -> Instruction
      | Write      : regid -> regid -> Instruction
      | Addition   : regid -> regid -> regid -> Instruction
      | Compare    : regid -> regid -> Instruction
      | JumpIfLess : regid -> Instruction
      | Halt       : Instruction
    .
  (* >>>>> nanosail/NanosailToMicrosail/Types/Variants.ml:44 Union Inductive Type for Instruction *)

  (* <<<<< nanosail/NanosailToMicrosail/Types/Variants.ml:64 Constructors Inductive Type forInstruction *)
    Inductive InstructionConstructor : Set :=
      | Kread
      | Kwrite
      | Kaddition
      | Kcompare
      | Kjumpifless
      | Khalt
    .
  (* >>>>> nanosail/NanosailToMicrosail/Types/Variants.ml:64 Constructors Inductive Type forInstruction *)
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:57 Translated Type Definitions *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:69 Enum Tags *)
  Inductive Enums : Set :=
    | regname
    | Eregid
    | Emode
    | Ecomparison
  .
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:69 Enum Tags *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:81 Variant Tags *)
  Inductive Unions : Set :=
    | Uregistercontents
    | Uinstruction
  .
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:81 Variant Tags *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:75 Record Tags *)
  Inductive Records : Set :=
    | Rcapability
  .
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:75 Record Tags *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:150 No Confusion *)
  Section TransparentObligations.
    Local Set Transparent Obligations.

    Derive NoConfusion for Enums.
    Derive NoConfusion for regid.
    Derive NoConfusion for Mode.
    Derive NoConfusion for Comparison.
    Derive NoConfusion for Unions.
    Derive NoConfusion for RegisterContents.
    Derive NoConfusion for Instruction.
    Derive NoConfusion for RegisterContentsConstructor.
    Derive NoConfusion for InstructionConstructor.
    Derive NoConfusion for Records.
    Derive NoConfusion for Capability.
    Derive NoConfusion for RegName.
  End TransparentObligations.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:150 No Confusion *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:165 EqDec *)
  Derive EqDec for Enums.
  Derive EqDec for regid.
  Derive EqDec for Mode.
  Derive EqDec for Comparison.
  Derive EqDec for Unions.
  Derive EqDec for RegisterContents.
  Derive EqDec for Instruction.
  Derive EqDec for RegisterContentsConstructor.
  Derive EqDec for InstructionConstructor.
  Derive EqDec for Records.
  Derive EqDec for Capability.
  Derive EqDec for RegName.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:165 EqDec *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:127 Finite *)
  Section Finite.
    Import stdpp.finite.

    Local Obligation Tactic :=
      finite_from_eqdec.

    #[export,program] Instance regid_finite : Finite regid :=
      {| enum := [ regid1; regid2; regid3; regid4 ] |}.

    #[export,program] Instance Mode_finite : Finite Mode :=
      {| enum := [ Running; Halted ] |}.

    #[export,program] Instance Comparison_finite : Finite Comparison :=
      {| enum := [ Less; Equal; Greater ] |}.

    #[export,program] Instance RegName_finite : Finite RegName :=
      {| enum := [ RegName_r1;
                   RegName_r2;
                   RegName_r3;
                   RegName_r4;
                   RegName_pc;
                   RegName_cmp ] |}.

    #[export,program] Instance RegisterContentsConstructor_finite : Finite RegisterContentsConstructor :=
      {| enum := [ Kcapval; Ksimpleval ] |}.

    #[export,program] Instance InstructionConstructor_finite : Finite InstructionConstructor :=
      {| enum := [ Kread; Kwrite; Kaddition; Kcompare; Kjumpifless; Khalt ] |}.
  End Finite.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:127 Finite *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:87 Base Module *)
  Module Export UntitledBase <: Base.
    Import ctx.notations.
    Import ctx.resolution.
    Import env.notations.
    Import stdpp.finite.

    Local Open Scope string_scope.

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:49 Notations for Aliases *)
      Notation "ty.word" := (ty.int).
      Notation "ty.address" := (ty.word).
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:49 Notations for Aliases *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:79 typedeclkit *)
      #[export] Instance typedeclkit : TypeDeclKit :=
        {|
           enumi   := Enums;
           unioni  := Unions;
           recordi := Records;
        |}.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:79 typedeclkit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:137 Enum Denote *)
      Definition enum_denote (e : Enums) : Set :=
        match e with
        | regname     => RegName
        | Eregid      => regid
        | Emode       => Mode
        | Ecomparison => Comparison
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:137 Enum Denote *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:168 Union Denote *)
      Definition union_denote (u : Unions) : Set :=
        match u with
        | Uregistercontents => RegisterContents
        | Uinstruction      => Instruction
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:168 Union Denote *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:199 Record Denote *)
      Definition record_denote (r : Records) : Set :=
        match r with
        | Rcapability => Capability
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:199 Record Denote *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:224 typedenotekit *)
      #[export] Instance typedenotekit : TypeDenoteKit typedeclkit :=
        {|
           enumt := enum_denote;
           uniont := union_denote;
           recordt := record_denote;
        |}.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:224 typedenotekit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:255 Union Constructor *)
      Definition union_constructor (u : Unions) : Set :=
        match u with
        | Uregistercontents => RegisterContentsConstructor
        | Uinstruction      => InstructionConstructor
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:255 Union Constructor *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:343 Union Constructor Type *)
      Definition union_constructor_type (u : Unions) : union_constructor u -> Ty :=
        match u with
        | Uregistercontents => fun k => match k with
                                        | Kcapval    => ty.record Rcapability
                                        | Ksimpleval => ty.word
                                        end
        | Uinstruction      => fun k => match k with
                                        | Kread       => ty.prod (ty.enum Eregid) (ty.enum Eregid)
                                        | Kwrite      => ty.prod (ty.enum Eregid) (ty.enum Eregid)
                                        | Kaddition   => ty.tuple [ ty.enum Eregid;
                                                                    ty.enum Eregid;
                                                                    ty.enum Eregid ]
                                        | Kcompare    => ty.prod (ty.enum Eregid) (ty.enum Eregid)
                                        | Kjumpifless => ty.enum Eregid
                                        | Khalt       => ty.unit
                                        end
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:343 Union Constructor Type *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:381 EqDec/Finite Instances *)
      #[export] Instance eqdec_enum_denote E : EqDec (enum_denote E) :=
        ltac:(destruct E; auto with typeclass_instances).
      #[export] Instance finite_enum_denote E : finite.Finite (enum_denote E) :=
        ltac:(destruct E; auto with typeclass_instances).
      #[export] Instance eqdec_union_denote U : EqDec (union_denote U) :=
        ltac:(destruct U; cbn; auto with typeclass_instances).
      #[export] Instance eqdec_union_constructor U : EqDec (union_constructor U) :=
        ltac:(destruct U; cbn; auto with typeclass_instances).
      #[export] Instance finite_union_constructor U : finite.Finite (union_constructor U) :=
        ltac:(destruct U; cbn; auto with typeclass_instances).
      #[export] Instance eqdec_record_denote R : EqDec (record_denote R) :=
        ltac:(destruct R; auto with typeclass_instances).
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:381 EqDec/Finite Instances *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:497 Union Fold *)
      Definition union_fold (U : unioni) : { K & Val (union_constructor_type U K) } -> uniont U :=
        match U with
        | Uregistercontents => fun Kv => match Kv with
                                         | existT Kcapval ж1    => CapVal ж1
                                         | existT Ksimpleval ж1 => SimpleVal ж1
                                         end
        | Uinstruction      => fun Kv => match Kv with
                                         | existT Kread (ж1, ж2)              => Read ж1 ж2
                                         | existT Kwrite (ж1, ж2)             => Write ж1 ж2
                                         | existT Kaddition (tt, ж1, ж2, ж3) => Addition ж1 ж2 ж3
                                         | existT Kcompare (ж1, ж2)           => Compare ж1 ж2
                                         | existT Kjumpifless ж1               => JumpIfLess ж1
                                         | existT Khalt tt                      => Halt
                                         end
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:497 Union Fold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:585 Union Unfold *)
      Definition union_unfold (U : unioni) : uniont U -> { K & Val (union_constructor_type U K) } :=
        match U with
        | Uregistercontents => fun Kv => match Kv with
                                         | CapVal ж1    => existT Kcapval ж1
                                         | SimpleVal ж1 => existT Ksimpleval ж1
                                         end
        | Uinstruction      => fun Kv => match Kv with
                                         | Read ж1 ж2         => existT Kread (ж1, ж2)
                                         | Write ж1 ж2        => existT Kwrite (ж1, ж2)
                                         | Addition ж1 ж2 ж3 => existT Kaddition (tt, ж1, ж2, ж3)
                                         | Compare ж1 ж2      => existT Kcompare (ж1, ж2)
                                         | JumpIfLess ж1       => existT Kjumpifless ж1
                                         | Halt                 => existT Khalt tt
                                         end
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:585 Union Unfold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:635 Record Field Type *)
      Definition record_field_type (R : recordi) : NCtx string Ty :=
        match R with
        | Rcapability => [ "range_start" :: ty.address;
                           "range_end" :: ty.address;
                           "pointer" :: ty.address;
                           "read_access" :: ty.bool;
                           "write_access" :: ty.bool;
                           "execute_access" :: ty.bool ]
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:635 Record Field Type *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:709 Record Fold *)
      Definition record_fold (R : recordi) : NamedEnv Val (record_field_type R) -> recordt R :=
        match R with
        | Rcapability => fun fields => MkCapability fields.[??"range_start"] fields.[??"range_end"] fields.[??"pointer"] fields.[??"read_access"] fields.[??"write_access"] fields.[??"execute_access"]
        end%exp.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:709 Record Fold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:785 Record Unfold *)
      Definition record_unfold (R : recordi) : recordt R -> NamedEnv Val (record_field_type R) :=
        match R with
        | Rcapability => fun r => env.nil
                                    ► ("range_start" ∷ ty.address ↦ range_start r)
                                    ► ("range_end" ∷ ty.address ↦ range_end r)
                                    ► ("pointer" ∷ ty.address ↦ pointer r)
                                    ► ("read_access" ∷ ty.bool ↦ read_access r)
                                    ► ("write_access" ∷ ty.bool ↦ write_access r)
                                    ► ("execute_access" ∷ ty.bool ↦ execute_access r)
        end%env.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:785 Record Unfold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:816 Typedefkit *)
      #[export,refine] Instance typedefkit : TypeDefKit typedenotekit :=
        {| unionk           := union_constructor;
           unionk_ty        := union_constructor_type;
           recordf          := string;
           recordf_ty       := record_field_type;
           unionv_fold      := union_fold;
           unionv_unfold    := union_unfold;
           recordv_fold     := record_fold;
           recordv_unfold   := record_unfold;
        |}.
      Proof.
        - abstract (now intros [] []).
        - abstract (intros [] [[] x]; cbn in x;
                    repeat
                      match goal with
                      | x: unit     |- _ => destruct x
                      | x: prod _ _ |- _ => destruct x
                      end; auto).
        - abstract (now intros [] []).
        - abstract (intros []; now apply env.Forall_forall).
      Defined.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:816 Typedefkit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:854 Canonicals *)
      Canonical typedeclkit.
      Canonical typedenotekit.
      Canonical typedefkit.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:854 Canonicals *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:865 Varkit *)
      #[export] Instance varkit : VarKit := DefaultVarKit.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:865 Varkit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:899 RegDeclKit *)
      Section RegDeclKit.
        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:30 Reg Inductive Type *)
          Inductive Reg : Ty -> Set :=
            | r1  : Reg (ty.union Uregistercontents)
            | r2  : Reg (ty.union Uregistercontents)
            | r3  : Reg (ty.union Uregistercontents)
            | r4  : Reg (ty.union Uregistercontents)
            | pc  : Reg (ty.record Rcapability)
            | cmp : Reg (ty.enum Ecomparison)
          .
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:30 Reg Inductive Type *)

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:36 No Confusion for Reg *)
          Section TransparentObligations.
            Local Set Transparent Obligations.
            Derive Signature NoConfusion NoConfusionHom EqDec for Reg.
          End TransparentObligations.
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:36 No Confusion for Reg *)

        Definition 𝑹𝑬𝑮 : Ty -> Set := Reg.

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:101 REG_eq_dec Instance *)
          #[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=
            fun '(existT σ жx) '(existT τ жy) =>
            match жx, жy with
            | r1 , r1  => left eq_refl
            | r2 , r2  => left eq_refl
            | r3 , r3  => left eq_refl
            | r4 , r4  => left eq_refl
            | pc , pc  => left eq_refl
            | cmp, cmp => left eq_refl
            | _  , _   => right _
            end.
          Proof. all: transparent_abstract (intros H; depelim H). Defined.
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:101 REG_eq_dec Instance *)

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:142 Obligation Tactic *)
          Local Obligation Tactic :=
            finite_from_eqdec.
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:142 Obligation Tactic *)

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:126 REG_finite Instance *)
          Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=
            {| enum :=
               [ existT _ r1;
                 existT _ r2;
                 existT _ r3;
                 existT _ r4;
                 existT _ pc;
                 existT _ cmp ] |}
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:126 REG_finite Instance *).
      End RegDeclKit.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:899 RegDeclKit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:917 Memory Model *)
      Section MemoryModel.
        (* TODO *)
      End MemoryModel.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:917 Memory Model *)

    Include BaseMixin.
  End UntitledBase.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:87 Base Module *)

UNTRANSLATED DEFINITIONS

(*

  UNTRANSLATED DEFINITIONS

$[complete]
function execute_Write (r_address, r_word) = $[complete] match read_register(r_address) {
  SimpleVal(_) => Halted,
  CapVal(capability) => if and_bool(is_in_range(capability), capability.write_access) then
    $[complete] match read_register(r_word) {
      CapVal(_) => Halted,
      SimpleVal(v) => let _ : unit = write_memory(capability.pointer, v) in
      let _ : unit = next_instruction() in Running
    }
  else
    Halted
}
OCaml location: nanosail/SailToNanosail/Translate/Function.ml line 649
Sail location: model.sail line 106 chars 14-15
No message

$[complete]
function execute_Read (r_address, r_target) = $[complete] match read_register(r_address) {
  CapVal(capability) => if and_bool(is_in_range(capability), capability.read_access) then
    let value = read_memory(capability.pointer) in
    let _ : unit = write_register(r_target, SimpleVal(value)) in
    let _ : unit = next_instruction() in Running
  else
    Halted,
  SimpleVal(_) => Halted
}
OCaml location: nanosail/SailToNanosail/Translate/Function.ml line 649
Sail location: model.sail line 100 chars 14-15
No message

$[complete]
function execute_JumpIfLess r_target = $[complete] match read_register(r_target) {
  SimpleVal(_) => Halted,
  CapVal(capability) => let _ : unit =
    $[complete] match cmp {
      Less => pc = capability,
      _ => next_instruction()
    }
  in
    Running
}
OCaml location: nanosail/SailToNanosail/Translate/Function.ml line 649
Sail location: model.sail line 162 chars 14-15
No message

$[complete]
function execute_Compare (x, y) = $[complete] match read_register(x) {
  CapVal(_) => Halted,
  SimpleVal(v1) => $[complete] match read_register(y) {
    CapVal(_) => Halted,
    SimpleVal(v2) => let _ : unit =
      if lt_int(v1, v2) then cmp = Less
      else if gt_int(v1, v2) then cmp = Greater
      else cmp = Equal
    in
    let _ : unit = next_instruction() in Running
  }
}
OCaml location: nanosail/SailToNanosail/Translate/Function.ml line 649
Sail location: model.sail line 142 chars 11-12
No message

$[complete]
function execute_Addition (source1, source2, target) = $[complete] match read_register(source1) {
  CapVal(_) => Halted,
  SimpleVal(v1) => $[complete] match read_register(source2) {
    CapVal(_) => Halted,
    SimpleVal(v2) => let sum = add_atom(v1, v2) in
    let _ : unit = write_register(target, SimpleVal(sum)) in
    let _ : unit = next_instruction() in Running
  }
}
OCaml location: nanosail/SailToNanosail/Translate/Function.ml line 649
Sail location: model.sail line 125 chars 11-12
No message

$[complete]
function regval_of_Capability v = Regval_Capability(v)
OCaml location: nanosail/SailToNanosail/Translate/Nanotype.ml line 53
Sail location: UnknownLocation
Message: Unknown type register_value

$[complete]
function RegisterContents_of_regval merge#var = $[complete] match merge#var {
  Regval_RegisterContents(v) => Some(v),
  _ => None()
}
OCaml location: nanosail/SailToNanosail/Translate/Nanotype.ml line 53
Sail location:  line 1 chars 33-47
Message: Unknown type register_value

$[complete]
function regval_of_RegisterContents v = Regval_RegisterContents(v)
OCaml location: nanosail/SailToNanosail/Translate/Nanotype.ml line 53
Sail location: UnknownLocation
Message: Unknown type register_value

*)
