(* Canonical text dump of a CHERIoT harness state and deltas.
   Format: validation/docs/state-format.md. *)

From Stdlib Require Import
     Strings.Ascii
     Strings.String
     Lists.List
     ZArith.ZArith.
From Katamaran Require Import
     Base
     Bitvector
     Program.
From CheriotModel Require Import
     base
     machine.
From Validation Require Import
     Interp
     Harness
     Names.



Import ListNotations.
Import UntitledBase.
Import ModelProgram.
Import Harness.I.

Set Default Proof Mode "Classic".
Open Scope string_scope.
Open Scope Z_scope.

(*** Names, from the inductive types (Names.v) ***)

Definition reg_name_dep : forall σ (r : 𝑹𝑬𝑮 σ), named r.
Proof. intros σ r; destruct r; ltac2:(name_goal ()). Defined.

Definition reg_name (r : sigT 𝑹𝑬𝑮) : string := let (σ, x) := r in reg_name_dep σ x.

Definition enum_name_dep : forall E (v : enumt E), named v.
Proof. intros E v; destruct E; destruct v; ltac2:(name_goal ()). Defined.

Definition enum_name (E : enumi) (v : enumt E) : string := enum_name_dep E v.

(* Union values: the name of the value's constructor (Sail's), not of its
   [unionk] tag. *)
Definition union_name_dep : forall U (v : uniont U), named v.
Proof. intros U v; destruct U; destruct v; ltac2:(name_goal ()). Defined.

Definition union_name (U : unioni) (v : uniont U) : string := union_name_dep U v.

(* All registers, in declaration order; the generated Finite instance is
   local to its section, hence explicit. *)
Definition all_regs : list (sigT 𝑹𝑬𝑮) := @stdpp.finite.enum _ _ 𝑹𝑬𝑮_finite.

(*** Numbers ***)

Definition digit (d : Z) : ascii :=
  ascii_of_nat (Z.to_nat (if d <? 10 then 48 + d else 87 + d)).

Fixpoint hex_aux (k : nat) (z : Z) (acc : string) : string :=
  match k with
  | O => acc
  | S k' => hex_aux k' (Z.shiftr z 4) (String (digit (Z.land z 15)) acc)
  end.

(* "0x" and exactly [k] lowercase digits (z >= 0, z < 16^k). *)
Definition hex (k : nat) (z : Z) : string := "0x" ++ hex_aux k z "".

(* Digits of a bv n: ceil(n / 4), at least 1. *)
Definition hex_width (n : nat) : nat := Nat.max 1 (Nat.div (n + 3) 4).

(* Addresses: at least 8 digits. *)
Definition hex_addr (a : Z) : string :=
  hex (Nat.max 8 (Nat.div (Z.to_nat (Z.log2 a) + 4) 4)) a.

Fixpoint dec_aux (k : nat) (z : Z) (acc : string) : string :=
  match k with
  | O => acc
  | S k' =>
      let acc' := String (digit (z mod 10)) acc in
      if z <? 10 then acc' else dec_aux k' (z / 10) acc'
  end.

Definition dec (z : Z) : string :=
  let s := dec_aux (S (Z.to_nat (Z.log2 (Z.abs z)))) (Z.abs z) "" in
  if z <? 0 then "-" ++ s else s.

Definition dec_nat (n : nat) : string := dec (Z.of_nat n).

(*** Values ***)

(* One line of a dump: key and value. *)
Definition Item : Type := string * string.

Definition line (i : Item) : string := fst i ++ " " ++ snd i.

(* Leaves: [k v].  Records: [k.field ...].  Unions and sums: [k Ctor], then the
   payload under [k.Ctor] (none for unit).  Products, tuples, lists: [k.0], [k.1], ...
   (lists also [k.len]).  [fuel] bounds the nesting depth. *)
Fixpoint render (fuel : nat) (σ : Ty) (k : string) {struct fuel} : Val σ -> list Item :=
  match fuel with
  | O => fun _ => [(k, "<depth>")]
  | S fuel' =>
      let payload (τ : Ty) (k' : string) : Val τ -> list Item :=
        match τ return Val τ -> list Item with
        | ty.unit => fun _ => []
        | τ => render fuel' τ k'
        end in
      match σ return Val σ -> list Item with
      | ty.int => fun v => [(k, dec v)]
      | ty.bool => fun v => [(k, if v then "true" else "false")]
      | ty.string => fun v => [(k, """" ++ v ++ """")]
      | ty.unit => fun _ => [(k, "()")]
      | ty.enum E => fun v => [(k, enum_name E v)]
      | ty.bvec n => fun v => [(k, hex (hex_width n) (bv.unsigned v))]
      | ty.list τ => fun v =>
          (k ++ ".len", dec_nat (length v))
            :: concat (map (fun '(i, x) => render fuel' τ (k ++ "." ++ dec_nat i) x)
                         (combine (seq 0 (length v)) v))
      | ty.prod τ1 τ2 => fun '(a, b) => app (render fuel' τ1 (k ++ ".0") a) (render fuel' τ2 (k ++ ".1") b)
      | ty.sum τ1 τ2 => fun v =>
          match v with
          | inl a => (k, "inl") :: payload τ1 (k ++ ".inl") a
          | inr b => (k, "inr") :: payload τ2 (k ++ ".inr") b
          end
      | ty.tuple σs => fun v =>
          (fix tup (σs : Ctx Ty) : EnvRec Val σs -> list Item :=
             match σs return EnvRec Val σs -> list Item with
             | ctx.nil => fun _ => []
             | ctx.snoc σs' τ => fun '(vs, x) =>
                 app (tup σs' vs) (render fuel' τ (k ++ "." ++ dec_nat (ctx.length σs')) x)
             end) σs v
      | ty.union U => fun v =>
          let c := union_name U v in
          match unionv_unfold U v with
          | existT K p => (k, c) :: payload (unionk_ty U K) (k ++ "." ++ c) p
          end
      | ty.record R => fun v =>
          (fix fields {Δ} (e : NamedEnv Val Δ) : list Item :=
             match e with
             | env.nil => []
             | @env.snoc _ _ _ e' b x => app (fields e') (render fuel' (type b) (k ++ "." ++ name b) x)
             end) _ (recordv_unfold R v)
      end
  end.

Definition render_depth : nat := 16.

(*** Registers ***)

(* Registers left out of the dump, with the reason (state-format.md, "Coverage").  Everything else in [all_regs] is dumped. *)
Definition excluded : list (string * string) :=
  map (fun i => ("vr" ++ dec_nat i, "vector register: vector extension out of scope; 65536 bits"))
      (seq 0 32).

Definition excluded_names : list string := map fst excluded.

(*** Memory items ***)

Fixpoint insert_Z (x : Z) (l : list Z) : list Z :=
  match l with
  | [] => [x]
  | y :: l' => if x <? y then x :: l else if x =? y then l else y :: insert_Z x l'
  end.

(* Sorted, without duplicates. *)
Definition sort_Z (l : list Z) : list Z := fold_right insert_Z [] l.

(* mem <address> <byte>, by address; tag <granule address> <bool>, by address,
   from the oracle's write log (Harness.log_keys).  Tag keys are
   [-1 - tag address]; the granule address is 8 * tag address. *)
Definition mem_items (m : Memory) : list Item * list Item :=
  let ks := sort_Z (log_keys m) in
  let bytes := filter (fun k => 0 <=? k) ks in
  let tags := sort_Z (map (fun k => -1 - k) (filter (fun k => k <? 0) ks)) in
  (map (fun a => ("mem " ++ hex_addr a, hex 2 (m a))) bytes,
   map (fun t => ("tag " ++ hex_addr (8 * t), if read_tag m t then "true" else "false")) tags).

(*** States ***)

(* Bytewise comparison of keys, as String.compare, but on the bits of each
   character (String.compare converts each to N first: slower in vm_compute). *)
Definition bit_compare (x y : bool) (k : comparison) : comparison :=
  match x, y with
  | false, true => Lt
  | true, false => Gt
  | _, _ => k
  end.

Definition char_compare (a b : ascii) : comparison :=
  let '(Ascii a0 a1 a2 a3 a4 a5 a6 a7) := a in
  let '(Ascii b0 b1 b2 b3 b4 b5 b6 b7) := b in
  bit_compare a7 b7 (bit_compare a6 b6 (bit_compare a5 b5 (bit_compare a4 b4
    (bit_compare a3 b3 (bit_compare a2 b2 (bit_compare a1 b1 (bit_compare a0 b0 Eq))))))).

Fixpoint key_compare (s t : string) : comparison :=
  match s, t with
  | EmptyString, EmptyString => Eq
  | EmptyString, _ => Lt
  | _, EmptyString => Gt
  | String a s', String b t' =>
      match char_compare a b with
      | Eq => key_compare s' t'
      | c => c
      end
  end.

Definition key_leb (s t : string) : bool :=
  match key_compare s t with Gt => false | _ => true end.

(* Merge sort of items by key (bytewise). *)
Fixpoint merge_items (l1 : list Item) : list Item -> list Item :=
  fix merge_r (l2 : list Item) : list Item :=
    match l1, l2 with
    | [], _ => l2
    | _, [] => l1
    | x :: l1', y :: l2' =>
        if key_leb (fst x) (fst y) then x :: merge_items l1' l2 else y :: merge_r l2'
    end.

Fixpoint sort_pass (l : list (list Item)) : list (list Item) :=
  match l with
  | a :: b :: rest => merge_items a b :: sort_pass rest
  | _ => l
  end.

Fixpoint sort_loop (fuel : nat) (l : list (list Item)) : list Item :=
  match l with
  | [] => []
  | [a] => a
  | _ => match fuel with O => concat l | S f => sort_loop f (sort_pass l) end
  end.

Definition sort_items (l : list Item) : list Item :=
  sort_loop (length l) (map (fun i => [i]) l).

(* A dumped state: three sections, each sorted by key.  [extra]: pseudo-items
   sorted in with the registers (step_result). *)
Record Dumped : Type := mkDumped { d_regs : list Item; d_mem : list Item; d_tags : list Item }.

(* The dumped registers, sorted by name; computed once, when this file is
   compiled.  A register's keys are its name or start with its name and ".",
   and names are made of [A-Za-z0-9_], all after "." in ASCII: so each
   register's items sorted, in this order, are all items sorted. *)
Definition dumped_regs : list (sigT 𝑹𝑬𝑮) :=
  Eval vm_compute in
  let ex := excluded_names in
  let incl := filter (fun r => negb (existsb (String.eqb (reg_name r)) ex)) all_regs in
  flat_map (fun '(n, _) => filter (fun r => String.eqb (reg_name r) n) incl)
           (sort_items (map (fun r => (reg_name r, "")) incl)).

(* Register items, sorted by key. *)
Definition reg_items (γ : RegStore) : list Item :=
  flat_map (fun '(existT σ x) =>
              sort_items (render render_depth σ (reg_name (existT σ x)) (read_register γ x)))
           dumped_regs.

Definition dumped_with (extra : list Item) (st : State) : Dumped :=
  let '(ms, ts) := mem_items (st_mem st) in
  mkDumped (merge_items (sort_items extra) (reg_items (st_regs st))) ms ts.

Definition dumped (st : State) : Dumped := dumped_with [] st.

Definition items (d : Dumped) : list Item := app (d_regs d) (app (d_mem d) (d_tags d)).

(* Registers, then memory, then tags. *)
Definition dump (st : State) : list string := map line (items (dumped st)).

(*** Step result ***)

(* [step_result]: the value of the call that produced the state: [step]'s
   bool, or [init_model]'s () at step 0; a failure (stm_fail) as its message,
   a string (double quotes replaced by single ones). *)
Definition unquote (s : string) : string :=
  string_of_list_ascii
    (map (fun c => if Ascii.eqb c (ascii_of_nat 34) then ascii_of_nat 39 else c)
         (list_ascii_of_string s)).

Definition result_items {τ} (r : Result τ) : list Item :=
  match r with
  | inl msg => [("step_result", String (ascii_of_nat 34) (unquote msg ++ String (ascii_of_nat 34) ""))]
  | inr v => render render_depth τ "step_result" v
  end.

Definition dumped_step {τ} (r : Result τ) (st : State) : Dumped := dumped_with (result_items r) st.

Definition dump_step {τ} (r : Result τ) (st : State) : list string :=
  map line (items (dumped_step r st)).

(* Changes from [before] to [after], both sorted by [cmp]: [(k, Some v)] for
   an item whose key is new or whose value changed, [(k, None)] for a key that
   is gone (e.g. a union's payload after its constructor changed). *)
Fixpoint changes (cmp : string -> string -> comparison) (before : list Item) :
  list Item -> list (string * option string) :=
  fix changes_r (after : list Item) : list (string * option string) :=
    match before, after with
    | [], _ => map (fun '(k, v) => (k, Some v)) after
    | (kb, _) :: before', [] => (kb, None) :: changes cmp before' []
    | (kb, vb) :: before', (ka, va) :: after' =>
        match cmp kb ka with
        | Lt => (kb, None) :: changes cmp before' after
        | Gt => (ka, Some va) :: changes_r after'
        | Eq => if String.eqb vb va then changes cmp before' after'
                else (ka, Some va) :: changes cmp before' after'
        end
    end.

(* Memory and tag keys are sorted by address: shorter hex first. *)
Definition addr_compare (a b : string) : comparison :=
  match Nat.compare (String.length a) (String.length b) with
  | Eq => key_compare a b
  | c => c
  end.

Definition changes_dumped (b a : Dumped) : list (string * option string) :=
  app (changes key_compare (d_regs b) (d_regs a))
      (app (changes addr_compare (d_mem b) (d_mem a)) (changes addr_compare (d_tags b) (d_tags a))).

(* Items of [a] whose key is new or whose value changed. *)
Definition delta_dumped (b a : Dumped) : list Item :=
  flat_map (fun '(k, o) => match o with Some v => [(k, v)] | None => [] end) (changes_dumped b a).

Definition delta (before after : State) : list string :=
  map line (delta_dumped (dumped before) (dumped after)).

Definition delta_of (before after : Dumped) : list string := map line (delta_dumped before after).

(* The dumps of a run: reset result and state, then each step's. *)
Definition run_dumped (r0 : Result ty.unit) (st0 : State) (rs : list (Result ty.bool * State)) :
  list Dumped :=
  dumped_step r0 st0 :: map (fun '(r, st) => dumped_step r st) rs.

(* A run file: state i (0 = after reset) prefixed with "i ". *)
Definition dump_run (ds : list Dumped) : list string :=
  concat (map (fun '(i, d) => map (fun l => dec_nat i ++ " " ++ line l) (items d))
              (combine (seq 0 (length ds)) ds)).

(* A delta run file (state-format.md): a line "i" opens state i; state 0
   in full, every later state as its changes from the one before
   ("i key value", or "i -key" for a key that is gone). *)
Definition change_line (p : string) (c : string * option string) : string :=
  match c with
  | (k, Some v) => p ++ k ++ " " ++ v
  | (k, None) => p ++ "-" ++ k
  end.

Fixpoint delta_run_aux (i : nat) (prev : Dumped) (ds : list Dumped) : list string :=
  match ds with
  | [] => []
  | d :: ds' =>
      let n := dec_nat i in
      n :: map (change_line (n ++ " ")) (changes_dumped prev d) ++ delta_run_aux (S i) d ds'
  end.

Definition delta_run (ds : list Dumped) : list string :=
  match ds with
  | [] => []
  | d0 :: ds' => "0" :: map (fun l => "0 " ++ line l) (items d0) ++ delta_run_aux 1 d0 ds'
  end.

(* The excluded registers, one per line: name and reason. *)
Definition excluded_lines : list string := map (fun '(n, why) => n ++ " " ++ why) excluded.

(*** Start values not fixed by reset ***)

(* Another start store: every bitvector inverted, every bool negated, every int
   + 1, every enum the next constructor.  A register item that differs after
   reset from [reg_default] and from [reg_alt] depends on the start value. *)
Definition next_enum (E : enumi) (v : enumt E) : enumt E :=
  match stdpp.finite.enum (enumt E) with
  | [] => v
  | x0 :: _ =>
      (fix go (l : list (enumt E)) : enumt E :=
         match l with
         | [] => x0
         | x :: rest => if Equations.Prop.Classes.eq_dec x v then hd x0 rest else go rest
         end) (stdpp.finite.enum (enumt E))
  end.

Fixpoint perturb (fuel : nat) (σ : Ty) {struct fuel} : Val σ -> Val σ :=
  match fuel with
  | O => fun v => v
  | S fuel' =>
      match σ return Val σ -> Val σ with
      | ty.bvec n => fun v => bv.not v
      | ty.bool => negb
      | ty.int => fun v => v + 1
      | ty.enum E => next_enum E
      | ty.record R => fun v =>
          recordv_fold R (env.map (fun b x => perturb fuel' (type b) x) (recordv_unfold R v))
      | _ => fun v => v
      end
  end.

Definition reg_alt : RegStore := fun σ r => perturb render_depth σ (reg_default σ r).

(* Keys whose value after reset differs between the two start stores. *)
Definition start_dependent (fuel : nat) (m : Memory) : Outcome (list string) :=
  obind (reset_from fuel (mkState reg_default m)) (fun '(_, st1) =>
  omap (fun '(_, st2) => map fst (delta_dumped (dumped st1) (dumped st2)))
       (reset_from fuel (mkState reg_alt m))).

(*** Runs ***)

(* Reset, then up to [n] steps (Harness.run_steps); the dumps of every state,
   with step_result, and why the run stopped.  A failed reset: its state only. *)
Definition run_dumps (fuel n : nat) (m : Memory) : Outcome (list Dumped * Stop) :=
  obind (reset fuel m) (fun '(r0, st0) =>
    match r0 with
    | inl _ => Done (run_dumped r0 st0 [], Failed)
    | inr _ => omap (fun '(rs, s) => (run_dumped r0 st0 rs, s)) (run_steps fuel n st0)
    end).

Definition stop_name (s : Stop) : string :=
  match s with Halted => "Halted" | Failed => "Failed" | StepLimit => "StepLimit" end.
