(* Constructor names as Gallina strings, and raw printing of string lists.
   Generic: no dependency on a model.  See validation/docs/state-format.md.

   Name tables are built by a tactic from the inductive types themselves:

     Definition reg_name_dep : forall σ (r : 𝑹𝑬𝑮 σ), named r.
     Proof. intros σ r; destruct r; ltac2:(name_goal ()). Defined.

   gives a [match] with one string literal per constructor, so a new
   constructor gets its name without anyone editing a list. *)

From Stdlib Require Import
     Strings.Ascii
     Strings.String
     Lists.List.
From Ltac2 Require Import Ltac2.

(* The goal [named a]: the name of [a]'s head constructor.  Reduces to [string]. *)
Definition named {A : Type} (a : A) : Set := string.

(* Ltac2 string -> Gallina string literal (bytes; ASCII names). *)
Ltac2 gallina_of_string (s : string) : constr :=
  let tt := 'true in
  let ff := 'false in
  let c_ascii := 'Ascii.Ascii in
  let c_string := 'String.String in
  let c_empty := 'String.EmptyString in
  let bit n i := if Int.equal (Int.land (Int.lsr n i) 1) 1 then tt else ff in
  let ascii ch :=
    let n := Char.to_int ch in
    Constr.Unsafe.make (Constr.Unsafe.App c_ascii
      (Array.of_list [bit n 0; bit n 1; bit n 2; bit n 3; bit n 4; bit n 5; bit n 6; bit n 7])) in
  let len := String.length s in
  let rec go i :=
    if Int.equal i len then c_empty
    else Constr.Unsafe.make (Constr.Unsafe.App c_string (Array.of_list [ascii (String.get s i); go (Int.add i 1)])) in
  go 0.

(* Gallina string literal (closed, normal form) -> Ltac2 string. *)
Ltac2 string_of_gallina (c : constr) : string :=
  let tt := 'true in
  let rec bits (args : constr list) (i : int) :=
    match args with
    | [] => 0
    | b :: rest =>
        let v := if Constr.equal b tt then Int.lsl 1 i else 0 in
        Int.lor v (bits rest (Int.add i 1))
    end in
  let rec chars c :=
    match Constr.Unsafe.kind c with
    | Constr.Unsafe.App _ args =>
        (* String a s, a = Ascii b0 .. b7 *)
        let a := Array.get args 0 in
        let s := Array.get args 1 in
        match Constr.Unsafe.kind a with
        | Constr.Unsafe.App _ bs => Char.of_int (bits (Array.to_list bs) 0) :: chars s
        | _ => Control.throw (Invalid_argument (Some (Message.of_constr a)))
        end
    | _ => []                                   (* EmptyString *)
    end in
  let cs := chars c in
  let out := String.make (List.length cs) (Char.of_int 32) in
  List.iteri (fun i ch => String.set out i ch) cs;
  out.

(* Head constructor of a term, through applications. *)
Ltac2 rec head_constructor (c : constr) : constructor option :=
  match Constr.Unsafe.kind c with
  | Constr.Unsafe.Constructor cstr _ => Some cstr
  | Constr.Unsafe.App f _ => head_constructor f
  | _ => None
  end.

(* Solves [named a] with the name of [a]'s head constructor. *)
Ltac2 name_goal () := Control.enter (fun () =>
  lazy_match! goal with
  | [ |- named ?a ] =>
      match head_constructor a with
      | Some cstr =>
          let id := List.last (Env.path (Std.ConstructRef cstr)) in
          let s := gallina_of_string (Ident.to_string id) in
          exact $s
      | None => Control.throw (Invalid_argument (Some (Message.of_constr a)))
      end
  end).

(* Prints each element of a closed [list string] on its own line, unquoted.
   The list is computed with vm_compute first. *)
Ltac2 print_lines (c : constr) : unit :=
  let v := Std.eval_vm None c in
  let rec go l :=
    match Constr.Unsafe.kind l with
    | Constr.Unsafe.App _ args =>
        if Int.equal (Array.length args) 3 then
          (Message.print (Message.of_string (string_of_gallina (Array.get args 1)));
           go (Array.get args 2))
        else ()                                 (* nil *)
    | _ => ()
    end in
  go v.
