open Base
open PP
open Ast
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let pp_infix_binOp (binary_operator : binary_operator) =
  match binary_operator with
  | Plus   -> AC.return @@ plus
  | Times  -> AC.return @@ star
  | Minus  -> AC.return @@ minus
  | And    -> AC.return @@ twice ampersand
  | Or     -> AC.return @@ twice bar
  | Eq     -> AC.return @@ equals
  | Neq    -> AC.return @@ bang ^^ equals
  | Le     -> AC.return @@ langle ^^ equals
  | Lt     -> AC.return @@ langle
  | Ge     -> AC.return @@ rangle ^^ equals
  | Gt     -> AC.return @@ rangle
  | Pair   -> AC.not_yet_implemented [%here] (* Should not occur *) (* use fail *)
  | Cons   -> AC.not_yet_implemented [%here] (* Should not occur *)
  | Append -> AC.not_yet_implemented [%here] (* Should not occur *)


let ty_of_val =
  let rec aux = function
    | Val_unit          -> Ty_unit
    | Val_bool _        -> Ty_bool
    | Val_int _         -> Ty_int
    | Val_string _      -> Ty_string
    | Val_prod (v1, v2) -> Ty_tuple [aux v1; aux v2]
  in
  aux


let pp_value =
  let rec aux = function
    | Val_unit          -> string "tt"
    | Val_bool b        -> string (Bool.to_string b)
    | Val_int i         -> Coq.integer i
    | Val_string s      -> dquotes (string s)
    | Val_prod (v1, v2) -> Coq.product (aux v1) (aux v2)
  in
  aux


let rec pp_expression e =
  let rec pp_exp_list = function
    | []      -> AC.return @@ string "nil"
    | x :: xs ->
       let* x'  = pp_par_expression x
       and* xs' = pp_exp_list xs
       in
       AC.return @@ parens @@ simple_app [string "cons"; x'; xs']
  in
  let pp_exp_val = function
    | Val_bool true        -> AC.return @@ string "exp_true"
    | Val_bool false       -> AC.return @@ string "exp_false"
    | Val_int n            -> AC.return @@ simple_app [string "exp_int"; Coq.integer n]
    | Val_string s         -> AC.return @@ simple_app [string "exp_string"; dquotes (string s)]
    | Val_unit             -> AC.return @@ simple_app [string "exp_val"; string "ty.unit"; string "tt"]
    | Val_prod (_, _) as v -> begin
        let* tuple_type' = Nanotype.pp_nanotype (ty_of_val v)
        in
        let value' = pp_value v
        in
        AC.return @@ simple_app [
          string "exp_val";
          tuple_type';
          value'
        ]
      end
  in
  let pp_exp_binop bo e1 e2 =
    let* e1' = pp_par_expression e1
    and* e2' = pp_par_expression e2
    in
    match bo with
    | Pair ->
       AC.return @@ simple_app [
         string "exp_binop";
         string "bop.pair";
         e1';
         e2'
       ]
    | Cons ->
       AC.return @@ simple_app [
         string "exp_binop";
         string "bop.cons";
         e1';
         e2'
       ]
    | Append ->
       AC.return @@ simple_app [
         string "exp_binop";
         string "bop.append";
         e1';
         e2'
       ]
    | _  ->
      let* binop' = pp_infix_binOp bo
      in
      AC.return @@ infix 2 1 binop' e1' e2'
  in
  match e with
  | Exp_var v              -> AC.return @@ simple_app [string "exp_var"; dquotes (string v)]
  | Exp_val v              -> pp_exp_val v
  | Exp_neg e              -> let* e' = pp_par_expression e in AC.return @@ string "- " ^^ e'
  | Exp_not e              -> let* e' = pp_par_expression e in AC.return @@ simple_app [string "exp_not"; e']
  | Exp_binop (bo, e1, e2) -> pp_exp_binop bo e1 e2
  | Exp_list lst           -> begin
      let* lst' =
        if
          Configuration.(get use_list_notations)
        then
          let* expressions = AC.map ~f:pp_expression lst
          in
          AC.return @@ Coq.list expressions
        else
          pp_exp_list lst
      in
      AC.return @@ simple_app [string "exp_list"; lst']
    end
  | Exp_record { type_identifier; variable_identifiers } -> begin
      AC.return @@ simple_app [
                       string "exp_record";
                       string type_identifier;
                       Coq.list @@ List.map
                                     variable_identifiers
                                     ~f:(fun id -> simple_app [ string "exp_var"; string id ])
                     ]
    end
  | Exp_enum identifier -> AC.return @@ string identifier

and pp_par_expression e =
  let* e' = pp_expression e
  in
  AC.return @@ parens e'
