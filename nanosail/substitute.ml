open Ast
    

let sanitizing_substitution (identifier : Ast.identifier) : Ast.identifier =
  Auxlib.drop_chars_while identifier (fun c -> c = '\'')


let substitute_identifiers_in_numeric_expression (subst : identifier -> identifier) =
  let rec aux (numeric_expression : numeric_expression) =
    match numeric_expression with
    | NE_constant _           -> numeric_expression
    | NE_add (left, right)    -> NE_add (aux left, aux right)
    | NE_minus (left, right)  -> NE_minus (aux left, aux right)
    | NE_times (left, right)  -> NE_times (aux left, aux right)
    | NE_neg operand          -> NE_neg (aux operand)
    | NE_id identifier        -> NE_id (subst identifier)
    | NE_var identifier       -> NE_var (subst identifier)
  in
  aux


let sanitize_numeric_expression =
  substitute_identifiers_in_numeric_expression sanitizing_substitution
