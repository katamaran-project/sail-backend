default Order dec

$include <prelude.sail>

/*
  Patterns nested inside the fields of a constructor.
  The cases for such a constructor are merged into one case that matches its fields.
*/
enum Kind = { KData, KCap }

union Exception = {
  E_Align : unit,
  E_Ext   : int,
}

/* As TR_Failure in CHERIoT: a constructor with several fields, the first a union. */
union Result = {
  Ok      : int,
  Failure : (Exception, bool),
}

val classify : Result -> int
function classify(r) = match r {
  Failure(E_Ext(_), _) => 1,
  Failure(e, _)        => 2,
  Ok(n)                => n,
}

/* An enum nested in a unary constructor, and a catch-all case after it. */
union Access = {
  Read  : Kind,
  Write : Kind,
}

val is_data_read : Access -> bool
function is_data_read(a) = match a {
  Read(KData) => true,
  _           => false,
}

/* The catch-all binds the whole value. */
val read_kind : Access -> Kind
function read_kind(a) = match a {
  Read(KCap) => KCap,
  x          => match x { Read(k) => k, Write(k) => k },
}

/* C(e) next to a nested case of the same constructor. */
val first_of_failure : Result -> bool
function first_of_failure(r) = match r {
  Failure(E_Align(), b) => b,
  Failure(p)            => { let (_, b) = p in not_bool(b) },
  Ok(_)                 => false,
}
