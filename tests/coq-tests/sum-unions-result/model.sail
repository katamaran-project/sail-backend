default Order dec

$include <prelude.sail>
$include <result.sail>


val safe_div : (int, int) -> result(int, bool)
function safe_div(x, y) =
  if y == 0
  then Err(false)
  else Ok(x)

val result_to_int : result(int, bool) -> int
function result_to_int(r) = match r {
  Ok(n)  => n,
  Err(b) => if b then 1 else 0
}
