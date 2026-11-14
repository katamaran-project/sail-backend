val truncate = pure {coq: "truncate", _: "truncate"}: forall 'n. (bits('n), int) -> bits('n)

type regidx = bits(5)

function step step_no : int = {
  fetch();
  helper(step_no)
}

function fetch() = {
  let r : regidx = 0b00000 in xlen_value
}

function helper n = n

$[complete]
function bool_to_bits x = [bool_to_bit(x)]

function execute_ADDI imm = {
  bool_to_bits(true)
}

function clause execute ADDI(imm) = execute_ADDI(imm)

function unused() = ()

function clause execute FOO(x) = execute_FOO(x)

function execute_FOO x = vector_helper(x)

function vector_helper x = x + vlen_default

let vlen_default = 4

let xlen_value = 32

register vlenb : bits(8) = vlen_init()

function vlen_init() = 0x08

/*! Converts a capability to bits; a doc comment before a definition. */
$[complete]
function documented() = undocumented_callee()

function undocumented_callee() = ()

/*!
A doc comment with a blank line in it.

The definition after it starts in the same chunk as the end of the comment.
*/
function after_split_comment() = ()
