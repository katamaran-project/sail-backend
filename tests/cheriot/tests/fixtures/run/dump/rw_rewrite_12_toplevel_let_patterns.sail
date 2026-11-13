val truncate = pure {coq: "truncate", _: "truncate"}: forall 'n. (bits('n), int) -> bits('n)

type regidx = bits(5)

function step step_no : int = {
  fetch();
  helper(step_no)
}

function fetch() = {
  let r : regidx = 0b00000 in ()
}

function helper n = n

$[complete]
function bool_to_bits x = [bool_to_bit(x)]

function execute_ADDI imm = {
  bool_to_bits(true)
}

function clause execute ADDI(imm) = execute_ADDI(imm)

function unused() = ()
