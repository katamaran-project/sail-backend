/* Workaround for the inventory (not a fix): identical to wV in sail-riscv/model/riscv_vext_regs.sail,
   except that the bound on VLEN needed by v[VLEN - 1 .. 0] comes from an if instead of an assert.
   remove_blocks turns `assert(c); e` into `let _ = assert(c) in e`, which loses the flow typing,
   and the following recheck_defs crashes Sail (type_error.ml:577). Passed to sail with -splice. */
function wV (r, in_v) = {
  let v = in_v;
  match r {
    0 => vr0 = v,
    1 => vr1 = v,
    2 => vr2 = v,
    3 => vr3 = v,
    4 => vr4 = v,
    5 => vr5 = v,
    6 => vr6 = v,
    7 => vr7 = v,
    8 => vr8 = v,
    9 => vr9 = v,
    10 => vr10 = v,
    11 => vr11 = v,
    12 => vr12 = v,
    13 => vr13 = v,
    14 => vr14 = v,
    15 => vr15 = v,
    16 => vr16 = v,
    17 => vr17 = v,
    18 => vr18 = v,
    19 => vr19 = v,
    20 => vr20 = v,
    21 => vr21 = v,
    22 => vr22 = v,
    23 => vr23 = v,
    24 => vr24 = v,
    25 => vr25 = v,
    26 => vr26 = v,
    27 => vr27 = v,
    28 => vr28 = v,
    29 => vr29 = v,
    30 => vr30 = v,
    31 => vr31 = v,
    _ => assert(false, "invalid vector register number")
  };

  dirty_v_context();

  let VLEN = unsigned(vlenb) * 8;
  assert(0 < VLEN & VLEN <= sizeof(vlenmax));
  if   get_config_print_reg() & 0 < VLEN & VLEN <= sizeof(vlenmax)
  then print_reg("v" ^ dec_str(r) ^ " <- " ^ BitStr(v[VLEN - 1 .. 0]));
}

/* The following vector-register helpers are dead code for CHERIoT (no V instructions are in SAIL_SRCS,
   and no other translated file calls them), and each relies on an assert for flow typing,
   which remove_blocks breaks (see above). Their bodies are replaced by internal_error for the inventory. */
function get_num_elem(LMUL_pow, SEW) = internal_error(__FILE__, __LINE__, "get_num_elem: stubbed for the sail-backend inventory")
function read_single_vreg(num_elem, SEW, vrid) = internal_error(__FILE__, __LINE__, "read_single_vreg: stubbed for the sail-backend inventory")
function write_single_vreg(num_elem, SEW, vrid, v) = internal_error(__FILE__, __LINE__, "write_single_vreg: stubbed for the sail-backend inventory")
function read_vreg(num_elem, SEW, LMUL_pow, vrid) = internal_error(__FILE__, __LINE__, "read_vreg: stubbed for the sail-backend inventory")
function read_single_element(EEW, index, vrid) = internal_error(__FILE__, __LINE__, "read_single_element: stubbed for the sail-backend inventory")
function write_vreg(num_elem, SEW, LMUL_pow, vrid, vec) = internal_error(__FILE__, __LINE__, "write_vreg: stubbed for the sail-backend inventory")
function write_single_element(EEW, index, vrid, value) = internal_error(__FILE__, __LINE__, "write_single_element: stubbed for the sail-backend inventory")
function read_vmask(num_elem, vm, vrid) = internal_error(__FILE__, __LINE__, "read_vmask: stubbed for the sail-backend inventory")
function read_vmask_carry(num_elem, vm, vrid) = internal_error(__FILE__, __LINE__, "read_vmask_carry: stubbed for the sail-backend inventory")
function write_vmask(num_elem, vrid, v) = internal_error(__FILE__, __LINE__, "write_vmask: stubbed for the sail-backend inventory")
