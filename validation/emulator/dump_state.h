/* Per-step state dump of the C emulator (validation/docs/emulator.md).
   Model side: included by the generated model (-c_include); riscv_sim.c uses dump_sim.h. */
#ifndef DUMP_STATE_H
#define DUMP_STATE_H

#include <stdbool.h>
#include <stdint.h>
#include "sail.h"
#include "rts.h"

/* Called by dump_state.sail: one item each. */
unit dump_bits(const_sail_string key, const lbits v);
unit dump_bool(const_sail_string key, const bool v);
unit dump_word(const_sail_string key, const_sail_string v);

/* The model's calls to platform_write_mem / write_tag_bool, renamed by -D
   when the model is compiled: log the written addresses, then write. */
bool dump_platform_write_mem(const int write_kind, const uint64_t addr_size,
                             const sbits addr, const mpz_t n, const lbits data);
unit dump_write_tag_bool(const fbits tag_addr, const bool tag);

#endif
