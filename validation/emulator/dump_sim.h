/* Per-step state dump: the calls in the patched riscv_sim.c (validation/docs/validate.md). */
#ifndef DUMP_SIM_H
#define DUMP_SIM_H

#include <stdint.h>

void dump_state_open(const char *path);
/* State after step STEP (0: after reset); RESULT: the step_result value. */
void dump_state_step(uint64_t step, const char *result);
void dump_state_close(void);

#endif
