/* Per-step state dump in the canonical format (validation/docs/state-format.md).
   Run file: "<step> <line>" per line; registers sorted bytewise, then mem, then tag. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>

#include "dump_state.h"
#include "dump_sim.h"

unit zdump_state(unit); /* dump_state.sail */

static FILE *out = NULL;

static char **lines = NULL;
static size_t nlines = 0, caplines = 0;

typedef struct {
  uint64_t *a;
  size_t n, cap;
} addrs;

static addrs mem = {0}, tags = {0};

static void *xrealloc(void *p, size_t n)
{
  p = realloc(p, n);
  if (!p) {
    fprintf(stderr, "dump_state: out of memory\n");
    exit(1);
  }
  return p;
}

static void add_line(const char *key, const char *value)
{
  if (nlines == caplines) {
    caplines = caplines ? 2 * caplines : 1024;
    lines = xrealloc(lines, caplines * sizeof *lines);
  }
  size_t n = strlen(key) + 1 + strlen(value) + 1;
  char *l = xrealloc(NULL, n);
  snprintf(l, n, "%s %s", key, value);
  lines[nlines++] = l;
}

static void add_addr(addrs *s, uint64_t a)
{
  if (s->n == s->cap) {
    s->cap = s->cap ? 2 * s->cap : 1024;
    s->a = xrealloc(s->a, s->cap * sizeof *s->a);
  }
  s->a[s->n++] = a;
}

static int cmp_line(const void *x, const void *y)
{
  return strcmp(*(char *const *)x, *(char *const *)y);
}

static int cmp_addr(const void *x, const void *y)
{
  uint64_t a = *(const uint64_t *)x, b = *(const uint64_t *)y;
  return a < b ? -1 : a > b;
}

/* Sort and remove duplicates. */
static void normalise(addrs *s)
{
  if (s->n == 0)
    return;
  qsort(s->a, s->n, sizeof *s->a, cmp_addr);
  size_t k = 1;
  for (size_t i = 1; i < s->n; i++)
    if (s->a[i] != s->a[k - 1])
      s->a[k++] = s->a[i];
  s->n = k;
}

unit dump_bits(const_sail_string key, const lbits v)
{
  size_t digits = v.len == 0 ? 1 : (v.len + 3) / 4;
  char *hex = mpz_get_str(NULL, 16, *v.bits); /* lowercase */
  size_t h = strlen(hex);
  size_t pad = h < digits ? digits - h : 0;
  char *s = xrealloc(NULL, 2 + pad + h + 1);
  memcpy(s, "0x", 2);
  memset(s + 2, '0', pad);
  memcpy(s + 2 + pad, hex, h + 1);
  add_line(key, s);
  free(s);
  void (*freefunc)(void *, size_t);
  mp_get_memory_functions(NULL, NULL, &freefunc);
  freefunc(hex, h + 1);
  return UNIT;
}

unit dump_bool(const_sail_string key, const bool v)
{
  add_line(key, v ? "true" : "false");
  return UNIT;
}

unit dump_word(const_sail_string key, const_sail_string v)
{
  add_line(key, v);
  return UNIT;
}

bool dump_platform_write_mem(const int write_kind, const uint64_t addr_size,
                             const sbits addr, const mpz_t n, const lbits data)
{
  if (out) {
    uint64_t len = mpz_get_ui(n);
    for (uint64_t i = 0; i < len; i++)
      add_addr(&mem, addr.bits + i);
  }
  return platform_write_mem(write_kind, addr_size, addr, n, data);
}

unit dump_write_tag_bool(const fbits tag_addr, const bool tag)
{
  if (out)
    add_addr(&tags, tag_addr);
  return write_tag_bool(tag_addr, tag);
}

void dump_state_open(const char *path)
{
  out = fopen(path, "w");
  if (!out) {
    fprintf(stderr, "dump_state: cannot open %s\n", path);
    exit(1);
  }
}

void dump_state_step(uint64_t step, const char *result)
{
  if (!out)
    return;
  nlines = 0;
  zdump_state(UNIT);
  add_line("step_result", result); /* pseudo-item (state-format.md) */
  qsort(lines, nlines, sizeof *lines, cmp_line);
  for (size_t i = 0; i < nlines; i++) {
    fprintf(out, "%llu %s\n", (unsigned long long)step, lines[i]);
    free(lines[i]);
  }
  normalise(&mem);
  for (size_t i = 0; i < mem.n; i++)
    fprintf(out, "%llu mem 0x%08llx 0x%02llx\n", (unsigned long long)step,
            (unsigned long long)mem.a[i], (unsigned long long)read_mem(mem.a[i]));
  normalise(&tags);
  for (size_t i = 0; i < tags.n; i++)
    fprintf(out, "%llu tag 0x%08llx %s\n", (unsigned long long)step,
            (unsigned long long)(8 * tags.a[i]),
            read_tag_bool(tags.a[i]) ? "true" : "false");
  fflush(out);
}

void dump_state_close(void)
{
  if (out)
    fclose(out);
  out = NULL;
}
