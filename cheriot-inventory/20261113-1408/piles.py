#!/usr/bin/env python3
"""
Sort the extracted items into piles (root cause / missing feature) and print the pile table.

Usage: piles.py [run] [items-reachable.csv]      (default run: full-workaround)
Writes piles-<run>.csv (one row per item, with its pile) and prints, per pile:
  items (sites), distinct definitions, definitions reachable from step/execute, examples.

The rules are ordered; the first match wins. They key on the nanosail location (the match
arm that raised the NYI) and the message, as described in INVENTORY.md.
"""
import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

here = Path(__file__).resolve().parent
run = sys.argv[1] if len(sys.argv) > 1 else 'full-workaround'
items_path = Path(sys.argv[2]) if len(sys.argv) > 2 else here / 'items-reachable.csv'

SIZE_POLYMORPHIC_WARNING = 'Encountered polymorphic function'

RULES = [
    # (pile id, predicate on row)
    ('P01 compile-time-known integer arguments',
     lambda r: r['stage'] == 'hard-failure'
     or 'known at compile time' in r['message']
     or 'expected constant in bitvector type' in r['message']
     or 'failed to determine bitvector size' in r['message']),
    ('P02 polymorphic unions (type parameters)',
     lambda r: r['construct'] in ('Kind.ml: Type', 'Type.ml: TypeVariable')
     or ('expected pattern AP_' in r['message'] and ('Type:Application' in r['message'] or 'Alias["TODO"]' in r['message']))),
    ('P03 nested patterns inside sum constructors',
     lambda r: 'only binder patterns supported in sum constructors' in r['message']),
    ('P04 vector literal with bits(1) elements',
     lambda r: 'Elements of bit vector should all be bits' in r['message']),
    ('P05 loops, try, early return',
     lambda r: r['construct'] in ('Function.ml: AE_for', 'Function.ml: AE_loop', 'Function.ml: AE_try', 'Function.ml: AE_return')),
    ('P06 mutable local variables (var)',
     lambda r: 'assignment to local variable (nonregister)' in r['message']),
    ('P07 register references (register(T), ref, r->F() = v)',
     lambda r: 'unknown type register' in r['message'] or r['construct'] == 'Function.ml: AV_ref'),
    ('P08 top-level let with a computed value',
     lambda r: r['construct'].startswith('ValueDefinition.ml')),
    ('P09 mappings to/from strings and bitvectors (assembly, printing)',
     lambda r: r['construct'] in ('Match.ml: String', 'Match.ml: Bitvector')),
    ('P10 vectors of non-bits (register arrays)',
     lambda r: r['construct'] == 'Type.ml: Vector'),
    ('P11 numeric types: ranges, if in nexp, parametric structs',
     lambda r: r['construct'] in ('Recursive.ml: Nexp_if', 'Type.ml: Range', 'Match.ml: Range', 'Function.ml: Typ_app')
     or r['message'] == 'Unexpected type'),
    ('P12 non-translated definition kinds: overload, pragma',
     lambda r: r['construct'] in ('Translation.ml: DEF_overload', 'Translation.ml: DEF_pragma')),
    ('P13 size-polymorphic functions (int(n), bits(n) in signatures)',
     lambda r: r['stage'] == 'warning' and SIZE_POLYMORPHIC_WARNING in r['message']),
    ('P14 skipped by configuration (hidden gaps)',
     lambda r: r['stage'] == 'skipped'),
    ('P15 casts ignored', lambda r: r['message'] == 'Ignored cast'),
]


def pile_of(row):
    for pile, predicate in RULES:
        if predicate(row):
            return pile
    return ''


def main():
    rows = [r for r in csv.DictReader(open(items_path)) if r['run'] == run]
    for r in rows:
        r['pile'] = pile_of(r)
    out = here / f'piles-{run}.csv'
    with open(out, 'w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)
    failing_stages = {'untranslated', 'nyi', 'hard-failure', 'error', 'skipped', 'warning'}
    groups = defaultdict(lambda: [0, set(), set(), defaultdict(int)])
    unassigned = defaultdict(int)
    for r in rows:
        if r['stage'] not in failing_stages:
            continue
        if not r['pile']:
            if r['stage'] in ('untranslated', 'nyi', 'hard-failure', 'error'):
                unassigned[(r['stage'], r['construct'], r['message'][:60])] += 1
            continue
        g = groups[r['pile']]
        g[0] += 1
        g[1].add(r['name'])
        if r['reachable']:
            g[2].add(r['name'])
        g[3][r['stage']] += 1
    print(f'run {run}')
    print(f"{'pile':66} {'items':>6} {'defs':>5} {'reach':>5}  stages")
    for pile in sorted(groups):
        n, defs, reach, stages = groups[pile]
        print(f'{pile:66} {n:6} {len(defs):5} {len(reach):5}  {dict(stages)}')
    if unassigned:
        print('\nnot assigned to a pile:')
        for key, n in sorted(unassigned.items(), key=lambda kv: -kv[1]):
            print(f'  {n:5}  {key}')
    print(f'\nwrote {out}')


if __name__ == '__main__':
    main()
