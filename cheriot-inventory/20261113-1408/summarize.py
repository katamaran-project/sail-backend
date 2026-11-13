#!/usr/bin/env python3
"""Summarise items-reachable.csv per construct: items, definitions, and definitions reachable from step.
Usage: summarize.py <run> [stage ...]"""
import csv, collections, sys
run = sys.argv[1]
stages = sys.argv[2:] or ['untranslated', 'hard-failure', 'nyi']
rows = [r for r in csv.DictReader(open('items-reachable.csv')) if r['run'] == run and r['stage'] in stages]
groups = collections.defaultdict(lambda: [0, set(), set()])
for r in rows:
    key = r['construct'] if not r['construct'].endswith(': _') else r['construct'] + ' ' + r['message'][:40]
    g = groups[key]; g[0] += 1; g[1].add(r['name'])
    if r['reachable']: g[2].add(r['name'])
print(f"{'items':>5} {'defs':>5} {'reach':>5}  construct / reachable examples")
for key, (n, defs, reach) in sorted(groups.items(), key=lambda kv: (-len(kv[1][2]), -len(kv[1][1]))):
    print(f'{n:5} {len(defs):5} {len(reach):5}  {key[:70]}  {sorted(reach)[:8]}')
print(f'total: {len(rows)} items, {len({r["name"] for r in rows})} defs, {len({r["name"] for r in rows if r["reachable"]})} reachable defs')
